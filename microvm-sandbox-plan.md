# Implement Cloud Hypervisor Agent Sandboxes in `maxhbr/myconfig`

## Objective
Add a secure, reproducible microVM execution tier for autonomous coding agents to this NixOS configuration (github.com/maxhbr/myconfig). Use: microvm.nix, Cloud Hypervisor, KVM, NixOS guests, Workmux as the primary UI, disposable standalone Git clones, a dedicated private bridge, and the existing host LiteLLM proxy as the model API credential boundary. Treat every agent process and guest workload as potentially hostile. Secure default prioritizes isolation over convenience.

## Required outcome
User launches an isolated agent session via Workmux, e.g. `workmux add --agent microvm-claude feature-name` or `workmux add --agent microvm-pi feature-name`. Workflow: create/select disposable standalone Git clone; allocate available microVM slot; mount only that clone at /workspace; start NixOS guest via Cloud Hypervisor; run selected agent as unprivileged guest user; allow model API access through host LiteLLM proxy; prevent access to host credentials/sockets/home/LAN/VPN/unrelated repos; preserve workspace changes after guest stops; destroy all other guest state; allow host user to inspect/export/merge changes manually. Do not auto merge/commit/push/delete.

## Security model
Additional isolation tier (does not replace host-user or process-jail tiers). Hierarchy: normal agent wrapper -> process jail -> dedicated host agent user -> Cloud Hypervisor microVM. Guest must NOT access: host home, original repo checkout, other Workmux worktrees, host SSH keys, SSH/GPG agent sockets, password stores, browser profiles, cloud creds, GitHub/GitLab creds, k8s creds, signing keys, Docker/Podman sockets, D-Bus/systemd sockets, host Nix daemon socket, writable host Nix store, arbitrary host services, host LAN, WireGuard peers, other VPNs, cloud metadata, other agent microVMs. Only writable host-backed path: standalone clone at /workspace.

## Architecture
Host f13: existing Workmux + LiteLLM proxy on 127.0.0.1:4000; microvm.nix host module; Cloud Hypervisor; KVM; private bridge agentbr0 (host 192.168.83.1/24, TAP per slot, bridge-only LiteLLM forwarding endpoint); fixed pool of slots agent-0..agent-3; per-task standalone clones mounted into one slot at /workspace. Guest: Cloud Hypervisor, declarative read-only base, ephemeral non-workspace state, guest-local unprivileged user agent, writable /workspace virtiofs, no host Nix daemon, no host creds, no general LAN, model API via 192.168.83.1:4000.

## Implementation strategy
Fixed pool of declaratively configured VM slots (agent-0..agent-3). No runtime-generated Nix config names. Fixed slots give stable unit names, MACs, IPs, predictable firewall rules, concurrency limits, declarative defs, easy testing/cleanup. Workmux task name may be arbitrary; slot name need not match.

## Expected file layout
Add: modules/myconfig.ai/myconfig.ai.microvm/{default.nix,guest.nix,network.nix,launcher.nix,workmux.nix}. Modify: modules/myconfig.ai/default.nix, hosts/host.f13/ai.f13.nix. Add docs/agent-microvm.md. Only modify flake.nix if an exported module/package is genuinely required. Do not add/replace the microvm.nix input.

## 1. Register new AI module
Add module to modules/myconfig.ai/default.nix using existing import style. Expose namespace myconfig.ai.microvm, disabled by default. Suggested interface: enable=false; slotCount=4; defaultVcpu=4; defaultMemoryMiB=8192; bridgeName=agentbr0; subnet=192.168.83.0/24; gatewayAddress=192.168.83.1; litellmPort=4000; workspaceRoot=/var/lib/agent-microvms/workspaces; runtimeRoot=/var/lib/agent-microvms; stateRoot=/var/lib/microvms; allowPublicInternet=false; allowInterVmTraffic=false; allowPrivateNetworks=false; enableSsh=true; sshPublicKeyFile=null. Assertions: slotCount>0; positive CPU/mem; absolute workspaceRoot/runtimeRoot; non-empty bridge name; SSH requires explicit public key; private network / inter-VM / public internet disabled by default.

## 2. Import microvm.nix host module
When enabled, import inputs.microvm.nixosModules.host via repo's flake-input mechanism. Provides /var/lib/microvms, microvm@<name>.service, TAP handling, virtiofsd, Cloud Hypervisor. Enable only on f13, not globally.

## 3. Enable on f13
Modify hosts/host.f13/ai.f13.nix: myconfig.ai.microvm { enable=true; slotCount=4; defaultVcpu=4; defaultMemoryMiB=8192; allowPublicInternet=false; allowPrivateNetworks=false; allowInterVmTraffic=false; sshPublicKeyFile=./dedicated-agent-vm-key.pub; }. Dedicated key, not host authorized_keys. Not enabled via broad myconfig.ai.enable.

## 4. Fixed VM slot pool
One declarative microVM per slot agent-0..agent-3. Each: deterministic name, hostname, locally-administered MAC, private IPv4, dedicated TAP, own runtime dir, workspace mount source, lock file, systemd service, console/SSH target. Allocation: agent-0 02:00:00:83:00:10 192.168.83.10 ... agent-3 02:00:00:83:00:13 192.168.83.13. Generate from slot index. No random addressing.

## 5. Minimal guest module
Independent of host config. Do NOT import: desktop, Home Manager profiles, host networking, WireGuard, password-store, host secret mgmt, deployment, host agent-user, container runtime, device-specific, graphical. Import only inputs.microvm.nixosModules.microvm + minimal guest config. microvm.hypervisor=cloud-hypervisor; vcpu=4; mem=8192 (overridable). Disable graphics, serial console + optional SSH. networking.hostName=slotName. Current stateVersion.

## 6. Guest user
users.users.agent: isNormalUser; uid=1000; home=/home/agent; createHome=true; extraGroups=[]; hashedPassword="!". Not root. Do not reuse host agent/assistant/offline identities. No wheel/docker/podman/libvirtd/kvm/host groups. Prefer no sudo.

## 7. Guest packages
Minimal set: bash coreutils curl diffutils fd file findutils git gnugrep gnumake gnused jq less openssh patch procps ripgrep rsync tree unzip which. Add agent binaries via repo package defs: Claude, Pi, Codex, OpenCode. No Docker/Podman. No runtime CLI download. No host Nix daemon.

## 8. Guest Nix model
Immutable guest by default. All packages built into guest config. No host Nix daemon/socket/writable store. Guest state outside /workspace disposable. Optional guest-local Nix daemon later.

## 9. Workspace model
Do not mount primary checkout or linked git worktree. Use standalone per-task clone: git clone --no-local /path/to/source /var/lib/agent-microvms/workspaces/<task>. Standalone .git/objects/refs/index/config/worktree. Original repo not visible in guest.

## 10. Workspace mounting
Slot references stable relative workspace path in microVM state dir (/var/lib/microvms/agent-0/workspace). virtiofs share: proto=virtiofs; tag=workspace; source=workspace; mountPoint=/workspace; readOnly=false. Before launch bind mount clone into slot workspace source. No symlink boundary. Guest sees exactly one writable host-backed mount /workspace. Do not mount / /home /etc /run /dev /tmp /nix /var/lib or any host socket.

## 11. UID/GID handling
Document virtiofs ownership strategy. Guest agent can rw /workspace; host user can manage files; no weakened perms elsewhere; guest IDs don't map to privileged host IDs. Options: dedicated host sandbox UID matching guest UID; explicit virtiofs UID/GID translation; controlled staging dir. Runtime test for host ownership.

## 12. Private VM bridge
NetworkManager-compatible (do not migrate to systemd-networkd). Bridge agentbr0, subnet 192.168.83.0/24, host 192.168.83.1/24. Attach each slot TAP. NM manages bridge or ignores TAPs in controlled way. Firewall policy before bridge is safe (forwarding already enabled).

## 13. Default network policy
No general internet. Default profile permits only guest -> 192.168.83.1:4000. Block host services on other ports, host LAN, WireGuard peers, VPN ifaces, private IPv4, link-local, multicast, other guests, IPv6 (unless filtered), cloud metadata. Block forwarding to 10/8, 100.64/10, 127/8, 169.254/16, 172.16/12, 192.168/16, 224/4, 240/4. Narrow exception 192.168.83.1:4000. Explicitly block 169.254.169.254. No TAP-to-TAP forwarding. No guest-to-guest.

## 14. Firewall implementation
Follow existing NixOS firewall architecture (no nftables migration). NixOS firewall options + narrow extraCommands, extraStopCommands cleanup, rules scoped to bridge/subnet/TAP. Idempotent. Dedicated chains AGENT_MICROVM_INPUT/FORWARD/OUTPUT. Test packet path, not just eval.

## 15. IPv6
Disable IPv6 on agent bridge + guest interfaces unless equivalent IPv6 policy done+tested. Document as MVP limitation.

## 16. LiteLLM bridge endpoint
Keep main LiteLLM on 127.0.0.1:4000. Add bridge-only forwarding 192.168.83.1:4000 -> 127.0.0.1:4000 via systemd-socket-proxyd. systemd.sockets.agent-litellm-proxy listenStreams=192.168.83.1:4000 Accept=false; service ExecStart=systemd-socket-proxyd 127.0.0.1:4000, DynamicUser, NoNewPrivileges, PrivateTmp, ProtectSystem=strict, ProtectHome. Listen only on bridge address; not 0.0.0.0/LAN.

## 17. Guest model API config
OPENAI_BASE_URL=http://192.168.83.1:4000/v1; OPENAI_API_KEY=not-needed placeholder. Do not forward real upstream key. No secrets in Nix store/flake/scripts/argv/logs/images/workspaces. Guest must not receive SSH_AUTH_SOCK, GPG_AGENT_INFO, AWS_*, GOOGLE_*, AZURE_*, KUBECONFIG, GITHUB_TOKEN, GH_TOKEN, GITLAB_TOKEN.

## 18. SSH access
SSH only on private guest iface. PermitRootLogin=no; PasswordAuthentication=false; KbdInteractiveAuthentication=false; AllowAgentForwarding=false; X11Forwarding=false; PermitTunnel=false; AllowTcpForwarding=false. One dedicated public key for agent user. No host authorized_keys. No agent forwarding. Guest SSH not reachable from LAN.

## 19. Guest agent entry point
Command agent-run: refuse root; verify /workspace mounted+writable; cd /workspace; print hostname + agent; exec agent as argv; no eval; return exit status. e.g. agent-run claude / agent-run pi. No global dangerous flags; YOLO only explicit in Workmux agent def.

## 20. Host launcher
Nix package agent-microvm with subcommands run/stop/status/ssh/console/destroy/list. Bash set -euo pipefail, no eval, quote paths, flock for slot allocation.

## 21. Slot allocation
Lock global allocator; inspect slots; select unused; acquire slot lock; release allocator; create task runtime dir; create standalone clone; bind mount; start VM; wait readiness; launch agent; hold slot lock for session; cleanup on exit/interrupt. Locks /run/agent-microvms/allocator.lock and per-slot locks. No double-allocation.

## 22. Task names
Validate strictly [a-zA-Z0-9._-]. Reject empty, /, .., whitespace-only, shell metachars, control chars, absolute paths, leading -, over-long. Use only as dir/metadata label; never interpolate into Nix code.

## 23. Source repo validation
git rev-parse --show-toplevel. Validate: is git repo; absolute canonical path; exists; not /; not host home; not inside agent runtime root; not symlink escaping; not a microVM workspace. Use realpath. Do not trust $PWD.

## 24. Standalone clone creation
Under /var/lib/agent-microvms/workspaces/<task-id>. git clone --no-local. Optional task branch. No hard-linked clones, no alternates. Verify git-dir and git-common-dir resolve inside workspace; abort if outside.

## 25. Branch and result handling
Task branch agent/<task-name>. No auto push/merge. At end print inspect/import commands: git diff > changes.patch; git format-patch base..HEAD; from original repo git fetch "$workspace" agent/<task>:refs/heads/agent/<task>. Helper allowed but import explicit.

## 26. Bind-mount lifecycle
Before start: mkdir -p /var/lib/microvms/$slot/workspace; mount --bind $workspace ...; findmnt verify. Cleanup: stop VM; confirm CH exited; unmount; remove slot transient files; release lock. Do not auto-delete clone on stop/destroy. Separate explicit workspace-remove command. Refuse cleanup with uncommitted changes unless destructive flag.

## 27. VM lifecycle
Use microvm.nix service model microvm@agent-0.service. systemctl start/stop/status. No untracked background CH process. systemd for supervision/logging/resource/restart/cleanup/ordering.

## 28. Resource controls
Fixed vCPU/mem; max slot count; no unlimited VM creation; bounded open files/procs; clean shutdown timeout; auto-kill after timeout; no restart loop. systemd hardening on CH+virtiofsd where compatible (NoNewPrivileges, PrivateTmp, ProtectHome, ProtectSystem=strict, RestrictSUIDSGID, LockPersonality) but not where it blocks /dev/kvm, TAP, virtiofs sockets, VM disks, runtime dirs. Document exceptions.

## 29. Workmux integration
Add agent entries microvm-claude/microvm-pi/microvm-codex/microvm-opencode to existing Workmux registry. Each invokes host launcher with repo path, task name, agent binary, prompt/context, secure network profile. User workflow stays workmux add --agent microvm-claude feature-name. Do not replace Workmux; reuse status/pane/task/cleanup. Launcher is backend not frontend.

## 30. Interaction with existing jails
Do not call process-jail wrapper from microVM launcher unless documented defense-in-depth. Do not inherit jail defaults forwarding OPENAI_API_KEY or broad bind mounts. Independent security profile. Keep existing jails.

## 31. Network profiles
Design for profiles. Initial: proxy-only (permit 192.168.83.1:4000, SSH response, block everything else). Later: offline, package-access, internet. Do not make internet the secure default.

## 32. DNS
proxy-only needs no general DNS if LiteLLM accessed by IP. Avoid exposing host resolver. Later profiles use controlled resolver, block internal names/addresses.

## 33. Logging
systemd journal for CH, virtiofsd, launchers, LiteLLM bridge, network setup failures, agent exit status. journalctl -u microvm@agent-0.service etc. Do not log secrets/env dumps/prompts/source/keys/tokens/passwords. Task logs in workspace only if explicitly enabled.

## 34. Status command
Reports slot, service state, IP, MAC, task, workspace path, bind-mount status, agent type, start time, SSH readiness, guest readiness, lock owner. No secrets.

## 35. Destroy semantics
Stop: keeps workspace/metadata/git/logs. Destroy: removes guest ephemeral runtime, slot transient state, bind mount, VM process state; must NOT delete workspace/git/patches. Remove workspace: separate explicit op; detect uncommitted changes and unexported commits; require confirmation/destructive flag.

## 36. Documentation
docs/agent-microvm.md covering activation, launching via Workmux, listing, status, connecting, logs, stopping, destroying, inspecting changes, importing branch, removing workspace, security properties, limitations.

## 37. Nix assertions
Invalid slot counts; duplicate IPs/MACs; invalid bridge name/gateway; missing SSH key; unsafe workspace roots; public internet enabled by default; private-network/inter-VM without override. Launcher also does runtime validation for dynamic names/paths.

## 38. Automated tests
Module evaluates with defaults; f13 evaluates; test-f13 builds/evaluates with module disabled; unique slot IPs/MACs; guest config builds; host config builds; launcher builds; shell scripts pass shellcheck. Separate eval tests from hardware KVM tests. Do not claim KVM runtime success from CI eval-only.

## 39. Repository validation commands
./nixfmtall.sh --check; nix flake check; nix build .#nixosConfigurations.test-f13.config.system.build.toplevel; and f13 build when private inputs available. Build guest closure + launcher explicitly. Record commands + results.

## 40. Runtime host validation
test -r /dev/kvm; CH can open KVM; ip address show agentbr0; ss -ltnp | grep 192.168.83.1:4000; systemctl status microvm@agent-0.service; findmnt /var/lib/microvms/agent-0/workspace; TAP belongs to bridge; no unexpected forwarding.

## 41. Guest validation
id; hostname; findmnt /workspace; test -w /workspace; ip address; ip route. Agent runs unprivileged. Guest can reach http://192.168.83.1:4000/v1. Guest cannot reach 192.168.83.1 other ports, other slot IPs, host LAN, WireGuard, default router, 169.254.169.254, public internet, host Nix daemon socket, host SSH/GPG sockets, container sockets, host home, original repo. Workspace files have intended host ownership.

## 42. Persistence test
Create files in /workspace, /tmp, /home/agent; stop; destroy; restart same workspace. Expect /workspace file remains, /tmp gone, /home/agent gone unless documented persistent home. Secure default persists only workspace.

## 43. Negative launcher tests
Reject run --repository / ; --repository $HOME ; --name ../bad ; --name /tmp/bad ; duplicate name. Concurrent duplicate: only one succeeds. Symlinks cannot escape roots. Interrupted launch cleans up VM/bind mount/lock/metadata/TAP but not workspace.

## 44. Network negative tests
From guest record failure to 169.254.169.254, 10.0.0.1, 172.16.0.1, 192.168.0.1, other slot IPs, host SSH, host Nix daemon, host dev services, WireGuard peers, public internet. Use real host config, not just test-f13.

## 45. Security acceptance criteria
Full checklist: reuse microvm.nix input; import host module only when enabled; enable only on f13; Cloud Hypervisor; guest own kernel; fixed bounded slot pool; unique deterministic MAC/IP per slot; disposable root/home; only /workspace persists; standalone clone; original repo hidden; no shared git common dir; virtiofs workspace; bind mount not symlink; non-root agent; root SSH disabled; password SSH disabled; agent forwarding disabled; no host home mount; no host creds; no Nix daemon socket; no Docker/Podman sockets; Nix store not writable; LiteLLM loopback-only; bridge-only endpoint; no upstream key in guest; no public internet default; no host LAN; no WireGuard; no metadata; no guest-to-guest; bounded CPU/mem; bounded VM count; lock-protected allocation; stop/destroy keep workspace; Workmux remains UI; jails still work; formatting passes; nix flake check passes; test-f13 builds; guest config builds; at least one VM tested on real KVM; firewall tested from guest.

## 46. Non-goals
Do not add another microvm.nix input; replace NetworkManager; migrate firewall backend; rewrite module hierarchy; remove agent-user/jail support; mount linked worktree; expose shared git metadata; mount host home; forward SSH/GPG agents; forward real API keys; expose container sockets; expose Nix daemon; make Nix store writable; unrestricted LAN; public internet by default; inter-VM by default; generate Nix from task names; auto push/merge/delete; claim build-only proves runtime firewall secure.

## 47. Deliverables
myconfig.ai.microvm module; minimal guest module; declarative CH slot definitions; NM-compatible bridge; explicit firewall/forwarding; bridge-only LiteLLM forwarder; safe slot allocator/launcher; standalone clone creation+validation; bind-mount lifecycle; Workmux agent registrations; SSH/console access; guest agent execution tooling; documentation; Nix assertions; eval/build tests; runtime validation results; written security assessment.

## 48. Final report format
Sections: Files changed; Architecture; Workmux integration; Security boundary; Validation performed (separate eval/build/runtime KVM/network/negative; only claim actually-executed); Remaining limitations (CH/KVM/guest kernel/virtiofs attack surface, writable workspace exposure, prompt/source disclosure to API endpoint, resource exhaustion, DoS on host proxy, future public-internet risks, firewall-ordering-dependent controls, untested controls, test-f13 vs private f13 differences).
