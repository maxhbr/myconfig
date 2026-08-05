# Lightweight `myconfig.ai.microvm` Implementation Plan

## Implementation status

| Phase | Status | Notes |
| --- | --- | --- |
| 0 — baseline and measurement | partially done | Behaviour-preserving refactors are verified with the repo's `nix eval` snapshot/diff workflow (`AGENTS.md`) instead of a bespoke benchmark harness; a machine-readable runtime benchmark (closure size, launch latency, process counts) is **not** implemented — it needs a KVM host and belongs with the out-of-CI runtime-validation tier. |
| 1 — opt-in lightweight profile | **done** | `myconfig.ai.microvm.profile = "full" \| "lite"`, table in `../profiles.nix`, wired in `default.nix` (`_module.args.agentProfile`) + `guest.nix`. Test: `checks.microvm-eval-lite-profile`. |
| 2 — build only selected agents | **done** | `myconfig.ai.microvm.enabledAgents` (plus the `lite` profile default `[ "codex" ]`). The selection is applied ONCE in `../agents.nix`, so guest closure, `agent-run`, batch dispatch, launcher validation/help, workmux registrations and agent-state paths all follow. Test: `checks.microvm-eval-enabled-agents`. |
| 3 — runtime config staging | not started | |
| 4 — consolidate writable shares | not started | |
| 5 — split interactive/batch | not started | |
| 6 — VSOCK transport | not started | |
| 7 — clone/startup optimisation | partially done | Clone: `git clone --local --no-hardlinks` with a `--no-local` fallback plus an explicit `objects/info/alternates` check (≈10× faster on this repo: 0.6 s vs 5 s, measured by hand — both variants produce a fully independent clone). Readiness: exponential-backoff SSH polling (250 ms → 2 s) under the unchanged 90 s ceiling, replacing the fixed 3 s interval. NOT done: readiness as a positive protocol signal (needs phases 3/6) and the install-unit generation guard (see deviations). |
| 8 — minimize guest closure | **done** | The `lite` profile builds the documented minimal CLI toolset, a plain bash login shell (no fish) and drops NixOS' `environment.defaultPackages`; per-agent `extraPackages` in the registry keeps agent-specific runtimes tied to the selection. Test: the phase-8 assertions of `checks.microvm-eval-lite-profile`. |
| 9 — testing | incremental | Each landed phase adds eval checks to `tests/microvm.nix`; the VM/adversarial tiers of this phase remain out of CI (see `docs/agent-microvm-runtime-validation.md`). |
| 10 — documentation and rollout | incremental | `docs/agent-microvm.md` documents every landed option. |

### Recorded deviations

- **Phase 1, `persistentAgentState.enable = false`**: there is no such option.
  Agent-state persistence is already opt-in *per run*
  (`agent-microvm run|submit --persist-agent-state`, see `../state.nix`), i.e.
  the plan's intended lite default is the existing behaviour of every profile.
  Nothing to configure.
- **Phase 1, `networkProfile = "proxy-only"`**: already the module-wide secure
  default, so the `lite` profile does not restate it.
- **Phase 2, per-agent registry restructuring**: the registry already has the
  shape the plan asks for (`package`, `executable`, `interactiveArgs`,
  `batchArgs`, `guestEnvironment`, `persistentState`), so only the *selection*
  was added. `configPaths` / `statePaths` / `extraPackages` are deferred to the
  phases that need them (3 and 8).
- **Phase 2, closure measurement**: recorded as a *structural* check
  (`microvm-eval-enabled-agents` asserts the deselected agents' store paths are
  absent from the guest `environment.systemPackages`) rather than as a byte
  count, for the same reason phase 0's benchmark is deferred.
- **Phase 8, "every package has a documented consumer"**: documented as a
  per-package rationale comment above `guestMinimalPackages` in `../guest.nix`,
  and locked down by an eval check listing the same set. NixOS' own
  `requiredPackages` (coreutils-full, curl, openssh, which, …) is load-bearing
  for a bootable system and is therefore neither removed nor asserted absent;
  only the module's discretionary additions and
  `environment.defaultPackages` are minimized.
- **Phase 8, closure-size regression check**: expressed structurally (asserted
  package membership) rather than as a byte budget — a size assertion would
  need a KVM/build tier and would churn with every nixpkgs bump.
- **Phase 7, install-unit restart guard**: NOT implemented. Comparing
  `<stateRoot>/<slot>/current` with the expected runner path would require the
  launcher (a host `systemPackages` entry, reachable from the host home-manager
  config) to reference every guest's `declaredRunner`, and the guest config in
  turn copies already-evaluated host home-manager file entries
  (`guest-home.nix`) — a plausible infinite-recursion loop. The current
  unconditional `systemctl restart install-microvm-<slot>.service` is an
  idempotent symlink relink costing milliseconds, so the guard is deferred
  until phase 3 removes the host→guest home-manager coupling.
- **Phase 7, readiness definition**: the extended readiness criteria
  (config staging finished, proxy forwarding healthy, agent executable present)
  presuppose phases 3 and 6; only the polling *strategy* was changed here.
- **Phase 1, store pinning**: `microvm.optimize.enable` and
  `microvm.storeDiskType` currently *default* to `true` / `erofs` upstream, so
  pinning them is behaviour-preserving today. It is done anyway (for `lite`
  only) so an upstream default change cannot silently deoptimise the
  lightweight guest.

## Objective

Refactor `modules/myconfig.ai/myconfig.ai.microvm` into a lighter-weight execution environment for untrusted coding agents while preserving the important security properties of the current design:

- separate guest kernel through Cloud Hypervisor/KVM;
- disposable guest root and home;
- no direct access to the host home directory;
- no host Nix daemon socket;
- no Docker, Podman, SSH-agent, GPG-agent, browser, password-manager, or cloud-provider sockets;
- standalone disposable repository clones;
- host-side resource limits;
- proxy-only model access by default;
- no general internet, LAN, VPN, metadata-service, DNS, or inter-VM access in the secure profile;
- host-owned batch control data must remain inaccessible to the unprivileged guest agent.

The intended end state is a small, prebuilt MicroVM profile suitable for interactive YOLO-mode coding-agent sessions.

## Target architecture

Implement a lightweight profile with the following default shape:

```text
Cloud Hypervisor/KVM
├── 1 prebuilt slot
├── 2 vCPU
├── 4 GiB RAM
├── separate EROFS guest Nix store
├── one selected agent package
├── minimal generic CLI toolset
├── disposable root filesystem
├── tmpfs/disposable agent home
├── one writable per-session virtiofs share
│   ├── workspace/
│   ├── config-seed/
│   ├── worker/
│   ├── controller/
│   └── state/            # optional
├── optional read-only SSH-host-key share
├── runtime-staged, allowlisted host agent configuration
└── VSOCK-only access to the host LiteLLM proxy
```

Do not change the existing full-featured behavior without an explicit compatibility decision. Prefer introducing a new profile or opt-in options first, then changing defaults only after validation.

---

## Required security invariants

Treat these as non-negotiable acceptance criteria throughout the work.

1. **No live host-home mount**

   The guest must never receive a mount of `$HOME`, `~/.config`, or another broad host directory. Only explicitly allowlisted configuration paths may be staged.

2. **No host control sockets**

   Never expose:

   - `/nix/var/nix/daemon-socket`;
   - Docker or Podman sockets;
   - `$SSH_AUTH_SOCK`;
   - GPG-agent sockets;
   - password-manager sockets;
   - browser profiles or browser debugging sockets;
   - cloud-provider credential directories;
   - Kubernetes configuration;
   - the host D-Bus session socket.

3. **No host Nix store by default**

   Keep the separate guest store as the secure default. A read-only host-store mode may be added only as an explicitly documented, weaker optional profile.

4. **Disposable mutations**

   Agent writes to its home and configuration must disappear when the session is destroyed unless persistence is explicitly enabled for a narrowly defined state directory.

5. **Independent repository clone**

   The guest workspace must not be a Git worktree linked to the original repository. It must have an independent Git object database and Git configuration.

6. **Proxy-only network remains closed**

   The lightweight secure profile must not accidentally gain ordinary IP networking, DNS, LAN access, VPN access, metadata-service access, or unrestricted internet access.

7. **Guest cannot overwrite trusted control files**

   Batch job specifications, prompts, controller state, and final-result metadata that are trusted by the host must remain root-owned and non-writable by the `agent` user.

8. **No secrets copied accidentally**

   Runtime configuration staging must be based on an explicit allowlist. It must not recursively copy an entire agent configuration directory unless each contained file is known to be safe.

---

## Phase 0 — Baseline and measurement

### Tasks

- Document the current launch path for:
  - interactive sessions;
  - batch sessions;
  - slot installation;
  - repository cloning;
  - guest boot;
  - SSH readiness;
  - model-proxy forwarding;
  - cleanup.
- Record the current values for:
  - VM closure size;
  - store image size;
  - build time from a warm Nix cache;
  - launch-to-agent-ready latency;
  - number of host processes per running slot;
  - idle RSS per running slot;
  - number of virtiofsd processes;
  - number of systemd units generated per slot;
  - number of TAP devices, firewall rules, and proxy processes.
- Add a repeatable benchmark script under the module or repository test tooling.
- Capture one baseline for a single interactive Codex session and one for a batch session.

### Suggested benchmark output

```json
{
  "profile": "current",
  "agent": "codex",
  "closure_bytes": 0,
  "store_image_bytes": 0,
  "warm_build_seconds": 0,
  "launch_to_ready_ms": 0,
  "host_process_count": 0,
  "virtiofsd_count": 0,
  "idle_rss_bytes": 0
}
```

### Acceptance criteria

- The benchmark can be run repeatedly with one command.
- Results are machine-readable.
- Later phases can be compared against the same baseline.

---

## Phase 1 — Add an opt-in lightweight profile — **DONE**

Implemented as `../profiles.nix` (the authoritative profile table) plus the
`myconfig.ai.microvm.profile` option in `../default.nix`, which resolves the
profile ONCE and hands it to `../guest.nix` via `_module.args.agentProfile`.
Acceptance criteria are locked down by `checks.microvm-eval-lite-profile`
(`tests/microvm.nix`), which also asserts the negative half: the profile
default stays `full`, an explicit `resourceClasses` outranks the profile table,
and `lite` + the deprecated slot options is rejected. Runtime boot of a lite
guest requires KVM and is therefore part of the out-of-CI runtime-validation
tier, not of `nix flake check`.

### Goal

Create a stable compatibility boundary before changing implementation details.

### Tasks

Add a profile option similar to:

```nix
myconfig.ai.microvm.profile = "full"; # existing behavior
myconfig.ai.microvm.profile = "lite"; # new behavior
```

The `lite` profile should initially select:

```nix
{
  resourceClasses = {
    lite = {
      count = 1;
      vcpu = 2;
      memoryMiB = 4096;
    };
  };

  networkProfile = "proxy-only";
  persistentAgentState.enable = false;
}
```

Keep explicit user options higher priority than profile defaults.

Pin and document:

```nix
microvm.optimize.enable = true;
microvm.storeDiskType = "erofs";
```

### Files likely affected

- `default.nix`
- option declarations
- slot/resource-class generation
- module documentation

### Acceptance criteria

- Existing configurations retain current behavior under `profile = "full"`.
- `profile = "lite"` generates exactly one slot unless overridden.
- The lite profile boots and runs an agent before any deeper refactoring begins.
- Evaluation includes assertions for incompatible combinations.

---

## Phase 2 — Build only selected agents — **DONE**

Implemented as the `enabledNames` argument of `../agents.nix` (the selection is
applied at the single source of truth, so no consumer carries its own filter)
plus the `myconfig.ai.microvm.enabledAgents` option resolved in
`../default.nix`. Unknown tokens, an empty selection and a selection with no
batch-capable agent are rejected by module assertions. The `lite` profile
defaults to `[ "codex" ]`; `full` keeps every declared agent, so existing hosts
are unaffected (verified with the evaluated-slice diff from `AGENTS.md`).

### Goal

Remove unused agent runtimes and configuration from the guest closure.

### Tasks

Add an option:

```nix
myconfig.ai.microvm.enabledAgents = [ "codex" ];
```

Filter all agent-dependent behavior through this option:

- packages added to `environment.systemPackages`;
- `agent-run` command dispatch;
- generated wrapper scripts;
- guest configuration files;
- Home Manager or config-staging paths;
- Workmux registrations;
- state directories;
- shell completion or environment initialization;
- tests.

Reject unknown agent names during Nix evaluation.

Prefer a registry structure in which each agent defines:

```nix
{
  package = ...;
  executable = "...";
  configPaths = [ ... ];
  statePaths = [ ... ];
  extraPackages = [ ... ];
  environment = { ... };
}
```

### Acceptance criteria

- A Codex-only image does not contain packages for Pi, Qwen, OpenCode, or other disabled agents.
- Disabled agent commands fail with a clear error.
- Closure size is measured before and after this change.
- The full profile can still enable all supported agents.

---

## Phase 3 — Replace guest Home Manager activation with runtime configuration staging

### Goal

Use the current host agent configuration at launch time without mounting it live and without rebuilding the VM when instructions or settings change.

### Design

The host launcher copies only allowlisted configuration into a per-session staging directory. The existing writable session share exposes the staged copy to the guest. A guest oneshot service copies it into the disposable agent home before the agent or SSH session starts.

```text
host allowlisted config
        │
        │ copy with symlink dereferencing
        ▼
slot/session/config-seed
        │
        │ virtiofs
        ▼
/run/agent-session/config-seed
        │
        │ root-owned oneshot copy
        ▼
/home/agent
```

### Tasks

Add per-agent config declarations to the registry. Example:

```nix
configPaths = [
  ".codex/config.toml"
  ".codex/skills"
  ".agents/skills"
  ".gitconfig"
];
```

Implement host-side staging with these properties:

- paths are resolved relative to the configured host home;
- missing optional paths are allowed;
- paths cannot escape the host home through `..`;
- symlinks into `/nix/store` are dereferenced;
- symlinks to locations outside approved roots are rejected or copied only under an explicit policy;
- staged files are owned by root;
- no sockets, devices, FIFOs, or setuid files are copied;
- secrets are excluded by default;
- the destination is cleaned before every launch;
- a manifest records which paths were staged.

Use a safe copying mechanism such as `rsync --archive --copy-links` plus explicit validation. Do not shell-expand untrusted paths.

Add a guest service similar to:

```nix
systemd.services.seed-agent-home = {
  wantedBy = [ "multi-user.target" ];
  before = [
    "sshd.service"
    "agent-job-controller.service"
  ];
  after = [ "local-fs.target" ];

  serviceConfig = {
    Type = "oneshot";
    NoNewPrivileges = true;
    PrivateTmp = true;
    ProtectSystem = "strict";
    ProtectHome = false;
  };

  script = ''
    install -d -m 0700 -o agent -g users /home/agent

    if test -d /run/agent-session/config-seed; then
      ${pkgs.rsync}/bin/rsync \
        --archive \
        --chmod=Du=rwx,Dgo=,Fu=rw,Fgo= \
        /run/agent-session/config-seed/ \
        /home/agent/

      chown -R agent:users /home/agent
    fi
  '';
};
```

After this works, remove guest Home Manager activation from the lite profile. Retain the old path for the full profile until compatibility is proven.

### Secret-handling policy

Create an explicit denylist for common credential material, including:

```text
auth.json
credentials.json
tokens.json
*.pem
*.key
id_rsa
id_ed25519
cookies*
session*
```

Do not rely only on filename matching. Prefer a positive allowlist of exact files and directories.

Model-provider credentials should remain in the host proxy. The guest should receive only the proxy endpoint and any non-sensitive routing metadata required to use it.

### Acceptance criteria

- Editing an allowlisted host instruction or configuration file affects the next VM launch without rebuilding the guest.
- Agent changes to `/home/agent` do not modify host files.
- The staged configuration contains no sockets, device nodes, or unexpected secrets.
- The guest no longer runs Home Manager activation in the lite profile.
- Boot and closure measurements show the effect of removing guest Home Manager.

---

## Phase 4 — Consolidate writable virtiofs shares

### Goal

Reduce virtiofsd process count and simplify mount management.

### Target session tree

```text
<slot-session>/
├── workspace/
├── config-seed/
├── input/
├── controller/
├── worker/
└── state/
```

Expose it as a single writable share:

```text
/run/agent-session
```

Bind mount or symlink:

```text
/run/agent-session/workspace -> /workspace
```

Use permissions to preserve trust boundaries:

```text
slot-session/       root:root  0755
workspace/          agent      0700
config-seed/        root:root  0555
input/              root:root  0555
input/spec          root:root  0400
input/prompt        root:root  0444
controller/         root:root  0700
worker/             agent      0700
state/              agent      0700
```

Keep SSH private host keys in a separate read-only share. Do not place them in the writable session tree.

### Tasks

- Replace separate workspace, job, state, and config shares with one session share in the lite profile.
- Update guest mount declarations.
- Update launcher path handling.
- Update cleanup logic.
- Update batch controller and worker paths.
- Add ownership and mode assertions before VM launch.
- Ensure a malicious workspace symlink cannot redirect writes into `controller/`, `input/`, or `config-seed/`.

### Acceptance criteria

- Lite interactive mode uses one writable virtiofs share plus at most one read-only SSH-key share.
- Batch trust boundaries remain intact.
- The agent cannot modify root-owned input or controller data.
- Cleanup removes the complete per-session tree reliably.
- No path traversal or symlink escape is possible during host writes.

---

## Phase 5 — Separate interactive and batch capabilities

### Goal

Avoid carrying both execution modes in every lite image.

### Proposed option

```nix
myconfig.ai.microvm.mode = "interactive";
myconfig.ai.microvm.mode = "batch";
myconfig.ai.microvm.mode = "combined"; # compatibility/full profile
```

### Interactive mode includes

- SSH server and readiness;
- workspace;
- configuration staging;
- optional persistent state;
- agent launcher.

### Interactive mode excludes

- batch controller;
- batch worker protocol;
- batch input/result handling;
- job-specific systemd units.

### Batch mode includes

- workspace;
- configuration staging;
- root-owned job input;
- unprivileged worker;
- trusted result controller;
- deterministic exit/result handling.

### Batch mode excludes

- SSH server;
- SSH host-key share;
- known-hosts generation;
- SSH polling;
- interactive shell support unless explicitly enabled.

### Acceptance criteria

- Interactive-lite has no batch services or job protocol files.
- Batch-lite has no SSH daemon, SSH host keys, or SSH readiness polling.
- Combined mode preserves current behavior.
- Nix assertions reject options that have no meaning in the selected mode.

---

## Phase 6 — Add a VSOCK transport for proxy-only mode

### Goal

Eliminate TAP devices, bridge management, firewall rules, guest IP configuration, DNS configuration, and TCP bridge forwarders from the secure proxy-only profile.

### Design

```text
guest agent
  -> 127.0.0.1:<guest-proxy-port>
  -> guest TCP-to-VSOCK forwarder
  -> host CID 2
  -> host VSOCK-to-TCP forwarder
  -> host LiteLLM loopback listener
```

Use the deterministic per-slot VSOCK CID already present in slot metadata.

### Tasks

- Add a transport option:

```nix
myconfig.ai.microvm.transport = "tap";
myconfig.ai.microvm.transport = "vsock";
```

- Allow `vsock` only with `networkProfile = "proxy-only"` or a fully offline profile.
- Implement host and guest forwarding with a small, auditable tool.
- Bind the host TCP side only to the LiteLLM loopback endpoint.
- Ensure the guest cannot select an arbitrary host TCP destination.
- Remove or skip, for VSOCK slots:
  - TAP creation;
  - private bridge membership;
  - IP allocation;
  - guest networkd configuration;
  - DNS configuration;
  - iptables/nftables chains;
  - NetworkManager unmanaged-device declarations;
  - bridge listener services;
  - NAT and forwarding rules.
- Keep the TAP backend for profiles that intentionally permit package or internet access.
- Add a protocol-level health check.

### Security requirements

The VSOCK proxy must be destination-fixed. Do not implement a generic CONNECT proxy that lets the guest access arbitrary host ports.

The host listener must validate the expected guest CID or use one listener per slot.

### Acceptance criteria

- In proxy-only VSOCK mode, the guest has no network interface other than loopback.
- There is no TAP device or per-slot bridge/firewall configuration.
- The agent can call the LiteLLM endpoint.
- Attempts to access arbitrary host ports fail.
- Attempts to resolve DNS or reach LAN/internet addresses fail.
- Multi-slot tests prove that one slot cannot use another slot’s proxy path.

---

## Phase 7 — Optimize repository cloning and startup

### Repository clone — **DONE**

Implemented in `../launcher.nix` (`create_clone` / `verify_clone`). Deviation:
`--no-checkout` is *not* used — the guest agent needs a working tree, and the
clone is checked out exactly once either way. `--shared` / `--reference` are
additionally forbidden by a build-time check on the generated launcher.

Replace local cloning through normal Git transport with an independent optimized local clone where safe:

```bash
git clone \
  --local \
  --no-hardlinks \
  --no-checkout \
  -- "$source_repo" "$destination"
```

Requirements:

- never use `--shared`;
- verify there is no `.git/objects/info/alternates`;
- verify the Git common directory is inside the disposable clone;
- reject source repositories whose Git metadata resolves outside the expected root;
- document Git’s local-clone race behavior;
- optionally fall back to `--no-local` when the source repository is being mutated concurrently.

Measure this change independently.

### Readiness — **PARTIALLY DONE** (option 3, the interim step)

Replace fixed three-second SSH polling with one of:

1. VSOCK readiness notification;
2. a root-owned readiness file in the controller directory;
3. short exponential-backoff SSH polling as an interim step.

Do not consider the VM ready until:

- the workspace is mounted;
- configuration staging completed;
- required proxy forwarding is healthy;
- the selected agent executable exists;
- SSH is listening in interactive mode.

### Slot installation

Before restarting the slot install unit, compare the expected runner/store path with the currently installed symlink. Restart only when the generation changed.

### Acceptance criteria

- Clone remains independent from the source repository.
- Warm launch latency is lower than baseline.
- No fixed multi-second readiness delay remains.
- Repeated launches do not restart install units unnecessarily.

---

## Phase 8 — Minimize the guest package closure — **DONE** (for the lite profile)

Implemented in `../guest.nix` (`guestMinimalPackages` with a per-package
rationale, `guestShell`, `environment.defaultPackages = [ ]`) driven by the
`minimalGuestPackages` field of `../profiles.nix`, plus the registry's new
per-agent `extraPackages` so an agent's own runtime dependencies are added only
while that agent is selected. The `full` profile keeps its historical toolset
and fish login shell verbatim.

### Tasks

Define a small common package set for the lite profile:

```nix
[
  bashInteractive
  coreutils
  findutils
  git
  gnugrep
  gnused
  gawk
  patch
  ripgrep
  jq
  openssh # interactive mode only
]
```

Add packages only when required by the selected agent or repository workflow.

Review and remove:

- unused shells;
- documentation;
- locale data beyond what is needed;
- compilers and language runtimes included only for disabled agents;
- duplicate CLI tools;
- package managers not required by the target workload;
- batch utilities from interactive images;
- SSH packages from batch images.

Do not remove basic diagnostic tools needed to understand failures. Prefer keeping a deliberately small troubleshooting set.

### Acceptance criteria

- Every package in the lite image has a documented consumer.
- Agent-specific runtimes are included only when that agent is enabled.
- Interactive and batch closures differ appropriately.
- Closure-size regression checks are added.

---

## Phase 9 — Testing

### Evaluation tests

Add Nix evaluation tests for:

- unknown agent names;
- incompatible profile/mode/transport combinations;
- zero-slot configurations;
- duplicate VSOCK CIDs;
- duplicate resource identifiers;
- invalid config paths;
- attempts to stage paths outside the host home;
- persistence options used when persistence is disabled;
- VSOCK transport selected with internet access.

### VM integration tests

Test at least:

1. interactive-lite, Codex only, VSOCK transport;
2. batch-lite, Codex only, VSOCK transport;
3. full compatibility profile;
4. TAP internet/package profile if supported;
5. two concurrent lite slots.

Verify:

- agent can modify `/workspace`;
- agent cannot modify source repository;
- agent cannot modify staged host configuration;
- home changes disappear after session destruction;
- disabled agent executables are absent;
- host sockets are absent;
- host home is absent;
- host store is absent in secure mode;
- host Nix daemon is absent;
- internet, LAN, VPN, DNS, and metadata access fail in proxy-only mode;
- LiteLLM access succeeds;
- arbitrary host-port access fails;
- agent cannot modify root-owned batch control files;
- slot cleanup removes mounts, processes, and temporary state.

### Adversarial filesystem tests

From inside the guest, attempt:

- symlink replacement in the workspace;
- hardlink attacks where supported;
- path traversal through staged config;
- modification of `controller/` and `input/`;
- remount attempts;
- access to other slots;
- access to SSH private host keys;
- creation of setuid files;
- persistence through `/nix/store`, root, home, logs, and machine-id.

### Resource tests

Verify host-side enforcement of:

- memory limit;
- CPU quota;
- process/task limit;
- disk usage;
- maximum session duration if configured.

### Acceptance criteria

- Tests run in CI or through a documented local command.
- Security failures are fatal.
- Benchmarks report improvements or regressions for each major phase.
- Full-profile compatibility tests remain green.

---

## Phase 10 — Documentation and rollout

### Documentation

Document:

- the threat model;
- what the MicroVM protects;
- what it does not protect;
- config-staging rules;
- credential handling;
- full versus lite profiles;
- interactive versus batch modes;
- VSOCK versus TAP transport;
- persistence semantics;
- how to add a new agent safely;
- how to inspect staged configuration;
- how to benchmark and troubleshoot launches.

Explicitly state that any credential intentionally exposed to the guest may be exfiltrated or abused by the agent. Filesystem isolation cannot prevent misuse of credentials the process is authorized to read.

### Rollout sequence

1. Merge baseline measurement tooling.
2. Merge the lite profile with existing implementation paths.
3. Merge selected-agent filtering.
4. Merge runtime config staging behind an option.
5. Enable config staging by default only in the lite profile.
6. Consolidate shares.
7. Split interactive and batch modes.
8. Add VSOCK transport behind an option.
9. Make VSOCK the lite proxy-only default after tests pass.
10. Optimize cloning, readiness, and install-unit behavior.
11. Review whether any lite changes should become full-profile defaults.

Keep each phase independently reviewable and revertible.

---

## Definition of done

The work is complete when all of the following are true:

- A Codex-only interactive lite VM can be launched with one command.
- It uses one slot, two vCPUs, and approximately 4 GiB RAM by default.
- It uses a separate optimized guest store.
- It does not run guest Home Manager activation.
- It receives current, allowlisted host agent configuration through a disposable staged copy.
- It has one writable session virtiofs share and, when needed, one read-only SSH-key share.
- It has no ordinary network interface in proxy-only mode.
- It can access only the fixed host LiteLLM endpoint through VSOCK.
- It cannot access the host home, host store, host Nix daemon, control sockets, LAN, internet, VPN, DNS, or metadata services.
- Disabled agents and their runtimes are absent from the guest closure.
- Repository clones are independent and disposable.
- Interactive and batch profiles do not include each other’s unnecessary services.
- Launch latency, closure size, host process count, and virtiofsd count are all lower than the recorded baseline.
- The full profile remains available and compatible.
- Security and integration tests pass.

---

## Coding-agent operating instructions

While implementing this plan:

- Work in small commits aligned with the phases above.
- Do not combine security-boundary changes with unrelated formatting or refactoring.
- Preserve existing behavior unless a phase explicitly changes it.
- Add tests before changing defaults.
- Report benchmark results in every performance-related commit.
- Stop and document the issue rather than weakening an invariant to make a test pass.
- Do not add broad host mounts, generic host-port forwarding, privileged guest services, or shared credentials as shortcuts.
- Prefer explicit Nix assertions over runtime failure for invalid configurations.
- Keep the secure lite profile understandable from its generated units and mount declarations; avoid hidden imperative state.
