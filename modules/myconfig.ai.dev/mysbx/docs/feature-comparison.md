<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Feature comparison: `mysbx` vs. the older sandbox tiers

Status: snapshot, checked against commit `5084135115` (2026-09-29).
The `gvisor` tier (`agent-gvisor`) has since been removed (bd
myconfig-e6z); its image, gVisor pin and podman host setup moved to
[`../gvisor.nix`](../gvisor.nix). Its column is kept for the comparison.

This file compares `mysbx` with the sandbox tiers in
[`../../sandboxes/`](../../sandboxes) and the bubblewrap jail wrappers in
[`../../fns/`](../../fns). The per-tier prose is in
[`../../docs/agent-sandboxing-tiers.README.md`](../../docs/agent-sandboxing-tiers.README.md). The `mysbx` decisions are
in [`design/`](./design). Every cell names its source: a file, an option
or a decision id (`cli.md D8`, `backends.md D2`, …).

## 1. The candidates

| Key | Module / entry point | Commands | Enabled on (eval of `test-<host>`) |
| --- | --- | --- | --- |
| `jail` | [`fns/bubblewrap-app.nix`](../../fns/bubblewrap-app.nix), options in [`myconfig.ai.jail.nix`](../../sandboxes/myconfig.ai.jail.nix) | `agent-bubblewrap-{pi,opencode,claude,herdr,…}`, `…-worktree`, `agent-bubblewrap-alacritty-workmux-tmux` | every host with the agent modules |
| `nono-tier` | [`myconfig.ai.nono-agent-sandbox.nix`](../../sandboxes/myconfig.ai.nono-agent-sandbox.nix) + [`fns/nono-app.nix`](../../fns/nono-app.nix) | `agent-nono-{pi,opencode,claude,codex}` | f13, p14, thing, workstation (`mkDefault true` under `myconfig.ai.dev.enable`) |
| `qemu` | [`myconfig.ai.qemu-agent-sandbox/`](../../sandboxes/myconfig.ai.qemu-agent-sandbox) | `agent-qemu-pi`, `agent-qemu-herdr`, `agent-qemu-workmux-tmux`, `agent-qemu-alacritty-workmux-tmux` | every host with pi/herdr |
| `gvisor` | [`myconfig.ai.gvisor-agent-sandbox/`](../../sandboxes/myconfig.ai.gvisor-agent-sandbox) | `agent-gvisor`, `agent-gvisor-load-image` | f13, p14, thing, workstation (`myconfig.ai.dev/default.nix`) |
| `microvm` | [`myconfig.ai.microvm/`](../../sandboxes/myconfig.ai.microvm) | `agent-microvm`, workmux `microvm-<agent>` panes | none (`myconfig.ai.dev.microvm.enable` is off everywhere) |
| `mysbx` | [`mysbx/`](..) | `mysbx` | f13, p14, thing, workstation |

`mysbx` has four backends. `config.backend` or `--backend` selects one
(`cli.md` D7/D18). The generated user layer sets `backend = "bubblewrap"`.

| Backend | Mechanism | Source |
| --- | --- | --- |
| `bubblewrap` | bubblewrap namespaces | `mysbx-rs/src/bwrap.rs` |
| `nono` | bubblewrap builds the view, then `nono run` adds Landlock, seccomp and the egress proxy | `mysbx-rs/src/nono.rs`, `backends.md` D1 |
| `podman-gvisor` | rootless `podman run --runtime=runsc` on the image of the `gvisor` tier | `mysbx-rs/src/podman_gvisor.rs` |
| `podman-krun` | the same argv, runtime swapped to crun+libkrun: one rootless KVM microVM per run, mounts over virtio-fs | `podman_gvisor.rs` (`krun: true`), `backends.md` D2 |

In the tables below, `mysbx/<backend>` means one backend. `mysbx` alone
means all four.

## 2. Isolation boundary and threat model

| Candidate | Boundary | What an escape reaches | Source |
| --- | --- | --- | --- |
| `jail` | user + mount namespaces, host kernel | your uid on the host | `fns/bubblewrap-app.nix` |
| `nono-tier` | Landlock + seccomp on the host tree, host kernel | your uid on the host | `fns/nono-app.nix` |
| `qemu` | QEMU guest kernel (KVM, TCG fallback), unprivileged guest `agent` user | the QEMU and virtiofsd processes (your uid) | `qemu-agent-sandbox/builders.nix`, microvm.nix `runners/qemu.nix` (`accel = "kvm:tcg"`) |
| `gvisor` | gVisor Sentry (user-space kernel) in rootless podman | the runsc sandbox process (subuid-mapped) | `gvisor-agent-sandbox/README.md` |
| `microvm` | Cloud Hypervisor guest kernel, guest `agent` user, root-owned host side | the VMM (a systemd unit per slot) | `microvm/docs/agent-microvm-security-model.md` |
| `mysbx/bubblewrap` | `--unshare-all` + `--clearenv`, tmpfs `/mysbx-home`, host kernel | your uid on the host | `bwrap.rs`, `config.md` D9, D14 |
| `mysbx/nono` | the bubblewrap view plus Landlock grants from the resolved layout. seccomp only when the network is denied or allowlisted | your uid on the host | `backends.md` D1 "Grants follow the resolved layout" |
| `mysbx/podman-gvisor` | gVisor Sentry, `--cap-drop=ALL`, `no-new-privileges`, `--read-only`, keep-id user | the runsc process | `podman_gvisor.rs` |
| `mysbx/podman-krun` | libkrunfw guest kernel. VMM and guest share one security context | the VMM = your rootless podman process | `backends.md` D2 "Threat model delta vs. gVisor" |

`podman-krun` is not a stronger boundary than gVisor. What it adds is
isolation from host-kernel bugs and full kernel compatibility (nested
podman). It emits no `--cap-drop`/`no-new-privileges`, because the krun
handler never execs the OCI process (`backends.md` D2).

## 3. Network policy

| Candidate | Default | Deny | Finer policy | Source |
| --- | --- | --- | --- | --- |
| `jail` | host stack (`network` combinator) | none | none | `fns/bubblewrap-app.nix` |
| `nono-tier` | nono default: outbound allowed | none in the wrappers | `extraAllowDomains`/`extraConnectPorts`/`extraListenPorts` args, unused by the `agent-nono-*` wrappers | `fns/nono-app.nix` |
| `qemu` | SLiRP NAT outbound + one host-loopback SSH port | `AGENT_QEMU_{PI,HERDR}_NETWORK=0` (`allowNetwork = false`) | none | `qemu-agent-sandbox/builders.nix`, `runner.nix` |
| `gvisor` | pasta `--map-guest-addr` to the LiteLLM forwarder, full outbound NAT | `--network none` | none | `gvisor-agent-sandbox/README.md` "Model access" |
| `microvm` | `networkProfile = "proxy-only"`: bridge-only LiteLLM endpoint, nothing else | `offline` | `package-access` (one proxy port), `internet` (+`acknowledgeInsecureNetwork`). All profiles drop guest-to-guest, RFC1918 and metadata traffic | `microvm/default.nix` `networkProfile` |
| `mysbx` | `network = true`: the host stack (bwrap/nono) or pasta (podman) | `network = false` on every backend | allowlist keys enforced on `nono` only; see table below | `config.md` D5, D20, D21 |

How `mysbx` handles each network key, per backend. "Refused" means the
run exits `70` before anything is created, including under `--dry-run`
(`lib.rs` step 4b):

| Key | `bubblewrap` | `nono` | `podman-gvisor` | `podman-krun` |
| --- | --- | --- | --- | --- |
| `network = false` | enforced: no `--share-net` | enforced: bwrap netns + `--block-net` | enforced: `--network none` | enforced: `--network none`, TSI dials from an empty netns (`backends.md` D2) |
| `allow-domains` / `connect-ports` / `listen-ports` | refused | enforced via the nono proxy. `listen-ports` alone refused (bd myconfig-a14), URL forms refused | refused (pasta does not filter, bd myconfig-6di.3) | refused (TSI does not filter, bd myconfig-6di.5.5) |
| allowlist + `network = false` | refused | refused | refused | refused |
| `egress = "proxy-only"` | not a schema key yet: the strict parser rejects it on every backend (`config.md` D20, bd myconfig-mo3.2) | same | same | same |
| nix daemon `/nix/var/nix` | bound only with the shared network | bound with the shared network, dropped under an allowlist (bd myconfig-nj9) | never mounted | never mounted |

Model endpoint: bwrap/nono reach the loopback LiteLLM directly. The
podman backends use `myconfig.ai.dev.litellm-forwarder` with
`gvisor.pastaSpec` and `gvisor.env` (see `../README.md`, "Model endpoint
under the podman-gvisor backend").

## 4. Credential handling

| Candidate | Model API key | Other credentials | Source |
| --- | --- | --- | --- |
| `jail` | host key inside: `OPENAI_API_KEY` is always forwarded, plus `myconfig.ai.dev.jail.fwdEnvs` | host agent state dirs bound rw (`userDataDirs`) | `myconfig.ai.jail.nix`, `bubblewrap-app.nix` |
| `nono-tier` | host key inside: `OPENAI_API_KEY` + `myconfig.ai.dev.nono.fwdEnvs` | host state dirs `--allow` rw | `myconfig.ai.nono.nix`, `nono-app.nix` |
| `qemu` | real key, pushed over the SSH session env at launch | seeded config copy (`fns/seed-agent-config.nix`) | `builders.nix` header |
| `gvisor` | whatever the user passes (`--env`, `--env-file`). The generated env file holds only the base URL | allowlisted home seed (`home.seedPaths`) with endpoint rewrites | `gvisor-agent-sandbox/README.md` |
| `microvm` | never in the guest: the host LiteLLM holds it, the guest has placeholders | root-staged config seed, allowlist + credential denylist | `agent-microvm-security-model.md` |
| `mysbx` | none by default. Only variables named in `forward-env` (`myconfig.ai.dev.mysbx.forwardedEnvVars`, set by the private flake) are forwarded. Podman backends reach a keyless proxy through the forwarder | a per-repo ed25519 keypair in `<repo>.mysbx/state/.ssh`. No host `~/.ssh`, no `SSH_AUTH_SOCK` (`config.md` D22) | `lib.rs` `FORWARDED_ENV_VARS`, `default.nix` `forwardedEnvVars` |

`mysbx` has no "key never enters the sandbox" mode yet. That needs
`proxy-only` (bd myconfig-mo3.2) and the credential story (bd myconfig-t24).

## 5. Workspace model

| Candidate | Workspace | Worktrees | Git trust | Result handoff |
| --- | --- | --- | --- | --- |
| `jail` | `$PWD` rw live, `$HOME` refused (`rejectHomeCwd`) | `…-worktree` wrappers create one in `__worktrees`, main repo ro | same uid | live edits |
| `nono-tier` | `$PWD` rw live (`--allow-cwd`), `$HOME` refused | none | same uid | live edits |
| `qemu` | `$PWD` rw at `/workspace` (virtiofs) | the workmux runner shares repo + `__worktrees` | virtiofsd maps to your uid | live edits |
| `gvisor` | isolated clone per session at `<repo>__agent-gvisor/NAME`, host checkout never mounted | none | keep-id user | `merge` / `fetch` / `push` |
| `microvm` | standalone clone per task (`workspaceLayout = central \| beside-repo`), branch `agent/microvm/<task>`, root-owned index | workmux panes on the host | launcher-set `safe.directory` | the user imports the branch |
| `mysbx` | `live` (default): repo rw at its real path + approved `git-dirs` + `<repo>__worktrees` sibling when it exists. `--session NAME`: clone at `<repo>.mysbx/clones/NAME` on `agent/mysbx/NAME`, nothing of the host repo mounted, all mounts ro, `--rw` refused. A repo that is or contains `$HOME` is refused (`repo.rs`) | `mysbx worktree list \| diff \| hunk` (read-only, `worktree.md` W1–W5) | same uid on bwrap/nono/gvisor. `podman-krun` binds a per-run `safe.directory` file at `/etc/mysbx/gitconfig` (bd myconfig-zj2) | live, or `fetch` / `merge` / `push` / `diff` + `session list \| destroy \| hunk` (`workspace.md` D6/D7) |

## 6. Persistent state

| Candidate | What survives a run | Source |
| --- | --- | --- |
| `jail` | the host agent dirs themselves (rw bind) | `bubblewrap-app.nix` `userDataDirs` |
| `nono-tier` | the host agent dirs (`--allow`) | `nono-app.nix` |
| `qemu` | nothing: ephemeral root + tmpfs home | `builders.nix` |
| `gvisor` | per-session home (bound at `/home/agent`), the stopped container, the `--nix` store volume | `gvisor-agent-sandbox/docs/spec.md` §5 layout |
| `microvm` | the clone, archived results, and agent state with `--persist-agent-state` | `microvm/launcher.nix` |
| `mysbx` | `state-dirs` entries, backed by `<repo>.mysbx/state/<entry>` and bound at `/mysbx-home/<entry>` on every backend. Wired entries: opencode's state, pi's `.pi/agent/sessions`. Clone sessions bind no state-dirs (bd myconfig-9co). Containers and VMs are per run | `config.md` D15, `programs.pi-coding-agent` `config.stateDirs` |

## 7. Per-run vs. host-level configuration

| Candidate | Configuration input | To change mounts / policy |
| --- | --- | --- |
| `jail` | Nix call-site args + `JAIL_EXTRA_RO_PATHS` / `JAIL_EXTRA_RW_PATHS` | rebuild, or the env escape hatch |
| `nono-tier` | Nix call-site args | rebuild |
| `qemu` | `AGENT_QEMU_PI_*` env vars, impure per-launch `nix build` of the runner | per launch |
| `gvisor` | NixOS defaults baked as `AGENT_GVISOR_*` + per-session flags (`--mount`, `--network`, `--memory`, …) | per session |
| `microvm` | NixOS eval time: `resourceClasses`, `networkProfile`, `enabledAgents`, `capabilities`, the slot pool | `nixos-rebuild` |
| `mysbx` | runtime TOML: the user layer (generated from `myconfig.ai.dev.mysbx.config`) + the repo sidecar `<repo>.mysbx/config.toml` + per-run flags (`--backend`, `--multiplexer`, `--session`, `--ro`/`--rw`). Wrapper pins (`MYSBX_*`) hold only infrastructure | edit the sidecar (`mysbx edit`). No rebuild (`config.md` D1, D7) |

## 8. Resource limits

| Candidate | Mechanism | Enforced? |
| --- | --- | --- |
| `jail`, `nono-tier` | none | — |
| `qemu` | VM `vcpu` (default 4) / `mem` | yes (VM) |
| `gvisor` | `--memory --cpus --pids-limit` | not rootless: default runtime flag `ignore-cgroups` (`rust/src/state.rs`) |
| `microvm` | `resourceClasses.<c>.{count,vcpu,memoryMiB}` + `hypervisorTasksMax/CPUWeight/IOWeight` | yes (VM + systemd) |
| `mysbx/bubblewrap`, `mysbx/nono` | none | — |
| `mysbx/podman-gvisor` | env pins `MYSBX_GVISOR_{MEMORY,CPUS,PIDS_LIMIT}` → podman flags | not rootless (`ignore-cgroups`). mysbx prints a warning (`lib.rs`) |
| `mysbx/podman-krun` | the same pins → `krun.cpus` / `krun.ram_mib` annotations. pids and fractional values refused | yes (VM, bd myconfig-6di.5.6) |

No `mysbx` config key sets limits. That is bd myconfig-91j.

## 9. Nix inside the sandbox

| Candidate | Nix | Source |
| --- | --- | --- |
| `jail` | host store ro + `/nix/var/nix` ro: builds go through the host daemon | `bubblewrap-app.nix` `bindFullNixStore` |
| `nono-tier` | `--read /nix/store` only, no daemon grant | `nono-app.nix` |
| `qemu` | host store ro over virtiofs, no daemon | `builders.nix` |
| `gvisor` | `nix.enable`: a per-session writable store volume, daemonless, `sandbox = false` (on under `myconfig.ai.dev`) | `gvisor-agent-sandbox/docs/nix-in-sandbox.md` |
| `microvm` | own guest store disk, no in-guest nix workflow | `microvm/guest.nix` §5 |
| `mysbx/bubblewrap` | host store ro + the daemon socket (shared network only) + pinned `nix.conf` (`MYSBX_NIX_CONF`) | `bwrap.rs` |
| `mysbx/nono` | as bubblewrap. Under an allowlist the daemon is dropped | `backends.md` D1 |
| `mysbx/podman-gvisor` | image store only, no writable store (bd myconfig-9mh) | `podman_gvisor.rs` |
| `mysbx/podman-krun` | `krun.nix.enable` (on under `myconfig.ai.dev`): overlay on the image store, upper layer on guest tmpfs, single-user, per run, costs VM RAM | `backends.md` D2, `nix/krun-guest-nix.nix` |

## 10. Nested containers

Only `mysbx/podman-krun` supports nested containers. With
`krun.nestedPodman.enable` (default: `virtualisation.podman.enable` under
`myconfig.ai.dev`), `nix/krun-guest-conf.nix` bakes `podman` into the
shared image. Storage is on guest tmpfs, `netns = "host"`, and it runs as
guest root. A nested container that runs as a non-root uid cannot write
to virtio-fs mounts. The live run is bd myconfig-6di.5.7 (`backends.md`
D2 "Scope decisions"). No other candidate provides a container runtime
inside the sandbox.

## 11. GUI / display

| Candidate | Display | Source |
| --- | --- | --- |
| `jail` | none. `/run` is not bound. `JAIL_EXTRA_RO_PATHS=/run/user/<uid>` re-exposes the whole runtime dir | `bubblewrap-app.nix` |
| `nono-tier`, `qemu`, `gvisor` | none (`qemu`: `graphics.enable = false`) | module sources |
| `microvm` | none (graphics disabled) | `microvm/guest.nix` §5 |
| `mysbx/bubblewrap`, `mysbx/nono` | `display = "waypipe"`: a per-run waypipe channel, guest socket `/mysbx-home/wayland-0`, the compositor socket never enters | `config.md` D18, bd myconfig-6di.4.6 |
| `mysbx/podman-gvisor` | waypipe with the in-image binary (`gvisor.waypipe`) | `default.nix` |
| `mysbx/podman-krun` | refused: AF_UNIX does not cross virtio-fs (bd myconfig-ef6) | `lib.rs` step 4d |

`mysbx gui [ARG…]` opens a host alacritty window that runs the inner
`mysbx` (`cli.md` D15). `browser.enable` defaults to off in
`mysbx/default.nix`. `myconfig.ai.dev/default.nix` sets it to `true`, so
it is on on every `myconfig.ai.dev` host. The browser (`--no-sandbox`
wrapper, `nix/browser.nix`) goes on the bwrap/nono `PATH` through
`extraTools`. It is not in the podman image.

## 12. Multiplexer / session support

| Candidate | Sessions | Source |
| --- | --- | --- |
| `jail` | `agent-bubblewrap-alacritty-workmux-tmux`, `agent-bubblewrap-herdr`. Socket in `__worktrees/.agent-bubblewrap` | `myconfig.ai.workmux/jail.nix`, `programs.herdr.nix` |
| `nono-tier` | none | — |
| `qemu` | `agent-qemu-herdr`, `agent-qemu-workmux-tmux` | `builders.nix` |
| `gvisor` | `defaultCommand` = herdr in the container. `shell` / `logs` attach to a running session | `gvisor-agent-sandbox/README.md` |
| `microvm` | host-side workmux `microvm-<agent>` panes. `agent-run herdr` in the guest | `microvm/workmux.nix`, `agents.nix` |
| `mysbx/bubblewrap` | `multiplexer = tmux \| workmux \| herdr \| aoe \| orca \| none`. The socket is in `/mysbx-home/.mysbx-tmux`. `--multiplexer` sets it for one run. The generated user layer defaults to `workmux` | `config.md` D16/D17, `cli.md` D14 |
| `mysbx/nono` | the same except `orca`. `--allow-unix-socket-dir-bind` + pathname AF_UNIX mediation (`backends.md` D1) | bd myconfig-6di.4.5, bd myconfig-peo (workmux sidebar, in progress) |
| `mysbx/podman-gvisor`, `mysbx/podman-krun` | refused: no in-image entry (`MultiplexerUnavailable`). `orca` refused separately (bd myconfig-2m8) | `podman_gvisor.rs`. Not planned: bd myconfig-3y2 closed, the multiplexer is a config choice |

## 13. Startup cost

No candidate has measured startup numbers in this repo. By mechanism:

| Candidate | Per-start work |
| --- | --- |
| `jail`, `nono-tier`, `mysbx/bubblewrap`, `mysbx/nono` | process spawn (`nono`: bwrap + nono supervisor) |
| `qemu` | impure `nix build` of a small wrapper + VM boot (`builders.nix` header) |
| `gvisor` | clone + home seed on `start`, then a container start |
| `microvm` | prebuilt slot, boot of a systemd-managed VM. Waits for a free slot (`--wait`) |
| `mysbx/podman-gvisor` | container start. The image is loaded once per rebuild (`mysbx podman-load-image`) |
| `mysbx/podman-krun` | a microVM boot per run. Nix/podman state rebuilt per run on guest tmpfs |

## 14. Required host privileges

| Candidate | Needs |
| --- | --- |
| `jail`, `mysbx/bubblewrap` | unprivileged user namespaces |
| `nono-tier`, `mysbx/nono` | the above + a Landlock-capable kernel |
| `qemu` | `/dev/kvm` for acceleration (TCG fallback). Rootless virtiofsd, no host config |
| `gvisor`, `mysbx/podman-gvisor` | rootless podman, runsc registered in `containers.conf`, subuid/subgid (`autoSubUidGidRange`), all set by the gvisor tier module. `mysbx` also needs `gvisor.image` (null → refused) |
| `mysbx/podman-krun` | the above image + rw `/dev/kvm` (the `kvm` group), checked before exec (`lib.rs` `kvm_available`). `--group-add=keep-groups` keeps the group for the VMM (bd myconfig-b5o) |
| `microvm` | root for every command (`sudo agent-microvm`, `passwordlessControl`), root-owned `runtimeRoot`/`stateRoot`, bridge `agentbr0` + per-slot TAPs + firewall chains, a rebuild to change the pool |

## 15. CLI and contract

| Axis | `gvisor` | `microvm` | `mysbx` |
| --- | --- | --- | --- |
| Verbs | `start list status run shell logs stop merge fetch push destroy doctor` | `run stop destroy status doctor capabilities list dashboard ssh console submit cancel recover usage workspace-remove` | bare = enter, `run -- CMD`, `gui`, `init`, `edit`, `fetch merge push diff`, `session …`, `worktree …`, `status`, `ssh-pubkey`, `podman-load-image` (`mysbx-rs/src/usage.txt`) |
| Unattended | `run --detach` | `submit` + JSON result | `run --result` writes `result.json`, no detach (bd myconfig-dys) |
| Exit codes | non-zero on failure | `0/1/124/130/70` | `0/1/2/70/124/130/143` (`cli.md` D8, D17) |
| Health check | `doctor` | `doctor` | `status` (config only), `podman-load-image --test` (bd myconfig-iyz) |
| Completion | fish | none | fish (`mysbx-rs/completions/mysbx.fish`) |

The jail, nono-tier and qemu wrappers take no flags and pass their
arguments to the agent.

## 16. Validation state

| Candidate | Automated | Live |
| --- | --- | --- |
| `jail`, `nono-tier`, `qemu` | eval only | manual |
| `gvisor` | cargo tests + CLI stub harness + completion check (`nix/checks.nix`) | `agent-gvisor doctor` |
| `microvm` | `tests/microvm.nix` + eval assertions | `runtime-validation.sh` on KVM. Currently enabled on no host |
| `mysbx` | cargo goldens per backend + `nix/checks.nix` + `nix/config-eval-test.nix` | nono exercised on f13 (bd myconfig-27o, herdr follow-up bd myconfig-nif). podman-gvisor endpoint not verified live (bd myconfig-jq2). podman-krun runbook `krun-live-validation.md` in progress (bd myconfig-6di.5.7), `krun.nix` probed on f13 (bd myconfig-pz6) |

## 17. Old-tier features `mysbx` still lacks

| Gap | Old tier that has it | Bead |
| --- | --- | --- |
| model key never inside the sandbox (proxy-only egress) | `microvm` `networkProfile = "proxy-only"` | bd myconfig-mo3.2, bd myconfig-t24. podman side bd myconfig-6di.3 |
| public egress without the host LAN / private ranges | `microvm` (every profile) | bd myconfig-fvi |
| resource limits as config, and any limit on bwrap/nono | `microvm` `resourceClasses`, `qemu` `vcpu`/`mem` | bd myconfig-91j |
| unattended / detached runs, attach to a running sandbox | `gvisor` `run --detach` + `shell`, `microvm` `submit` | bd myconfig-dys |
| writable nix store on `podman-gvisor` | `gvisor` `nix.enable` / `--nix` | bd myconfig-9mh |
| waypipe on `podman-krun` | — (no old tier has a display). Parity with the other mysbx backends | bd myconfig-ef6 |
| state in clone sessions | `gvisor` per-session home, `microvm` `--persist-agent-state` | bd myconfig-9co |
| one-command host health check | `gvisor` / `microvm` `doctor` | bd myconfig-iyz |
| containers removed after the run | `gvisor` `destroy` | bd myconfig-che |
| native QEMU VM backend | `qemu`, `microvm` | bd myconfig-6di.6 (deferred fallback) |

`mysbx` does not plan to replace `agentUsers` (a separate uid, not a
sandbox) or `microvm`'s root-owned slot pool (`backends.md` D2
"Alternatives considered").

## Updating this document

- Re-check it when a backend, a refusal or a tier changes, and refresh the
  commit hash in the status line.
- Keep cells short. Cite file paths, options and decision ids, not line
  numbers.
- Describe the code as it is now. The history belongs in commit messages
  and beads.
