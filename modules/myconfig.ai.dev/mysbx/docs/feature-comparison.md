<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Feature comparison: `mysbx` vs. the older sandbox tiers

Status: **mixed historical comparison and current `mysbx` summary**.
The canonical current six-backend guide is
[`backend-comparison.md`](./backend-comparison.md); use it for supported
combinations, option semantics, known gaps and evidence limits.

All non-`mysbx` rows are historical snapshots checked against commit
`5084135115` (2026-09-29), not a current inventory or installation guide.
Their paths, commands and host lists are retained as historical sources,
including tiers since removed. In particular, the `gvisor` tier
(`agent-gvisor`, bd myconfig-e6z) was removed; its image, gVisor pin and
Podman host setup moved to [`../podman.nix`](../podman.nix). Nothing here
reintroduces that tier.

The historical comparison covers the sandbox tiers under
`../../sandboxes/` and bubblewrap jail wrappers under `../../fns/`;
the historical per-tier prose is
[`agent-sandboxing-tiers.README.md`](../../docs/agent-sandboxing-tiers.README.md).
The `mysbx` cells below summarize current behavior, with sources in
[`design/`](./design) and the implementation. Historical live results
are not acceptance of every current option combination.

## 1. The candidates

| Key | Module / entry point (legacy rows: snapshot paths) | Commands | Availability (legacy rows: snapshot eval of `test-<host>`) |
| --- | --- | --- | --- |
| `jail` | [`fns/bubblewrap-app.nix`](../../fns/bubblewrap-app.nix), options in [`myconfig.ai.jail.nix`](../../sandboxes/myconfig.ai.jail.nix) | `agent-bubblewrap-{pi,opencode,claude,herdr,…}`, `…-worktree`, `agent-bubblewrap-alacritty-workmux-tmux` | every host with the agent modules |
| `nono-tier` | [`myconfig.ai.nono-agent-sandbox.nix`](../../sandboxes/myconfig.ai.nono-agent-sandbox.nix) + [`fns/nono-app.nix`](../../fns/nono-app.nix) | `agent-nono-{pi,opencode,claude,codex}` | f13, p14, thing, workstation (`mkDefault true` under `myconfig.ai.dev.enable`) |
| `qemu` | [`myconfig.ai.qemu-agent-sandbox/`](../../sandboxes/myconfig.ai.qemu-agent-sandbox) | `agent-qemu-pi`, `agent-qemu-herdr`, `agent-qemu-workmux-tmux`, `agent-qemu-alacritty-workmux-tmux` | every host with pi/herdr |
| `gvisor` | `myconfig.ai.gvisor-agent-sandbox/` (removed; historical path) | `agent-gvisor`, `agent-gvisor-load-image` | f13, p14, thing, workstation (`myconfig.ai.dev/default.nix`) |
| `microvm` | [`myconfig.ai.microvm/`](../../sandboxes/myconfig.ai.microvm) | `agent-microvm`, workmux `microvm-<agent>` panes | none (`myconfig.ai.dev.microvm.enable` is off everywhere) |
| `mysbx` (current) | [`mysbx/`](..) | `mysbx` | bubblewrap/nono always available when mysbx is enabled; heavier backends are host opt-ins (`backends.md` D10) |

`mysbx` has **six backends**. `backend` in a configuration layer or
`--backend` selects one (`cli.md` D7/D18). The generated user layer
normally sets `backend = "bubblewrap"`; availability does not select a
backend.

| Backend | Mechanism | Source |
| --- | --- | --- |
| `bubblewrap` | bubblewrap namespaces | `mysbx-rs/src/bwrap.rs` |
| `nono` | bubblewrap builds the view, then `nono run` adds Landlock, seccomp and optional port/proxy policy | `mysbx-rs/src/nono.rs`, `backends.md` D1 |
| `podman-gvisor` | rootless `podman run --runtime=runsc` on the shared OCI image built by `../podman.nix` | `mysbx-rs/src/podman_gvisor.rs` |
| `podman-krun` | the shared Podman builder with crun+libkrun: one rootless KVM microVM per run, mounts over virtio-fs | `podman_gvisor.rs` (`krun: true`), `backends.md` D2 |
| `krun` | bubblewrap → direct libkrun launcher → KVM microVM, with an in-process virtiofs server and host/baked store choice; no OCI image | `mysbx-rs/src/krun.rs`, `backends.md` D3/D6/D7 |
| `microvm-qemu` | bubblewrap → QEMU KVM microVM, one rootless virtiofsd per share device, SLiRP networking; shares the direct guest init and host/baked store choice | `mysbx-rs/src/microvm_qemu.rs`, `backends.md` D9 |

In the tables below, `mysbx/<backend>` means one backend. A `mysbx` row
covers all six only where it says so; backend-specific differences are
spelled out rather than implied by the name.

## 2. Isolation boundary and threat model

| Candidate | Boundary | What an escape reaches | Source |
| --- | --- | --- | --- |
| `jail` | user + mount namespaces, host kernel | your uid on the host | `fns/bubblewrap-app.nix` |
| `nono-tier` | Landlock + seccomp on the host tree, host kernel | your uid on the host | `fns/nono-app.nix` |
| `qemu` | QEMU guest kernel (KVM, TCG fallback), unprivileged guest `agent` user | the QEMU and virtiofsd processes (your uid) | `qemu-agent-sandbox/builders.nix`, microvm.nix `runners/qemu.nix` (`accel = "kvm:tcg"`) |
| `gvisor` | gVisor Sentry (user-space kernel) in rootless podman | the runsc sandbox process (subuid-mapped) | `gvisor-agent-sandbox/README.md` |
| `microvm` | Cloud Hypervisor guest kernel, guest `agent` user, root-owned host side | the VMM (a systemd unit per slot) | `microvm/docs/agent-microvm-security-model.md` |
| `mysbx/bubblewrap` | `--unshare-all` + `--clearenv`, tmpfs `/mysbx-home`, host kernel | your uid on the host | `bwrap.rs`, `config.md` D9, D14 |
| `mysbx/nono` | the bubblewrap view plus Landlock grants from the resolved layout; pinned nono 0.80.0 installs pathname AF_UNIX seccomp mediation even with unrestricted IP networking (`AllowAll`) | your uid on the host | `nono.rs`, `nix/mysbx.nix` profile; current guide |
| `mysbx/podman-gvisor` | gVisor Sentry, `--cap-drop=ALL`, `no-new-privileges`, `--read-only`, keep-id user | the runsc process | `podman_gvisor.rs` |
| `mysbx/podman-krun` | libkrunfw guest kernel. VMM and guest share one security context | the VMM = your rootless podman process | `backends.md` D2 "Threat model delta vs. gVisor" |
| `mysbx/krun` | libkrunfw guest kernel; launcher/VMM and in-process file server inside host bubblewrap | the launcher and its bubblewrap-visible host resources (your uid) | `krun.rs`, `backends.md` D3 |
| `mysbx/microvm-qemu` | pinned-kernel guest. qemu and the virtiofsd daemons are out-of-process but same-uid; virtiofsd uses `--sandbox none` inside bubblewrap | the qemu and virtiofsd processes (your uid) | `backends.md` D9 |

`podman-krun` is not established as a stronger boundary than gVisor.
Its guest kernel offers host-kernel-bug isolation and compatibility for
features such as provisioned nested Podman, not a proven security ranking.
It emits no `--cap-drop`/`no-new-privileges`, because the krun handler
never execs the OCI process (`backends.md` D2).

## 3. Network policy

| Candidate | Default | Deny | Finer policy | Source |
| --- | --- | --- | --- | --- |
| `jail` | host stack (`network` combinator) | none | none | `fns/bubblewrap-app.nix` |
| `nono-tier` | nono default: outbound allowed | none in the wrappers | `extraAllowDomains`/`extraConnectPorts`/`extraListenPorts` args, unused by the `agent-nono-*` wrappers | `fns/nono-app.nix` |
| `qemu` | SLiRP NAT outbound + one host-loopback SSH port | `AGENT_QEMU_{PI,HERDR}_NETWORK=0` (`allowNetwork = false`) | none | `qemu-agent-sandbox/builders.nix`, `runner.nix` |
| `gvisor` | pasta `--map-guest-addr` to the LiteLLM forwarder, full outbound NAT | `--network none` | none | `gvisor-agent-sandbox/README.md` "Model access" |
| `microvm` | `networkProfile = "proxy-only"`: bridge-only LiteLLM endpoint, nothing else | `offline` | `package-access` (one proxy port), `internet` (+`acknowledgeInsecureNetwork`). All profiles drop guest-to-guest, RFC1918 and metadata traffic | `microvm/default.nix` `networkProfile` |
| `mysbx` | `network = true`: host namespace (bwrap/nono), pasta (Podman; `podman-krun` uses TSI through it), host-namespace TSI (direct krun), SLiRP (QEMU) | `network = false` on all six backends | allowlist keys supported on `nono` only, with caveats below; `egress` is rejected everywhere | `config.md` D5/D21, `backends.md` D6/D9; current guide |

How `mysbx` handles each network key, per backend. "Refused" means the
run exits `70` before anything is created, including under `--dry-run`
(`lib.rs` step 4b):

| Key | `bubblewrap` | `nono` | `podman-gvisor` | `podman-krun` | `krun` | `microvm-qemu` |
| --- | --- | --- | --- | --- | --- | --- |
| `network = false` | no `--share-net` | bwrap netns + `--block-net` | `--network none` | `--network none`, TSI dials from an empty netns | implicit vsock disabled + bwrap netns | no netdev + bwrap netns |
| `allow-domains` / `connect-ports` / `listen-ports` | refused | proxy/port grants; see caveats below. `listen-ports` alone and URL forms refused | refused | refused | refused | refused |
| allowlist + `network = false` | refused | refused | refused | refused | refused | refused |
| `egress` (including `"proxy-only"`) | unknown schema key; rejected | same | same | same | same | same |
| implicit host nix daemon `/nix/var/nix` | only with shared network | only with shared network and no allowlist | none | none | none (host-store db copy is not daemon access) | none (same) |

Nono domain and port grants are **not a conjunction**. Port-only grants
are host-independent; domains use a hostname proxy. With both, Landlock
has independent port grants, but the default proxy supervisor still
denies direct remote connections: mixed grants do not demonstrate an
arbitrary-host bypass. Proxy bypass settings derived from the ports can
instead cause connectivity conflicts. Test the exact policy before
recommending it; see the [current guide](./backend-comparison.md).
The old nono 0.74.0 AF_UNIX rate-limit observations are historical, not
a current contract for pinned 0.80.0.

Model endpoint transport differs: bwrap/nono use the shared host
namespace (subject to nono policy), direct krun uses TSI, and QEMU uses
the SLiRP gateway. **Both** Podman variants consume
`MYSBX_PODMAN_ENV` and `MYSBX_PODMAN_PASTA_SPEC`
(`podman.env` / `podman.pastaSpec`) for the LiteLLM forwarder. A host
endpoint probe is not proof of payload reachability under its policy.

## 4. Credential handling

| Candidate | Model API key | Other credentials | Source |
| --- | --- | --- | --- |
| `jail` | host key inside: `OPENAI_API_KEY` is always forwarded, plus `myconfig.ai.dev.jail.fwdEnvs` | host agent state dirs bound rw (`userDataDirs`) | `myconfig.ai.jail.nix`, `bubblewrap-app.nix` |
| `nono-tier` | host key inside: `OPENAI_API_KEY` + `myconfig.ai.dev.nono.fwdEnvs` | host state dirs `--allow` rw | `myconfig.ai.nono.nix`, `nono-app.nix` |
| `qemu` | real key, pushed over the SSH session env at launch | seeded config copy (`fns/seed-agent-config.nix`) | `builders.nix` header |
| `gvisor` | whatever the user passes (`--env`, `--env-file`). The generated env file holds only the base URL | allowlisted home seed (`home.seedPaths`) with endpoint rewrites | `gvisor-agent-sandbox/README.md` |
| `microvm` | never in the guest: the host LiteLLM holds it, the guest has placeholders | root-staged config seed, allowlist + credential denylist | `agent-microvm-security-model.md` |
| `mysbx` | none by default. Credential names must be declared in `forward-env` (`myconfig.ai.dev.mysbx.forwardedEnvVars`, set by the private flake). Podman backends can reach the keyless proxy through the forwarder | live runs get a per-repo ed25519 keypair in `<repo>.mysbx/state/.ssh`; clone sessions receive neither that key nor persisted state. No implicit host `~/.ssh` or `SSH_AUTH_SOCK` (`config.md` D22) | `lib.rs` `FORWARDED_ENV_VARS`, `default.nix` `forwardedEnvVars` |

A keyless proxy can be used by configuration, but `mysbx` has no
implemented proxy-only egress profile that guarantees it is the sole
endpoint. `egress` is a proposal (`config.md` D20, bd myconfig-mo3.2),
not an accepted key.

## 5. Workspace model

| Candidate | Workspace | Worktrees | Git trust | Result handoff |
| --- | --- | --- | --- | --- |
| `jail` | `$PWD` rw live, `$HOME` refused (`rejectHomeCwd`) | `…-worktree` wrappers create one in `__worktrees`, main repo ro | same uid | live edits |
| `nono-tier` | `$PWD` rw live (`--allow-cwd`), `$HOME` refused | none | same uid | live edits |
| `qemu` | `$PWD` rw at `/workspace` (virtiofs) | the workmux runner shares repo + `__worktrees` | virtiofsd maps to your uid | live edits |
| `gvisor` | isolated clone per session at `<repo>__agent-gvisor/NAME`, host checkout never mounted | none | keep-id user | `merge` / `fetch` / `push` |
| `microvm` | standalone clone per task (`workspaceLayout = central \| beside-repo`), branch `agent/microvm/<task>`, root-owned index | workmux panes on the host | launcher-set `safe.directory` | the user imports the branch |
| `mysbx` | `live` (default): repo rw at its real path + approved `git-dirs` + `<repo>__worktrees` sibling (created for interactive runs when missing). `--session NAME`: clone at `<repo>.mysbx/clones/NAME` on `agent/mysbx/NAME`, its own worktrees, no host repo, configured mounts ro, `--rw` refused. A repo that is or contains `$HOME` is refused (`repo.rs`) | `mysbx worktree list \| diff \| hunk` (read-only, `worktree.md` W1–W5) | same uid on bwrap/nono/gvisor; guest-root VM backends supply generated Git/libgit2 trust for approved paths | live, or `fetch` / `merge` / `push` / `diff` + `session list \| destroy \| hunk` (`workspace.md` D6/D7) |

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
| `mysbx/bubblewrap`, `mysbx/nono` | none; set limit keys print a warning (`config.md` D23) | — |
| `mysbx/podman-gvisor` | `memory`/`cpus`/`pids-limit`, overridden by `MYSBX_PODMAN_{MEMORY,CPUS,PIDS_LIMIT}` | rootless default `ignore-cgroups` warns and omits **all three** limit flags; only passed when cgroups are enabled (`podman_gvisor.rs`) |
| `mysbx/podman-krun` | CPU/RAM → `krun.cpus` / `krun.ram_mib` annotations; configured `pids-limit` warns and is dropped, explicit `MYSBX_PODMAN_PIDS_LIMIT` and fractional CPUs are refused | VM sizing, not host-process limits |
| `mysbx/krun`, `mysbx/microvm-qemu` | CPU/RAM size the VM; `pids-limit` refused (despite an earlier generic ignored-limit warning) | VM sizing, not host-process limits |

The config keys are `config.md` D23 (bd myconfig-91j).

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
| `mysbx/podman-krun` | `krun.nix.enable`: single-user overlay on the image-owned store, with a per-run loop-mounted ext4 scratch when pinned, announced guest-tmpfs fallback otherwise | `backends.md` D2, `nix/krun-guest-nix.nix` |
| `mysbx/krun`, `mysbx/microvm-qemu` | Nix binary always in the tools; `store = "host"` exposes the whole host store ro, `"baked"` exposes an EROFS tool closure/db. Writable overlay/state requires the per-run scratch; **no managed tmpfs fallback** without it | `krun.rs`, `nix/krun-rootfs.nix`, `config.md` D26 |

For the direct VM backends an explicit `store` wins; otherwise the
silent default is `baked` **only with** a nonempty
`MYSBX_KRUN_STORE_DISK` pin, and `host` without it. Baked mode narrows
what the **guest** sees, not the outer VM bubblewrap runtime view,
which still includes the whole host store. It does not change Podman's
image-owned store into a host-store bind.

Current module-wiring gap: `krunScratchSize` is pinned only when
`podman.enable && krun.nix.enable`. A direct-only/QEMU-only host can
enable guest Nix yet receive no scratch; Nix binary availability does
not imply a managed writable store. Complete baked-store live
acceptance is still unrecorded; see the [current guide](./backend-comparison.md).

## 10. Nested containers

`krun.nestedPodman.enable` provisions the **OCI image** for
`mysbx/podman-krun`, not the direct `krun` or QEMU rootfs/PATH. It is a
module opt-in (the enclosing `myconfig.ai.dev` module defaults it from
`virtualisation.podman.enable`). `nix/krun-guest-conf.nix` bakes the
runtime/config into the shared image. Storage is on guest tmpfs,
`netns = "host"`, and it runs as guest root. A nested container that
runs as a non-root uid cannot write to virtio-fs mounts. The runbook is
[`krun-live-validation.md`](./krun-live-validation.md) §2.2; it is not a
recorded full nested-Podman acceptance pass. Direct krun is not blanket
feature parity with this provisioning.

## 11. GUI / display

| Candidate | Display | Source |
| --- | --- | --- |
| `jail` | none. `/run` is not bound. `JAIL_EXTRA_RO_PATHS=/run/user/<uid>` re-exposes the whole runtime dir | `bubblewrap-app.nix` |
| `nono-tier`, `qemu`, `gvisor` | none (`qemu`: `graphics.enable = false`) | module sources |
| `microvm` | none (graphics disabled) | `microvm/guest.nix` §5 |
| `mysbx/bubblewrap`, `mysbx/nono` | `display = "waypipe"`: a per-run waypipe channel, guest socket `/mysbx-home/wayland-0`, the compositor socket never enters | `config.md` D18, bd myconfig-6di.4.6 |
| `mysbx/podman-gvisor` | waypipe with the in-image binary (`podman.waypipe`) | `default.nix` |
| `mysbx/podman-krun`, `mysbx/microvm-qemu` | refused: host AF_UNIX sockets do not cross virtio-fs | `lib.rs`, `microvm_qemu.rs` |
| `mysbx/krun` | **not implemented, currently accepted in error**: host client may start, but there is no guest server/transport | `lib.rs`, `krun.rs`; current guide |

`mysbx gui [ARG…]` opens a host alacritty window that runs the inner
`mysbx` (`cli.md` D15) and, once that run is over, prints a closing
`mysbx: finished successfully` / `finished with status N` line before
the held window is left to the operator. `browser.enable` defaults to off
in `mysbx/default.nix`. `myconfig.ai.dev/default.nix` sets it to `true`,
so it is on on every `myconfig.ai.dev` host. The browser (`--no-sandbox`
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
| `mysbx/bubblewrap` | `multiplexer = tmux \| workmux \| herdr \| aoe \| orca \| none`. Tmux socket dir `/mysbx-home/.mysbx-tmux`; herdr has a separate API dir. `--multiplexer` sets the choice for one run. The generated user layer defaults to `workmux` where that integration is enabled | `config.md` D16/D17, `cli.md` D14 |
| `mysbx/nono` | the same except `orca`. `--allow-unix-socket-dir-bind` + pathname AF_UNIX mediation; old 0.74.0 rate-limit observations are not current 0.80.0 guarantees | `nono.rs`, current guide |
| `mysbx/podman-gvisor`, `mysbx/podman-krun` | `tmux \| workmux \| herdr \| aoe \| none`, from image entries (`podman.muxEntries`). tmux dir `/mysbx-home/.mysbx-tmux` (gvisor), `/dev/shm/mysbx-tmux` (podman-krun). `orca` refused | `podman_gvisor.rs` |
| `mysbx/krun`, `mysbx/microvm-qemu` | `tmux \| workmux \| herdr \| aoe \| none`, pinned direct guest entries; `orca` refused; sockets guest-local | `krun.rs`, shared guest init |

Default private sockets are not a blanket no-persistence guarantee:
`state-dirs = [".config/herdr"]` is accepted and persists herdr's API
directory on bubblewrap/nono. The tmux guard does not cover every
multiplexer API directory. Clone sessions omit persisted state and the
repo key.

## 13. Startup cost

Historical timings exist, but not a controlled current six-backend
benchmark. The early direct-krun **282 ms** spike is not a current
wrapper benchmark. The later **25 min 42 s direct / 12.7 s Podman-krun**
figures are complete smoke-suite durations, not isolated boot timings.
QEMU's historical one-shot KVM timing is a different workload. These
numbers do not justify a current speed ranking. By mechanism:

| Candidate | Per-start work |
| --- | --- |
| `jail`, `nono-tier`, `mysbx/bubblewrap`, `mysbx/nono` | process spawn (`nono`: bwrap + nono supervisor) |
| `qemu` | impure `nix build` of a small wrapper + VM boot (`builders.nix` header) |
| `gvisor` | clone + home seed on `start`, then a container start |
| `microvm` | prebuilt slot, boot of a systemd-managed VM. Waits for a free slot (`--wait`) |
| `mysbx/podman-gvisor` | container start. The image is loaded once per rebuild (`mysbx podman-load-image`) |
| `mysbx/podman-krun` | microVM boot and OCI setup per run; disposable Nix scratch (disk or tmpfs) and nested-Podman tmpfs |
| `mysbx/krun` | direct VM boot and stage setup; host-store db copy/reconciliation when scratch and networking require it, skipped in baked mode |
| `mysbx/microvm-qemu` | QEMU VM boot plus virtiofsd starts (one per share device); same store/init choices as direct krun, no proven current relative-speed result |

## 14. Required host privileges

| Candidate | Needs |
| --- | --- |
| `jail`, `mysbx/bubblewrap` | unprivileged user namespaces |
| `nono-tier`, `mysbx/nono` | the above + a Landlock-capable kernel |
| `qemu` | `/dev/kvm` for acceleration (TCG fallback). Rootless virtiofsd, no host config |
| `gvisor` (historical) | rootless podman, runsc registered in `containers.conf`, subuid/subgid (`autoSubUidGidRange`), all set by the gvisor tier module |
| `mysbx/podman-gvisor` | rootless Podman, registered runsc and subuid/subgid setup from `../podman.nix`; `podman.enable` opt-in and a loaded `podman.image` (null → refused) |
| `mysbx/podman-krun` | the above image + rw `/dev/kvm` (the `kvm` group), checked before exec (`lib.rs` `kvm_available`) and by `mysbx doctor`. `--group-add=keep-groups` keeps the group for the VMM (bd myconfig-b5o) |
| `mysbx/krun` | unprivileged user namespaces, rw `/dev/kvm`, pinned direct launcher/libkrun/rootfs; no Podman image |
| `mysbx/microvm-qemu` | unprivileged user namespaces, rw `/dev/kvm`, pinned QEMU launcher/kernel/rootfs and rootless virtiofsd (`--sandbox none`). Normal mysbx requires KVM; no supported TCG fallback |
| `microvm` | root for every command (`sudo agent-microvm`, `passwordlessControl`), root-owned `runtimeRoot`/`stateRoot`, bridge `agentbr0` + per-slot TAPs + firewall chains, a rebuild to change the pool |

## 15. CLI and contract

| Axis | `gvisor` | `microvm` | `mysbx` |
| --- | --- | --- | --- |
| Verbs | `start list status run shell logs stop merge fetch push destroy doctor` | `run stop destroy status doctor capabilities list dashboard ssh console submit cancel recover usage workspace-remove` | bare = enter, `run -- CMD`, `gui`, `init`, `edit`, `fetch merge push diff`, `session …`, `worktree …`, `status`, `doctor`, `attach`, `ssh-pubkey`, `podman-load-image` (`mysbx-rs/src/usage.txt`) |
| Unattended / detached | `run --detach` | `submit` + JSON result | `run --result` writes outcome JSON; bare `--detach` / `--detachable` + `attach` accepted on bwrap/nono/direct krun/QEMU, refused on both Podman backends; no accepted-backend live matrix recorded |
| Exit codes | non-zero on failure | `0/1/124/130/70` | `0/1/2/70/124/130/143` (`cli.md` D8, D17) |
| Health check | `doctor` | `doctor` | `doctor [BACKEND...]`: binaries, startup probe, image freshness, `/dev/kvm`, Landlock ABI, model endpoint (`cli.md` D20, bd myconfig-iyz) |
| Completion | fish | none | fish (`mysbx-rs/completions/mysbx.fish`) |

The jail, nono-tier and qemu wrappers take no flags and pass their
arguments to the agent.

## 16. Validation state

| Candidate | Automated | Live |
| --- | --- | --- |
| `jail`, `nono-tier`, `qemu` | eval only | manual |
| `gvisor` | cargo tests + CLI stub harness + completion check (`nix/checks.nix`) | `agent-gvisor doctor` |
| `microvm` | `tests/microvm.nix` + eval assertions | `runtime-validation.sh` on KVM. Currently enabled on no host |
| `mysbx` | cargo goldens per backend + `nix/checks.nix` + `nix/config-eval-test.nix`; static checks are not live enforcement proof | historical nono/guest-Nix probes; both krun variants smoke-passed on KVM. Direct guest-Nix results and later store-walk failure in [`krun-validation-log.md`](./krun-validation-log.md); fd cause of the silent failure unconfirmed. QEMU's [`eleven KVM probes`](./microvm-qemu-validation-log.md) and initial console size do not cover baked-store, scratch stress, nested Podman, detach or mid-session resize. Complete baked-store acceptance and all accepted detach paths' live matrix remain unrecorded |

Podman names already include a unique run ID; the Podman detach refusal
still exists, but missing unique names is no longer its explanation.
See the [current guide](./backend-comparison.md) and
[`detach-validation-log.md`](./detach-validation-log.md) for the evidence
gap. `doctor` does not replace these runs: direct VM probes do not boot
a guest, and nono's host endpoint probe does not exercise its allowlist.

## 17. Historical gaps and their current disposition

The old-tier capabilities are historical references, not a claim that
those tiers remain installed. Some former gaps below have landed; their
validation may still be incomplete.

| Gap | Old tier that has it | Bead |
| --- | --- | --- |
| model key never inside the sandbox (proxy-only egress) | `microvm` `networkProfile = "proxy-only"` | bd myconfig-mo3.2, bd myconfig-t24. podman side bd myconfig-6di.3 |
| public egress without the host LAN / private ranges | `microvm` (every profile) | bd myconfig-fvi |
| any limit on bwrap/nono (the config keys exist, `config.md` D23) | `microvm` `resourceClasses`, `qemu` `vcpu`/`mem` | bd myconfig-cea |
| detached interactive runs implemented on bwrap/nono/direct krun/QEMU; refused on Podman, live matrix unrecorded | `gvisor` `run --detach` + `shell`, `microvm` `submit` | bd myconfig-7ioq; [`detach-validation-log.md`](./detach-validation-log.md) |
| writable nix store on `podman-gvisor` | `gvisor` `nix.enable` / `--nix` | bd myconfig-9mh |
| waypipe on VM backends: Podman-krun/QEMU refuse; direct krun incorrectly accepts without an implementation | — (no old tier has a display) | bd myconfig-ef6 / myconfig-dak.9; current guide |
| state in clone sessions | `gvisor` per-session home, `microvm` `--persist-agent-state` | bd myconfig-9co |
| per-run Podman container cleanup implemented (`--rm`); names include the run ID | `gvisor` `destroy` | `podman_gvisor.rs`; former gap bd myconfig-che |
| native QEMU VM backend landed; historical eleven-probe KVM acceptance only, not all current options | `qemu`, `microvm` | bd myconfig-6di.6 / myconfig-26z4; [`microvm-qemu-validation-log.md`](./microvm-qemu-validation-log.md) |

`mysbx` does not plan to replace `agentUsers` (a separate uid, not a
sandbox) or `microvm`'s root-owned slot pool (`backends.md` D2
"Alternatives considered").

## Updating this document

- Keep [`backend-comparison.md`](./backend-comparison.md) canonical for
  current behavior; update these `mysbx` summaries when contracts change.
- Preserve the labeled old-tier snapshots and their original commit/date;
  do not turn historical source paths into current installation claims.
- Keep cells short. Cite file paths, options and decision ids, not line
  numbers. Separate implemented paths from recorded live acceptance.
