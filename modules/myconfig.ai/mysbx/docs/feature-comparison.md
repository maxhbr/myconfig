<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Feature comparison: `mysbx` vs. the existing sandboxing tiers

Status: snapshot. Point of analysis: commit `cedb47d3b3`
(`cedb47d3b34d4c7b9745ebab843b5a6f9e65d0ed`, 2026-09-05).

`mysbx` is **not implemented yet** — today the binary knows `help`, `version`
and `init` and confines nothing (`../mysbx-rs/src/lib.rs`). This document is
the target-state checklist: it puts the sandbox implementations that already
exist in this repo side by side, so the `mysbx` design decisions
([`design/cli.md`](./design/cli.md), [`design/config.md`](./design/config.md))
can be checked against what is already working.

The authoritative ladder description (prose, per tier) is
[`../../docs/README.md`](../../docs/README.md) — this file does not replace it,
it only compares axes across tiers from the `mysbx` point of view.

Planned `mysbx` behaviour is marked `(planned)` and carries the decision id it
comes from (e.g. `cli.md D2`), so drift between design and code stays visible.

## 1. The candidates

| Key | Tier | Module / entry point | Generated commands |
| --- | --- | --- | --- |
| `agentUsers` | 1 | [`myconfig.agentUsers.nix`](../../../myconfig.agentUsers.nix) | `<name>-tmux`, `<name>-alacritty-tmux` |
| `bwrap-jail` | 2 | [`fns/bubblewrap-app.nix`](../../fns/bubblewrap-app.nix) + [`myconfig.ai.jail.nix`](../../myconfig.ai.jail.nix) | `agent-bubblewrap-pi`, `-opencode`, `-claude`, `…-tmp`, `…-worktree` |
| `bwrap-simple` | 2 | [`fns/bubblewrap-simple-app.nix`](../../fns/bubblewrap-simple-app.nix) | `<name>-bwrap` (`pi-bwrap`, `codex-bwrap`, `fish-bwrap`, …) |
| `nono` | 2 | [`myconfig.ai.nono-agent-sandbox.nix`](../../myconfig.ai.nono-agent-sandbox.nix) + [`fns/nono-app.nix`](../../fns/nono-app.nix) | `agent-nono-pi`, `-opencode`, `-claude`, `-codex` |
| `qemu` | 3 | [`myconfig.ai.qemu-agent-sandbox/`](../../myconfig.ai.qemu-agent-sandbox) | `agent-qemu-pi`, `agent-qemu-herdr`, `agent-qemu-workmux-tmux` |
| `gvisor` | 3.5 | [`myconfig.ai.gvisor-agent-sandbox/`](../../myconfig.ai.gvisor-agent-sandbox) | `agent-gvisor` |
| `microvm` | 4 | [`myconfig.ai.microvm/`](../../myconfig.ai.microvm) | `agent-microvm`, `microvm-<agent>` workmux panes |
| `mysbx` | — | [`mysbx/`](..) | `mysbx` |

`agentUsers` is listed for completeness but left out of the tables below: it
does not confine a process, it moves it to another uid. `mysbx` does not aim
to replace it.

## 2. CLI surface

| Axis | `bwrap-jail` | `bwrap-simple` | `nono` | `qemu` | `gvisor` | `microvm` | `mysbx` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Shape | one wrapper per agent | one wrapper per app | one wrapper per agent | one wrapper per agent | one binary, verb subcommands | one binary, verb subcommands | one binary, verb subcommands (`cli.md` D3) |
| Bare invocation | starts the agent in `$PWD` | starts the app in `$PWD` | starts the agent in `$PWD` | boots a VM, execs the agent | `agent-gvisor NAME` = `start NAME` | usage, exit `2` | prints usage today; **enter a sandbox shell** (planned, `cli.md` D2) |
| Subcommands | none | none | none | none | `start list status run shell logs stop merge fetch push destroy doctor` | `run stop destroy status doctor capabilities list dashboard ssh console submit cancel recover usage workspace-remove` | `init version help`; `run COMMAND` planned (`cli.md` D3) |
| Own flags | none (args → agent) | none (args → app) | none (args → agent) | none (args → agent) | `--repo --base --branch --image --config --mount --env --env-file --network --memory --cpus --pids-limit --nix --force --home-seed …` | `--name --repository --agent --branch --resource-class --wait --attach --timeout --prompt-file --persist-agent-state --no-preflight` | `-h/--help`, `-V/--version` |
| `--` payload separator | n/a | n/a | n/a | n/a | yes (`-- COMMAND…`) | yes (`ssh <slot> -- cmd…`) | yes (planned, `cli.md` D4) |
| Unit of work | the CWD | the CWD | the CWD | the CWD | a *named session* per repo | a *named task* per repo | the repository = the CWD (`cli.md` D1) |
| Unattended/batch mode | no | no | no | no | `run --detach` | `submit` (job spec + prompt file, structured result) | no |
| Backend choice | fixed (bubblewrap) | fixed (bubblewrap) | fixed (nono) | fixed (QEMU) | fixed (podman+runsc), runtime overridable via `AGENT_GVISOR_PODMAN_RUNTIME` | fixed (Cloud Hypervisor) | explicit config/flag, never auto-detected (`cli.md` D7, `config.backend`) |
| Configuration input | NixOS options at build time | Nix call site | NixOS options at build time | `AGENT_QEMU_PI_*` env vars | flags + `AGENT_GVISOR_*` env + `--env-file` | NixOS options + flags | TOML: user config + sidecar (`config.md` D1) |
| Implementation | `writeShellApplication` | `writeShellApplication` | `writeShellApplication` | shell + impure `nix build` | Rust, zero deps | large generated bash | Rust, zero deps, hand-rolled parser (`cli.md` D5) |
| Exit-code contract | none stated | none stated | none stated | none stated | non-zero on failure; `doctor` non-zero when broken | `0/1/124/130/70` documented | `0/1/2` + payload passthrough (`cli.md` D8) |
| Output convention | none stated | none stated | none stated | none stated | podman/git output passthrough | tables, JSON result for `submit` | stderr `mysbx: `, stdout `## ` (`cli.md` D9) |
| Shell completion | n/a | n/a | n/a | n/a | fish completion, sync-checked | none | none |

Sources: `../mysbx-rs/src/usage.txt`, `../mysbx-rs/src/lib.rs`,
[`design/cli.md`](./design/cli.md),
`../../myconfig.ai.gvisor-agent-sandbox/rust/src/usage.txt`,
`../../myconfig.ai.microvm/launcher.nix` (the `usage()` heredoc),
`../../myconfig.ai.microvm/docs/agent-microvm-howto.md` (exit codes),
`../../fns/bubblewrap-app.nix`, `../../fns/bubblewrap-simple-app.nix`,
`../../fns/nono-app.nix`,
`../../myconfig.ai.qemu-agent-sandbox/builders.nix`.

## 3. Sandboxing features

| Axis | `bwrap-jail` | `bwrap-simple` | `nono` | `qemu` | `gvisor` | `microvm` | `mysbx` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Mechanism | bubblewrap namespaces | bubblewrap namespaces | Landlock + seccomp (`nono`) | QEMU microVM, own kernel | rootless podman + `runsc` | Cloud Hypervisor, own kernel | bubblewrap first, then podman+gVisor / nono, later qemu/microvm (`../README.md` roadmap) |
| Kernel boundary | no | no | no | yes | user-space kernel | yes | none yet |
| Runs as | your uid | your uid | your uid | guest `agent` user | container user | guest `agent` user | your uid (planned) |
| Filesystem policy | curated allow-list of binds, env cleared (`--clearenv`) | ro config dirs + writable XDG dirs | `--allow` / `--read` / `--allow-cwd` | virtiofs shares only | image + explicit `--mount` | virtiofs shares only | **default deny**, everything declared (`config.md` D9) |
| Workspace | `$PWD` rw (+ `__worktrees` sibling) | `$PWD` rw | `$PWD` rw (`--allow-cwd`) | `$PWD` rw at `/workspace` | isolated git clone at `<repo>__agent-gvisor/NAME`, mounted at the host path — host checkout never bind-mounted | standalone clone, `workspaceLayout = central\|beside-repo` | repo rw by default (`[repo] path/mode`), no clone/worktree model chosen yet |
| Extra mounts | `extraReadOnly/ReadWriteEnvPaths`, `JAIL_EXTRA_*_PATHS` | `readOnlyConfigDirs`, `writableDirs` | `extraAllowDirs`, `extraReadOnlyDirs`, `--allow-unix-socket` | fixed (CWD + store) | `--mount`/`--config HOST:DEST[:ro\|rw]` | fixed share set | `[[mounts]] path/dest/mode`, `ro`/`rw` only (`../mysbx-rs/src/config.rs`) |
| Host `/nix/store` | bound read-only (`bindFullNixStore`) | via the app closure | via the app closure | read-only virtiofs | not shared; optional writable store volume (`--nix`) | not shared — own EROFS guest store | undecided |
| Network default | on (`network` combinator: resolv.conf + CA bundle) | on (`shareNet = true`) | off unless `--allow-domain` / `--allow-connect-port` / `--listen-port` | SLiRP user-mode NAT, outbound only + one loopback SSH port | rootless podman default, `--network`/`AGENT_GVISOR_NETWORK` (pasta spec), in-sandbox loopback forwarders | private bridge `agentbr0` with per-TAP L2 isolation, `networkProfile` (default `proxy-only`) | off (`network = false`, `config.md` D9) |
| Env forwarding | `try-fwd-env` list + `myconfig.ai.jail.fwdEnvs`, always `OPENAI_API_KEY` | `envVars` attrset | same shape via `myconfig.ai.nono.fwdEnvs` | pushed over the SSH session env at launch | `--env` / `--env-file` | none needed for model access | `[env]` table |
| Model credentials | real host key inside the sandbox | n/a | real host key inside the sandbox | real key, over SSH env | seeded config, endpoints rewritten to a sandbox-reachable proxy | **never reaches the guest** — host LiteLLM via bridge-only forwarder | **open question** — not decided in `config.md` |
| Agent-config seeding | `try-ro-bind` of `configDirs`, rw `userDataDirs` | `readOnlyConfigDirs` | `--read` of config dirs, `--allow` of state dirs | [`fns/seed-agent-config.nix`](../../fns/seed-agent-config.nix), rsync over SSH | `home.seedPaths` + `AGENT_GVISOR_HOME_SEED_REWRITE` | root-owned staged copy via `config-seed.nix` | user config decides which host config is exposed (`config.md` D6) |
| Per-repo state dir | none | none | none | throwaway runtime dir | `<repo>__agent-gvisor/` + registry | root-owned task→clone index under `runtimeRoot` | sidecar `<repo>.mysbx/`, outside repo *and* sandbox (`config.md` D2, D10) |
| Repo-local config trusted? | n/a | n/a | n/a | n/a | n/a | n/a | no — never read from inside the repo; sidecar may narrow, not widen (`config.md` D3, D7) |
| Resource limits | none | none | none | VM `vcpu`/`mem` | `--memory --cpus --pids-limit` | prebuilt `resourceClasses` (vcpu/mem/slots) | `backend` limits foreseen (`config.md` D5), not implemented |
| Refuses `$HOME` as CWD | yes (`rejectHomeCwd`) | — | yes (`rejectHomeCwd`) | yes | n/a (clone-based) | n/a (clone-based) | not implemented |
| `myconfig.ai.sandboxTools` hook | yes | no | yes | yes | yes | yes | no |
| Result handoff | edits are live in `$PWD` | live | live | live | `merge` / `fetch` / `push` subcommands | import the branch from the clone | live (planned) |
| Startup cost | ~none | ~none | ~none | seconds (boot) | ~a second (container) | prebuilt slot + host config | ~none (planned) |

Sources: `../../fns/bubblewrap-app.nix`, `../../fns/bubblewrap-simple-app.nix`,
`../../fns/nono-app.nix`, `../../myconfig.ai.jail.nix`, `../../myconfig.ai.nono.nix`,
`../../myconfig.ai.qemu-agent-sandbox/builders.nix`,
`../../myconfig.ai.gvisor-agent-sandbox/README.md` and its `docs/spec.md`,
`../../myconfig.ai.microvm/docs/agent-microvm-security-model.md`,
`../../myconfig.ai.sandboxTools.nix`, `../mysbx-rs/src/config.rs`,
`../mysbx-rs/tests/assets/valid/full.toml`, [`design/config.md`](./design/config.md).

## 4. Implementation state

| Axis | `bwrap-jail` | `bwrap-simple` | `nono` | `qemu` | `gvisor` | `microvm` | `mysbx` |
| --- | --- | --- | --- | --- | --- | --- | --- |
| Enable option | implicit with the agent modules | implicit | `myconfig.ai.nono-agent-sandbox.enable`, `mkDefault true` under `myconfig.ai.enable` | implicit with `pi`/`herdr` | `myconfig.ai.gvisor-agent-sandbox.enable`, default off | `myconfig.ai.microvm.enable`, default off | `myconfig.ai.mysbx.enable`, default off |
| Enabled on | every AI host | every AI host | every AI host | every AI host | `f13` | `f13` | `f13` |
| Written spec | module comments | none | module comments | `agent-qemu-pi.README.md` | `docs/spec.md` (authoritative) | 9 docs incl. architecture + security model | `docs/design/{cli,config}.md` (draft) |
| Automated tests | eval only | eval only | eval only | eval only | cargo tests + executed CLI stub harness + completion check, in `nix flake check` | `tests/microvm.nix` + eval assertions | cargo tests over `tests/assets/{valid,invalid}` (config parsing only) |
| Runtime validation | manual | manual | manual | live-boot caveat noted in its README | `agent-gvisor doctor` | `runtime-validation.sh` on real KVM (required, see its docs) | none — nothing to validate yet |
| Maturity | works, daily driver | works | works, least exercised | works, boot validation caveat | most complete CLI contract | most complete isolation + docs | **skeleton**: `init` only, no backend |

Sources: `../../default.nix` (the `mkDefault` for `nono`),
`../../../../hosts/host.f13/ai.f13.nix`, `../default.nix`, `../README.md`,
`../../myconfig.ai.gvisor-agent-sandbox/nix/checks.nix`,
`../../myconfig.ai.microvm/runtime-validation.sh`,
`../../docs/README.md`.

## 5. What `mysbx` still has to build

Everything below exists in at least one tier above and has no counterpart in
`../mysbx-rs/src/`:

- **A backend.** No confinement at all is implemented; `run(...)` in
  `../mysbx-rs/src/lib.rs` dispatches only `help|version|init`.
- **Entering the sandbox** — the primary action per `cli.md` D2.
- **`run COMMAND` / the `--` payload split** (`cli.md` D3, D4).
- **A credential story.** Every existing tier had to answer this and they
  answer it differently (host key in the env vs. SSH `SetEnv` vs. proxy-only
  with no key in the guest). `config.md` leaves it open; the `microvm` answer
  is the strongest and the most expensive.
- **A workspace model.** `bwrap`/`nono`/`qemu` use the live CWD, `gvisor` and
  `microvm` use an isolated clone plus a handoff (`merge`/`fetch`/`push`,
  branch import). `mysbx` currently only says "repo rw by default".
- **Refusing `$HOME` as CWD** — a cheap guardrail that both bubblewrap and
  nono wrappers already have.
- **`myconfig.ai.sandboxTools` participation** — the cross-tier hook for
  shared sandbox packages/env that five of six tiers honour.
- **Flag layer.** `cli.md` D6 defines flags > sidecar > user config >
  defaults, but no flag beyond `--help`/`--version` exists.

## 6. What to take from where

- **CLI shape**: `agent-gvisor` — verb subcommands, a positional shorthand for
  the common case, and a hard `--` split. Its `docs/spec.md`-first workflow
  (spec, then tests, then code) is the one to copy for `mysbx`.
- **Exit codes and machine-readable results**: `agent-microvm`
  (`0/1/124/130/70`, JSON result). `cli.md` D8 is a subset; extend it before
  the first backend lands rather than after.
- **Isolation defaults**: `microvm`'s "no key in the guest, egress only to a
  local proxy" is the target for D9's default-deny network.
- **Mount vocabulary**: `--config HOST:DEST[:ro|rw]` from `agent-gvisor` maps
  almost 1:1 to the `[[mounts]]` table, so the flag layer can reuse it.
- **Cross-tier hook**: honour `myconfig.ai.sandboxTools.extraPackages` /
  `.extraEnv` from the start; it is how the other tiers stay consistent.

## Updating this document

- Update it when a tier is added or removed, when a `mysbx` subcommand or
  backend lands, or when a decision in `design/` changes.
- Keep cells to one line and link the owning doc instead of restating it.
- Do not cite line numbers — they rot; cite file paths plus the option or
  function name.
- Refresh the commit hash and date in the status line whenever the tables are
  re-checked against the code.
