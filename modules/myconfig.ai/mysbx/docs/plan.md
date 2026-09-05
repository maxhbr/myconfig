<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Implementation plan

Status: agreed. Written at commit `102104b2c4` (2026-09-05), when `mysbx`
implemented `help`, `version` and `init` and confined nothing.

This file is the phase plan. Each MVP work item has its own file in
[`TODOs/`](./TODOs) with the concrete checklist; this file states *what* the
phases are, *why* the MVP is cut where it is cut, and what is explicitly not
designed yet.

Vocabulary is fixed in [`../CONTEXT.md`](../CONTEXT.md) (repo, sidecar, user
config, layer, backend, payload, base, mount). Design decisions live in
[`design/cli.md`](./design/cli.md) and [`design/config.md`](./design/config.md);
the comparison against the existing tiers is
[`feature-comparison.md`](./feature-comparison.md).

## Phase 1 — the MVP

**Goal**: `cd <repo> && mysbx` drops you into a bubblewrap sandbox, and
`mysbx run -- <cmd>` runs one command in it. Both are fully determined by the
two configuration layers, and `--dry-run` prints the exact `bwrap` argv that
would be executed.

**Deliberately in scope**

- The bubblewrap backend, invoked from Rust as a plain argv (no shell, no Nix
  indirection).
- Both configuration layers, with the D7 narrow-not-widen rule *implemented*,
  not deferred.
- `--dry-run` as the acceptance surface: the argv is the product.

**Deliberately out of scope**: model-API credentials, agent wrappers, a
generated user config, workspace clones, any second backend, `nono`-style
per-domain network policy.

### What the MVP is, precisely

| Aspect | Decision |
| --- | --- |
| Commands | bare `mysbx` (interactive shell), `mysbx run -- CMD`, `mysbx init`, `version`, `help` |
| Backend | bubblewrap only; `backend` must say so explicitly (`cli.md` D7) |
| Repo discovery | nearest ancestor with an existing `<dir>.mysbx` → else the git work-tree root → else the current directory |
| Repo mount | always, `rw`, at its real host path inside the sandbox; not expressible in the config |
| Guard | hard error when the resolved repo is `$HOME` or `/` |
| Sidecar | created implicitly by the bare form when missing (`init` stays idempotent) |
| Config schema | `backend`, `network`, `[[mounts]]`, `[env]` — no `[repo]` table |
| Network | shared by default; `network = false` adds `--unshare-net` |
| Layer merge | flags > sidecar > user config > defaults (`cli.md` D6), sidecar may narrow only (`config.md` D7) |
| Base | the `fns/bubblewrap-app.nix` base, reused as a list of decisions (see below) |
| Environment | `--clearenv`, forward `TERM COLORTERM LANG LC_ALL EDITOR VISUAL` when set, then `[env]` |
| Payload shell | `bash` from the MVP's own closure, not the host `$SHELL` |
| Exit codes | `0` / `1` runtime / `2` usage; payload code propagated (`cli.md` D8) |
| Validation | golden argv tests in cargo + `--dry-run`; manual acceptance by the operator |

### The base

Taken from `../../fns/bubblewrap-app.nix`, which is a *parameterised* base, so
every knob is a decision:

| Base element | MVP | Note |
| --- | --- | --- |
| `--unshare-all` | yes | network re-shared unless `network = false` |
| `/nix/store` ro | yes | agents shell out to arbitrary store paths |
| `/usr/bin` ro | yes | `/usr/bin/env` shebangs |
| `--proc`, `--dev` | yes | |
| `/etc/localtime` | yes | timestamps |
| tmpfs `/tmp` | yes | **not** the host-backed `/tmp/<name>` |
| `~/tmp` rw | no | agent-session convenience, not a sandbox essential |
| `/run` | no | D-Bus, PipeWire, the nix-daemon socket, agent sockets |
| dev-tool closure on `PATH` | yes, as-is | git, ripgrep, fd, jq, nix, python3, coreutils, … |
| `OPENAI_API_KEY` auto-forward | **no** | under `mysbx` a key is an ordinary user-config `[env]` entry (`config.md` D6) |

### Honest security claim

With a permissive base and network on by default, the MVP is **not** the
"default deny" tool `config.md` D9 currently describes. Its claim is the one
`../../docs/README.md` makes for `agent-bubblewrap-pi`: *a strong accident
barrier and a moderate malice barrier*. Rewriting D9 to say so is part of
work item 1 — a design doc that overstates the confinement is worse than no
doc.

What the MVP does buy over the existing `agent-bubblewrap-*` wrappers: the
confinement is **data** (two TOML files) instead of Nix call sites, it is
**inspectable** (`--dry-run`), and it is **testable** (one pure function).

### Work items

Ordered; each is independently reviewable.

1. [`TODOs/mvp-1-schema-and-design-docs.md`](./TODOs/mvp-1-schema-and-design-docs.md) — schema change + design docs, no behaviour
2. [`TODOs/mvp-2-repo-discovery.md`](./TODOs/mvp-2-repo-discovery.md) — repo/sidecar resolution, guard, implicit init
3. [`TODOs/mvp-3-layer-merge.md`](./TODOs/mvp-3-layer-merge.md) — the two layers, D7, canonicalization
4. [`TODOs/mvp-4-bwrap-argv.md`](./TODOs/mvp-4-bwrap-argv.md) — the pure argv function and its golden tests
5. [`TODOs/mvp-5-cli-and-dry-run.md`](./TODOs/mvp-5-cli-and-dry-run.md) — the CLI surface
6. [`TODOs/mvp-6-packaging.md`](./TODOs/mvp-6-packaging.md) — Nix packaging and checks

Item 4 carries the security claim; review it against the base table above.

### Definition of done

- `cd <repo> && mysbx run --dry-run -- ls /` prints the argv, one argument per
  line, on stdout, and exits `0`.
- The golden tests pin that argv for: minimal config, a `ro` and a `rw` mount,
  `network = false`, an `[env]` entry, and a sidecar that narrows the user
  config.
- Running in `$HOME` or `/` fails with exit `1` and a `mysbx: ` message.
- A sidecar that tries to widen the user config fails with exit `1`.
- No `myconfig.ai` module outside `mysbx/` changes.
- Manual acceptance (operator, not CI): the sandbox shows only the declared
  mounts, and `~/.ssh` is unreachable.

## Phase 2 and later — sketches, not designs

Each item below is **not designed yet**. They are recorded so the MVP is
visibly not a dead end, and because they were raised and deferred while the
MVP was cut.

**2a — a generated user config.** Seed
`$XDG_CONFIG_HOME/mysbx/config.toml` from home-manager, derived from the same
`myconfig.ai.<agent>.enable` flags the other tiers use. This is the layer that
decides which host credentials are exposed (`config.md` D6), so a generated
allow-list nobody reviewed would undermine D7. First item of phase 2.

**2b — credentials and the model API.** Every existing tier answers this
differently: the host key in the environment (`bwrap-jail`, `nono`), over the
SSH session (`qemu`), rewritten endpoints (`gvisor`), or never in the guest at
all (`microvm`). `config.md` leaves it open. The answer interacts with 2c.

**2c — network policy.** Bring back something finer than the MVP's on/off
switch — `nono`'s `--allow-domain` / `--allow-connect-port` model is the
closest existing precedent, and it is what makes "the sandbox may reach the
model proxy and nothing else" expressible.

**2d — the toolchain and `myconfig.ai.sandboxTools`.** The MVP hardcodes a
dev-tool closure. Five of the six existing tiers honour
`myconfig.ai.sandboxTools.extraPackages` / `.extraEnv`; `mysbx` should join
them rather than grow a parallel list.

**2e — the workspace model.** `bwrap`/`nono`/`qemu` edit the live repo;
`gvisor` and `microvm` use an isolated clone plus an explicit handoff
(`merge` / `fetch` / `push`, branch import). The MVP edits the live repo. A
clone mode is the prerequisite for unattended runs.

**2f — further backends.** `README.md` names podman+gVisor and `nono` next,
qemu and microvm long-term. The MVP's `bwrap_argv` boundary is the seam: a
backend is a function from merged config + payload to a process invocation.

**2g — the D7 widening escape hatch.** `config.md` D7 leaves open how a repo
requests access the user config does not grant (a one-off flag, or an
allow-list keyed by repo path). Not needed until a real repo needs it.

## Updating this file

- When a work item lands, tick it in its `TODOs/` file; delete the file only
  when the item is fully done and the design docs reflect it.
- When a phase-2 sketch turns into work, it gets its own decision in
  `design/` first, then work items.
- Keep the security claim above honest: if the base or the network default
  changes, that paragraph changes with it.
