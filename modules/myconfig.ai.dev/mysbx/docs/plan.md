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
- Both configuration layers, with the D7 trust model *implemented*, not
  deferred: both files declare mounts directly, the sidecar keeps the
  `[env]`/`network` restrictions.
- `--dry-run` as the acceptance surface: the argv is the product.

**Deliberately out of scope**: model-API credentials, agent wrappers, a
generated user config, workspace clones, any second backend, `nono`-style
per-domain network policy.

### What the MVP is, precisely

| Aspect | Decision |
| --- | --- |
| Commands | bare `mysbx` (interactive shell), `mysbx run -- CMD`, `mysbx init`, `mysbx edit` (the sidecar config in `$EDITOR`, `cli.md` D12), `version`, `help` |
| Backend | bubblewrap only; `backend` must say so explicitly (`cli.md` D7) |
| Repo discovery | nearest ancestor with an existing `<dir>.mysbx` → else the git work-tree root → else the current directory |
| Repo mount | always, `rw`, at its real host path inside the sandbox; not expressible in the config |
| Guard | hard error when the resolved repo is `$HOME`, a directory containing `$HOME`, or `/` |
| Sidecar | created only by the explicit `mysbx init` (idempotent) or `mysbx edit`; a run refuses an uninitialized repo with the init hint (`cli.md` D13) |
| Config schema | `backend`, `network`, `multiplexer`, `[[mounts]]`, `[env]`, `state-dirs` — no `[repo]` table |
| Network | shared by default; `network = false` adds `--unshare-net` |
| Layer merge | flags > sidecar > user config > defaults (`cli.md` D6); both layers' `[[mounts]]` concatenate, user layer first (`config.md` D7) |
| Base | the `fns/bubblewrap-app.nix` base, reused as a list of decisions (see below) |
| Environment | `--clearenv`, forward the allowlist of `FORWARDED_ENV_VARS` (lib.rs) when set — the terminal/locale block `TERM COLORTERM LANG LC_ALL EDITOR VISUAL` plus the model-credential block `OPENAI_API_KEY OPENAI_BASE_URL ANTHROPIC_API_KEY ANTHROPIC_BASE_URL ANTHROPIC_AUTH_TOKEN OPENROUTER_API_KEY OPENROUTER_BASE_URL` (bd myconfig-20j: a credential lives only in the host environment, so an `[env]` entry cannot forward it; the same always-forward the jail/nono tiers give `OPENAI_API_KEY`) — then `[env]`, then `HOME` and `PATH` (infrastructure, not overridable — `config.md` D14), then `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` pointing at the pinned CA bundle when the wrapper set one (infrastructure for the same reason — a layer must not repoint the sandbox's TLS trust anchors at host content, bd myconfig-938) — plus `TMUX_TMPDIR` on an interactive run with a `multiplexer`, infrastructure for the same reason (`config.md` D16/D17). The NixOS module additionally sets `RIPGREP_CONFIG_PATH` in the generated `[env]` (review-3 item 6) and `GIT_EXTERNAL_DIFF` when Home Manager activates difftastic's `diff.external` (bd myconfig-kvo: the mounted `~/.config/git` would otherwise send `git diff` through the host's difftastic — the baseline entry points `GIT_EXTERNAL_DIFF` at a wrapper that renders the DEFAULT unified diff, so agent payloads parse `git diff` output again) |
| Payload shell | `bash` from the MVP's own closure, not the host `$SHELL` — replaced by the pinned entry of the selected `multiplexer`, interactive form only (`cli.md` D11) |
| Exit codes | `0` / `1` runtime / `2` usage; payload code propagated (`cli.md` D8) |
| Validation | golden argv tests in cargo + `--dry-run`; manual acceptance by the operator |

### The base

Taken from `../../myconfig.ai.dev/fns/bubblewrap-app.nix`, which is a *parameterised* base, so
every knob is a decision:

| Base element | MVP | Note |
| --- | --- | --- |
| `--unshare-all` | yes | network re-shared unless `network = false` |
| resolver set (ro) | yes, when network is shared | `/etc/hosts`, `/etc/nsswitch.conf`, `/etc/resolv.conf`, `/etc/ssl`, `/etc/static`, `/run/systemd/resolve` — `--ro-bind-try`, the resolver path set of the `network` combinator of `fns/bubblewrap-app.nix` plus `/etc/static`; DNS/TLS are unusable without them (review-1 finding 5). `/etc/static` is the NixOS half of the set (bd myconfig-938): `/etc/ssl` is a symlink farm whose entries point at `/etc/static/ssl/...`, and bwrap resolves only the SOURCE path of a bind, not the symlinks inside it — without the bind the CA bundle dangles inside the sandbox and every TLS tool fails with `unable to get local issuer certificate` (the simpler wrapper `fns/bubblewrap-simple-app.nix` walks the same set). `/run/systemd/resolve` is the only `/run` exception to the row below, ro and narrow |
| `/nix/store` ro | yes | agents shell out to arbitrary store paths |
| `/nix/var/nix` ro | only when the network is shared, `--ro-bind-try` | the store database and the nix-daemon socket: `nix` on the tools PATH is unusable without them (review-1 finding 6), but a ro bind does not stop the payload from *talking* to the daemon, and the daemon builds fixed-output derivations — which keep network access. Binding it under `network = false` would make the report's "denied" false, so it rides with `--share-net` (review-2 item 3). Nor can a configured mount smuggle it in: a source at, below, or an ancestor of `/nix/var/nix` — binding `/nix` read-only exposes the socket through the wider window — is refused when the network is denied (review-3 item 2). Consequence, said out loud: `nix` needs the shared network |
| host `/etc/nix/nix.conf` | **no** | it may hold `access-tokens` (GitHub/GitLab credentials) and `netrc-file` pointers; a read-only bind hands them to the payload all the same (review-2 item 3) |
| generated `nix.conf` ro | yes, when the wrapper pins one (`MYSBX_NIX_CONF`) | a minimal *sanitized* client config from `nix/mysbx.nix` — flake CLI plus the public cache, no credentials, nothing copied from the host — bound at `/etc/nix/nix.conf`. Unwrapped builds pin nothing and run `nix` with its built-in defaults |
| pinned CA bundle | yes, when the wrapper pins one (`MYSBX_CA_BUNDLE`) | nss-cacert's `ca-bundle.crt` from the wrapper's own closure, set as `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` in the sandbox env, after `[env]` like `HOME`/`PATH` (bd myconfig-938). Belt and suspenders on top of the resolver binds: the pinned bundle works whatever the host's `/etc` layout is. The same mechanism the gvisor agent image uses. Unwrapped builds pin nothing and rely on the resolver binds alone |
| `/usr/bin` ro | yes | `/usr/bin/env` shebangs |
| `/bin/sh` ro | yes, when the wrapper pins one (`MYSBX_BINSH`) | a de-facto ABI of the Unix userland: tmux runs every `run-shell`/`if-shell`/`#()` job through `execl("/bin/sh", …)` (tmux ≥ 3.5a hardcodes `_PATH_BSHELL` for jobs — `default-shell` covers panes and popups only), and `#!/bin/sh` shebangs need it. The minimal root has no `/bin` at all, so without the bind every such job dies with `execl failed` — on the workmux sidebar this surfaced as `'kill -USR1 $(tmux show-option …)' returned 1` popups and sidebars that never appear. The pin is bash's own `bin/sh` from the wrapper's closure (the same bind `vendor/alexdavid-jail.nix`'s base combinator makes for the jail tier); the dest is protected like every base-bind root. Unwrapped builds pin nothing and run without `/bin/sh` |
| `--proc`, `--dev` | yes | |
| `/etc/localtime` | yes | timestamps |
| tmpfs `/tmp` | yes | **not** the host-backed `/tmp/<name>` |
| tmpfs `$HOME` (`/mysbx-home`) | yes | an in-sandbox home so `cd ~`, `~/.bash_history`, git & co. work; empty, writable, outside `/home` (`config.md` D14) |
| multiplexer socket dir (`/mysbx-home/.mysbx-tmux`) | only on an interactive run with a `multiplexer`, and only as `TMUX_TMPDIR` | no bind and no tmpfs of its own: the selected multiplexer's entry creates it inside the home tmpfs, so its server is reachable from this sandbox alone — never from the host's `/tmp/tmux-<uid>` (`/tmp` is a fresh tmpfs) and never from another sandbox (nothing may bind or persist the path, `config.md` D16/D17) |
| host `$HOME` bind | **no** | the host home stays unreachable; its *value* is not forwarded either — exposing parts of it is an explicit `[[mounts]]` entry of a trusted layer (`config.md` D6/D7) |
| `~/tmp` rw | no | agent-session convenience, not a sandbox essential |
| `/run` | no | D-Bus, PipeWire, agent sockets; the nix-daemon socket arrives via the `/nix/var/nix` row above (and only with a shared network); the resolver exception is the only `/run` path bound |
| dev-tool closure on `PATH` | yes, as-is | git, tig, ripgrep, fd, jq, nix, python3, coreutils, … — the exact shipped list lives in [`nix/mysbx.nix`](./nix/mysbx.nix) (`toolsEnv`; see mvp-6 for what was dropped from the `bubblewrap-app.nix` base list), plus the shared `myconfig.ai.dev.sandboxTools.extraPackages` (phase 2d) and whatever belongs in mysbx alone via `myconfig.ai.mysbx.extraTools` (today: `pi`, `opencode`, `rtk`, the selected multiplexer) |
| `OPENAI_API_KEY` auto-forward | **yes, with the credential block** — credentials live only in the host environment, never in a store path, so an `[env]` entry cannot forward them; the allowlist (`FORWARDED_ENV_VARS`, lib.rs) forwards the `OPENAI_*`/`ANTHROPIC_*`/`OPENROUTER_*` block when set (bd myconfig-20j), mirroring the always-`OPENAI_API_KEY` of the jail/nono tiers |

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
2. [`TODOs/mvp-2-repo-discovery.md`](./TODOs/mvp-2-repo-discovery.md) — repo/sidecar resolution, guard, init (the implicit init it describes was later dropped: `cli.md` D13)
3. [`TODOs/mvp-3-layer-merge.md`](./TODOs/mvp-3-layer-merge.md) — the two layers, D7, canonicalization
4. [`TODOs/mvp-4-bwrap-argv.md`](./TODOs/mvp-4-bwrap-argv.md) — the pure argv function and its golden tests
5. [`TODOs/mvp-5-cli-and-dry-run.md`](./TODOs/mvp-5-cli-and-dry-run.md) — the CLI surface
6. [`TODOs/mvp-6-packaging.md`](./TODOs/mvp-6-packaging.md) — Nix packaging and checks

Item 4 carries the security claim; review it against the base table above.
All six items are done; the MVP is complete.

### Definition of done

- `cd <repo> && mysbx init && mysbx run --dry-run -- ls /` prints the
  bwrap executable (argv[0], review-1 finding 7) followed by the argv,
  one argument per line, on stdout, and exits `0`. Without the `init`
  the run exits `1` and names it (`cli.md` D13).
- The golden tests pin that argv for: minimal config, a `ro` and a `rw` mount,
  `network = false`, an `[env]` entry, and mounts from both layers at once.
- Running in `$HOME`, in a repo whose root contains `$HOME`, or in `/`
  fails with exit `1` and a `mysbx: ` message.
- A sidecar that re-enables the network or overrides a user-set `[env]`
  variable fails with exit `1`; a sidecar `[[mounts]]` entry needs no
  user-config counterpart (`config.md` D7).
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

**2d — the toolchain and `myconfig.ai.sandboxTools`.** DONE (bd
myconfig-9mw): `mysbx` consumes
`myconfig.ai.dev.sandboxTools.extraPackages` / `.extraEnv` like every
other tier — the packages concatenate onto the dev-tool closure via
`extraTools`, the env lands in the generated `[env]` table.
`myconfig.ai.dev.mysbx.extraTools` stays as the mysbx-specific
extension ON TOP of the hook (selected-multiplexer payload, per-agent
CLIs like `pi`), never as a parallel copy of it: tooling wanted in
EVERY tier goes through the hook exactly once.

**2e — the workspace model.** `bwrap`/`nono`/`qemu` edit the live repo;
`gvisor` and `microvm` use an isolated clone plus an explicit handoff
(`merge` / `fetch` / `push`, branch import). The MVP edits the live repo. A
clone mode is the prerequisite for unattended runs.

**2f — further backends.** `README.md` names podman+gVisor and `nono` next,
qemu and microvm long-term. The MVP's `bwrap_argv` boundary is the seam: a
backend is a function from merged config + payload to a process invocation.

**2g — per-repo opt-out of user mounts.** `config.md` D7 settled the other
direction (a sidecar declares its own mounts), but leaves open how a user
drops one of their own host-wide mounts for a single suspicious repository.
Not needed until a real repo needs it.

## Updating this file

- When a work item lands, tick it in its `TODOs/` file; delete the file only
  when the item is fully done and the design docs reflect it.
- When a phase-2 sketch turns into work, it gets its own decision in
  `design/` first, then work items.
- Keep the security claim above honest: if the base or the network default
  changes, that paragraph changes with it.
