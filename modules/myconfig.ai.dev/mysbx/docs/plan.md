<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Implementation plan

Status: the phase-1 MVP is complete, and implementation has moved well
beyond it. There are **six implemented backends**: `bubblewrap`, `nono`,
`podman-gvisor`, `podman-krun`, direct `krun`, and `microvm-qemu`.
Backend selection is explicit; heavier backends require host opt-in.
The canonical current inventory, option semantics and evidence limits
are in [`backend-comparison.md`](./backend-comparison.md).

Clone sessions and handoff verbs, resource-limit keys, `run --result`,
and detached interactive paths have landed. That is not universal
support or live acceptance: rootless gVisor's default `ignore-cgroups`
warns and drops all limit flags; VM CPU/RAM settings size the guest;
Podman-krun drops configured PID limits but refuses an explicit env
pin, and direct VM PID limits are refused. Both Podman variants still
refuse detach despite already having unique per-run names. All accepted
detach paths lack a recorded live matrix, and complete baked-store live
acceptance remains unrecorded. `egress` (including `"proxy-only"`) is
still a proposal rejected by strict parsing on every backend.

This is the **historical phase plan**, not the current feature matrix.
Each MVP work item has its original checklist in [`TODOs/`](./TODOs).
The phase-1 scope and acceptance criteria below record the original cut;
its exclusions are not claims that those features are still absent.
Phase-2 entries distinguish their sketches from the current outcome.

Vocabulary is fixed in [`../CONTEXT.md`](../CONTEXT.md) (repo, sidecar, user
config, layer, backend, payload, base, mount). Design decisions live in
[`design/cli.md`](./design/cli.md) and [`design/config.md`](./design/config.md);
the comparison against the older tiers is
[`feature-comparison.md`](./feature-comparison.md), whose non-mysbx rows
are labeled historical snapshots, including removed tiers.

## Phase 1 — the MVP (historical scope)

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
| Environment | `--clearenv`, forward the allowlist of `FORWARDED_ENV_VARS` (lib.rs) when set — the technical block `TERM COLORTERM LANG LC_ALL EDITOR VISUAL` and nothing else — extended by the merged `forward-env` of the configuration layers and the `MYSBX_FORWARD_ENV` wrapper pin, which is how model credentials reach a sandbox (bd myconfig-20j: a credential lives only in the host environment, so an `[env]` entry cannot forward it; the deployment names the key/token/URL variables, this repository hard-codes none) — then `[env]`, then `HOME` and `PATH` (infrastructure, not overridable — `config.md` D14), then `SSL_CERT_FILE`/`GIT_SSL_CAINFO`/`NIX_SSL_CERT_FILE` pointing at the pinned CA bundle when the wrapper set one (infrastructure for the same reason — a layer must not repoint the sandbox's TLS trust anchors at host content, bd myconfig-938) — plus `TMUX_TMPDIR` on an interactive run with a `multiplexer`, infrastructure for the same reason (`config.md` D16/D17). The NixOS module additionally sets `EDITOR`/`VISUAL` to the `neovim` of the dev-tool closure in the generated `[env]` — overriding the forwarded host value on purpose, which routinely names a program the sandbox cannot run (`code --wait`, an `emacsclient` talking to a host socket) or a bare name that is not on the sandbox `PATH` — plus `RIPGREP_CONFIG_PATH` (review-3 item 6) and `GIT_EXTERNAL_DIFF` when Home Manager activates difftastic's `diff.external` (bd myconfig-kvo: the mounted `~/.config/git` would otherwise send `git diff` through the host's difftastic — the baseline entry points `GIT_EXTERNAL_DIFF` at a wrapper that renders the DEFAULT unified diff, so agent payloads parse `git diff` output again) |
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
| tmpfs `/dev/shm` | yes | bubblewrap's `--dev` creates none, so POSIX shared memory is unusable without it — a chrome-family browser crashes outright. Private to the sandbox like every other tmpfs; the podman-gvisor backend gets its own from podman |
| `/etc/localtime` | yes | timestamps |
| tmpfs `/tmp` | yes | **not** the host-backed `/tmp/<name>` |
| tmpfs `$HOME` (`/mysbx-home`) | yes | an in-sandbox home so `cd ~`, `~/.bash_history`, git & co. work; empty, writable, outside `/home` (`config.md` D14) |
| tmux socket dir (`/mysbx-home/.mysbx-tmux`) | only on an interactive run with a `multiplexer`, and only as `TMUX_TMPDIR` | the entry creates it in the home tmpfs; the tmux path is guarded against mounts/state persistence (`config.md` D16/D17). This is not a blanket guarantee for every multiplexer API directory: `state-dirs = [".config/herdr"]` is accepted and persists herdr's API dir on bwrap/nono. Podman-krun uses `/dev/shm/mysbx-tmux` instead |
| host `$HOME` bind | **no** | the host home stays unreachable; its *value* is not forwarded either — exposing parts of it is an explicit `[[mounts]]` entry of a trusted layer (`config.md` D6/D7) |
| `~/tmp` rw | no | agent-session convenience, not a sandbox essential |
| `/run` | no | D-Bus, PipeWire, agent sockets; the nix-daemon socket arrives via the `/nix/var/nix` row above (and only with a shared network); the resolver exception is the only `/run` path bound |
| dev-tool closure on `PATH` | yes, as-is | git, tig, neovim, ripgrep, fd, jq, nix, python3, coreutils, … — the exact shipped list lives in [`nix/mysbx.nix`](../nix/mysbx.nix) (`toolsEnv`; see mvp-6 for what was dropped from the `bubblewrap-app.nix` base list), plus the shared `myconfig.ai.dev.sandboxTools.extraPackages` (phase 2d) and whatever belongs in mysbx alone via `myconfig.ai.dev.mysbx.extraTools` (today: `pi`, `opencode`, `rtk`, the selected multiplexer) |
| `OPENAI_API_KEY` auto-forward | **no, not by default** | the built-in allowlist (`FORWARDED_ENV_VARS`, lib.rs) is technical only, so no secret is inherited by accident; credentials live only in the host environment, never in a store path, so an `[env]` entry cannot forward them either. A deployment names the key, token and URL variables in `forward-env` (the private flake does this via `myconfig.ai.dev.mysbx.forwardedEnvVars`) and they forward when set, reaching the jail/nono tiers' behaviour by declaration rather than by hard-coded default (bd myconfig-20j) |

### Honest security claim

With a permissive base and network on by default, the MVP was **not** a
"default deny" tool. Its accepted claim, now recorded in `config.md` D9,
is the historical bubblewrap tier's *strong accident barrier and moderate
malice barrier*. The whole host store is read-only on bubblewrap/nono;
Podman has an image-owned store, while direct krun/QEMU can select a host
or baked store. Baked mode narrows the **guest-visible** store, not the
outer VM bubblewrap runtime view, which still includes the whole host
store. See the current guide rather than treating the MVP base as every
backend's exact layout.

What the MVP does buy over the existing `agent-bubblewrap-*` wrappers: the
confinement is **data** (two TOML files) instead of Nix call sites, it is
**inspectable** (`--dry-run`), and it is **testable** (one pure function).

### Security floor

The confinement level the shipped backends reach is the intended level and
must be kept — it is a floor, not a snapshot. For the VM-level backends
(`podman-krun`, direct `krun`, `microvm-qemu`), the payload runs under its
own guest kernel, not the host kernel. Host directory shares preserve
host-enforced `ro`/`rw` access; generated baked-store and per-run scratch
disks are explicit additional inputs, not arbitrary host-device grants.
The guest has no host `/dev/kvm` or exposed virtualization extensions, so
nesting a further VM is not supported. Normal mysbx QEMU runs require
host KVM; the launcher's `kvm:tcg` argument is not a supported fallback.

Kernel compatibility alone does not provision a nested runtime:
`krun.nestedPodman.enable` populates the OCI image for `podman-krun`,
not either direct VM's rootfs/PATH. No blanket direct-krun feature-parity
or stronger-than-gVisor claim follows from fewer layers. A change that
weakens the documented boundary is a regression, not an acceptable
trade; the update rule at the end of this file applies.

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
  the run exits `70` and names it (`cli.md` D13, D8).
- The golden tests pin that argv for: minimal config, a `ro` and a `rw` mount,
  `network = false`, an `[env]` entry, and mounts from both layers at once.
- Running in `$HOME`, in a repo whose root contains `$HOME`, or in `/`
  fails with exit `70` and a `mysbx: ` message (`cli.md` D8).
- A sidecar that re-enables the network or overrides a user-set `[env]`
  variable fails with exit `70`; a sidecar `[[mounts]]` entry needs no
  user-config counterpart (`config.md` D7).
- No `myconfig.ai` module outside `mysbx/` changes.
- Manual acceptance (operator, not CI): the sandbox shows only the declared
  mounts, and host `~/.ssh` is unreachable. Current live runs get their own
  per-repo keypair under sandbox `~/.ssh` (`config.md` D22:
  create-if-missing, recreate-if-deleted); clone sessions omit that key and
  persisted state.

## Phase 2 and later — original sketches and current outcomes

Each item below began as a sketch when the MVP was cut; 2a/2c/2d/2e/2f
have since landed. Historical tier examples explain the original choices,
not a current inventory.

**2a — a generated user config.** LANDED: the module generates
`$XDG_CONFIG_HOME/mysbx/config.toml` via Home Manager. Agent integrations
add their declared configuration/state. The two trusted layers still own
exposure (`config.md` D6/D7); generation does not make a credential safe
to expose by default.

**2b — credentials and the model API.** The original tier comparison
contrasted host-key forwarding, SSH seeding, endpoint rewrites and a
host-held proxy key. Current mysbx forwards credential variables only
when declared in `forward-env`; a keyless proxy is usable, but a
proxy-only egress profile remains unimplemented. Transport is backend
specific: host namespace for bwrap/nono, pasta for Podman (TSI from the
container namespace on Podman-krun), host-namespace TSI for direct krun,
and SLiRP for QEMU. `MYSBX_PODMAN_ENV` and
`MYSBX_PODMAN_PASTA_SPEC` are consumed by **both** Podman variants.

**2c — network policy.** The finer-than-on/off sketch became nono's
`--allow-domain` / `--allow-connect-port` policy. It is not the separate
proxy-only egress proposal, and is not a domain-and-port conjunction.

LANDED. The allowlist schema (bd myconfig-mo3.1) is
`allow-domains`/`connect-ports`/`listen-ports` (`config.md` D21): both
layers concatenate, user first, deduped keeping the first occurrence.
Only nono supports these keys; the other five backends refuse them.
`listen-ports` alone, URL-form domains, and allowlists combined with
`network = false` are refused.

Pinned nono is **0.80.0**. Its pathname AF_UNIX mediation installs
seccomp even under unrestricted IP networking (`AllowAll`); the older
0.74.0 no-filter and rate-limit claims are not current guarantees.
Domain policy uses a proxy, while port grants are independent. In mixed
policy the default supervisor still denies direct remote connections;
independent grants are not proof of an arbitrary-host bypass, and
proxy-bypass variables can instead create connectivity conflicts. Exact
combined policies need live testing; see the current guide.

PROPOSED, NOT LANDED: `egress = "proxy-only"` (`config.md` D20, bd
myconfig-mo3.2). The strict parser rejects `egress` on **every** backend,
including bubblewrap. The isolated namespace plus LiteLLM socket relay
is a design proposal, not a shipped enforcement path.

**2d — the toolchain and `myconfig.ai.sandboxTools`.** DONE (bd
myconfig-9mw): `mysbx` consumes
`myconfig.ai.dev.sandboxTools.extraPackages` / `.extraEnv` like every
other tier — the packages concatenate onto the dev-tool closure via
`extraTools`, the env lands in the generated `[env]` table.
`myconfig.ai.dev.mysbx.extraTools` stays as the mysbx-specific
extension ON TOP of the hook (selected-multiplexer payload, per-agent
CLIs like `pi`), never as a parallel copy of it: tooling wanted in
EVERY tier goes through the hook exactly once.

**2e — the workspace model.** LANDED (bd myconfig-o6z):
[`design/workspace.md`](./design/workspace.md) — `live` stays the
default (`config.md` D13). `--session NAME` selects an isolated clone at
`<repo>.mysbx/clones/NAME`, branch `agent/mysbx/NAME`, with its own
worktrees and result file. Configured mounts are forced `ro`, `--rw` is
refused, and host repo/git metadata/worktrees, persisted state and the
per-repo SSH key are omitted. Host-side `fetch` / `merge` / `push` /
`diff` and `session list` / `destroy` / `hunk` provide the handoff.
`run --result` and detached interactive runs do **not** automatically
force clone mode; a clone is still opt-in via `--session`.

**2f — further backends.** LANDED: all six backends in the
[current guide](./backend-comparison.md), not a pending QEMU/microVM
roadmap. The MVP's pure argv boundary remains the seam, with backend
builders and golden tests:

- `bubblewrap`: the host-kernel namespace base.
- `nono`: bubblewrap's view followed by Landlock, seccomp and optional
  port/proxy policy. Orca is still refused; bwrap-like layout support is
  not blanket feature parity.
- `podman-gvisor`: rootless Podman with runsc and an image-owned store.
- `podman-krun`: the shared Podman builder with crun/libkrun, an
  image-owned store and optional in-image guest Nix/nested Podman.
- Direct `krun`: bubblewrap plus the libkrun launcher, no OCI image.
- `microvm-qemu`: bubblewrap plus QEMU and separate rootless virtiofsd
  processes, using the shared direct guest init and a pinned kernel.

The direct VMs choose `store = "host" | "baked"`; explicit configuration
wins, otherwise a nonempty `MYSBX_KRUN_STORE_DISK` pin selects baked,
and no pin selects host. Their Nix binary is always in the tools, but
the direct init needs scratch for managed writable Nix and has no tmpfs
fallback. Current wiring incorrectly pins scratch only with
`podman.enable && krun.nix.enable`, so direct-only hosts can miss it.

Waypipe works on bwrap/nono and provisioned gVisor; Podman-krun and QEMU
refuse it. Direct krun currently **accepts it in error without a guest
implementation**. VM compatibility is not a proven stronger boundary
than gVisor, nor proof of every provisioned Podman-krun feature on the
direct path.

Evidence is historical and workload-specific. The 282 ms direct spike
is not a current benchmark; the later 25 min 42 s direct / 12.7 s
Podman-krun figures are smoke-suite totals, not isolated boot timings.
The later silent direct store-walk failure has no confirmed fd root
cause. QEMU's eleven KVM probes prove their listed scope, including
initial terminal size, not mid-session resize, baked-store acceptance,
scratch stress, nested Podman or detach. See
[`krun-validation-log.md`](./krun-validation-log.md) and
[`microvm-qemu-validation-log.md`](./microvm-qemu-validation-log.md).

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
