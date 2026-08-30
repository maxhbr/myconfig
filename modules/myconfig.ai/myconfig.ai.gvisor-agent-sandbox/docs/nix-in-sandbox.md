# Nix inside the sandbox (`nix build` / `nix run` in a session)

An agent session occasionally needs real Nix: to build the flake it is
working on, to enter `nix develop`, or to build a tool the image does not
carry. This document explains how `--nix` sessions get a *writable* Nix
store, why the alternatives were rejected, what still does not work under
rootless Podman + gVisor, and which trade-offs were accepted. The module
side is `nix.enable` in `../default.nix`; the CLI contract is
`docs/spec.md` §2 (`--nix`), §4 (`AGENT_GVISOR_NIX*`), §8 (`meta` `nix=`),
§10 (the argv); the design lives in the Rust CLI
(`rust/src/podman.rs`, `rust/src/state.rs`).

## 1. Mechanism

A `--nix` session (recorded in `meta` as `nix=true`; default on when the
module sets `nix.enable`, off otherwise) differs from a plain session in
four ways:

1. **A per-session Podman volume at `/nix/store`** (name
   `<container>-nix`, i.e. `agent-<repo-id>-<name>-nix`):

   ```
   --mount type=volume,src=<container>-nix,dst=/nix/store
   ```

   The image *contains* a real `/nix/store` (the closure of the baked
   packages — every `/bin/*` is a symlink into it). Podman's **copy-up**
   seeds the empty named volume with the image's store content the first
   time the container starts. This is the property that makes the design
   work at all: the toolchain (`git`, `bash`, the agent CLIs, and `nix`
   itself — baked into the image when `nix.enable` is set) keeps working
   *before the first in-container process runs*, and the store becomes
   writable, so nix can substitute new paths into it.

   The volume is the only writable Nix store state; it lives in the
   rootless Podman volume store (`~/.local/share/containers/storage/`),
   NOT in the session directory. `destroy` removes it (`podman volume
   rm`, after a `volume exists` guard); `stop` + `run` keep it, and even `run --replace`
   (container recreation) keeps it, so a rebuilt store survives session
   restarts. `start --force` destroys and re-creates it (copy-up runs
   again).

2. **Daemon-less nix with on-home state.** The image's `/nix/var` stays
   read-only (no volume covers it), so the store database and state live
   on the session home bind mount instead — the only large writable disk
   besides the store volume:

   ```
   NIX_REMOTE=local                              # no daemon; direct local store
   NIX_STATE_DIR=/home/agent/.local/state/nix
   NIX_LOG_DIR=/home/agent/.local/state/nix/log
   TMPDIR=/home/agent/.cache/nix-tmp             # disk-backed, not the tmpfs /tmp
   ```

   `/bin/agent-gvisor-init` creates these directories before the first
   payload process runs (the wrapper is the payload whenever
   `AGENT_GVISOR_NIX=1`, even without loopback forwards).

   **The wrapper fails closed.** Unlike the loopback relays (a broken
   relay only warns), the Nix preflight aborts the session with exit 1
   when a state directory cannot be created or written, or when the store
   at `NIX_STORE_DIR` (`/nix/store` in every real session) does not accept
   a write. `--nix` is an explicit request for a usable store, and the
   copy-up + `keep-id` ownership behaviour under runsc is precisely the
   part that can silently not work on a given host (§7 V1): without the
   preflight a session would start and then fail deep inside some later
   `nix` invocation with an unrelated-looking error. The probe is a real
   file creation, not `test -w`, because an unmapped owner leaves the
   permission bits looking fine while every write returns `EACCES`.
   `nix/checks.nix` exercises all of this without a container
   (`agent-gvisor-init`, driven by `tests/agent-gvisor-init-harness.sh`).

   `NIX_REMOTE=local` is belt and braces: without it a nix built with
   daemon support might spend its first seconds probing `/nix/var/nix/daemon.socket`
   (absent, on a read-only layer) before falling back.

3. **In-sandbox `nix.conf`** via `AGENT_GVISOR_NIX_CONFIG` → container env
   `NIX_CONFIG`. The module renders it from the host configuration:

   ```
   sandbox = false
   experimental-features = nix-command flakes
   substituters = <config.nix.settings.substituters>
   trusted-public-keys = <config.nix.settings.trusted-public-keys>
   <nix.extraConfig lines>
   ```

   `sandbox = false` is not a shortcut, it is a hard requirement (see §3).
   The substituter settings mirror the host so a sandbox build substitutes
   exactly like a host build — from cache.nixos.org and any LAN cache,
   which is the difference between a one-minute and an overnight build.

4. **Nix baked into the image** (`nix.package`, default `config.nix.package`)
   whenever `nix.enable` is set, via the module's `extraImagePackages`.
   The in-sandbox nix therefore speaks the same store/NAR protocol as the
   host's pinned flake inputs. Note there is no version coordination
   *beyond* that: a store path substituted from a cache is
   content-addressed and protocol-independent, so substitution never has
   a version skew. Only on-the-wire cache URLs/keys are configured, and
   those are host-mirrored.

## 2. Alternatives, and why they were rejected

- **Bind-mount the host's `/nix/store` (read-only).** Rejected on security
  grounds: it exposes the *entire* host store — every secret-bearing
  derivation ever built on this machine — to a hostile agent. It is also
  useless for building: a read-only store cannot accept new paths, and
  the store database (`/nix/var/nix/db`) needs writes anyway.
- **A chroot store at a non-`/nix/store` path** (e.g. under the session
  home, with `NIX_STORE_DIR`). Rejected: substituted packages carry
  absolute shebangs (`#!/nix/store/…`) and absolute references pointing
  at literal `/nix/store/…` paths, so anything nix substitutes would be
  unrunnable from the other store location.
- **Mount a session-owned empty directory over `/nix/store`** and re-expose
  the image's store paths via symlinks to a second, read-only image view
  (`--mount type=image,src=<image>,dst=/__image-root`). Rejected for a
  fatal bootstrapping flaw: *masking `/nix/store` breaks every `/bin/*`
  tool — including `/bin/bash` itself — until an in-container process
  creates the symlink farm, but no in-container process can even start
  (`exec` of the payload would already need `/bin/bash` → `/nix/store/…`,
  which is masked). The farm would have to be created host-side before
  container start, which requires shipping the image's closure list to
  the CLI — complexity with no advantage over copy-up.
- **Drop `--read-only`** (writable container rootfs): everything
  "just works" mechanically — the image store stays visible and the
  writable layer accepts new paths. Rejected as the default because it
  gives up documented hardening (§4) and the store dies with the
  container (`--replace` recreations lose it). It remains the documented
  *fallback* if copy-up misbehaves on a host (see §7, V1b).

## 3. Rootless + gVisor constraints (what cannot work)

- **No user namespaces, no `mount(2)`:** gVisor does not implement user
  namespace creation inside the sandbox, and the container runs with
  `--cap-drop=ALL` (no `CAP_SYS_ADMIN` anyway). Nix's *build sandbox*
  (`sandbox = true`, or `relaxed`) needs `unshare(CLONE_NEWUSER)` and/or
  bind mounts — neither exists here. Hence `sandbox = false`: builds run
  directly in the container. The *container itself* remains the
  isolation boundary; what is lost is nix's second, in-container
  sandbox between a build and the session (§5).
- **No daemon:** there is no nix-daemon in the sandbox and no build-user
  pool (`nixbld*` users are not in the image). The store is operated
  single-user, directly by the session user, via `NIX_REMOTE=local`.
  Nix ≥ 2.4 supports this; it is the same mode a laptop uses without
  the daemon.
- **No cgroup isolation of builds** (default): the rootless defaults pass
  `--runtime-flag=ignore-cgroups`, so `--memory 8g` is NOT enforced. An
  agent can exhaust host memory through a build (§5). Hosts that delegate
  cgroup controllers and drop `ignore-cgroups` get real limits.
- **Inner OCI seccomp profile:** nix with `sandbox = false` needs
  fork/exec, file creation, `posix_fallocate`, symlinks, etc. — all in
  the default allowlist. It does *not* need `mount`/`unshare` (that is
  the point of `sandbox = false`). If a future nix feature hits the
  profile anyway, `start --nix --seccomp-unconfined` is the documented
  escape hatch; note runsc applies its own sentry-level seccomp to
  everything regardless, so the gVisor boundary itself never opens.
- **Network:** flakes fetch over the session's pasta network, like any
  other outbound traffic. No special handling.

## 4. Flake workflow inside a session

- `nix build`, `nix run`, `nix develop`, `nix flake …` all work against
  the flake in the mounted worktree; `flake.lock` (committed) pins the
  inputs, fetched from their registries (e.g. GitHub) over pasta.
- `path:` flake inputs pointing OUTSIDE the worktree (this repo has none
  in `_flake.nix_`, but e.g. `../priv` style inputs would) are not
  reachable: only the worktree, the pool, the session home and explicit
  `--mount`/`--config` paths exist inside the sandbox. Add a `--mount`
  if such an input must build in-session.
- `result` symlinks created by `nix build` point into `/nix/store` —
  they are meaningful only inside the sandbox. They are untracked
  debris on the host after a `merge`; add `result` (and `result-*`) to
  the repo's `.gitignore` so agents cannot accidentally commit them.
- The download cache (`~/.cache/nix`) and the logs land on the session
  home bind, i.e. in `<repo>__agent-gvisor/__sessions/<name>/home/` on
  the host — gone with the session, like everything else in it.
- `nix develop` of this repository works, but the development shell is
  built with `sandbox = false` like everything else, and
  `nix develop --impure` is required where the flake demands it (same
  as on the host).

## 5. Security trade-offs (explicit)

What `--nix` adds to a session's attack surface:

1. **Nix builds execute in the sandbox without an inner sandbox.** On the
   host, a malicious `buildCommand` is confined by nix's sandbox (user
   namespaces, no network). In a `--nix` session it runs as the session
   user in the gVisor sandbox: the gVisor boundary (kernel-surface
   reduction, no host syscalls) still applies, and the network is still
   pasta-mediated — but the build can talk to the network and touch
   everything in the session (the worktree, the home, the store volume).
   Never run `nix build` on a flake you would not otherwise let the agent
   execute arbitrary code from; the session is the sandbox.
2. **Host disk exhaustion.** The store volume and the on-home state/tmp
   live on the host filesystem, with no quota (podman named volumes have
   none, and `ignore-cgroups` also means no memory cap). A hostile agent
   can fill the host disk via the volume. This is the same exposure the
   worktree and session home already have, but nix makes it cheap to
   generate hundreds of GB. Monitor/cap by host filesystem quotas if
   that matters on a given host.
3. **Cache trust inherited from the host.** `substituters` /
   `trusted-public-keys` mirror `config.nix.settings`, so the sandbox
   trusts exactly the caches the host trusts — a cache compromise affects
   sandbox builds the same way it affects host builds, no new trust is
   introduced, and none is removed.
4. **What is NOT exposed:** the host `/nix/store` stays invisible; the
   image store paths are baked, public, store-object data only. The
   volume content is a session-owned copy of those plus whatever the
   agent builds — destroyed with the session. `--read-only`,
   `--cap-drop=ALL`, `no-new-privileges` and the default inner seccomp
   profile are unchanged for `--nix` sessions.

## 6. Disk and memory characteristics

- The first `start` of a `--nix` session copies the image's store closure
  into the volume (a one-time cost, roughly the image's uncompressed
  `/nix/store` size — ~1–3 GB for a typical agent image). With the
  default rootless storage driver (`overlay`) this is a real copy;
  subsequent starts of the same session reuse the volume. `--force`
  restarts pay it again.
- Build disk use lands in the volume and on the session home
  (`NIX_STATE_DIR`, `TMPDIR`); build *memory* is host memory, uncapped
  under `ignore-cgroups`. gVisor adds per-sandbox overhead (~200–400 MB
  resident for the sentry + gofer processes), which counts against the
  host, not the (unenforced) limit.

## 7. Host verification checklist

The implementation is static-verified (argv tests, meta round-trip,
completions sync, module eval) in `nix/checks.nix`; the following needs a
real host with rootless Podman + runsc + the loaded image, in this order:

Until V1–V5 have been run on a host, keep `nix.enable` off there: the
module defaults to off for exactly that reason.

1. **Volume + copy-up + keep-id under runsc** (the core unknown):
   `agent-gvisor start t1 --nix --detach`, then inside
   (`agent-gvisor shell t1`) check that `ls /nix/store` shows the image
   closure AND that the store dir is writable by the agent
   (`touch /nix/store/.probe`). If the copy-up lands as container-root
   (uid unmapped under `keep-id`) and the write fails: this host needs
   the read-only-rootfs fallback — image-level writable rootfs —
   and the volume approach must be revisited upstream.
   The failure is *loud*: `start --nix` aborts with
   `agent-gvisor-init: error: /nix/store is not writable in this sandbox`
   (§1.2), so V1 cannot pass by accident.
2. **Daemon-less nix:** `nix store info` in the session must print the
   store without daemon errors; `nix build nixpkgs#hello` must
   substitute, build and run.
3. **Flakes end-to-end:** `nix flake archive` / `nix build .#<pkg>` on a
   small flake in the worktree, over the pasta network, with the
   module-mirrored substituters.
4. **Seccomp sanity:** a build with `sandbox = false` under the default
   inner profile; only if something fails, retry with
   `--seccomp-unconfined` and record what syscall was missing.
5. **Lifecycle:** `stop` + `run` keeps the substituted store; `destroy`
   removes the volume (`podman volume ls` no longer lists
   `agent-…-<name>-nix`).
6. **Memory/limits:** on a host with delegated cgroups (no
   `ignore-cgroups`), confirm a `--memory 8g` session gets OOM-killed
   builds rather than host exhaustion.

## 8. FAQ

- *Why a named volume instead of a bind mount of a session directory?*
  A bind mount over `/nix/store` masks the image's store and nothing can
  re-expose it before the first process runs (§2). Copy-up is the only
  podman mechanism that populates the mount *from the image* at
  container-create time.
- *Why not share one store volume across sessions?* Named volumes are
  per-session by design: concurrent nix processes in one store need
  locking and the shared store would outlive sessions, violating the
  disposable-session model (and compounding the disk-exhaustion surface).
  Substitution is cheap; sharing is not worth the coupling. (A host
  wanting a warm cache runs a LAN substituter — already mirrored into
  the sandbox by the module.)
- *Does `--nix` weaken isolation vs. a plain session?* Not the container
  boundary (§5.4); it adds in-container execution of builds and a host
  disk surface, both inherent to "nix inside the sandbox".
