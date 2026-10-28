# Design: backends

Status: decision record, reconciled with the completed source audit at
`7846d39c36` and pinned nono **0.80.0**. This audit launched no live
sandboxes. **Shipped** below means implemented, not live-validated for
every combination; **proposed** marks unimplemented plans. Historical
0.74.0 audits and live logs remain evidence for their recorded versions
and workloads, not acceptance of today's full matrix.

This file holds decisions and their rationale about backend confinement.
Use the [current six-backend comparison](../backend-comparison.md) for
the implemented option matrix and known gaps. Configuration keys stay in
[config.md](./config.md); the command-line surface stays in
[cli.md](./cli.md). Backend selection itself is cli.md D7/D18.

## Decisions

### D1: The `nono` backend is bubblewrap with nono inside

`backend = "nono"` runs the payload as a chain of three processes:

```
bwrap <the bubblewrap backend's argv, unchanged> \
  -- <nono> run --profile <store path> <grants> <network flags> \
  -- <env> <payload environment> <payload>
```

bubblewrap builds the sandbox exactly as the bubblewrap backend does.
nono then confines the payload inside that sandbox with Landlock,
seccomp and its egress proxy. `<payload>` is whatever the bubblewrap
backend would run: the shell, a `run` command, or a multiplexer entry.

There is no second `nono` mode. The first cut (bd myconfig-6di.2) ran
nono directly on the host tree, and it is removed. It could not
express any mount `dest`, and every mount the generated user layer
contains has one (D14 of config.md). The backend name stays `nono`,
so configs and `--backend nono` keep working.

#### Who owns what

| Concern | Owner | Mechanism |
| --- | --- | --- |
| Filesystem view: base binds, tmpfs `/mysbx-home`, mount `dest`s, clone at the repo path | bubblewrap | binds, as on the bubblewrap backend |
| Read-only versus read-write | bubblewrap | read-only binds; the kernel returns `EROFS` |
| Network namespace | bubblewrap | `--unshare-all`, then `--share-net` or not |
| Filesystem access inside the view | nono | Landlock grants (second layer) |
| System calls | nono | network-mode filters plus the generated pathname AF_UNIX mediation filter, including with unrestricted IP networking |
| Egress policy | nono | independent port grants and, for domains, a proxy on `127.0.0.1` plus supervision; not a domain/port conjunction |
| Payload environment | mysbx | a pinned `env` between nono and the payload |

All layout checks of the bubblewrap backend apply unchanged. These are
`check_dest`, `check_hidden_mounts`, `check_symlinkable_dests`, the
policy-file refusal and the daemon-directory rule. nono never sees a
layout that bubblewrap would refuse.

#### Grants follow the resolved layout

mysbx derives the nono grants from the layout bubblewrap builds, never
from the raw config:

- `--read /nix/store`.
- The workspace, read-write, plus `--allow-cwd`: the repo root in a
  live run, or the clone bound at the repo path in a clone run
  (workspace.md D3). Also the approved git directories and the
  worktrees sibling in a live run.
- `/mysbx-home`, read-write, as one grant. The sandbox home is a
  writable tmpfs on the bubblewrap backend (config.md D14), and the
  same holds here. The read-only seeds below it stay read-only
  through their read-only binds. Landlock cannot make a hole inside a
  writable grant, and here it does not need to.
- Each configured mount `dest` outside `/mysbx-home`: `--read` or
  `--allow`, `--read-file` or `--allow-file` for a file (bd
  myconfig-2pv). The mode is the effective mode at that path: when
  two binds share a `dest`, the later bind wins, the same rule
  bubblewrap applies. A grant never makes a path writable that
  bubblewrap made read-only (bd myconfig-uay).
- The base paths the view needs: the private `/tmp` and `/dev/shm`
  read-write, and the read-only system paths. These come from the
  mysbx nono profile below.

`/mysbx-nono` (see "nono's own inputs") is never granted.

Filesystem-wise, nono adds little over bubblewrap. The view is
already minimal, and the grants come from the same layout, so a
layout bug would produce a matching grant. The value of this backend
is the egress policy and seccomp mediation. The bubblewrap backend
has neither. **Shipped in pinned 0.80.0:** the generated profile's
pathname AF_UNIX mediation installs seccomp even with unrestricted
IP networking. The earlier nono 0.74.0 `Seccomp: 0` observation was
historical, not the current generated-profile behavior. nono runs in
every network mode, so the backend still has one process-chain shape.

#### Network

| Merged config | bubblewrap | nono |
| --- | --- | --- |
| `network = false` | no `--share-net`: an empty netns | `--block-net` |
| `network = true`, no allowlist | `--share-net` | no network flag (nono's default is outbound allowed) |
| `network = true`, allowlist (config.md D21) | `--share-net` | `--allow-domain`, `--allow-connect-port`, `--listen-port` per merged entry |

**Shipped policy semantics (nono 0.80.0):** the default `Auto`
execution strategy installs proxy supervision for domain policy.
`allow-domains` and `connect-ports` are NOT a conjunction:

| Entries | Effective mechanism |
| --- | --- |
| Ports without domains | Independent destination-port grants, not host restrictions; DNS is not added automatically |
| Domains without connect ports | Hostname-filtering CONNECT proxy; a domain entry alone does not restrict the upstream port; the supervisor denies direct remote connections |
| Domains and connect ports | Landlock installs independent port grants, but the proxy supervisor still denies direct remote connections; neither a domain-and-port intersection nor a demonstrated ANYHOST port bypass |

Mixed settings can derive `NO_PROXY`/direct-connect choices from the
ports while the supervisor denies those connections. This connectivity
conflict needs **live acceptance of the exact policy** before recommending
mixed profiles; inspecting Landlock grants alone does not establish an
effective bypass.

Rules for the allowlist case:

- An allowlist must restrict outbound traffic. `listen-ports` alone
  is refused (bd myconfig-a14); it is not a standalone egress policy.
- The Nix daemon builds fixed-output derivations with network access
  that nono does not filter. Under an allowlist, bubblewrap does not
  bind `/nix/var/nix`, and nono grants no daemon socket (bd
  myconfig-nj9). With the shared network and no allowlist, the socket
  is bound and granted, as on the bubblewrap backend.
- `allow-domains` entries are plain host names. nono tunnels them with
  `CONNECT` and injects no CA certificate, so mysbx's CA pins stay
  authoritative (config.md D14). URL forms with path globs need nono's
  TLS interception. They are out of scope, and the config.md D21
  schema refuses them on this backend (bd myconfig-6di.4.4).

#### Two environments

nono reads many variables: every `nono run` flag has a `NONO_*`
equivalent, and `HOME`, `XDG_CONFIG_HOME`, `XDG_STATE_HOME` and
`TMPDIR` steer its config, profile and state lookup. nono also removes
about thirty "dangerous" names from the child environment (for example
`PYTHONPATH`, `NODE_OPTIONS`, `GEM_HOME`). So the payload environment
must not pass through nono:

- **nono's environment** is infrastructure only. bubblewrap's
  `--clearenv` and `--setenv` set `PATH`, a `HOME` and XDG
  directories below `/mysbx-nono`, and `NONO_NO_UPDATE_CHECK=1`.
  Nothing from a config layer, from `forward-env` or from the host
  reaches nono. A layer can therefore never widen the policy through
  a `NONO_*` variable or redirect nono's config lookup.
- **The payload environment assignments** follow the bubblewrap backend:
  forwarded variables, then `[env]`, then the `HOME`, `PATH` and CA
  pins of config.md D14. A pinned `env` from mysbx's closure applies
  it as nono's child. It unsets every variable of nono's environment
  first (`-u`), so the payload never sees an XDG directory below
  `/mysbx-nono`. nono's filter never sees the payload environment, so
  an `[env]` `PYTHONPATH` reaches the payload as on bubblewrap. The
  values sit in the argv of `env`. This is the same exposure as
  bubblewrap's `--setenv` argv today.

In domain-allowlist mode nono injects its proxy variables:
`HTTP_PROXY`, `HTTPS_PROXY`, `NO_PROXY`, their lower-case forms,
`NONO_PROXY_TOKEN`, `NONO_NO_PROXY` and `NODE_USE_ENV_PROXY`.
**Documented protection, not implemented:** reserving these names,
dropping payload overrides and marking them `[config, ignored — set by nono]`.
The current payload builder instead restores configured AND forwarded
proxy variables through the pinned `env`, after nono's injection. They
can overwrite that environment and break connectivity. The already
installed Landlock/seccomp/supervisor policy still applies; this is not
a demonstrated policy bypass, nor grounds for promising that all
possible overrides are harmless.

#### Naming-service probes and the IPC-denial block (bd myconfig-7ov)

The pathname AF_UNIX mediation filter traps every pathname
`connect(2)`/`bind(2)`, and glibc's NSS probes `/var/run/nscd/socket`
before every user/group lookup. The path is not in the view, so no
capability covers it and each probe is denied — benign (glibc falls
back to files/DNS) but loud: the run footer's "IPC denial:" block
lists the probes. The historical nono 0.74.0 audit found no knob to
quieten it — `diagnostics.suppress_system_services` hid only
profile-prompt entries, and `--no-diagnostics` gated only the failure
footer, not the successful session's denial block. This explains the
known-benign nscd/dbus observations in that log, not a fresh live test
of 0.80.0. Current denial reporting has its own budget (below), so the
footer need not enumerate every denial. Name operations against paths
the view deliberately does not carry are expected; other entries are
worth reading.

#### Pathname mediation replaces connect-ENOENT with a policy denial (bd myconfig-bk2)

Under `linux.af_unix_mediation = "pathname"` the supervisor resolves
a CONNECT's target child-relative and `canonicalize()`s it
(`crates/nono-cli/src/exec_strategy/supervisor_linux.rs`,
`decide_af_unix_pathname`). A nonexistent socket target cannot
canonicalize and is denied before the kernel can answer ENOENT.
**Shipped in 0.80.0:** normal policy denials return **EACCES, not
EPERM**; a nonexistent target still does NOT preserve ENOENT-retry
semantics. The earlier EPERM observation belongs to nono 0.74.0.

The historical herdr race and workaround remain relevant rationale:
its TUI spawned the background server and immediately connected;
herdr retried ENOENT but treated the observed EPERM as fatal. The
entry pre-starts `herdr server` (gated on the socket file — herdr
0.9.1's `status server` exited 0 even when nothing ran), then polls
for a real API answer before `exec herdr` in a foreground run or the
reattach loop of detach.md B7 in a detached one
(./nix/herdr-entry.nix). This avoids depending on ENOENT today too.
tmux does NOT need the same treatment: its client falls into
`server_start`, forks through `proc_fork_and_daemon` and connects on
the fd handed back after binding (tmux 3.7c `client.c` / `proc.c`),
so its attach connect never targets an unbound pathname.

#### Pathname policy and denial-reporting budgets are separate (bd myconfig-27o)

**Shipped in pinned 0.80.0:** allowed pathname bind/connect operations
no longer consume the old 10/s, burst-5 limiter. Denial reporting has
a separate **20/s, burst-50** budget. Exhausting that reporting budget
suppresses reports, not operations: it does not change an allow/deny
decision or turn an allowed operation into a denial. Normal policy
denials use EACCES. A quiet footer is not proof that no denial occurred.

**Historical rationale (nono 0.74.0):** `supervisor_linux.rs` used
`RateLimiter::new(10, 5)` for trapped operations. Exhaustion denied
with EPERM before `record_af_unix_ipc_denial`, leaving only a debug
message. On f13, herdr's binds/NSS probes/API connects exhausted that
budget; the original 10Hz readiness poll failed 150/150 times (bd
myconfig-27o). That explains the shipped entry's socket stat wait,
1s grace and 1s API/relocation retries (./nix/herdr-entry.nix), but
it is NOT a current requirement that every AF_UNIX poll run at most
1Hz, nor evidence that allowed operations are still rate-limited.

#### nono's own inputs

- **Profile**: a store path built by the Nix module, passed as
  `--profile <path>`. mysbx never passes a profile name, because nono
  looks up names in a directory. nono's built-in `default` profile is
  not used: its grants change between nono releases, and its `$HOME`
  credential denies target paths that do not exist in the view. With
  `HOME=/mysbx-home` those denies even aborted nono once a grant
  covered an ancestor ("Landlock deny-overlap is not enforceable on
  Linux"). A mysbx-owned profile keeps the policy pinned and
  reviewable. The profile contents are bd myconfig-6di.4.3 and bd
  myconfig-2pe (the `/tmp` + `$TMPDIR` read/write grants).
- **State**: `/mysbx-nono`, a tmpfs created with the base mounts. It
  is never granted to the payload, and a mount `dest` at, below or
  above it is refused like every base path. nono refuses any grant
  that overlaps its protected state roots (`$XDG_STATE_HOME/nono` and
  the legacy `$HOME/.nono`). Both resolve below `/mysbx-nono`, so the
  `/mysbx-home` grant does not collide with them.
- **No network use by the launcher**: `NONO_NO_UPDATE_CHECK=1`.
- **Waypipe display**: shipped; the refusal was lifted after the
  historical nono 0.74.0 audit (bd myconfig-6di.4.6).
  Source-audit of that version's filter tables
  (`crates/nono/src/sandbox/linux.rs`): the block-all and TCP-only
  static baselines trap socket()/socketpair()/io_uring only — AF_UNIX
  socket and socketpair pass; the pathname AF_UNIX mediation filter
  (the one the mysbx profile ships) traps connect/bind/sendto/
  sendmsg/sendmmsg and its supervisor explicitly CONTINUES `sendmsg`
  with a NULL `msg_name` (fd passing over an established connection,
  `SCM_RIGHTS`); `memfd_create` is trapped by no filter. Live probe
  under the then-pinned profile inside a hand-built bwrap view: the
  waypipe server binds the fake compositor socket (`wayland-0`, a
  direct child of the sandbox home) and connects to the host client
  (`<socket_dir>/waypipe.sock`) end to end under `--block-net` with
  the two `--allow-unix-socket-dir-bind` grants; without the home
  grant the run reports `bind ... (no matching unix_socket
  capability)` — the refusal dies exactly where the grant boundary
  is. That historical probe does not establish all current network
  combinations. In pinned 0.80.0 the generated pathname mediation
  filter also installs under the shared, unrestricted-IP default.
- **Unused nono features**: credential injection, TLS interception,
  rollbacks and audit sessions.

#### State, keys and the multiplexer

State directories are bound at `/mysbx-home/<entry>`, as on the
bubblewrap backend (config.md D15). The sandbox SSH key is at
`~/.ssh` (config.md D22), so the backend needs no `GIT_SSH_COMMAND`
pin. The multiplexer socket directory `/mysbx-home/.mysbx-tmux`
(config.md D16/D17) exists again. The multiplexer also needs nono's
unix-socket grant: `--allow-unix-socket-dir-bind` on the socket dir
(bd myconfig-6di.4.5, historical nono 0.74.0 audit — connect AND bind
on any direct-child socket, exactly the entry scripts' `$TMUX_TMPDIR/socket`
shape), emitted when a session starts. The grant follows the selected
entry ([`Multiplexer::unix_socket_dirs`]): herdr's API dir
`/mysbx-home/.config/herdr` (bd myconfig-7ov), and workmux's second
dir `/tmp` (bd myconfig-peo) — the sidebar daemon binds its snapshot
socket at `temp_dir()/workmux-sidebar-<hash>.sock` by its own design,
the payload env carries no TMPDIR, so that dir is the private tmpfs
`/tmp` (bd myconfig-7hh); without the grant the daemon's bind is
denied and `workmux sidebar` dies with "Sidebar daemon failed to
start". The filesystem side needs no new grant — every dir lies below
the rw home grant or the payload-writable tmpfs `/tmp`. The mysbx profile
ships `linux.af_unix_mediation = "pathname"` to require Unix-socket
capabilities rather than filesystem access alone. This mediation does
not establish that every granted directory is private per run.
`state-dirs = [".config/herdr"]` is accepted and persists herdr's API
directory on bubblewrap/nono. The tmux guards do not cover that separate
directory (config.md D17). This is an exception available to trusted
configuration, not an untrusted-repository escape.

#### Refusals

| First-cut refusal | Now |
| --- | --- |
| mount `dest` different from its source | lifted: bubblewrap binds it |
| `--session` clone runs | lifted: bubblewrap binds the clone at the repo path |
| `network = true` without an allowlist | lifted (bd myconfig-6di.4.4): the premise was wrong — nono allows outbound traffic by default, so bwrap shares the netns and nono adds no flag, bubblewrap parity |
| `listen-ports` alone on nono | refused (bd myconfig-a14, bd myconfig-6di.4.4): with only listen ports nono reports "outbound allowed" — an allowlist that does not restrict outbound is a lie |
| URL-form `allow-domains` on nono | refused (bd myconfig-6di.4.4): no TLS interception on this backend, entries stay plain host names |
| multiplexer sessions | lifted (bd myconfig-6di.4.5): the socket dir gets `--allow-unix-socket-dir-bind`, the profile ships pathname AF_UNIX mediation; the entry-pin requirement is the bwrap layout's |
| `display = "waypipe"` | lifted (bd myconfig-6di.4.6): audited — AF_UNIX socket/socketpair pass every static baseline, the mediation filter continues `sendmsg` with a NULL `msg_name` (the fd-passing case), and `memfd_create` is trapped by no filter; the two socket dirs get `--allow-unix-socket-dir-bind` grants, live-probed end-to-end under `--block-net` |
| an allowlist on `bubblewrap` or `podman-gvisor` | unchanged (config.md D21) |
| `network = false` with an allowlist | unchanged (config.md D21) |
| `multiplexer = "orca"` on `nono` or `podman-gvisor` | refused (bd myconfig-2m8): the payload is an Electron AppImage via appimage-run with its own Xvfb — the nono grants cover the mux socket dirs, not the Electron/X11 socket and syscall surface (the 6di.4.6 audit covered waypipe only), and the gvisor image ships no AppImage/Xvfb runtime at all; bubblewrap alone accepts it |

#### Rationale

- The original nono 0.74.0 audit established the layering rationale:
  nono has no mount namespace or path remap of its own; it adds
  Landlock, seccomp and its proxy/supervisor policy. This remains
  the architecture with pinned 0.80.0. Its own documentation rejects
  mount namespaces for portability ("Why not mount namespaces?" in
  `docs/cli/internals/security-model.mdx`).
- Historical live evidence: the chain was tested with nono 0.74.0
  inside a mysbx bubblewrap sandbox. Landlock applies inside the
  user namespace. nono resolves
  its paths in bubblewrap's mount namespace: a remapped
  `/mysbx-home/.config/git` is readable, a write to it fails with
  `EROFS`, and an ungranted path fails with `EACCES`. `--block-net`
  blocks traffic. `--allow-domain` works through the proxy on
  `127.0.0.1` in a shared netns (the allowed domain answers, others
  get a 403 at `CONNECT`, and direct DNS fails).
- bubblewrap owns the read-only structure, so the Landlock additivity
  bug is gone by construction (bd myconfig-uay). `/tmp` is private,
  so the host `/tmp` exposure is gone too (bd myconfig-7hh). config.md
  D14 stays the same on every backend.

#### Alternatives considered

- **nono at real `$HOME` paths without `dest`**: loses the
  sandbox-specific configs (the workmux config, `/etc/tmux.conf`). It
  also leaves the home unwritable and state directories where tools
  do not look, and it needs the D14 assertion relaxed.
- **Redirect through environment variables** (`GIT_CONFIG_GLOBAL`,
  `XDG_CONFIG_HOME`): does not cover tools that hard-code `$HOME`
  (`~/.pi`, `~/.agents`), and does not cover state directories.
- **A per-run symlink home under nono alone**: sound, but it rebuilds
  a sandbox home without a mount namespace, and `dest`s outside the
  home stay impossible.
- **Per-backend mount filtering in Nix**: the Nix module cannot see a
  run-time `--backend` choice.
- **Keep the first cut next to the layered mode**: doubles the surface
  and keeps the Landlock-only bugs alive.

### D2: The `podman-krun` backend is `podman-gvisor` with the OCI runtime swapped to crun/libkrun

`backend = "podman-krun"` (bd myconfig-6di.5) is a RUNTIME VARIANT of
`podman-gvisor`: the same argv builder, the same container image pinning,
the same layout checks — only the OCI runtime changes. `runsc` (gVisor's
user-space kernel) is replaced by `crun` built against `libkrun`
(nixpkgs `crun`, `withLibkrun` — the default on `x86_64-linux`), so each
run is a KVM microVM with its own kernel (libkrunfw's stock kernel) and
mounts travel over virtio-fs instead of runsc's gofer. Rootless, no
bridge, no tap, no root: the microVM is started from the unprivileged
podman process through `/dev/kvm`, and libkrun's TSI (Transparent
Socket Impersonation) over the container's own netns give the guest
its network — no device the payload could otherwise reach (the
verified network model: the section after the refusal table, bd
myconfig-6di.5.5).

The podman-gvisor argv stays BYTE-IDENTICAL; the krun variant is a
separate enum value swapping `--runtime=runsc` for
`--runtime=<the crun+libkrun store path>` PLUS an exactly enumerated
set of differences that exist because the krun execution model
enforces different things — each verified against the crun/libkrun
sources (bd myconfig-6di.5.4). Both Podman variants use an
**image-owned store**, with no implicit whole-host `/nix/store` bind.
Tools must be present in the loaded image or explicitly supplied by
configuration; do not transfer D3's host-store premise to this tier.
The shared dispatch consumes `MYSBX_PODMAN_ENV` and
`MYSBX_PODMAN_PASTA_SPEC` for **both** variants. Endpoint environment
and network pins are not gVisor-only, though actual reachability still
depends on the selected network transport and policy.

The enumerated krun differences are:

- `--annotation run.oci.handler=krun` — crun runs the libkrun VM
  handler ONLY with this annotation (src/libcrun/custom-handler.c
  `find_handler_for_container`); without it the pinned crun silently
  runs a PLAIN container, no VM, weaker than gVisor. The annotation
  is the VM's on switch, not decoration.
- `--group-add=keep-groups` (bd myconfig-b5o) — the VMM is the
  container entrypoint process (the krun handler runs the VM
  in-process), and crun would setgroups it to the OCI config's
  `additionalGids` — under `keep-id`, the mapped user's group only.
  When `/dev/kvm` access comes from the host user's supplementary
  `kvm` GROUP, the host-side doctor gate passes (mysbx's own
  process carries the group) while the VMM would lose it and die
  with EACCES. podman turns this flag into the annotation
  `run.oci.keep_original_groups=1` (cmd/podman/containers/
  create.go — refused together with any other `--group-add`, and
  mysbx emits no other), and crun's `can_setgroups` then SKIPS the
  setgroups call (linux.c) — the entrypoint keeps the host's
  supplementary groups. Gvisor argv unchanged: runsc opens no
  /dev/kvm.
- NO `--cap-drop=ALL` / `--security-opt=no-new-privileges` — crun's
  krun handler never execs the OCI process (the guest init execs the
  payload as guest root, reading only `Env`/`args`/`WorkingDir` from
  the OCI config, libkrun init/init.c), so the flags would advertise
  enforcement that does not exist. `--read-only` stays: crun remounts
  the prepared root read-only host-side and virtiofs hands the guest
  exactly that tree.
- CPU/RAM limits as `--annotation krun.cpus=…`/`krun.ram_mib=…`
  instead of cgroup flags (bd myconfig-6di.5.6), enforced as VM sizing.
  An unrepresentable CPU/RAM value is refused. A configured PID
  limit warns and is dropped; an explicit `MYSBX_PODMAN_PIDS_LIMIT`
  pin is refused. By contrast, rootless podman-gvisor's default
  `ignore-cgroups` warns and omits ALL memory/CPU/PID limit flags,
  including environment overrides; flags require cgroups enabled.
- `--userns=keep-id` stays but means something different: it maps the
  HOST-SIDE rootfs preparation (the bind sources are prepared under
  the host user's uid so the virtiofs server can share them), while
  inside the guest the payload runs as guest root and virtiofs maps
  guest-root accesses to the host user unchanged (passthrough.rs
  `set_creds` — guest uid 0 is the host user's own uid, and chown to
  any OTHER uid is refused with EPERM unless the server holds
  CAP_SETUID). The honest consequence: files created by the payload
  in the shared mounts are host-uid-owned as on the other backends.
- the guest-root git trust (bd myconfig-zj2) — the OTHER consequence
  of the same uid model: the workspace mounts keep the host uid the
  virtiofs stat reports (ownership is NOT rewritten), and guest git
  refuses every ordinary command with `detected dubious ownership`.
  git reads `safe.directory` ONLY from the protected system+global
  config scope (config.c read_protected_config: ignore_repo,
  ignore_worktree, ignore_cmdline — the GIT_CONFIG_COUNT env block
  is dead for this key by design), so the builder binds a per-run
  sidecar file read-only at `/etc/mysbx/gitconfig`, exports
  `GIT_CONFIG_GLOBAL` to it (last, after every config `[env]`), and
  the file trusts EXACTLY the approved workspace paths (workspace,
  approved git-dirs, worktrees sibling, with `/*` forms for the
  trees — never a bare `*`, the microvm launcher's posture) plus
  `[include]`s of the two in-sandbox user-config paths, so an
  operator-seeded `~/.gitconfig` stays reachable. The gvisor variant
  needs none of it (the keep-id user owns the mounts); a trust
  there is refused (`GitTrustOnGvisor`).
- the libgit2 trust (bd myconfig-jn0) — nix fetches `git+file`
  flakes through libgit2 (1.9.7, nix 2.34 `git-utils.cc`), which
  still refused the repo (`not owned by current user`, error 7).
  Verified against the sources: nix calls `git_repository_open`
  without `GIT_REPOSITORY_OPEN_FROM_ENV`, so libgit2 ignores
  `GIT_CONFIG_GLOBAL`/`GIT_CONFIG_SYSTEM` and reads only
  `/etc/gitconfig`, `$HOME/.gitconfig` and the XDG file. Nix never
  sets `GIT_OPT_SET_OWNER_VALIDATION`. libgit2 also matches
  `safe.directory` by EXACT workdir path only, with no `/*` prefix
  form (repository.c `validate_ownership_cb`). The builder
  therefore binds a second per-run file (`gittrust/<pid>/
  system-gitconfig`) read-only at `/etc/gitconfig`. It holds
  exact entries for the repo root and each checkout (a directory
  with a `.git`) that exists in the worktrees sibling at launch
  (clone runs: the repo path only). It has no includes: git reads
  it as system config next to the GIT_CONFIG_GLOBAL file. A
  worktree created during the run is trusted by git (via `/*`)
  but not by libgit2 until the next run. `$HOME` and
  `XDG_CONFIG_HOME` are not redirected because nix reads its own
  config and cache through them.
- the per-run scratch disk (bd myconfig-0pi) — opt-in per build
  (the module pins `MYSBX_KRUN_SCRATCH_SIZE` only when BOTH
  `podman.enable` and `krun.nix.enable` are true): the host truncates
  one SPARSE file per run under `<repo>.mysbx/scratch/<pid>.img`
  to the cap, binds it rw at
  `/run/mysbx-scratch.img` and exports
  `MYSBX_KRUN_SCRATCH_IMG=/run/mysbx-scratch.img` (the LAST `--env`,
  after every config `[env]` — no layer can repoint the scratch at a
  path it mounts). The guest nix wrapper loop-mounts it as a
  disk-backed ext4 nix scratch; see the nix scope decision below
  for the full consequences. A `Some` on the gvisor variant is
  refused (`KrunScratchOnGvisor`) — the loop-mount machinery lives
  in the krun guest's wrappers.

The existing golden tests plus a before/after snapshot of the gvisor
argv enforce the gvisor's byte-identity (see the tests of bd
myconfig-6di.5.3), and the krun golden pins the enumerated
differences (bd myconfig-6di.5.4).

#### Threat model delta vs. gVisor — stated honestly

libkrun is NOT a stronger isolation boundary than gVisor. libkrun's
upstream security model puts the VMM and the guest kernel in ONE
security context: a guest escape reaches the VMM's process, which is
the same unprivileged host process that started it. There is no
additional boundary between the guest kernel and the VMM. What the
krun variant buys instead:

- **Host-kernel-bug isolation**: a Linux kernel bug exploitable by the
  payload attacks the GUEST kernel (libkrunfw's stock kernel), not the
  host kernel. gVisor's Sentry reimplements the syscall surface
  instead — a different, partial kernel; krun runs a real one.
- **Guest-kernel compatibility**: cgroup namespaces, overlayfs inside
  user namespaces, FUSE, netfilter and tun are the features the
  original nested-Podman goal needed (now an opt-in image feature
  below). This is not proof of every kernel feature or workload;
  gVisor's Sentry would have to emulate the required surface.

The docs never claim krun is stronger than gVisor: the gain is host
kernel bug isolation and kernel compatibility, not a second boundary
between guest and VMM.

#### Alternatives considered

| Alternative | Why rejected |
| --- | --- |
| qemu + virtiofsd + passt, native | originally deferred to avoid a second argv builder and daemon/network wiring; D9 later shipped the native QEMU fallback with SLiRP, NOT passt |
| cloud-hypervisor directly | no user-mode networking — it needs a TAP device on a bridge, and a bridge/tap is a root-owned host device, which this design refuses |
| firecracker | disks only, no virtio-fs — no live repo mounts, which the whole mysbx workspace model (`live` repo bind) depends on |
| reusing the `myconfig.ai.microvm` slots | root-owned prebuilt slot pool whose mounts are fixed at NixOS eval time — incompatible with the runtime TOML config merge that decides mounts per run; also far heavier than a per-run microVM |
| kata containers | containerd-based and root-oriented; heavy, and the rootless story is weaker than podman+crun/libkrun |
| nested qemu inside the podman-gvisor container | recursive virtualization of a container image that already runs under runsc — no |

#### Refusals of the first cut

The first-cut intent was to refuse anything unenforceable. **Shipped
exception (config.md D23):** unsupported configured resource limits may
warn and be dropped, never silently; the PID environment pin still
refuses. The table retains first-cut rationale and names current behavior
where it differs.

| Feature | First-cut rationale / shipped status |
| --- | --- |
| `network = false` | enforced, and VERIFIED against the crun/libkrun sources (bd myconfig-6di.5.5): `--network none` gives the container an empty netns, the VMM (the rootless podman process) is created INSIDE it, and every guest egress is a TSI proxy connection the VMM dials from that netns — no route, no resolver, exactly the gvisor semantics. See the network model below |
| `allow-domains`/`connect-ports`/`listen-ports` | refused (config.md D21's table stays: krun's egress is libkrun's TSI proxy — an UNFILTERED dial from the container netns, no per-domain/port hook in the muxer — verified, see the network model below; same gap as podman-gvisor's pasta) |
| `egress = "proxy-only"` | not a shipped schema key; strict parsing rejects it on every backend. A future implementation needs default-deny networking: pasta's `--map-guest-addr` only adds a forwarder path (config.md D20; bd myconfig-6di.3 / myconfig-jq2) |
| `display = "waypipe"` | still refused: AF_UNIX socket objects do not cross virtiofs (source audit bd myconfig-6di.5.4). A guest server/transport and live acceptance would be required to lift it; that work is proposed, not validated |
| multiplexer sessions | supported (bd myconfig-55u): the entry scripts are baked into the shared image (`podman.muxEntries`, pinned as `MYSBX_PODMAN_MUX_ENTRY_*`), and `TMUX_TMPDIR` is the guest-native `/dev/shm/mysbx-tmux` (`podman_gvisor.rs::KRUN_MUX_SOCKET_DIR`) — virtio-fs files are host-uid-owned while the payload is guest root, so tmux refuses a socket dir there. Sockets stay guest-internal (a guest bind()/connect() pair on virtio-fs works; only host↔guest crossing does not). A mount at, below or above that dir is refused. An unpinned multiplexer is still refused (`MultiplexerUnavailable`) |
| host AF_UNIX sockets across virtio-fs | refused where a feature needs them (verified: the virtiofs server forwards stat/read/write/mknod over host inodes, libkrun passthrough.rs — a socket file reaches the guest as a dead inode and a guest bind()/connect() has no host socket object to reach; any feature built on a host socket crossing the mount is refused, not best-effort) |
| `--cap-drop=ALL` / `no-new-privileges` | NOT emitted on krun (bd myconfig-6di.5.4): crun's krun handler never execs the OCI process — the guest init runs the payload as guest root, reading only Env/args/WorkingDir from the OCI config (libkrun init/init.c) — so the flags would advertise enforcement that does not exist. `--read-only` stays (crun remounts the prepared root read-only host-side; virtiofs hands the guest that exact tree) |
| mounts / state-dirs / dest remap / clone sessions | the shared podman mount model, UNCHANGED: crun applies every OCI mount host-side on the container rootfs (container.c/linux.c — binds, tmpfses, ro remount), and libkrun shares the prepared tree as the virtiofs root. The gvisor layout survives verbatim: the home tmpfs, the state-parent tmpfses, the workspace bind, the state binds and the configured mounts all land on the tree the guest sees. Verified against the crun sources in bd myconfig-6di.5.4; live validation bd myconfig-6di.5.7 |
| uid mapping | `--userns=keep-id` stays but maps the HOST-SIDE preparation only: inside the guest the payload runs as guest root (the init never setuids) and the virtiofs server maps guest-root accesses to the host user's own uid unchanged (passthrough.rs `set_creds`; chown to any OTHER uid is EPERM unless the server holds CAP_SETUID). Files the payload writes in the shared mounts are host-uid-owned, as on the other backends; the nested-podman setuid story is bd myconfig-6di.5.8's risk |
| `/dev/kvm` availability | checked by doctor and before runtime launch: the user needs rw access (normally the `kvm` group); unavailable KVM refuses, never silently selects a different runtime |
| resource limits | CPU/RAM enforced by `krun.cpus` / `krun.ram_mib`, independent of cgroups (bd myconfig-6di.5.6). Configured `pids-limit` warns and is dropped; explicit `MYSBX_PODMAN_PIDS_LIMIT` refuses (no whole-VM PID controller). Fractional vCPUs and memory <= 128 MiB refuse rather than taking crun's silent default |

#### The network model, verified (bd myconfig-6di.5.5)

How the krun guest reaches the network, traced through the pinned
sources (crun 1.30 krun.c, libkrun lib.rs/vsock muxer):

- crun's krun handler adds a net device ONLY for the annotations
  `krun.tap_name` / `krun.use_passt` (krun.c
  `libkrun_configure_network`) — the mysbx argv sets NEITHER.
- With no net device, libkrun's implicit vsock heuristic fires
  (`enable_tsi = net.list.is_empty()`, lib.rs) and the vsock device is
  created with `TsiFlags::HIJACK_INET`; the guest kernel boots with
  `tsi_hijack` on its cmdline (vmm/builder.rs).
- Egress is then TSI — Transparent Socket Impersonation: the guest
  kernel routes socket syscalls at the vsock CID, the muxer creates a
  TSI proxy (muxer.rs `process_proxy_create` — per-connection, gated
  on `HIJACK_INET`), and the VMM performs the connection from ITS OWN
  network namespace.
- The VMM is the crun/podman process, created inside whatever netns
  `--network` selected: the podman default (pasta) or the empty
  none-netns of `network = false`. The guest therefore has EXACTLY
  the egress of the container netns — no independent guest route,
  nothing the payload could use past the netns.

Consequences, stated honestly:

- `network = false` IS the same enforcement as podman-gvisor: the VMM
  has no route, and TSI dials from it. The run refuses the same nix
  daemon mount contradiction the gvisor tier refuses.
- allowlists are refused exactly as on podman-gvisor: TSI is an
  UNFILTERED proxy (any AF_INET/AF_INET6 connect() the guest makes, the
  VMM dials) — no per-domain or per-port hook exists in the muxer, so
  the D21 refusal fires (pipeline step 4b) with krun's mechanism named.
- `egress = "proxy-only"` is not a schema key yet (config.md D20), so
  the strict parser refuses the config on EVERY backend before any
  backend question; when D20 lands, krun shares the podman-gvisor
  gap (pasta's `--map-guest-addr` ADDS a forwarder path, it does not
  remove the default route), so krun will refuse it too unless the
  guest netns can be made default-deny — same shared fix to track
  under bd myconfig-6di.3 / myconfig-jq2.
- DNS travels the same TSI path (the VMM resolves from the netns's
  resolver) — no guest-specific resolver story is needed for the first
  cut, and none is invented here.

#### Scope decisions (from the epic, bd myconfig-6di.5)

- **Original goal: nested rootless podman inside the guest; shipped
  as an opt-in OCI-image feature** (bd myconfig-6di.5.8).
  `krun.nestedPodman.enable` provisions the shared Podman image only,
  NOT the direct krun/QEMU rootfs or PATH. Kernel compatibility alone
  does not give the direct backends successor parity. The guest
  kernel (libkrunfw) has user namespaces,
  overlayfs, FUSE, tun and nftables. The "rootless" of the nested
  podman is the VM boundary itself, NOT a guest-uid story: the
  payload runs as GUEST ROOT (verified, bd myconfig-6di.5.4), so the
  setuid `newuidmap` plan of the epic is UNNECESSARY AND
  UNBUILDABLE — nixpkgs cannot ship a setuid or fcap binary at all
  (shadow's packaging forces 0755, `security.wrappers` is a NixOS
  rootfs mechanism dockerTools has no equivalent of), and virtiofs
  would not carry the bit either. What ships instead (bd
  myconfig-6di.5.8): one guest tree (`krun-guest-conf.nix`) with
  `bin/podman`, a storage wrapper around `pkgs.podman` — whose own
  helper closure carries conmon, crun, catatonit, netavark, passt,
  aardvark-dns and fuse-overlayfs — plus `containers.conf` with
  `events_logger="file"`, `cgroup_manager="cgroupfs"`,
  `cgroups="disabled"`, `no_pivot_root=true`, `tmp_dir`,
  `image_copy_tmp_dir`, crun's `--root` and the network config dir
  below the guest tmpfs mounts, and `netns="host"` — the guest has
  no NIC, only loopback and TSI, so a netavark bridge + NAT would
  have no egress interface and libkrunfw has no xtables for
  netavark's iptables driver; nested containers share the guest's
  stack and its exact egress; `storage.conf` with the overlay
  driver, `graphroot` `/var/tmp/containers/storage` and `runroot`
  `/run/containers/storage`; `policy.json`
  with the NixOS/skopeo default `insecureAcceptAnything` — without
  any policy file containers/image refuses every pull;
  `/etc/subuid`+`/etc/subgid` for the guest-root user), baked into
  the shared agent image via `krun.nestedPodman.{enable,packages}`
  (off by default — a gvisor-only host pays no podman closure).
  Storage lives on GUEST-native tmpfs (bd myconfig-6di.5.16): every
  mount of the outer argv — the read-only root and podman's
  `--read-only-tmpfs` surfaces /run, /tmp, /var/tmp alike — is
  applied host-side by crun and reaches the guest as ONE virtio-fs
  share (libkrun's guest init mounts only /dev, /proc, /sys, cgroup2,
  /dev/pts and /dev/shm). Container storage cannot live there: the
  virtiofs server (libkrun passthrough.rs) forwards chown unchanged
  to the unprivileged host process — EPERM for podman's layer
  chowns — refuses to create files as any uid but 0 and its own,
  and cannot set the trusted.* xattrs an overlayfs upper needs. The
  wrapper therefore mounts a guest tmpfs at `/var/tmp/containers`
  and `/run/containers` (as guest root, once per VM) before it
  execs podman, and fails rather than falling back to virtio-fs.
  RAM-cost: each tmpfs may take up to half the VM's memory (the
  tmpfs default), sized via the krun limit pins (bd
  myconfig-6di.5.6) — a large nested image needs a larger VM or
  fails with ENOSPC. The storage is per-run: a new run is a new VM.
  Residual limit: a nested container running as a non-root uid
  cannot write to a virtio-fs path it is given (e.g. `-v` of the
  workspace). Live validation is bd
  myconfig-6di.5.7's runbook (the agent sandbox has no /dev/kvm).
- **Opt-in: Nix inside the guest** (bd myconfig-pz6,
  `krun.nix.enable`; live-validated on f13 by the probes of that
  bead). This tier uses the image-owned store, not an implicit host
  store bind. The shared image carries guest-root `bin/nix*` wrappers
  (`krun-guest-nix.nix`, through the `podman.imagePackages` seam).
  The gvisor tier builds that image with its closure registered in
  `/nix/var/nix` (`imageIncludeNixDB`, dockerTools `includeNixDB`).
  On the first invocation in a VM, a wrapper does the following as
  guest root:
  - It mounts the nix scratch at `/run/mysbx-nix`: the disk-backed
    ext4 of the per-run scratch file when the run provides one
    (`MYSBX_KRUN_SCRATCH_IMG`, bd myconfig-0pi — loop-mounted, see
    the consequences below), a guest tmpfs otherwise.
  - It COPIES the image database there.
  - It mounts an overlayfs over `/nix/store`. The lower layer is the
    image's OWN store, and the upper layer is on the scratch.

  Nix then runs single-user: `NIX_REMOTE=local`, state, logs,
  `NIX_CACHE_HOME` and `TMPDIR` on scratch (disk or tmpfs), and `NIX_CONFIG` with
  `build-users-group =`, `sandbox = false` and the host-mirrored
  caches. Nothing is mounted before boot, so the image closure
  resolves throughout. That is the wall that stopped the earlier
  host-store design. Consequences:
  - Historical Podman-image probes found lower-entry copy-up failing
    on virtiofs fileattr support (`EOPNOTSUPP`, logged as
    `failed to retrieve lower fileattr`), including `nix store optimise`.
    The registered database/gcroots keep the image closure; new paths
    and GC operate on the upper. This observed limitation is NOT a
    general guarantee that a read-only lower makes the merged store
    immutable, especially with D3's native EROFS lower.
  - Nix state cannot live on virtio-fs. The server forwards chown
    unchanged (EPERM), and libgit2 refuses the host-uid-owned home
    cache.
  - The store is per-run and costs VM RAM. The tmpfs takes up to half
    the VM memory, and the 1024 MiB crun default is too small for
    dev shells, so the wrapper warns below 4 GiB. Set
    `MYSBX_PODMAN_MEMORY` (8g or more for `nix develop`). Every run
    substitutes again. The RAM cost is REMOVED by the per-run
    scratch disk when its pin is present (bd myconfig-0pi,
    `krun.nix.scratchSize`, normally pinned with `podman.enable`
    AND `krun.nix.enable`): the host truncates one SPARSE
    file per run under `<repo>.mysbx/scratch/<pid>.img` to the size
    cap (`MYSBX_KRUN_SCRATCH_SIZE`, a podman `--memory`-shaped
    value, default 32g — creation costs no disk space, only the
    guest's writes fill it), binds it over virtio-fs at
    `/run/mysbx-scratch.img` and names it in the payload env
    (`MYSBX_KRUN_SCRATCH_IMG`, the last `--env`, after every
    config layer). The guest wrapper attaches it to a loop device
    (`losetup --find --show`), `mkfs.ext4`s it and mounts it as the
    nix scratch — the ext4 is the GUEST KERNEL's own filesystem:
    chown, overlay xattrs and whiteouts work natively, nothing is
    proxied over the virtiofs xattr surface — then tries to remove
    the path right after the attach, so the space frees itself when
    the VM dies. That removal is best-effort (the path is a bind
    target and may be busy) and nothing depends on it: a `--result`
    run removes its own file after the backend exits, and every run
    first sweeps `<repo>.mysbx/scratch/<pid>.img` files whose mysbx
    pid is gone (crashed or exec-mode runs). The mysbx pid lives for
    the whole run in both run modes, so a parallel run's file is
    never swept. The tmpfs stays as the FALLBACK for runs without the pin
    and ANNOUNCES itself ("refuse or announce, never silently
    switch"); a run whose kernel cannot attach the loop device
    fails the nix call with the diagnosis (exit 125). The
    virtio-blk alternative was REJECTED by probe (e) of bd
    myconfig-0pi: crun's krun handler parses no disk annotation and
    never calls `krun_add_disk` (verified against crun 1.30 and
    upstream main; libkrun's `blk` feature is off in nixpkgs), so
    the loop mount over the sidecar bind is the mechanism. Live
    probes (loop module + ext4 in the libkrunfw guest, mount +
    unlink-while-attached, speed vs tmpfs): bd myconfig-0pi's
    runbook, ../krun-live-validation.md.
  - Builds run as guest root without nix's own sandbox, so the VM is
    the boundary. `network = false` makes substitution and fetches
    fail. As any other uid (podman-gvisor, agent-gvisor), the
    wrappers exec nix untouched.
- **Secondary: nesting other sandboxes** — no guest compatibility
  probes, no work beyond what nested podman needs.
- **Out of scope: nono inside krun** — the stock libkrunfw kernel
  (no Landlock) is fine; a custom kernel is out of scope.

#### Risks carried into the children

- **setuid/ownership over virtio-fs** (nested podman — bd
  myconfig-6di.5.4/.8): RESOLVED to a non-risk by the design of bd
  myconfig-6di.5.8. The epic's setuid `newuidmap` plan is dead on
  two independent grounds: nixpkgs cannot ship a setuid/fcap binary
  into an image at all (shadow's packaging forces 0755 —
  `security.wrappers` is a NixOS rootfs mechanism with no dockerTools
  equivalent), and even a setuid bit set at build time would not
  survive: dockerTools rsync-chowns every layer to 0:0 without
  xattrs, and virtiofs stat (verified in .4) exposes only the host
  user's own uid to the guest. It is also UNNECESSARY: the payload
  runs as guest root (bd myconfig-6di.5.4 — the guest init never
  setuids), and guest root writes a single-line `/proc/<pid>/uid_map`
  directly — `newuidmap` only exists to let an UNPRIVILEGED user
  write the MULTI-line maps out of `/etc/subuid` (nixos/programs/
  shadow.nix), which guest root needs no helper for. The residual
  honest risk: the guest's uid world on virtio-fs is single-uid
  (the server creates every file as the host user, refuses any
  other creating uid than guest root, and a chown to another uid is
  EPERM), so files a nested container writes to the SHARED mounts
  (the workspace bind, state dirs) appear host-uid-owned — correct
  behavior, but the nested pod's storage must not live on virtio-fs
  at all (it does not: guest tmpfs, bd myconfig-6di.5.16). Live
  confirmation of a full nested `podman run` is bd
  myconfig-6di.5.7's runbook.

#### Live validation (bd myconfig-6di.5.7)

Source checks and static gates are not a full live acceptance matrix.
The [runbook](../krun-live-validation.md), scripted by
`nix/krun-live-validation.sh`, covers boot, guest kernel, exit codes,
live-repo edit, ro rootfs and network=false; manual probes cover
mounts/state/uid, nested Podman, guest Nix, nproc/MemTotal and DNS.
The [historical log](../krun-validation-log.md) records smoke and
Nix runs, including both variants' 2026-10-14 smoke pass; do not read
that as completion of every manual probe or current option combination.
No new VM acceptance was performed in the source audit.

### D3: The direct `krun` backend shares the whole host store read-only; the rootfs bakes nothing but the guest init (bd myconfig-dak.2)

**Status:** the original title records the host-store design and its
rationale; the shipped baked-store amendment below is an alternative.
The early bd myconfig-dak.1 spike on f13 recorded 282 ms warm for
bwrap → mysbx-krun → VM. That small spike is NOT comparable to later
wrapped runs or a current performance ranking (D8).

`backend = "krun"` (bd myconfig-dak) drives libkrun DIRECTLY,
with no podman, no crun, no OCI image:
mysbx builds a KrunSpec, a small launcher binary (dlopen of the
pinned libkrun, `MYSBX_KRUN_LIB` — the spike's `krun-rs`) runs
under bwrap so the host-side virtiofs server can only open what
the bwrap argv left visible, and a Nix-built PLAIN DIRECTORY is
the rootfs (read-only via `krun_add_virtiofs3(KRUN_FS_ROOT_TAG)`).
The spec's shares and selected store disk determine the guest view:

- **The host store, whole, read-only (option (a) of the epic).** One
  `--ro-share` of `/nix/store` gives the guest every store path;
  the bwrap backends already grant exactly this visibility
  (`--ro-bind /nix/store /nix/store`), so host mode has the same
  guest store visibility as that tier. The virtiofs server enforces
  the lower share's ro flag; the guest cannot make that backing
  share writable. With scratch the merged overlay is writable:
  read-only backing does not forbid writes in the private upper.
  Per-run toolchains therefore need NO config surface at all:
  whatever the host builds is already visible; a repo that wants a
  specific toolchain just runs its store path. This is also the
  SPIKE-PROVEN layout (probes 1–3: store binaries ran, the ro
  share refused writes, the rw repo share reached the host).
  Option (b) — a per-run bwrap view sharing only the closure of
  the rootfs + configured tools — is REJECTED as the default: the
  closure must be recomputed per run (a nix call inside `mysbx
  run`), a missing path in the view is a runtime ENOENT with no
  diagnosis, and the visible-store premise of the bwrap tier is
  not actually improved (the whole store is already readable).
  A per-run `[tools]` list (option (c)) remains a FUTURE config
  key: it can only NARROW (bind specific closures when a host
  wants the store hidden), never widen — the whole-store host mode
  is what the spike validated, not today's pin-dependent default.
- **The whole-store share is now OPTIONAL per run (guest-store.md,
  the shipped narrowing).** The `store = "host" | "baked"` config
  key (either layer, the sidecar wins; when both are silent the
  built-in default resolves `baked` ONLY for `krun`/`microvm-qemu`
  with a NONEMPTY `MYSBX_KRUN_STORE_DISK`, otherwise `host` —
  `Merged::resolved_store`, mysbx-rs; an explicit choice always
  wins) selects per run: `host` is D3's layout above; `baked`
  DROPS the whole-store share and the j23 nix-var ride-along and
  attaches the guest-own store disk the wrapper bakes — a
  read-only erofs image of the wrapper's own tool closure with a
  REGISTERED db (the dockerTools recipe, no host-db copy and no
  reconcile), mounted at `/nix` by the init. With scratch, the
  private overlay writes on top; without scratch, there is no
  managed writable-Nix setup. The EROFS lower stays immutable, but
  writes to existing paths in the merged view can copy up: do not
  promise that every write to a baked guest-visible path fails.
  The guest's store view narrows to its baked tool closure and
  configured extra roots, not arbitrary host-built tools. The disk is
  attached by the launchers (`--store-disk`, krun's `krun_add_disk2` with
  read_only=true and qemu's `readonly=on` — the write protection
  rides the virtio config into the guest, the init tells the disk
  from the writable scratch by the `/sys/block` ro flag), gated on
  the host's opt-in (`krun.store.enable` bakes and pins
  `MYSBX_KRUN_STORE_DISK`; a baked run without the pin is refused,
  never a silent host-store run). Every backend other than `krun`
  and `microvm-qemu` refuses `baked`, including `podman-krun`.
  This narrows the GUEST view, not the VMM's host-side
  view: outer bwrap still binds the WHOLE host `/nix/store` for
  runtime dependencies. Other workspace/state/configured shares
  remain. The payload is guest root; the VMM/file server runs as
  the invoking host uid, not a separate host security principal.
- **Rootfs contents (what is baked vs. shared):** the rootfs bakes
  the guest init (bd myconfig-dak.5), bootstrap/filesystem helpers
  (static busybox, mkfs.ext4, sqlite3 and store-scan), shell symlinks
  and mountpoints, not a provisioned OCI userland. Busybox handles
  the entry's shebang/applets before the store is mounted: a
  store-symlinked bootstrap shell would dangle (spike finding 10).
  `/bin/sh` and `/bin/bash` resolve once the selected store is up.
  The rootfs's `/nix/store` symlink targets the host-mode share at
  `/tmp/mysbx-shares/stage-ro/store` on guest tmpfs (spike finding 9:
  nesting a second virtiofs device below the root device returned
  EBUSY; staged placement avoids it). Baked mode mounts its disk
  at `/nix` instead. Payload toolchains resolve through the selected
  store, with a writable overlay only when scratch is attached.
  One generic rootfs serves every repo/toolchain set, built by the
  Nix wrapper (dak.4); store contents are a separate choice.
- **In-guest Nix on top (shipped):** direct krun and QEMU use the
  SAME `krun-rootfs.nix` init, NOT D2's Podman-image runtime wrapper.
  With D7's per-run scratch disk the init prepares the writable
  overlay, Nix state, logs/cache and TMPDIR. Host mode can copy and
  reconcile the host database with networking enabled; baked mode
  copies its own database without host reconciliation. The lower
  stays read-only; the merged store is writable on private scratch.
  Without scratch there is NO runtime tmpfs fallback and no managed
  writable-Nix setup. The Nix binary is ALWAYS in the tools; binary
  availability is not gated on `krun.nix.enable`.

  **Known module-wiring gap:** `../../default.nix:482` gates the
  `krunScratchSize` pin on `podman.enable && krun.nix.enable`.
  Direct-only/QEMU-only opt-in can therefore request guest Nix but
  receive no scratch/writable setup. D2's announced tmpfs fallback
  belongs ONLY to the Podman-image wrapper, not this init.
- **No implicit host Nix daemon or usable host-socket forwarding.**
  Giving the guest the host daemon would allow writes to the HOST
  store and reads beyond the selected guest view — more than a ro
  share grants. In-guest Nix is the supported story. This is not a
  universal refusal of every socket-bearing custom mount: virtiofs
  can expose an inode without forwarding the host socket object.
- **The guest-root git trust rides in as two IN-MEMORY overlay
  files (bd myconfig-dak.5 → dak.8).** The payload runs as guest
  root over virtiofs files that keep their host uid, so the SAME
  `dubious ownership` refusal fires as under podman-krun (D2's
  zj2/jn0 model): mysbx computes the same two trust texts
  (`GIT_CONFIG_GLOBAL`'s target at `/etc/mysbx/gitconfig`, set
  last after every config `[env]`; libgit2's system scope reads
  `/etc/gitconfig`). It renders `--krun-overlay
  trustfs@<path>:0100644:<b64>` for a SEPARATE read-only `trustfs`
  device, NOT the root device. The launcher registers the texts
  there with `krun_fs_add_overlay_file`; the shared init places
  the trust shares at their config destinations. The pointers
  alias launcher-owned Vecs across `krun_start_enter`, so direct
  krun needs no host `gittrust/<pid>/` sidecar files. Podman-krun
  retains its sidecar-file route and sweep (bd myconfig-6xl);
  QEMU instead uses read-only staging slots (D9).
- **Share destinations are constrained to the rootfs's placeable
  share roots** — the baked mountpoints (`/etc`, `/home`, `/srv`,
  `/mnt`, `/media`, `/opt`, `/data`) plus the surfaces that are already
  tmpfs (`/tmp`, `/mysbx-home`): the guest init places a share below a
  first path component by mounting a tmpfs OVER it, which needs the
  mountpoint to exist on the read-only root — a dest below anything else
  is a builder refusal (`UnknownShareRoot`), never a run-time ENOENT the
  init cannot diagnose. `/nix` is refused separately (`NixShareRoot`: a
  tmpfs there would hide the baked `/nix/store` link), and the
  guest-native multiplexer socket dir (`/mysbx-home/.mysbx-tmux` on
  direct krun AND QEMU; `/dev/shm/mysbx-tmux` is PODMAN-KRUN's) is refused
  with its own diagnosis (`KrunMuxSocketDest`) BEFORE the placement rule
  — a share over it would hand the payload a dead socket, so "move it
  below a baked root" would be the wrong advice. The repo itself runs
  under the same first-component check, which is why a checkout below a
  root the guest cannot place under (a Nix sandbox's `/build`, say) is a
  refused run. `krun::placeable_share_root` is the predicate form of the
  rule, for callers that CHOOSE a path.

The KrunSpec builder (dak.3) turns the merged config into exactly
this: cpus, ram, the rootfs pin, one ro store share (dropped for a
`store = "baked"` run, its slot exempt from the mount cover check BY
SLOT — a positional slice would exempt the workspace), one rw
workspace share per the merged mounts' live/clone layout, the rw
`<repo>__worktrees` sibling share (live run, sibling exists), the
state-dirs shares, the configured mounts (implicit shares first; a
mount covering one is refused like bwrap's `HiddenMount`), env, and
the payload argv. Scratch (dak.7) and trust files (dak.8) shipped;
passt and the guest waypipe/vsock channel (dak.9) remain PROPOSED.
**Known acceptance bug:** direct krun accepts `display = "waypipe"`
and can start the host client, but has no guest channel/server. It
is not a supported graphical path; Podman-krun and QEMU refuse it.
These features do not re-open the store-selection question.

Direct krun and QEMU also accept the detached interactive paths;
[detach-validation-log.md](../detach-validation-log.md) records NO
live matrix. The ordinary smoke/Nix logs below do not validate detach.

**The virtiofs device budget groups the shares (bd
myconfig-xpq, live finding of the first wrapped run).** Every
`krun_add_virtiofs3` tag is a FULL virtiofs device, and libkrun's
MMIO budget is 11 slots (arch IRQ_BASE=5..IRQ_MAX=15) minus
balloon, rng, the implicit console and the implicit vsock — about 6
fs slots total. One share per device exhausts them at ~6 mounts
(`IrqsExhausted` → `build_microvm` Err → `krun_start_enter`
returns -EINVAL, with no message: the `error!` macro needs a
logger nothing initializes). The backend therefore STAGES the
shares: mysbx builds one per-run tree per access mode under
`<sidecar>/krun-stage/<pid>/{ro,rw}/`, bwrap binds each share's
host dir into its slot, and each tree is ONE virtiofs device
(`stage-ro`, read-only; `stage-rw`, read-write — the mode is the
device's, enforced by the virtiofs server end to end). The store
share rides in the ro tree like every other ro share when present
(a baked run carries none, and the manifest keeps its ro device
anyway: its env records must not fall back to the 2048-byte
cmdline); the rootfs's baked `/nix/store` link targets
`/tmp/mysbx-shares/stage-ro/store`, so the device count is a
CONSTANT 2 stage devices whatever the config mounts (root and, on
direct krun, `trustfs` are additional devices); the share records carry
`DEVICE SLOT DEST ro|rw` and the init mounts each device once,
bind-mounting `<device-mount>/<slot>` at each dest (a bind is no
new virtiofs device, so finding 9 does not apply, and `getcwd()`
reports the dest, not the `/tmp/mysbx-shares` backing path). The guest init traces
every step to the console when the host sets `MYSBX_KRUN_TRACE=1`
(a silent hang leaves diagnosable evidence — the live-run finding
of bd myconfig-2n8).

**The kernel cmdline budget moves env and shares into a manifest
(the seventh live finding, the root cause of every wrapped hang
since grouping).** The x86 guest kernel copies exactly
`COMMAND_LINE_SIZE` = 2048 bytes of the cmdline libkrun builds
(`head64.c copy_bootdata`; libkrun's own 64 KiB `CMDLINE_MAX_SIZE`
never reaches the kernel), and the real config's env+shares block
measured 3075 bytes — the tail, the `--` payload argv included,
silently never booted. The spike's ~600-byte cmdline is why the
spike worked. The cmdline now carries only structurally tiny
things (`KRUN_INIT`, the `--` payload argv, the manifest pointer);
env, shares and the workdir ride as records in a MANIFEST FILE the
launcher writes into the ro stage tree (`<STAGE_ROOT>/ro/manifest`,
tab-separated, values base64 — arbitrary bytes survive), the guest
init mounts `stage-ro` FIRST, reads it, applies the env, places the
shares, chdirs (the manifest's `chdir` record also fixes the
pre-existing `krun_set_workdir` bug: `/init.krun` consumed
`KRUN_WORKDIR` before the workspace share existed, silently landing
at `/`), then execs the payload. The share loop reads its records
from a file with redirection, never a pipeline subshell.

`mysbx doctor krun` (bd myconfig-dak.10) covers the backend's own
refusal surface host-side and CHEAP: `/dev/kvm`, the launcher pin
(`MYSBX_KRUN_LAUNCHER`, NO PATH fallback — the run path has none,
and doctor must not be greener than the run), the rootfs pin
(set AND shaped: a directory with `bin/mysbx-init`), and a
launcher self-probe (an invalid flag must reach argument parsing
and print the launcher's own unknown-argument wording — the probe
fails by design, exit 2, only the wording is matched).
Deliberately NO VM boot: doctor pays milliseconds, the full boot
is the runbook's probe. The [runbook](../krun-live-validation.md)
§4/§5 and [historical log](../krun-validation-log.md) record shares,
trust, network, scratch and guest-Nix probes, including a real
`nix flake check` reaching the host's genuine flake error on 'thing'
(2026-10-02). Later slowdowns and silent failure remain in that log;
it is not current all-option, baked-store, stress or detach acceptance.

### D6: The direct krun backend's network is libkrun's TSI vsock proxy; `network = false` disables it (bd myconfig-dak.6)

The direct backend adds NO net device — not passt, not a TAP. Its
network model is the one bd myconfig-6di.5.5 verified for
`podman-krun`, taken to its source:

- **`network = true` (shared) is libkrun's IMPLICIT VSOCK with
  TSI** — Transparent Socket Impersonation. With no net device
  configured, libkrun attaches a vsock device with
  `TsiFlags::HIJACK_INET` (lib.rs: with `feature = "net"` the
  heuristic is `net.list.is_empty() && legacy_net_cfg.is_none()`;
  WITHOUT the feature — the stock nixpkgs build — `enable_tsi` is
  unconditionally `true`), the guest kernel boots with `tsi_hijack`
  on its cmdline, and every AF_INET/AF_INET6 socket the guest opens
  is proxied: the vsock muxer performs the connection from the VMM's
  own network namespace. The VMM is the mysbx-krun launcher process,
  inside the bwrap sandbox — which, with the network shared, shares
  the HOST netns. The guest therefore has exactly the host's
  egress: DNS, direct connects, everything. This is the same
  semantics podman-krun gives its containers (D2's verified model),
  with one fewer layer (no OCI runtime in between).
- **DNS needs a guest resolver file.** The TSI proxy dials, but
  nothing writes `/etc/resolv.conf` inside the guest (libkrun's
  DHCP path is for net devices; TSI has none). The guest init
  therefore writes one from the manifest: the wrapper reads the
  HOST's `/etc/resolv.conf` and hands its contents through the
  manifest (`MYSBX_KRUN_RESOLV`), the init writes it to
  `/etc/resolv.conf` before the payload execs. The host's resolver
  addresses are exactly what the TSI proxy can reach (it dials from
  the host netns), so resolution and connects agree.
- **`network = false` kills the vsock entirely.** The launcher
  calls `krun_disable_implicit_vsock` (present in the stock lib —
  the vsock is NOT net-gated, only the net devices are), which sets
  `VsockConfig::Disabled`: no vsock device is attached, `tsi_hijack`
  never reaches the cmdline, and the guest kernel has NO socket
  path to the host at all — stricter than an empty netns, which
  would still have loopback. The bwrap chain additionally
  `--unshare-net`s, so the launcher has an isolated network namespace
  without an external route: neither VMM nor guest could dial out even if a
  future libkrun version re-introduced a path. Defense in depth,
  both layers.
- **`krun_set_passt_fd` is out of the first cut.** The symbol needs
  libkrun built `withNet` (nixpkgs `libkrun.override { withNet =
  true; }`), which the wrapped package does not pin. A net-enabled
  libkrun would allow a passt fd instead of TSI (per-connection
  NAT from the host's pasta) — the podman-gvisor-shaped egress with
  `--map-guest-addr` support for the LiteLLM forwarder. Until the
  wrapper pins one, `network = shared` means TSI, and the port
  mapping entries (`listen-ports`/`allow-domains`/`connect-ports`)
  stay refused exactly as on the other VM backends (config.md D21):
  TSI is an unfiltered proxy — any AF_INET connect the guest makes,
  the VMM dials — no per-domain or per-port hook exists in the
  muxer, and honesty refuses what cannot be enforced.

### D7: The direct krun backend's nix scratch is a virtio-blk disk the guest init owns (bd myconfig-dak.7)

The direct backend does not inherit D2's loop-mount workaround: the
launcher owns the libkrun context, so the disk is attached as a REAL
virtio-blk device — no loop module, no losetup, no unlink-while-attached
race (bd myconfig-0pi's probes were podman-krun's constraint: crun's
krun handler cannot attach disks, and that is exactly what the direct
backend removes).

- **When scratch is enabled, the host creates one sparse raw file per run**, under
  `<repo>.mysbx/scratch/<pid>.img`, truncated to the size cap — the
  SAME file contract as the podman-krun scratch (bd myconfig-0pi):
  never reused, never shared between parallel runs, swept at startup
  of files whose mysbx pid is gone. Creation costs no disk space;
  only the guest's writes fill it.
- **The launcher attaches it with `krun_add_disk2(ctx, "scratch",
  path, KRUN_DISK_FORMAT_RAW, false)`.** RAW, always: the image is
  mkfs'd by the guest itself, so nothing needs probing — and the
  libkrun security note forbids re-probing an image a guest could
  write (a guest with full write access to a raw image could recast
  it as qcow2 and reference host files). The format is pinned by
  knowledge, not by data. `krun_add_disk2` needs libkrun built with
  the `blk` feature — the wrapper's pinned libkrun takes
  `override { withBlk = true; }` (the same seam as dak.6's deferred
  `withNet`; a run whose lib lacks the symbol is refused with the
  diagnosis, never silently without a scratch).
- **The guest init finds the writable device, not a fixed name**:
  the original scratch-only layout had one `/dev/vd*`; baked mode
  can add a read-only store disk. The shared init distinguishes
  these using `/sys/block` read-only flags rather than assuming
  the scratch is the only disk. The run announces scratch in the
  manifest env (`MYSBX_KRUN_SCRATCH=1`); the init `mkfs.ext4 -q -F`s
  the selected writable device ONCE per run (the file is per-run,
  never reused — no stale fs survives) and mounts it as the Nix
  scratch root: overlay upper/work over the host share or baked
  store lower, Nix state, logs, cache and `TMPDIR` all on the
  ext4 — the guest kernel's OWN filesystem, chown and overlay
  xattrs/whiteouts work natively, nothing over the virtiofs xattr
  surface.
- **The `/nix` placement is deliberate (bd myconfig-anw).** In host
  mode, generic share placement refuses `/nix` roots (a tmpfs at `/nix`
  hides the baked `/nix/store` link); the scratch overlay lives
  there anyway, by its own rule — and the baked link is a SYMLINK
  into the stage tree, where a mount through the link lands at the
  link's TARGET (the sim's finding: an overlay bound at
  `/nix/store` would shadow the whole ro stage device's mount).
  The init therefore mounts a tmpfs at `/nix` (the scratch's OWN
  placement, after every share — no share ever lives below
  `/nix`), creates the real `/nix/store` dir on it, and mounts the
  overlay there with `lowerdir` naming the share's backing path
  directly. The store share stays the lower layer, the scratch the
  upper. Baked mode uses the EROFS store subtree as lower instead.
  Without scratch the selected store stays read-only, with NO managed
  RAM fallback; D2's runtime tmpfs fallback is a different wrapper.
  This holds for direct krun and QEMU. The sparse file is ephemeral,
  not a persistent store cache; merged-store writes never make the
  lower share/disk writable.
- **The host file is unlinked after the backend exits** (`--result`
  mode removes its own; the startup sweep takes crashed/exec-mode
  runs'). Closing the VM's disk fd releases its open reference;
  result-mode removal and the startup sweep reclaim the named files,
  the same per-run contract as Podman-krun.

<a id="d8-podman-krun-is-superseded-by-krun-in-principle-kept-until-the-head-to-head-retires-it-bd-myconfig-dak10"></a>

### D8: Direct-krun succession needs feature parity and live acceptance (bd myconfig-dak.10)

**Status: historical succession proposal, NOT established parity or a
shipped retirement decision.** The rationale is still one fewer OCI
layer between mysbx and the VM, but the completed source audit does
not support blanket direct-successor parity:

| | `podman-krun` (D2) | `krun` (D3) |
| --- | --- | --- |
| layers to the VM | mysbx → podman → crun (OCI config) → libkrun → VM | mysbx → bwrap → launcher → libkrun → VM |
| filesystem contract | crun prepares an OCI rootfs; image-owned store | explicit staged shares; plain rootfs; host or baked store |
| network | TSI through the container netns (D2) | TSI through the launcher netns; network=false disables vsock (D6) |
| writable Nix | image wrappers; loop scratch or announced tmpfs fallback | shared direct/QEMU init; virtio-blk scratch, NO tmpfs fallback (D7); module pin gap in D3 |
| nested Podman | opt-in provisions tools/config in the OCI image | opt-in does NOT provision direct rootfs/PATH |
| display | waypipe refused | accepted in error; guest channel/server absent |

The 2026-10-02 guest-Nix run on 'thing' demonstrated a useful direct
workflow (../krun-live-validation.md §5;
[log](../krun-validation-log.md)), not closure of every Podman feature
gap. Likewise, the early **282 ms** warm spike (`nix/krun-direct-spike.sh`
probe 4) is a different workload, not a wrapped production boot number.
The later f13 log ALREADY records both smoke suites on 2026-10-14:
**direct 25 min 42 s; podman-krun 12.7 s**. Those historical suite
durations and the spike do not establish a current speed ranking. The
same log retains the later store-walk slowdown/silent-failure evidence;
its fd-retention diagnosis is not a confirmed cause of every failure.

The epic historically favored direct-first fixes (fd ceilings, trust
spellings and db copy), using the thinner chain as its rationale. That
history does not establish current feature coverage or stability.

**Proposed, not justified by this audit:** freezing Podman-krun feature
work or retiring it after a head-to-head and a host-default cycle. Keep
both variants available. Any future retirement needs explicit feature
coverage/provisioning, comparable current workloads and stability/live
acceptance, not a claim that the head-to-head log does not yet exist or
that direct krun covers everything. No runtime default or availability
changes are made by this decision-record correction.

### D9: The `microvm-qemu` backend is `krun` with libkrun swapped for qemu + rootless virtiofsd (bd myconfig-6di.6)

`backend = "microvm-qemu"` is the deferred fallback of the epic's
alternatives table: the SAME contract as the direct `krun` backend
(D3/D6/D7) on a different VMM. Where libkrun bundles its own kernel
and in-process virtiofs server, this backend composes stock parts —
one qemu microVM per run, one UNPRIVILEGED virtiofsd per filesystem
device (root included), and SLiRP user-mode networking — the same pieces
`myconfig.ai.qemu-agent-sandbox` proved rootless
(builders.nix: `--sandbox none` virtiofsd, `type = "user"` SLiRP,
no host config).

The chain is the krun chain with the launcher swapped:

```
bwrap → mysbx-microvm-qemu → virtiofsd ×N + qemu → VM
```

(shipped, bd myconfig-6di.6 and its children; live coverage is bounded below)

The builder is the krun builder's twin (`mysbx-rs/src/microvm_qemu.rs`):
the merged config renders the SAME staged-spec argv — `--cpus/--ram`,
`--rootfs/--kernel`, `--ro-device/--rw-device` stage trees
(myconfig-xpq's grouping: constant device count, per-share slots),
`--ro-share/--rw-share TAG:SLOT@DEST`, `--env K=V`, `--chdir`, `--`
payload — so layout checks, share roots and common refusals carry
over, with golden-argv coverage. The HOST-side bwrap wrap shares the
whole-store bind, stage binds and `--unshare-net` under network=false;
QEMU additionally needs runtime `/dev`/`/tmp` and resolver plumbing
listed below. It is not literally a byte-identical launcher-only swap.

What differs from `krun`, stated honestly:

| | `krun` (D3) | `microvm-qemu` (D9) |
| --- | --- | --- |
| VMM | libkrun, in-process (the launcher IS the VMM) | qemu-system, out-of-process |
| kernel | libkrunfw's stock kernel, bundled | a PINNED nixpkgs kernel (`microvm-qemu.kernel`, a bzImage) |
| virtiofs server | in-process, per `krun_add_virtiofs3` tag | one rootless virtiofsd per device, including root (`--sandbox none`); per-run sockets and Drop cleanup on ordinary exit/error paths; abrupt-death cleanup unverified |
| rootfs read-only flag | the virtiofs device's own ro flag | virtiofsd `--readonly` for the root and the ro stage device |
| network = shared | TSI vsock proxy (D6) | SLiRP user mode (`-netdev user`), the qemu tier's model — the guest init configures the NIC statically (10.0.2.15/24, default route 10.0.2.2, DNS 10.0.2.3 — qemu's fixed SLiRP defaults), gated on the `MYSBX_MICROVM_NET` manifest marker (bd myconfig-k106), and rewrites the guest resolver to the SLiRP DNS proxy — the host's own nameservers are host-local and dead behind the NAT (bd myconfig-m0to); libslirp's upstream discovery reads the host resolv.conf through a `--ro-bind-try` in the qemu arm's bwrap wrap |
| network = false | implicit vsock disabled — NO socket path | NO netdev at all; bwrap additionally `--unshare-net`s the chain |
| nix scratch | virtio-blk via `krun_add_disk2` (D7) | a virtio-blk `-drive` on the same per-run sparse file, same guest init |
| guest-own store disk (`store = "baked"`) | `krun_add_disk2` with read_only=true — VIRTIO_BLK_F_RO, the same fixed id (`nixstore`) | a virtio-blk `-drive` with `readonly=on` on the same erofs image, same id; the kernel pin adds EROFS_FS=y (libkrunfw's x86_64 config already carries it) — the ro flag reaches /sys/block identically on both, which is how the init tells the disk from the writable scratch (guest-store.md) |
| exit code | `krun_start_enter` returns the payload's | the init reports it over a virtio-serial port; PID-1 exit panics a stock guest, so the channel is out of band; the payload runs under `setsid -c` — a new session with the console as ctty, so interactive payloads keep job control and ^C (bd myconfig-w8jd) |
| accel | KVM only (libkrun has no TCG) | launcher argv uses `kvm:tcg`, but the NORMAL mysbx run path checks and REQUIRES KVM before launch; no supported KVM-less fallback |
| console | the VMM's own stdio | `-serial mon:stdio` + `console=ttyS0` (bd myconfig-mbib): `-nodefaults` kills the default serial backend, so the argv wires it explicitly — the caller's terminal IS the guest console. A 16550 carries NO winsize (a live finding of bd myconfig-26z4, bd myconfig-a2uq): the guest ctty reported a 0x0 grid, herdr refused to attach and fish rendered blind. The pipeline therefore reads the caller's terminal size and rides it through the manifest env (`MYSBX_MICROVM_CONSOLE_SIZE=ROWSxCOLS`); the init stty's the console from it before the payload runs. Residual, inherent to the device: a resize mid-session does not propagate — the guest keeps the boot-time snapshot |
| git trust | libkrun's in-memory `trustfs` device (`--krun-overlay` flags) | the RO staging tree (bd myconfig-govd): lib.rs writes the two trust texts into `<stage>/ro/gittrust-*` before the run, the trust shares ride `stage-ro` slots, and NO `--krun-overlay` flag is emitted (the launcher refuses them) |
| host-side bwrap `/dev` | none needed (the virtiofs server is in-process) | `--dev-bind /dev/null` + `--dev-bind /dev/urandom` + `--tmpfs /tmp` (bd myconfig-ya1o): without any `--dev` flag bwrap exposes NO `/dev` at all — the launcher's `Stdio::null()` virtiofsd spawn dies ENOENT; the tmpfs is the runtime dir's socket surface (the ro rootfs's `/tmp` is read-only) |
| model endpoint | the baked `127.0.0.1` URLs are right: TSI dials from the launcher's netns (the host's), so the guest's loopback URL reaches the host's loopback proxy | the guest's loopback is its OWN behind SLiRP — nothing maps `127.0.0.1` to the host — so the backend's env pins (`MYSBX_MICROVM_QEMU_ENV`, bd myconfig-mzys) repoint the model endpoint at the SLiRP gateway `http://10.0.2.2:<litellm port>/v1`, which libslirp maps to the HOST loopback (no host-side forwarder, unlike podman-gvisor's pasta). The module pins `OPENAI_BASE_URL` plus the per-agent variables (`MYCONFIG_LITELLM_BASE_URL`, `OPENCODE_CONFIG_CONTENT`, `CUSTOM_BASE_URL`) whenever the host runs litellm |

**The guest side is shared, not forked.** The rootfs derivation is
the krun rootfs (`nix/krun-rootfs.nix`) consumed as-is: the baked
`bin/mysbx-init` mounts virtiofs devices by TAG (the manifest's
share records carry `stage-ro`/`stage-rw`), and virtiofs
tags are namespaced per-VM in qemu exactly as in libkrun — the
same init serves both backends. The manifest channel (the x86
cmdline-budget finding) is KEPT even though qemu's `-append` has
no 2048-byte limit: the argv interface stays byte-stable and
testable, and the guest init stays backend-agnostic. Both direct VMs
use `/mysbx-home/.mysbx-tmux`, NOT Podman-krun's `/dev/shm/mysbx-tmux`.
They share D3/D7's no-scratch/no-writable-Nix behavior, the always-present
Nix binary and the module scratch-pin gap. The nestedPodman image opt-in
does not provision either direct rootfs/PATH.

The boot protocol is the kernel's own, which libkrun's matches:
`-append` carries `root=/dev/root rootfstype=virtiofs` — and NO
`rootwait`: init/do_mounts.c's wait_for_root loops on
early_lookup_bdev, which is BLOCK-ONLY, so a virtiofs tag never
satisfies it and the boot would hang forever (bd myconfig-7scm).
`rootfstype=virtiofs` stays: mount_root's nodev arm needs
root_fs_names set to reach mount_nodev_root. The KERNEL mounts the
virtiofs tag `/dev/root` as the root fs (the init never mounts
it), `init=/bin/mysbx-init` (the baked entry
the kernel execs as PID 1), the manifest pointer and the exit
channel as `mysbx.manifest=` / `mysbx.exit-channel=` / `mysbx.trace=`
params (the init's ENV route is libkrun's envp mechanism, which a
stock kernel does not carry to init — the cmdline is the qemu
bridge, consumed by the same init). The PID-1 bootstrap completes
what libkrun's implicit init does for the krun guest (bd
myconfig-af0j): proc/sysfs/devtmpfs plus devpts (mux sessions
allocate PTYs in the guest), a tmpfs `/dev/shm`, the `/dev/fd`
link and `lo` UP (the baked /etc/hosts localhost mapping is
dialable) — all inside the `/proc/uptime` guard, so the krun arm
is byte-identical. The kernel
pin (`nix/microvm-qemu-kernel.nix`) is a nixpkgs kernel rebuilt
with the virtio stack, ext4 and overlayfs BUILT IN — the static
busybox init loads no modules, and the nixpkgs default config
builds virtio_fs, virtio_pci, ext4 and overlayfs as modules
(FUSE too: VIRTIO_FS=y selects it).

The payload argv rides the MANIFEST, never the cmdline (bd
myconfig-hq7k): the stock kernel's `--` cmdline split is lossy —
`next_arg` (lib/cmdline.c) whitespace-splits, a token carrying
`=` lands in init's ENVP via `repair_env_string` (init/main.c),
and MAX_INIT_ARGS caps the vector at 31 tokens. `sh -c "exit 7"`
would arrive as `sh -c exit 7` — a *new command* `7`, exiting 0:
exit codes silently lie. The manifest therefore gains `argv
<index> <base64>` records (one per token, indices ascending,
validated by the init's running count), carrying each token
losslessly — spaces, quotes, `=`, empty tokens alike. The
launcher writes the records, keeps the cmdline payload-free, and
the init's manifest loader replaces its own `$*` with the decoded
records before exec. The krun backend is untouched (libkrun's
envp route is lossless); the manifest stays the ONE channel whose
format both backends share.

**Why qemu, not libkrun — the retained rationale, not a speed ranking.**
The original direct-libkrun preference was fewer processes and no
separate kernel pin. QEMU supplies composable VMM/kernel parts (for
example a kernel config libkrunfw does not ship) and a possible stepping
stone to a full NixOS guest (`myconfig.ai.qemu-agent-sandbox`). A
KVM-less TCG path remains PROPOSED at the mysbx level: the launcher
spelling alone does not bypass the normal KVM gate. Enabling either
backend makes it available, not the selected default (D10).

HISTORICAL measurement (2026-10-25, thing, KVM): one-shot boot-to-payload
~2.4 s kernel time (`[2.3]` root mounted → `[2.4]` init runs), ~3.1 s
wall including teardown; three rootless virtiofsd daemons ready in
~10 ms. The [full log](../microvm-qemu-validation-log.md) demonstrates
that workload. Neither it nor the noncomparable 282 ms krun spike
confirms a current faster/slower ranking; D8 records the later direct
and Podman suite timings.

**Threat model delta vs. `krun` — the same honesty rule.** Not a
stronger boundary: a guest escape reaches the qemu and virtiofsd
processes, the invoking user's own uid — the same single security
context libkrun has (D2's delta applies verbatim). What differs:
the file servers are separate processes, so fd accounting moves out
of the launcher into virtiofsd. That does NOT prove bounded retention
or eliminate the dj4 class of server-side accumulation; it needs stress
measurement. virtiofsd's `--sandbox none` trusts the bwrap view, which
still includes the whole host store even for a narrowed baked guest.
The payload is guest root; host VMM/file servers run as the invoking
uid. `network = false` is the same two-layer kill (no netdev +
`--unshare-net`). Allowlists stay refused (config.md D21): SLiRP, NOT
passt, supplies no per-domain/port policy hook here.

Cleanup is implemented with `Drop` for ordinary return/error paths,
including daemon termination and runtime-directory removal. It is not
evidence of cleanup after launcher SIGKILL or other abrupt death; that
lifecycle case has no recorded acceptance.

#### Refusals

Shipped refusals include the shared krun builder's checks plus
QEMU-specific kernel/display checks:

- `pids-limit`, the allowlist keys, `orca`, a multiplexer
  without a baked entry — the krun refusals verbatim.
- `display = "waypipe"` — REFUSED in the first cut, for the same
  reason `podman-krun`'s D2 row refuses it: AF_UNIX socket objects
  do not cross virtiofs. Direct krun currently ACCEPTS waypipe in
  error, without a guest server/channel; it is not implemented
  parity. The dak.9 vsock transport and any QEMU equivalent are
  PROPOSED, not shipped, and do not automatically lift this refusal.
- A share `dest` outside the baked share roots — the same
  `UnknownShareRoot` (the guest init is shared).
- A missing kernel pin — the one qemu-specific refusal: a run
  names `MYSBX_MICROVM_QEMU_KERNEL` or refuses, never a PATH
  fallback (the launcher-pin rule of D3's doctor arm).

#### Structure

The epic myconfig-6di.6 breaks into: the decision record (this
section), the builder module + golden tests (bd myconfig-tvry),
the launcher binary `mysbx-microvm-qemu` (bd myconfig-kvq8), the
rootfs/kernel pins and module options (bd myconfig-2l9b), and the
doctor arm + checks + runbook (bd myconfig-6x9c). Live validation
on a KVM host is bd myconfig-26z4, scripted by
`nix/microvm-qemu-live-validation.sh <repo>` (run with the wrapped
`MYSBX` on PATH from a rebuilt switch): the boot, the pinned guest
kernel, the exit-code channel (`sh -c "exit 7"` → 7, bd
myconfig-hq7k/dnmk), the serial console (bd myconfig-mbib), the
workspace bind, the ro root, the SLiRP NIC and its `network =
false` kill (bd myconfig-k106), the dry-run surface (no
`--krun-overlay`, bd myconfig-govd), the guest bootstrap (bd
myconfig-af0j) and the console size (bd myconfig-a2uq). The eleven
historical scripted probes and manual fish/herdr use passed on thing,
KVM, 2026-10-25 —
[log](../microvm-qemu-validation-log.md). This is NOT baked-store,
scratch-stress, nested-Podman or detach acceptance. Direct krun and
QEMU accept detached interactive paths, but the
[detach ledger](../detach-validation-log.md) has no recorded live
matrix for them. The static CI gate is
`mysbx-microvm-qemu-wrapper-test` (nix/checks.nix): the wrapper's
three closure pins and the dry-run flags, on STUB pins — the real
kernel stays a host build.

### D10: Backend availability is a host opt-in; a disabled backend is refused, never fallen back from (bd myconfig-dq2w)

The four heavy backends — `podman-gvisor`, `podman-krun`, `krun`,
`microvm-qemu` — are OFF by default and opt-in per host through the
module:

- `myconfig.ai.dev.mysbx.podman.enable` — BOTH podman backends: the
  shared agent image, the rootless podman + gvisor host setup
  (./podman.nix) and every `MYSBX_PODMAN_*` wrapper pin.
- `myconfig.ai.dev.mysbx.krun.enable` — the DIRECT-libkrun backend
  only (`krun.direct.*`; the podman-krun runtime and guest-nix
  features ride `podman.enable` with the image they need).
- `myconfig.ai.dev.mysbx.microvm-qemu.enable` — the native-qemu
  backend (launcher, rootfs, kernel, env pins).

`bubblewrap` and `nono` are always on: they are the base of the tier
and small closures. A host that opts into nothing runs exactly those
two, and its `mysbx` wrapper carries no gvisor/podman/libkrun/qemu
input at all — the closure goal of the opt-in.

The wrapper pins the surviving set as `MYSBX_ENABLED_BACKENDS`
(space-separated; unset means every backend — the unwrapped-build
contract of a hand `nix-build` of ./nix/mysbx.nix), and the CLI
refuses everything else BEFORE the session clone and before the argv:

```
$ mysbx --backend krun -- true
mysbx: backend `krun` is disabled on this host
  enable it with myconfig.ai.dev.mysbx.krun.enable and rebuild, or pick one of: bubblewrap, nono
```

The explicit gate is load-bearing, not cosmetic: `podman-krun` has a
PATH fallback (`MYSBX_KRUN_RUNTIME` defaults to a bare `crun`), so
without the gate a host that opted out would silently run an ordinary
container instead of a microVM — exactly the silent downgrade every
other refusal in this file exists to prevent. Absent pins alone
cannot catch it.

The division of labor across the layers:

- EVAL time (../default.nix): a host-wide `config.backend` naming a
  disabled backend fails the build with an assertion naming the
  option — the same shape as the multiplexer availability gate. A
  SIDECAR naming one stays a runtime refusal (trusted layer, D7 of
  config.md).
- BUILD time (./nix/mysbx.nix): `enabledBackends` is validated
  against the crate's universe and pinned; a disabled backend's
  closure inputs are simply not built.
- RUN time (../mysbx-rs): both backend validation sites (the early
  `--backend` flag check and the authoritative merged-config match,
  which covers the `--krun`/`--podman-krun`/… shorthands) consult
  `doctor::enabled_backends()` and refuse with the message above,
  exit 70. `mysbx doctor` marks disabled backends `(disabled)` in its
  listing and refuses to probe a named disabled one; the podman
  no-image refusals (`podman-load-image` included) append the
  `podman.enable` hint when the gate is closed.

The static CI gates are `mysbx-disabled-backends-wrapper-test`
(nix/checks.nix — the flags-off wrapper pins the always-on pair and
none of the heavy pins) and the backend scenarios of
`nix/config-eval-test.nix` (the assertions).
