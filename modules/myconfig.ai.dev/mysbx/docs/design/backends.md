# Design: backends

Status: draft. This file holds decisions about how a backend confines
the payload. Configuration keys stay in [config.md](./config.md); the
command-line surface stays in [cli.md](./cli.md). Backend selection
itself is cli.md D7/D18.

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
| System calls | nono | nono's seccomp filter, in the restricted network modes only |
| Egress per domain and port | nono | nono's proxy on `127.0.0.1` in the shared netns |
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
is the egress filter and its seccomp filter. The bubblewrap backend
has neither. nono installs a seccomp filter only with `--block-net`
or an allowlist. With the shared network it installs none (tested
with nono 0.74.0: `Seccomp: 0` in the payload), so there this backend
adds only the Landlock layer. nono still runs in that case, so the
backend has one shape for every network mode.

#### Network

| Merged config | bubblewrap | nono |
| --- | --- | --- |
| `network = false` | no `--share-net`: an empty netns | `--block-net` |
| `network = true`, no allowlist | `--share-net` | no network flag (nono's default is outbound allowed) |
| `network = true`, allowlist (config.md D21) | `--share-net` | `--allow-domain`, `--allow-connect-port`, `--listen-port` per merged entry |

Rules for the allowlist case:

- An allowlist must restrict outbound traffic. `listen-ports` alone
  must either produce a restricted invocation or be refused (bd
  myconfig-a14).
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
- **The payload environment** is exactly the bubblewrap backend's:
  forwarded variables, then `[env]`, then the `HOME`, `PATH` and CA
  pins of config.md D14. A pinned `env` from mysbx's closure applies
  it as nono's child. It unsets every variable of nono's environment
  first (`-u`), so the payload never sees an XDG directory below
  `/mysbx-nono`. nono's filter never sees the payload environment, so
  an `[env]` `PYTHONPATH` reaches the payload as on bubblewrap. The
  values sit in the argv of `env`. This is the same exposure as
  bubblewrap's `--setenv` argv today.

In allowlist mode nono injects its proxy variables: `HTTP_PROXY`,
`HTTPS_PROXY`, `NO_PROXY`, their lower-case forms, `NONO_PROXY_TOKEN`,
`NONO_NO_PROXY` and `NODE_USE_ENV_PROXY`. These are infrastructure.
mysbx drops entries with these names from the payload environment,
and `--verbose` marks them `[config, ignored — set by nono]`, the same
treatment config.md D14 gives `HOME`. Overriding them could only
break connectivity, because direct connections are blocked. It could
never widen access, so the entry is ignored rather than refused.

#### Naming-service probes and the IPC-denial block (bd myconfig-7ov)

The pathname AF_UNIX mediation filter traps every pathname
`connect(2)`/`bind(2)`, and glibc's NSS probes `/var/run/nscd/socket`
before every user/group lookup. The path is not in the view, so no
capability covers it and each probe is denied — benign (glibc falls
back to files/DNS) but loud: the run footer's "IPC denial:" block
lists the probes. nono 0.74.0 exposes no knob to quieten it —
`diagnostics.suppress_system_services` hides only profile-prompt
entries, and `--no-diagnostics` gates only the failure footer, not
the denial block of a successful session — so mysbx documents the
block as known-benign: name operations against paths the view
deliberately does not carry (nscd, dbus) are expected, everything
else in the block is worth reading.

#### Pathname mediation turns connect-ENOENT into EPERM (bd myconfig-bk2)

Under `linux.af_unix_mediation = "pathname"` the supervisor resolves
a CONNECT's target child-relative and `canonicalize()`s it
(nono 0.74.0, `crates/nono-cli/src/exec_strategy/supervisor_linux.rs`,
`decide_af_unix_pathname`); a socket file that does not exist yet does
not canonicalize, and the connect is DENIED with EPERM before the
kernel could answer ENOENT — the denial footer names it "target could
not be canonicalized" (`ipc_denial_details`). The consequence for
entry scripts: **connect-to-a-nonexistent-socket is EPERM, not
ENOENT, and no payload may rely on ENOENT-retry semantics.** herdr's
TUI spawns its background server and immediately connects, so the
first connect races the server's `bind()`; herdr retries ENOENT but
treats EPERM as fatal. The herdr entry therefore pre-starts the
server itself (`herdr server`, gated on the socket file — herdr 0.9.1's
`status server` exits 0 even when nothing runs) and polls the API for
a real answer before `exec herdr` (./nix/herdr-entry.nix). tmux does
NOT need the same treatment: its client falls into `server_start`,
forks the server through `proc_fork_and_daemon` and connects on the
fd the server hands back after binding (tmux 3.7c `client.c` /
`proc.c`), so its attach connect never targets an unbound pathname.

#### The mediation supervisor rate-limits every bind/connect (bd myconfig-27o)

Every pathname bind()/connect() the filter traps is a seccomp
notification the supervisor answers, and the supervisor prices them
with a token bucket — nono 0.74.0, `supervisor_linux.rs`
`RateLimiter::new(10, 5)`: 10 tokens/s refill, burst 5. The decisive
property: an exhausted bucket denies with EPERM and the denial is
NOT recorded in the run's IPC-denial footer (the rate-limit branch
returns before `record_af_unix_ipc_denial`; the message is
debug-only), so a rate-limited failure looks exactly like a grant
failure — except the footer stays quiet about it. Live cost on f13:
herdr's server startup alone spends most of the burst (two binds,
two nscd probes), and every `herdr` CLI costs several tokens more
(NSS probes plus two API connects), so a poll loop faster than ~1/s
against the herdr API can NEVER succeed — it outruns the refill and
denies itself forever (the pre-start's original 10Hz poll failed
150/150 probes, which is the f13 "server did not become ready"
symptom of bd myconfig-27o). The herdr entry's wait is therefore
SHAPED by the bucket (./nix/herdr-entry.nix): a free 10Hz stat()
wait for the socket file, a fixed 1s grace (bootstrap + refill), and
API probes at a 1s cadence; the workspace relocation retries its
create/close once per second, because a rate-limited attempt is
transient by construction. The consequence for every future
nono-backend payload: **no AF_UNIX poll loop under this backend may
run faster than 1Hz, and a denial at burst time is not evidence of a
missing grant.**

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
- **Waypipe display**: audited against nono 0.74.0 (bd
  myconfig-6di.4.6) and LIFTED. Source-audit of the filter tables
  (`crates/nono/src/sandbox/linux.rs`): the block-all and TCP-only
  static baselines trap socket()/socketpair()/io_uring only — AF_UNIX
  socket and socketpair pass; the pathname AF_UNIX mediation filter
  (the one the mysbx profile ships) traps connect/bind/sendto/
  sendmsg/sendmmsg and its supervisor explicitly CONTINUES `sendmsg`
  with a NULL `msg_name` (fd passing over an established connection,
  `SCM_RIGHTS`); `memfd_create` is trapped by no filter. Live probe
  under the real pinned profile inside a hand-built bwrap view: the
  waypipe server binds the fake compositor socket (`wayland-0`, a
  direct child of the sandbox home) and connects to the host client
  (`<socket_dir>/waypipe.sock`) end to end under `--block-net` with
  the two `--allow-unix-socket-dir-bind` grants; without the home
  grant the run reports `bind ... (no matching unix_socket
  capability)` — the refusal dies exactly where the grant boundary
  is. Under the shared default no seccomp filter installs at all
  (nono 0.74.0, `NetworkMode::AllowAll`).
- **Unused nono features**: credential injection, TLS interception,
  rollbacks and audit sessions.

#### State, keys and the multiplexer

State directories are bound at `/mysbx-home/<entry>`, as on the
bubblewrap backend (config.md D15). The sandbox SSH key is at
`~/.ssh` (config.md D22), so the backend needs no `GIT_SSH_COMMAND`
pin. The multiplexer socket directory `/mysbx-home/.mysbx-tmux`
(config.md D16/D17) exists again. The multiplexer also needs nono's
unix-socket grant: `--allow-unix-socket-dir-bind` on the socket dir
(bd myconfig-6di.4.5, nono 0.74.0 — connect AND bind on any
direct-child socket, exactly the entry scripts' `$TMUX_TMPDIR/socket`
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
ships `linux.af_unix_mediation = "pathname"`: without it, nono's
default leaves pathname sockets reachable through ANY filesystem
grant, and the isolation claim of D16/D17 ("the socket never leaves
this sandbox") would rest on the binder's goodwill instead of the
filter.

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

- nono 0.74.0 is Landlock and seccomp only. It has no mount namespace
  and no path remap. Its own documentation rejects mount namespaces
  for portability ("Why not mount namespaces?" in
  `docs/cli/internals/security-model.mdx`).
- The chain was tested with nono 0.74.0 inside a mysbx bubblewrap
  sandbox. Landlock applies inside the user namespace. nono resolves
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
sources (bd myconfig-6di.5.4):

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
- resource limits as `--annotation krun.cpus=…`/`krun.ram_mib=…`
  instead of the cgroup flags (bd myconfig-6di.5.6) — with refusals
  for what the annotations cannot express.
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
- **Full kernel compatibility**: everything a real kernel does works —
  cgroup namespaces, overlayfs inside user namespaces, FUSE, netfilter,
  tun — which is what the REQUIRED scope decision below needs (nested
  rootless podman). gVisor's Sentry would have to emulate each of
  those.

The docs never claim krun is stronger than gVisor: the gain is host
kernel bug isolation and kernel compatibility, not a second boundary
between guest and VMM.

#### Alternatives considered

| Alternative | Why rejected |
| --- | --- |
| qemu + virtiofsd + passt, native | a second full argv builder: qemu machine flags, virtiofsd daemons, passt wiring — all the things podman already owns for rootless containers; kept as the deferred fallback (its own bead) only in case crun/libkrun fails in practice |
| cloud-hypervisor directly | no user-mode networking — it needs a TAP device on a bridge, and a bridge/tap is a root-owned host device, which this design refuses |
| firecracker | disks only, no virtio-fs — no live repo mounts, which the whole mysbx workspace model (`live` repo bind) depends on |
| reusing the `myconfig.ai.microvm` slots | root-owned prebuilt slot pool whose mounts are fixed at NixOS eval time — incompatible with the runtime TOML config merge that decides mounts per run; also far heavier than a per-run microVM |
| kata containers | containerd-based and root-oriented; heavy, and the rootless story is weaker than podman+crun/libkrun |
| nested qemu inside the podman-gvisor container | recursive virtualization of a container image that already runs under runsc — no |

#### Refusals of the first cut

Everything the krun variant cannot enforce is REFUSED, never accepted
and silently ignored — the same rule as every backend:

| Feature | First-cut status |
| --- | --- |
| `network = false` | enforced, and VERIFIED against the crun/libkrun sources (bd myconfig-6di.5.5): `--network none` gives the container an empty netns, the VMM (the rootless podman process) is created INSIDE it, and every guest egress is a TSI proxy connection the VMM dials from that netns — no route, no resolver, exactly the gvisor semantics. See the network model below |
| `allow-domains`/`connect-ports`/`listen-ports` | refused (config.md D21's table stays: krun's egress is libkrun's TSI proxy — an UNFILTERED dial from the container netns, no per-domain/port hook in the muxer — verified, see the network model below; same gap as podman-gvisor's pasta) |
| `egress = "proxy-only"` | refused until verified (config.md D20: pasta's `--map-guest-addr` adds a path to the forwarder, it does not remove the default route — same fix to share with bd myconfig-6di.3 / myconfig-jq2) |
| `display = "waypipe"` | refused in the first cut (the waypipe channel needs an AF_UNIX socket crossing virtio-fs, which passes inodes, not live socket objects — bd myconfig-6di.5.4 verified the gap against the libkrun sources; lifting needs an in-image entry with a guest-tmpfs socket dir, live-validated under bd myconfig-6di.5.7) |
| multiplexer sessions | supported (bd myconfig-55u): the entry scripts are baked into the shared image (`podman.muxEntries`, pinned as `MYSBX_PODMAN_MUX_ENTRY_*`), and `TMUX_TMPDIR` is the guest-native `/dev/shm/mysbx-tmux` (`podman_gvisor.rs::KRUN_MUX_SOCKET_DIR`) — virtio-fs files are host-uid-owned while the payload is guest root, so tmux refuses a socket dir there. Sockets stay guest-internal (a guest bind()/connect() pair on virtio-fs works; only host↔guest crossing does not). A mount at, below or above that dir is refused. An unpinned multiplexer is still refused (`MultiplexerUnavailable`) |
| host AF_UNIX sockets across virtio-fs | refused where a feature needs them (verified: the virtiofs server forwards stat/read/write/mknod over host inodes, libkrun passthrough.rs — a socket file reaches the guest as a dead inode and a guest bind()/connect() has no host socket object to reach; any feature built on a host socket crossing the mount is refused, not best-effort) |
| `--cap-drop=ALL` / `no-new-privileges` | NOT emitted on krun (bd myconfig-6di.5.4): crun's krun handler never execs the OCI process — the guest init runs the payload as guest root, reading only Env/args/WorkingDir from the OCI config (libkrun init/init.c) — so the flags would advertise enforcement that does not exist. `--read-only` stays (crun remounts the prepared root read-only host-side; virtiofs hands the guest that exact tree) |
| mounts / state-dirs / dest remap / clone sessions | the shared podman mount model, UNCHANGED: crun applies every OCI mount host-side on the container rootfs (container.c/linux.c — binds, tmpfses, ro remount), and libkrun shares the prepared tree as the virtiofs root. The gvisor layout survives verbatim: the home tmpfs, the state-parent tmpfses, the workspace bind, the state binds and the configured mounts all land on the tree the guest sees. Verified against the crun sources in bd myconfig-6di.5.4; live validation bd myconfig-6di.5.7 |
| uid mapping | `--userns=keep-id` stays but maps the HOST-SIDE preparation only: inside the guest the payload runs as guest root (the init never setuids) and the virtiofs server maps guest-root accesses to the host user's own uid unchanged (passthrough.rs `set_creds`; chown to any OTHER uid is EPERM unless the server holds CAP_SETUID). Files the payload writes in the shared mounts are host-uid-owned, as on the other backends; the nested-podman setuid story is bd myconfig-6di.5.8's risk |
| `/dev/kvm` availability | a doctor-style eval-time check: the wrapper asserts the user has rw access to `/dev/kvm` (the `kvm` group) — a host without it gets a refused run, never a silent fallback to another runtime |
| resource limits | mapped onto the krun VM annotations `krun.cpus` / `krun.ram_mib` (crun's krun handler, bd myconfig-6di.5.6) — the first mysbx limit mechanism with no cgroup dependency; `--pids-limit` refused (no pids controller is wired for a whole-VM "container"), fractional vCPUs and sub-128-MiB memory refused (crun silently defaults `ram_mib <= 128`) — never accepted and silently ignored |

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

- **Required: nested rootless podman inside the guest** (bd
  myconfig-6di.5.8). The guest kernel (libkrunfw) has user namespaces,
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
  bead). The argv is unchanged, and no host store is involved. The
  shared image carries guest-root `bin/nix*` wrappers
  (`krun-guest-nix.nix`, through the `podman.imagePackages` seam).
  The gvisor tier builds that image with its closure registered in
  `/nix/var/nix` (`imageIncludeNixDB`, dockerTools `includeNixDB`).
  On the first invocation in a VM, a wrapper does the following as
  guest root:
  - It mounts a guest tmpfs at `/run/mysbx-nix`.
  - It COPIES the image database there.
  - It mounts an overlayfs over `/nix/store`. The lower layer is the
    image's OWN store, and the upper layer is on the tmpfs.

  Nix then runs single-user: `NIX_REMOTE=local`, state, logs,
  `NIX_CACHE_HOME` and `TMPDIR` on the tmpfs, and `NIX_CONFIG` with
  `build-users-group =`, `sandbox = false` and the host-mirrored
  caches. Nothing is mounted before boot, so the image closure
  resolves throughout. That is the wall that stopped the earlier
  host-store design. Consequences:
  - Image paths are immutable. Overlay copy-up of a lower entry fails
    because virtiofs has no fileattr support (`EOPNOTSUPP`, logged
    as `failed to retrieve lower fileattr`). The registered database
    keeps nix from replacing them, and `nix store optimise` cannot
    work. GC is safe: the copied gcroots keep the image closure, and
    only upper-layer paths are deleted.
  - Nix state cannot live on virtio-fs. The server forwards chown
    unchanged (EPERM), and libgit2 refuses the host-uid-owned home
    cache.
  - The store is per-run and costs VM RAM. The tmpfs takes up to half
    the VM memory, and the 1024 MiB crun default is too small for
    dev shells, so the wrapper warns below 4 GiB. Set
    `MYSBX_PODMAN_MEMORY` (8g or more for `nix develop`). Every run
    substitutes again.
    Candidate to remove the RAM cost (bd myconfig-0pi, not yet
    probed): a per-run sparse ext4 file on disk, one per run and
    never reused or shared, e.g. `<repo>.mysbx/scratch/<run-id>.img`.
    The guest wrapper loop-mounts it as the scratch and unlinks the
    path at once, so the space is freed when the VM exits, and
    mysbx sweeps leftovers at startup. ext4 is the guest kernel's
    own filesystem, so chown and overlay xattrs work; virtio-fs
    carries only the file's bytes. The alternative is a libkrun
    virtio-blk disk, if crun's krun handler can attach one.
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

Everything above is source-verified and statically gated; what
needs a booting microVM is collected in one runbook,
../krun-live-validation.md, with the scripted half at
../../nix/krun-live-validation.sh (boot, guest kernel, exit codes,
live-repo edit, ro rootfs, network=false) and the manual probes
the script cannot see (mounts/state-dirs/uid over virtio-fs,
nested podman, guest nix, the VM annotations as nproc/MemTotal, DNS over
TSI). The agent sandbox has no /dev/kvm — the runbook is the
handoff.
