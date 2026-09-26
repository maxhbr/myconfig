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

#### nono's own inputs

- **Profile**: a store path built by the Nix module, passed as
  `--profile <path>`. mysbx never passes a profile name, because nono
  looks up names in a directory. nono's built-in `default` profile is
  not used: its grants change between nono releases, and its `$HOME`
  credential denies target paths that do not exist in the view. With
  `HOME=/mysbx-home` those denies even aborted nono once a grant
  covered an ancestor ("Landlock deny-overlap is not enforceable on
  Linux"). A mysbx-owned profile keeps the policy pinned and
  reviewable. The profile contents are bd myconfig-6di.4.3.
- **State**: `/mysbx-nono`, a tmpfs created with the base mounts. It
  is never granted to the payload, and a mount `dest` at, below or
  above it is refused like every base path. nono refuses any grant
  that overlaps its protected state roots (`$XDG_STATE_HOME/nono` and
  the legacy `$HOME/.nono`). Both resolve below `/mysbx-nono`, so the
  `/mysbx-home` grant does not collide with them.
- **No network use by the launcher**: `NONO_NO_UPDATE_CHECK=1`.
- **Unused nono features**: credential injection, TLS interception,
  rollbacks and audit sessions.

#### State, keys and the multiplexer

State directories are bound at `/mysbx-home/<entry>`, as on the
bubblewrap backend (config.md D15). The sandbox SSH key is at
`~/.ssh` (config.md D22), so the backend needs no `GIT_SSH_COMMAND`
pin. The multiplexer socket directory `/mysbx-home/.mysbx-tmux`
(config.md D16/D17) exists again. The multiplexer also needs nono's
unix-socket grants (bd myconfig-6di.4.5).

#### Refusals

| First-cut refusal | Now |
| --- | --- |
| mount `dest` different from its source | lifted: bubblewrap binds it |
| `--session` clone runs | lifted: bubblewrap binds the clone at the repo path |
| `network = true` without an allowlist | lifted: the premise was wrong, nono allows outbound traffic by default |
| multiplexer sessions | lifted when the unix-socket grants land (bd myconfig-6di.4.5), refused until then |
| `display = "waypipe"` | kept until nono's seccomp filter is audited for it (bd myconfig-6di.4.6) |
| an allowlist on `bubblewrap` or `podman-gvisor` | unchanged (config.md D21) |
| `network = false` with an allowlist | unchanged (config.md D21) |

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
