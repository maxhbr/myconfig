<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `dirmergefs` — architecture

`dirmergefs` (working title) is a FUSE filesystem. It shows several
directories as one merged directory tree:

```
dirmergefs /PATH/TO/INPUT /PATH/TO/MOUNT
```

- `INPUT/BASE` is the **base branch**. Every new file created through the
  mount is stored here.
- Every other directory directly below `INPUT` is a **branch**. Its contents
  are merged into the mount.
- Files that already exist are edited **in place**, in the branch that owns
  them. There is no copy-up.
- When two branches have the same path, a fixed rule picks the winner. The
  losers are hidden and a warning is written.

## 1. Example

```
INPUT/                              MOUNT/
├── BASE/                           ├── f1.txt        -> BASE/f1.txt
│   ├── f1.txt                      ├── f2.txt        -> BASE/f2.txt
│   └── f2.txt                      ├── f3.txt        -> Branch1/f3.txt
├── Branch1/                        └── subpath/to/file/
│   └── f3.txt                          └── f4.txt    -> Branch2/subpath/to/file/f4.txt
└── Branch2/
    └── subpath/to/file/f4.txt
```

- `touch MOUNT/new.txt` creates `INPUT/BASE/new.txt`.
- `echo x >> MOUNT/f3.txt` changes `INPUT/Branch1/f3.txt`.
- `touch MOUNT/subpath/to/file/new.txt` creates
  `INPUT/BASE/subpath/to/file/new.txt`. The missing parent directories are
  created in `BASE`.

## 2. Motivating use case: sandbox sessions

```
host                                         sandbox N
~/.local/app1/  ── dirmergefs mount ──┐
                                      │
~/.local/app1.d/            (INPUT)   │
├── BASE/        <- host writes ──────┤
├── session-1/   <- bind-mounted as ~/.local/app1 in sandbox 1
├── session-2/   <- bind-mounted as ~/.local/app1 in sandbox 2
└── ...                               │
                                      └─ host app sees BASE ∪ session-*
```

- Each sandbox gets one plain directory, `INPUT/session-N`. It does not need
  FUSE. The directory is shared the usual way: a bubblewrap `--bind`, a gVisor
  mount, or virtiofs for microVMs.
- On the host, `~/.local/app1` is the `dirmergefs` mount. The host app sees
  its own state plus the state of every session.
- A sandbox can only write its own branch. It cannot see or change `BASE`
  or other sessions.
- New sessions appear as new directories under `INPUT` while the filesystem
  is mounted. Branch discovery must therefore be **dynamic** (§5.2).

If a sandbox must also see `BASE`, the sandbox can use its own overlayfs,
`lower=BASE (ro)` and `upper=session-N`. This is out of scope for
`dirmergefs`, but see §11 for overlay whiteouts in branches.

## 3. Prior art and why it is not enough

| Tool | Gap for this use case |
| --- | --- |
| overlayfs | Writes to a lower layer copy the file up to the upper layer, so edits do not go to the owning branch. Changing the lower layers while mounted is undefined behaviour. The set of layers is fixed at mount time. |
| unionfs-fuse | Copy-on-write to the top branch. Same problem as overlayfs. |
| mergerfs | Closest match: `category.create=ff` with `BASE` first, in-place writes, and a union readdir. But it reads branch globs only at mount time (runtime changes need a `setxattr` on a control file), it does not report conflicts, and its rename and create policies do not match §6. |

A **prototype** with mergerfs plus a small watcher script that rewrites the
branch list is a good way to test the use case before writing code
(milestone M0).

## 4. Terminology

| Term | Meaning |
| --- | --- |
| INPUT | Directory that contains the branches. |
| MOUNT | Mount point that shows the merged view. |
| branch | A directory `INPUT/<name>` whose name does not start with `.`. |
| BASE | The branch `INPUT/BASE`. It always has the highest priority and receives all creates. |
| logical path | A path relative to MOUNT, for example `/subpath/to/file/f4.txt`. |
| candidate | A pair (branch, entry) where `branch/<logical path>` exists (checked with `lstat`). |
| winner | The candidate that the merged view shows. |
| shadowed | A candidate that is hidden by a winner. |
| owner | The branch of the winner. |

## 5. Branches

### 5.1 Priority order (deterministic)

1. `BASE`.
2. All other branches, sorted by the **bytes** of their names in ascending
   order. The sort does not depend on the locale.

Callers should give branches sortable names, for example
`20260101T120000-<id>` instead of `session-2` / `session-10`. There is an
optional flag `--order=desc` so that newer sessions win (see open
questions).

`BASE` must be first. This rule makes creates in `BASE` safe: a path that
is a directory in the view can never be a non-directory in `BASE` (§6.2).

### 5.2 Discovery

- Only the top level of `INPUT` is scanned. Directories, and symlinks to
  directories, become branches. Names that start with `.` are reserved, for
  example `INPUT/.dirmergefs/` for state. Other entries are ignored with a
  warning.
- Changes are detected with one inotify watch on `INPUT` (`IN_CREATE`,
  `IN_DELETE`, `IN_MOVED_*`). If inotify is not available, the daemon checks
  the mtime of `INPUT` on each root lookup instead.
- When the set of branches changes, the daemon replaces the branch list
  atomically (`ArcSwap`). It then tells the kernel to drop its cached
  entries for the root (`notify_inval_inode` / `notify_inval_entry`).
- When a branch is removed, files that are already open keep working
  through their open file descriptors. New lookups no longer see the branch.
- If `BASE` is missing at startup, the program exits with an error, or
  creates `BASE` when `--create-base` is given. If `BASE` disappears while
  mounted, creates return `EROFS` and a warning is logged.

## 6. Semantics

### 6.1 Resolution

Resolution uses `lstat` and never follows symlinks inside branches.

```
resolve(path):
    cands = [(b, lstat(b/path)) for b in branches_in_priority_order if exists]
    if cands is empty:                   -> ENOENT
    first = cands[0]
    if first is not a directory:
        winner = first                   # a file, symlink, fifo, ... masks everything below it
        shadowed = cands[1:]
    else:
        winner = merged dir of all dir candidates
        shadowed = non-dir candidates    # a dir hides lower files of the same name
    if shadowed is not empty: report_conflict(path, winner, shadowed)
    return winner
```

- A directory is merged only with other directories.
- The attributes shown for a merged directory (mode, owner, times) come from
  its highest-priority directory candidate.
- A branch adds to `a/b/c` only if `a/b` is a directory in the view. If a
  higher branch has a file `a`, the whole subtree `a/…` of lower branches is
  hidden. This follows from resolving each path component in turn, and it
  is reported as a conflict on `a`.
- `readdir(dir)` returns the union of names over all directory candidates.
  Each name appears once, and each name is resolved with the rule above.
  When the directory is opened, the daemon takes a snapshot of the names in
  sorted order. That makes `readdir` offsets stable.

### 6.2 Operations

| Operation | Behaviour |
| --- | --- |
| `lookup`, `getattr`, `readlink`, `open`, `read` | On the winner. |
| `write`, `truncate`, `fallocate`, `fsync` on an existing file | In place, on the file of the owner. |
| `create`, `mknod`, `mkdir`, `symlink` of a new name | In `BASE/<path>`. Missing parent directories are created in `BASE`, with the mode copied from the directory in the view. |
| `open(O_CREAT)` on an existing name | Opens the existing file in its owner. No new file is created. |
| `unlink` | Deletes the winner **and** all shadowed non-directory candidates (default `--unlink=all`). Without this, a shadowed file would come back and `rm -rf` would fail with `ENOTEMPTY`. With `--unlink=winner` only the winner is deleted, and a warning is logged when a shadowed file comes back. |
| `rmdir` | The view is empty only if every directory candidate is empty. Then the directory is removed in every branch that has it. |
| `chmod`, `chown`, `utimens` | On the winner. For a merged directory, on every directory candidate, so the directories do not drift apart. |
| `link` | Only if source and target resolve to the same branch. Otherwise `EXDEV`. |
| `setxattr`, `getxattr`, `listxattr`, `removexattr` | Passed to the winner. The namespace `user.dirmergefs.*` is virtual and read-only (§8). |
| `statfs` | Reports the filesystem of `BASE`. |
| `flock`, POSIX locks | See §11. The first version uses the kernel's local lock handling. |

Invariant: a create only happens when the name does not exist in the view.
Because `BASE` has the highest priority, no path component in `BASE` can be
a non-directory where the view shows a directory. So creating the parent
directories in `BASE` cannot fail because of a type conflict, and a new
entry in `BASE` never hides an existing visible file.

### 6.3 Rename

`rename(src, dst)` must keep the common "atomic save" pattern working.
Editors, git and SQLite write a temporary file, which lands in `BASE`, and
then rename it over the real file, which may be in a branch. The real file
must stay in its branch.

```
S = owner(src)
if src is a merged directory (dir in >1 branch):   -> EXDEV  (mv falls back to copy + delete)
if dst exists in view:
    if dst is a merged directory:                  -> EXDEV
    T = owner(dst)                                  # replace dst where it lives
else:
    T = S                                           # a plain rename stays in its branch
ensure parent(dst) exists as a directory in T       # mkdir -p, as in create
if T == S:                  renameat2(S/src, S/dst, flags)
elif same st_dev(S, T):     renameat2(S/src, T/dst, flags)     # still atomic
else:                       copy to T/.dirmergefs-tmp-XXXX, fsync, rename into T/dst, unlink S/src
apply the unlink policy to shadowed candidates of src   # otherwise they come back
```

- `RENAME_NOREPLACE` is checked against the view, under the structure lock
  (§7.4), and then passed to `renameat2` in `T`.
- `RENAME_EXCHANGE` works only when both paths have the same owner.
  Otherwise it returns `EXDEV`.
- Examples:
  - `mv MOUNT/f3.txt MOUNT/g3.txt` renames `Branch1/f3.txt` to
    `Branch1/g3.txt`.
  - vim writes `MOUNT/.f3.txt.swp` (in `BASE`) and renames it to `f3.txt`.
    The result is `Branch1/f3.txt`.

### 6.4 Conflicts and warnings

Conflict kinds are `file/file`, `dir/file`, `file/dir` (a subtree is
hidden) and `special` (for example an overlay whiteout, see §11).

The daemon finds conflicts lazily, during lookup and readdir. It cannot see
a conflict in a directory that nobody has visited yet. For a full report
there are two options:

- `dirmergefs check INPUT` scans all branches offline, prints every
  conflict, and exits with a non-zero code if it found any.
- `--scan-on-start` runs the same scan in the background after mounting.

Each conflict is logged once per key (logical path, winner branch, set of
shadowed branches) as a structured line to stderr/journald:

```
WARN conflict kind=file/file path=/config.toml winner=BASE shadowed=[20260101T120000-a,20260102T090000-b]
```

As an option, the current conflict set is also written to
`INPUT/.dirmergefs/conflicts.json`. This file is not visible in the mount.

## 7. Internal architecture

### 7.1 Stack

- Rust. The repo already builds Rust crates with `rustPlatform`.
- `fuser` for the FUSE low-level API, `rustix` for `openat2`, `renameat2`
  and the `*at` syscalls, `clap` for the CLI, `tracing` for logging,
  `inotify` and `arc-swap`.
- The daemon runs unprivileged through `fusermount3`. It uses
  `default_permissions`, so the kernel checks the mode bits, and it mounts
  with `nosuid,nodev`. With `--allow-other` other users can access the
  mount, which needs `programs.fuse.userAllowOther`.

### 7.2 Modules

```
src/
├── main.rs          CLI (mount | check | fold), startup validation, signal handling
├── config.rs        options: order, unlink policy, cache TTLs, symlink policy, ...
├── branches.rs      discovery, ordering, inotify watch, ArcSwap<BranchSet>
├── backend.rs       safe fd-based access to one branch (openat2 RESOLVE_BENEATH|RESOLVE_NO_SYMLINKS)
├── resolve.rs       pure resolution logic (§6.1) over a `Backend` trait
├── inodes.rs        inode table: ino <-> logical path, nlookup, generation
├── handles.rs       open file / dir handles (backing fd, readdir snapshot)
├── ops/             create, unlink, rename, setattr, xattr, ... (§6.2, §6.3)
├── conflicts.rs     conflict detection, deduplication, reporting
└── fs.rs            `impl fuser::Filesystem`, a thin layer that calls ops/*
```

`resolve.rs` and `ops/*` only use the `Backend` trait. They are
unit-tested against an in-memory backend, without FUSE.

### 7.3 Inodes and handles

- Inode numbers are virtual. The inode table maps each `ino` to its logical
  path and each logical path to its `ino`. Entries are removed on `forget`
  when `nlookup` reaches 0. Rename updates the paths of the whole subtree.
- The daemon does **not** cache the backing location. Each operation
  resolves the logical path again, so writes made directly into a branch by
  a sandbox show up in the view. (A later optimisation can add a short TTL
  cache.)
- `open` resolves the path, opens the backing file with `O_NOFOLLOW`, and
  stores the file descriptor in the handle. `read`/`write` then use
  `pread`/`pwrite` on that fd. A later option can use FUSE passthrough
  (Linux ≥ 6.9) to get close to native I/O speed.

### 7.4 Concurrency and consistency

- The daemon is multi-threaded. Read paths take no locks.
- A structure lock, sharded by parent directory, makes every
  "check-then-act" operation atomic *with respect to other FUSE clients*:
  create, `rename` with `NOREPLACE`, unlink with all copies, rmdir.
- Sandboxes that write branches directly are **not** synchronised with the
  daemon. The daemon uses fd-relative syscalls and returns their errors to
  the caller. There is no attempt to be atomic across branches.
- Kernel caching: `entry_timeout` and `attr_timeout` default to 1 s
  (`--strict` sets them to 0). No `keep_cache`, so the page cache is dropped
  on each open. No writeback cache. A later option can use inotify on
  visited directories plus `notify_inval_*` for exact invalidation.

### 7.5 Startup validation

- `INPUT` and `INPUT/BASE` exist and are directories.
- `MOUNT` is an empty directory, or `--nonempty` is given.
- `MOUNT` is not inside `INPUT`, and `INPUT` is not inside `MOUNT`. Either
  case would recurse and deadlock the daemon.
- The daemon runs in the foreground by default, which suits systemd. On
  `SIGTERM`/`SIGINT` it unmounts cleanly.

## 8. Introspection

Each entry in the view has these virtual read-only xattrs:

- `user.dirmergefs.branch`: owner branch name.
- `user.dirmergefs.backing`: backing path, relative to `INPUT`.
- `user.dirmergefs.shadowed`: list of shadowed branches, separated by
  newlines.

Example: `getfattr -n user.dirmergefs.branch MOUNT/f3.txt` gives `Branch1`.

## 9. Security

The threat model is that branches are written by **untrusted sandboxes**.
`INPUT` itself and `BASE` are trusted.

- Resolution never follows symlinks inside a branch. Paths are walked with
  `openat2(RESOLVE_BENEATH | RESOLVE_NO_SYMLINKS | RESOLVE_NO_MAGICLINKS)`,
  so a symlink such as `session/x -> /` cannot make the daemon access files
  outside the branch.
- Symlinks are shown as symlinks. The *host kernel* resolves them relative
  to the caller. A session can therefore plant `config -> ~/.ssh/id_ed25519`
  for the host app. This is the same risk as using the branch directly.
  `--symlinks=passthrough|hide|relative-only` reduces it.
- The mount uses `nosuid,nodev`. Device nodes and fifos from branches are
  shown but give no privilege.
- File names can be any bytes (`OsStr`), not only UTF-8. Log output escapes
  them.
- Resource abuse, such as huge directories or deep trees, is limited by the
  per-operation cost only. The daemon keeps no per-branch cache that could
  grow without limit. The readdir snapshot is the only O(n) allocation.

## 10. Integration in this repo

- Package: `pkgs/dirmergefs/`, or a separate repository that this flake
  consumes as an input.
- home-manager module `myconfig.dirmergefs.mounts.<name> = { input; mount;
  extraArgs; }`. It creates one systemd user service per mount, with
  `ExecStart=dirmergefs …` and `ExecStop=fusermount3 -u …`.
- If the service is not running, the app would write into the empty
  mountpoint directory. To prevent this, keep the mountpoint `chmod 000`
  while it is not mounted, and order the app's units `After=`/`Requires=`
  the mount service.
- Moving an existing directory to `dirmergefs`: `mv ~/.local/app1
  ~/.local/app1.d/BASE`, then mount at `~/.local/app1`.
- Sandbox wrappers (bubblewrap, gVisor, microVM) create
  `INPUT/<timestamp>-<session-id>` and bind it at the app path inside the
  sandbox.
- NixOS VM test in `tests/`: mount, run the scenarios from §1 and §6, add a
  branch while mounted, check the conflict warnings.

## 11. Known limitations and extensions

- **Locks**: FUSE handles POSIX locks and `flock` only inside the mount.
  A sandbox that writes the *same* backing file directly does not see these
  locks. The risk is low because each sandbox has its own branch. Extension:
  forward locks to the backing fds as OFD locks.
- **Overlay whiteouts**: A sandbox that uses overlayfs with `upper=session`
  can leave whiteouts (char devices 0:0) and opaque xattrs in its branch.
  The first version hides them and reports them as `special` conflicts.
  Extension: treat them as deletions of lower-priority candidates.
- **Hard links**: only within one branch. `st_nlink` comes from the backing
  file.
- **Directory rename across branches**: `EXDEV`, as in overlayfs without
  `redirect_dir`.
- **Fold / consolidate**: `dirmergefs fold INPUT <branch>` moves every
  entry of a branch that has no conflict into `BASE`, then lists the
  entries that are left. This is the offline step that "merges the sessions
  in the end".

## 12. Milestones

| | Scope |
| --- | --- |
| M0 | Prototype with mergerfs and a branch-list watcher to test the use case. |
| M1 | Read-only view: lookup, getattr, readdir, readlink, open, read. Static branches, conflict logging, `check` subcommand. Unit tests on the in-memory backend. |
| M2 | Writes: in-place write, create/mkdir/symlink in `BASE`, unlink, rmdir, setattr, xattrs, introspection xattrs. |
| M3 | Rename (§6.3), dynamic branch discovery, kernel cache invalidation. |
| M4 | Nix package, home-manager module, NixOS VM test, sandbox wrapper integration. |
| M5 | `fold`, FUSE passthrough I/O, lock forwarding, whiteout handling. |

Tests at each milestone:

- Unit tests for `resolve` and `ops` against the in-memory backend.
- Property tests that compare a random sequence of operations on the mount
  with a reference model.
- Integration tests on a real FUSE mount in a tmpdir.
- `pjdfstest` and `fsx` for general POSIX behaviour. Some failures are
  expected (`EXDEV` cases) and are listed.

## 13. Open questions

1. Ordering among non-`BASE` branches: should newer sessions win over older
   ones (`--order=desc`) by default, or should older ones win?
2. Should `unlink` delete shadowed copies by default (`all`), or delete only
   the winner and let the shadowed file come back (`winner`)?
3. Should a create inside a directory that exists **only** in one branch go
   to `BASE` (as specified now) or to that branch, like mergerfs `epff`?
4. Should there be read-only branches, for example finished sessions, where
   a write triggers a copy-up into `BASE`?
5. Should `dirmergefs` live in this repo (`pkgs/`) or in its own
   repository?
