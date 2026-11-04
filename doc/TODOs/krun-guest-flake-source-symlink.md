# Fix the krun guest's poisoned flake-source store symlink (nix)

Inside the mysbx krun guest, a fetch of a flake whose git tree lives
on the rw virtiofs share (the repo's own workspace) can register the
flake source in the store as a **symlink** into the share:

    /nix/store/<hash>-source -> /tmp/mysbx-shares/stage-rw/workspace

Seen with nix 2.34.8, registered by the guest's own nix mid-session
(the scratch db row carries `ca = fixed:r:sha256…` and epoch mtime).
The store layer then refuses every later eval of that flake —
including `?rev=`-pinned ones, because the fetcher cache resolves
them to the same poisoned path:

    error: path '/nix/store/<hash>-source' is a symlink

## Status

The divergence family is fixed in-repo (bd myconfig-mxu, branch
`krun-guest-nix-db-reconcile`): both krun variants reconcile the
copied host db at boot — rows whose store path is missing (the host
restaged the share) or a symlink (this poisoning) are dropped before
any payload nix runs, so a poisoned or stale registration never
survives into a new session. The reconcile cannot help MID-session
(the row bites until the session ends), and it does not answer WHY
nix registers a symlink here at all.

2026-10-14 live forensics (bd myconfig-yiw notes): the poison was
captured mid-session once. The creator was the session's own
`./get_input.sh nixpkgs` — `builtins.getFlake ("git+file://" +
toString /home/mhuber/myconfig/myconfig)` against the SANDBOX-SYMLINK
spelling, one minute after a commit, in the lock-file-update path.
The fetcher hashed the nar THROUGH the symlink (the live tree, not
the rev's export) and registered the source AS the symlink; the
same eval then failed with the `is a symlink` error.

2026-10-14 fresh-boot repro experiment (same bead notes): the
verbatim shape (fresh VM, fresh commit, `./get_input.sh nixpkgs`
within a minute) does NOT reproduce — healthy real-dir registration
with the rev's true nar hash. Dirty/staged-tree variants and
prefetch via the symlink spelling are healthy too. The poisoning
call is pinned precisely from the session log: the silent
`2>/dev/null` first run at 08:42:19, which created and tripped over
the symlink in ~2 s. Creation is not deterministic from the
observed inputs; the next step is a store-watcher wrapper
(inotify on /nix/store, argv logging) shipped in the guest to
catch the creation live.

## What remains (bd myconfig-yiw)

1. Catch the creation live: DONE instrumentationally (2026-10-31,
   branch `todo-review-krun-symlink`): the guest nix wrappers
   (`modules/myconfig.ai.dev/mysbx/nix/krun-guest-nix.nix`) now log
   every guest-root nix argv to `/tmp/mysbx-store-watch.log` and
   start a static inotify watcher
   (`.../mysbx/nix/krun-store-watch.{nix,c}`) on `/nix/store` that
   appends a typed line (DIR / SYMLINK + target) for every new
   `*-source` entry to the same log — a captured creation names its
   call. One watcher per VM (flock on
   `/dev/shm/mysbx-store-watch.lock`), fully fail-open. CAVEAT: the
   wrappers are the payload nix only in the PODMAN-krun guest
   (image `PATH=/bin` finds the wrapper tree); on f13's DIRECT
   krun backend the payload PATH carries `toolsEnv`'s raw `nix`
   (MYSBX_TOOLS_PATH) and a baked store disk — the wrapper (and
   with it the instrumentation) never runs there, even though the
   wrapper tree rides the disk's roots. The direct backend needs
   either the watcher armed in `krun-rootfs.nix`'s init or the
   wrappers placed on the payload PATH before live observation
   there is possible. Next: let agent sessions run (podman-krun
   sessions, or after arming the direct backend too), then read
   the log after a mid-session `get_input.sh`-style eval fails.
2. Identify the responsible nix code path (git fetcher fallback to
   path fetch for a dirty tree, symlink-preserving store
   registration) and file it upstream. Suspect ingredients: the
   share reaches the payload through an `ln -sfn` link placed by the
   guest init (`nix/krun-rootfs.nix`, spike finding 9's EBUSY makes
   the link the only writable-root placement), the overlayfs store
   upper layer, the copied host db, single-user mode,
   `sandbox = false`.
3. Fix by pinning a nix that registers a real copy (the guest's nix
   is a parameter of `nix/krun-guest-nix.nix`), or by changing the
   share placement if the cause is the flake path being a symlink.

## Triage 2026-10-31

ACTIONABLE (the bead's own "instrument rather than hope" next step)
— done in this branch:

- Shipped the store-watcher instrumentation: new
  `modules/myconfig.ai.dev/mysbx/nix/krun-store-watch.c` + `.nix`
  (static musl, the store-scan twin's pattern: inotify on a store
  dir, one typed log line per new `*-source` entry — DIR, or
  SYMLINK with its readlink target — plus a flock single-instance
  gate), wired into `krun-guest-nix.nix` (new `storeWatch` param;
  every guest-root wrapper argv is logged and the watcher started,
  all `|| :`-fail-open), into `default.nix`'s `krun.nix.packages`
  default and into the `checks.nix` call + a live functional
  assertion block (watcher logs symlink+dir, skips non-source,
  second instance exits quietly).
- Verified: `mysbx-krun-guest-nix-test` builds and PASSES (incl.
  the new watcher assertions, run in the check itself);
  `mysbx-generated-config-test` PASSES; `krun.nix.packages` and
  `vmStoreRoots` still evaluate on test-f13 and carry
  `mysbx-krun-guest-nix`; nixfmt 1.5.0 `--check` clean on all
  touched files; the built static watcher exercised live
  (SYMLINK/DIR lines, flock gate, non-source skip).
- New gap found while wiring (recorded in "What remains" 1): the
  DIRECT krun backend's payload runs `toolsEnv`'s raw nix — the
  wrappers (and this instrumentation) only cover the PODMAN-krun
  guest. The 2026-10-14 poison was captured in a DIRECT-backend
  session (`MYSBX_BACKEND=krun`), so arming the direct backend
  (init-side watcher or wrappers on the payload PATH) is a
  follow-up before live observation can succeed there.
- Not verified: live observation inside a real VM (needs f13
  hardware, out of scope here); the store-disk check build
  (13.6 GiB unpacked — too heavy for this sandbox).

Next check: after some podman-krun agent sessions ran, read
`/tmp/mysbx-store-watch.log` from a live VM (or via a share) for a
SYMLINK `*-source` line and the argv line above it.

## Related

The same overlay/db-copied store had a second failure mode — the
host restaging the ro store share while a VM lives (every flake
input of the old snapshot serves "Stale file handle") — and the
documented fchmodat2 re-add crash. All three are the one
db-snapshot-vs-live-share divergence, fixed together by the
boot-time reconcile; see bd myconfig-mxu.
