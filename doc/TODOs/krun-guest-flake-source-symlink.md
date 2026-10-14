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

1. Catch the creation live: ship a store-watcher wrapper around the
guest's nix (inotify on `/nix/store` for new `*-source` entries +
argv/stderr logging) in `nix/krun-guest-nix.nix`, then let agent
sessions run as usual. The observed creation is not deterministic
from (fresh VM, symlink URL spelling, fresh commit, dirty tree) —
all of those reproduce HEALTHY.
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

## Related

The same overlay/db-copied store had a second failure mode — the
host restaging the ro store share while a VM lives (every flake
input of the old snapshot serves "Stale file handle") — and the
documented fchmodat2 re-add crash. All three are the one
db-snapshot-vs-live-share divergence, fixed together by the
boot-time reconcile; see bd myconfig-mxu.
