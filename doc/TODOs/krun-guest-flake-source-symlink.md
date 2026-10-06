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

## What remains (bd myconfig-yiw)

1. Reproduce on a live krun guest with a controlled fetcher cache:
   evaluate `git+file:///…` for the workspace share repeatedly,
   watch when a `*-source` entry becomes a symlink instead of a real
   copy, with nix's fetcher logging on. The missing ingredient is
   suspected to be a mid-session COMMIT (the observed poisoning
   registered ~4 minutes after one).
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
