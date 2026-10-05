# Fix the krun guest's poisoned flake-source store symlink (nix)

Inside the mysbx krun guest, a fetch of a flake whose git tree lives
on the rw virtiofs share (the repo's own workspace) can register the
flake source in the store as a **symlink** into the share:

    /nix/store/<hash>-source -> /tmp/mysbx-shares/stage-rw/workspace

Seen with nix 2.34.8, registered by the guest's own nix mid-session
(the scratch db row carries `ca = fixed:r:sha256…` and epoch mtime).
The store layer then refuses every later eval of that flake —
including `?rev=`-pinned ones, because the fetcher cache
(`/mysbx-nix/cache/fetcher-cache-v4.sqlite`) resolves them to the
same poisoned path:

    error: path '/nix/store/<hash>-source' is a symlink

The guest init cannot sweep this at boot: the scratch is formatted
fresh per VM run, and the poisoning happens mid-session. A defensive
per-invocation sweep in `build-pkg-for-host.sh` was tried and
**reverted** — it only protected one script and papered over the
root cause.

## What to do

1. Reproduce on a live krun guest with a controlled fetcher cache:
   evaluate `git+file:///…` for the workspace share repeatedly,
   watch when a `*-source` entry becomes a symlink instead of a real
   copy, with nix's fetcher logging on.
2. Identify the responsible nix code path (git fetcher fallback to
   path fetch for a dirty tree, symlink-preserving store
   registration) and file it upstream. Suspect ingredients: the
   share reaches the payload through an `ln -sfn` link placed by the
   guest init (`nix/krun-rootfs.nix`, spike finding 9's EBUSY makes
   the link the only writable-root placement), the overlayfs store
   upper layer, the copied host db, single-user mode,
   `sandbox = false`.
3. Fix in this repo by pinning a nix that registers a real copy
   (the guest's nix is a parameter of `nix/krun-guest-nix.nix` /
   `MYSBX_NIX`), or by changing the share placement if the cause is
   the flake path being a symlink.

## Related

The same overlay/db-copied store has a second known mutation
defect: the fchmodat2 crash documented in `nix/krun-guest-nix.nix`
("a re-add deleting a lower-layer entry through the overlay"), and
a live session lost `/bin/bash` mid-build the same way (the bash
ELF's glibc interpreter path stopped resolving through the overlay
while a store-consuming build ran). Track the store-overlay
mutation family together.
