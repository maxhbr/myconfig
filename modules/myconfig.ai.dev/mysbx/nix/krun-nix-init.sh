#!/bin/bash
# agent-krun-init — the guest entry of the podman-krun Nix story
# (bd myconfig-6di.5.9, docs/design/backends.md D2).
#
# WHY THIS RUNS INSIDE THE GUEST
#
# The krun guest boots the payload as GUEST ROOT (verified, bd
# myconfig-6di.5.4 — the libkrun init never setuids), and the guest
# kernel (libkrunfw) has CONFIG_OVERLAY_FS=y. That is the one place in
# the whole mysbx stack where a real kernel can mount an overlayfs over
# the read-only host store view the argv already provides:
#
#   /nix/store-lower      the host /nix/store, bound ro by the argv
#   /nix/var-lower/nix    the host /nix/var/nix, bound ro by the argv
#   /nix                  a tmpfs (the argv), carrying the overlay's
#                         own surfaces: upper, workdir, and the overlay
#                         store's SQLite state
#
# This script mounts
#
#   overlay -o lowerdir=/nix/store-lower,upperdir=/nix/upper,workdir=/nix/work  /nix/store
#
# and exports a Nix configuration pointing at the merged view through a
# `local-overlay` store (Nix 2.34's experimental layered store):
#
#   store = local-overlay://?lower-store=local://?real=/nix/store-lower&state=/nix/var-lower/nix&read-only=true&upper-layer=/nix/upper&state=/nix/state
#
# (the inner query of the lower-store URI is percent-encoded when it is
# smuggled through the outer query — see the constant below). Nix then:
#
#   - reads the HOST database through the ro lower store — SQLite is
#     opened `immutable`, no locks, no WAL replay (see the staleness
#     note in backends.md D2),
#   - resolves host store paths through the overlay without copying
#     them (the acceptance of bd myconfig-6di.5.9),
#   - writes ONLY to the upper layer and /nix/state — the host store is
#     never touched.
#
# check-mount stays ON (Nix's default): it verifies /proc/self/mounts
# carries exactly the lowerdir/upperdir this script mounted, so a
# silently-different mount layout is a loud failure, not a quiet lie.
#
# The script must NOT carry a /nix/store shebang — the image has no /nix
# before this script builds one; a plain `#!/bin/bash` resolves against
# the image's own /bin. Failures FAIL CLOSED: `MYSBX_KRUN_NIX=1` is the
# argv's promise of a usable overlay store, so anything missing is an
# error, never a degraded plain run (the same rule agent-gvisor-init's
# Nix block follows).
set -u

log() { printf 'agent-krun-init: %s\n' "$*" >&2; }

die() {
    log "error: $*"
    log "  the mysbx podman-krun Nix story (bd myconfig-6di.5.9) cannot start;"
    log "  start the run without krun.nix, or see docs/design/backends.md D2"
    exit 1
}

# The overlay store URL, ONE source of truth for the guest Nix config.
# The inner `local://` URI's `?` and `&` are percent-encoded (`%3F`,
# `%26`) because they ride INSIDE the outer query of the
# `local-overlay://` URI (Nix's URL parser pct-decodes nested queries —
# verified against the pinned Nix 2.34.8 sources, and exercised by this
# exact spelling in the argv builder's tests).
#
# Layout constants — MUST match the argv builder
# (mysbx-rs/src/podman_gvisor.rs, the krun-nix block):
#   lower store dir   /nix/store-lower   (ro bind of the host /nix/store)
#   lower db/state    /nix/var-lower/nix (ro bind of the host /nix/var/nix)
#   upper layer       /nix/upper
#   overlay workdir   /nix/work
#   overlay state     /nix/state
overlay_store_uri() {
    printf 'local-overlay://?lower-store=local://%%3Freal=/nix/store-lower%%26state=/nix/var-lower/nix%%26read-only=true&upper-layer=/nix/upper&state=/nix/state'
}

setup_nix() {
    # The argv's infrastructure mounts must be in place: the ro binds
    # and the /nix tmpfs that carries the overlay's writable surfaces.
    grep -q ' /nix/store-lower ' /proc/mounts \
        || die "/nix/store-lower is not mounted — the host store bind is missing"
    grep -q ' /nix/var-lower/nix ' /proc/mounts \
        || die "/nix/var-lower/nix is not mounted — the host Nix database bind is missing"
    grep -q ' /nix ' /proc/mounts \
        || die "/nix is not a tmpfs — the overlay upper layer would not be writable"

    mkdir -p /nix/upper /nix/work /nix/state /nix/store \
        || die "cannot create the overlay layer directories on /nix"

    mount -t overlay overlay \
        -o lowerdir=/nix/store-lower,upperdir=/nix/upper,workdir=/nix/work \
        /nix/store \
        || die "the overlayfs mount over the host store failed (lowerdir=/nix/store-lower,upperdir=/nix/upper)"

    # The guest Nix configuration, in one inline file (NIX_CONFIG): the
    # experimental features the layered store needs, an empty build
    # users group (the guest has no nixbld users — builds run as guest
    # root in user-namespace sandboxes, CONFIG_USER_NS=y), and the
    # overlay store itself.
    export NIX_CONFIG="experimental-features = nix-command flakes local-overlay-store read-only-local-store
build-users-group =
require-drop-supplementary-groups = false
store = $(overlay_store_uri)"

    log "Nix store ready: host /nix/store as the read-only lower layer, upper on the /nix tmpfs"
    log "  (upper layer and database are per-run — nothing survives the VM)"
}

if [ -n "${MYSBX_KRUN_NIX-}" ]; then
    setup_nix
fi

# The payload. The argv always names one after this script; the guard
# only keeps a bare invocation from silently doing nothing.
if [ "$#" -eq 0 ]; then
    die "no payload — this script wraps a payload, it is not one"
fi
exec "$@"
