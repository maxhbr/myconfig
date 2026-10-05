#!/usr/bin/env bash
# krun-live-validation — the scripted half of the krun live
# validation runbooks (bd myconfig-6di.5.7 for the podman-krun
# variant, bd myconfig-dak.10's §4/§5 for the direct one,
# docs/krun-live-validation.md).
#
# Run ON a host with rw /dev/kvm from the repo checkout:
#
#   MYSBX=<path-to-mysbx> ./nix/krun-live-validation.sh <repo> [BACKEND]
#
# BACKEND selects the krun variant under test: "podman-krun" (the
# default, matching the runbook's D2 probes) or "krun" (the direct
# backend — probes 1-5 are shape-equivalent, probe 6's scratch
# sidecar then sets backend = "krun", and probe 7 audits the
# BACKEND'S OWN dry-run surface: the podman VM annotations for
# podman-krun, the launcher flags for krun).
#
# Each probe prints PASS/FAIL and names the bd decision it validates;
# the first FAIL exits non-zero. The script is deliberately dumb:
# plain bash + the sandbox itself, no nix needed beyond the wrapped
# mysbx on PATH (the rebuilt switch is a precondition, see the
# runbook §0).
set -euo pipefail

repo=${1:?usage: krun-live-validation.sh <repo> [podman-krun|krun]}
backend=${2:-podman-krun}
case "$backend" in
    podman-krun | krun) ;;
    *) echo "usage: krun-live-validation.sh <repo> [podman-krun|krun]" >&2; exit 2 ;;
esac
mysbx=${MYSBX:-mysbx}

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    printf '  see docs/krun-live-validation.md and docs/design/backends.md D2\n' >&2
    exit 1
}

pass() {
    printf 'PASS %s\n' "$1"
}

# The sandbox form every probe uses: a one-shot run in the repo.
run() {
    (cd "$repo" && "$mysbx" run --backend "$backend" -- "$@")
}

cd "$repo"

# 1. boot: a one-shot true (bd myconfig-6di.5.2 for podman-krun —
# the runtime swap boots a KVM microVM through the pinned
# crun+libkrun; bd myconfig-dak.1 for krun direct — the launcher
# boots the VM itself)
run true
pass "1 boot: a one-shot true"

# 2. guest kernel proof: uname -r is the libkrunfw kernel, not the
# host's (backends.md D2: a real guest kernel is the point of the
# variant)
host_kernel=$(uname -r)
guest_kernel=$(run uname -r | tr -d '\r\n')
if [ "$guest_kernel" = "$host_kernel" ]; then
    fail "2 guest kernel" "uname -r inside the sandbox equals the host's ($host_kernel) — the VM did not boot a guest kernel"
fi
pass "2 guest kernel: $guest_kernel (host: $host_kernel)"

# 3. exit code propagation (the podman backend is exec'd with mysbx's
# own stdio; a swallowed code is an infrastructure lie)
run sh -c 'exit 42' && rc=0 || rc=$?
[ "$rc" -eq 42 ] || fail "3 exit code propagation" "payload exit 42 arrived as $rc"
pass "3 exit code propagation: 42"

# 4. live-repo edit: a write through the workspace bind reaches the
# host tree (bd myconfig-6di.5.4 — the shared mount model)
marker=$(mktemp -u krun-marker.XXXXXX)
run touch "/$marker" 2>/dev/null || run touch "$repo/$marker"
[ -e "$repo/$marker" ] || fail "4 live-repo edit" "the sandbox write did not reach the host repo tree"
rm -f "$repo/$marker"
pass "4 live-repo edit through the workspace bind"

# 5. ro rootfs: / is not writable (the --read-only contract survives
# the virtiofs sharing, bd myconfig-6di.5.4)
if run sh -c ': > /PROBE' 2>/dev/null; then
    fail "5 ro rootfs" "a write to / succeeded — the read-only contract is broken"
fi
pass "5 ro rootfs: / is not writable"

# 6. network=false enforcement (bd myconfig-6di.5.5 for podman-krun
# — the VMM dials from the empty netns; bd myconfig-dak.6 for krun
# — the vsock is DISABLED, no socket path at all). A positive control in
# the repo first, so a host without egress does not pass as
# enforcement; then a scratch repo whose sidecar sets network = false,
# so the repo's own configuration is not touched. Each payload prints
# its curl exit code: a run mysbx refused prints nothing, and that is
# a FAIL, not a denial.
curl_rc() {
    local dir=$1 url=$2
    # shellcheck disable=SC2016 # $1/$? expand inside the sandbox
    (cd "$dir" && "$mysbx" run -- sh -c \
        'curl -sS --max-time 10 -o /dev/null "$1" 2>/dev/null; echo "curl-rc=$?"' \
        sh "$url") | sed -n 's/^curl-rc=\([0-9]*\).*/\1/p' | tail -n 1
}
rc=$(curl_rc "$repo" https://cache.nixos.org/nix-cache-info || true)
[ -n "$rc" ] || fail "6 network control" "the default-network run did not start"
[ "$rc" -eq 0 ] ||
    fail "6 network control" "the default-network run has no egress (curl exit $rc) — the denial probe would prove nothing"
pass "6 network control: default network reaches cache.nixos.org"

# Off /tmp: the sandbox's own /tmp is a tmpfs, and a workspace below
# it would not look like any real repo.
cache=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache"
scratch=$(mktemp -d "$cache/mysbx-krun-net-probe.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
netrepo="$scratch/net"
git init -q "$netrepo"
git -C "$netrepo" -c user.name=probe -c user.email=probe@invalid \
    -c commit.gpgsign=false commit -q --no-verify --allow-empty -m probe
(cd "$netrepo" && "$mysbx" init >/dev/null)
# A plain repo needs no git-dir approvals, so the generated sidecar
# config can be replaced wholesale.
printf 'backend = "%s"\nnetwork = false\n' "$backend" >"$netrepo.mysbx/config.toml"
for url in https://cache.nixos.org/nix-cache-info http://1.1.1.1/; do
    rc=$(curl_rc "$netrepo" "$url" || true)
    [ -n "$rc" ] || fail "6 network=false" "the network=false run did not start ($url)"
    [ "$rc" -ne 0 ] || fail "6 network=false" "$url was reachable with network = false"
    pass "6 network=false: $url denied (curl exit $rc)"
done

# 7. the krun variant's dry-run audit surface: the flags that NAME
# the machinery under test (bd myconfig-6di.5.4/.6 for podman-krun
# — the OCI annotations crun turns into VM behavior; bd
# myconfig-dak.3/dak.10 for krun — the launcher flags the golden
# tests pin).
dry=$("$mysbx" run --backend "$backend" --dry-run -- true)
case "$backend" in
    podman-krun)
        printf '%s\n' "$dry" | grep -q -- '--annotation' ||
            fail "7 krun $backend annotations" "the dry run carries no --annotation"
        printf '%s\n' "$dry" | grep -q 'run.oci.handler=krun' ||
            fail "7 krun $backend annotations" "the handler annotation is missing"
        printf '%s\n' "$dry" | grep -q 'run.oci.keep_original_groups' ||
            printf 'INFO 7 the keep_original_groups annotation is absent (keep-id semantics changed since bd myconfig-6di.5.4)\n'
        pass "7 krun annotations on the dry run: run.oci.handler=krun"
        ;;
    krun)
        # The launcher flags: VM size first (the config limits DO
        # apply to the VM, bd myconfig-6di.5.6's direct twin), the
        # rootfs + init pins, the staged devices of the shares.
        printf '%s\n' "$dry" | grep -q -- '--cpus' ||
            fail "7 krun flags" "the dry run carries no --cpus"
        printf '%s\n' "$dry" | grep -q -- '--ram' ||
            fail "7 krun flags" "the dry run carries no --ram"
        printf '%s\n' "$dry" | grep -q -- '--rootfs' ||
            fail "7 krun flags" "the dry run carries no --rootfs"
        printf '%s\n' "$dry" | grep -q -- '--ro-device' ||
            fail "7 krun flags" "the dry run carries no --ro-device (the staged share tree)"
        printf '%s\n' "$dry" | grep -q -- '--ro-share' ||
            fail "7 krun flags" "the dry run carries no --ro-share (the store share)"
        # network = shared is the launcher default: --network rides
        # ONLY on None runs — the same honesty on both variants (bd
        # myconfig-dak.6).
        if printf '%s\n' "$dry" | grep -q -- '--network none'; then
            printf 'INFO 7 the dry run carries --network none (a network=false layer?)\n'
        fi
        pass "7 krun flags on the dry run: launcher argv surface (cpus/ram/rootfs/shares)"
        ;;
esac


printf 'krun live validation (%s): all probes passed\n' "$backend"
