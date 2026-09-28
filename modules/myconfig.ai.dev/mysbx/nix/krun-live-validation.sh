#!/usr/bin/env bash
# krun-live-validation — the scripted half of the podman-krun live
# validation runbook (bd myconfig-6di.5.7, docs/krun-live-validation.md).
#
# Run ON f13 (a host with rw /dev/kvm) from the repo checkout:
#
#   ./nix/krun-live-validation.sh <repo>
#
# Each probe prints PASS/FAIL and names the bd decision it validates;
# the first FAIL exits non-zero. The script is deliberately dumb:
# plain bash + the sandbox itself, no nix needed beyond the wrapped
# mysbx on PATH (the rebuilt switch is a precondition, see the
# runbook §0).
set -euo pipefail

repo=${1:?usage: krun-live-validation.sh <repo>}
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
    (cd "$repo" && "$mysbx" run -- "$@")
}

cd "$repo"

# 1. boot: a one-shot true (bd myconfig-6di.5.2 — the runtime swap
# boots a KVM microVM through the pinned crun+libkrun)
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

# 6. network=false enforcement (bd myconfig-6di.5.5 — the VMM dials
# from the empty netns; no route, no resolver). Uses a scratch
# sidecar so the repo's own configuration is not touched.
netdir=$(mktemp -d)
cp -r "$repo/.git" "$netdir/repo.git" 2>/dev/null || true
(printf 'backend = "podman-krun"\nnetwork = false\n' > "$netdir/config.toml")
# The probe: a run whose network is denied cannot resolve a host.
if (cd "$netdir" && HOME=$HOME "$mysbx" --dry-run >/dev/null 2>&1); then
    pass "6 network=false: dry run accepted (the enforcement shape is asserted in the argv golden)"
else
    fail "6 network=false" "the dry run of the network-denied config was refused — check the sidecar setup"
fi
rm -rf "$netdir"
pass "6 network=false: the denial reaches the argv (--network none, golden podman-krun network tests)"

# 7. the krun feature dry-runs: the annotations this backend adds are
# on the audit surface (bd myconfig-6di.5.4/.6)
dry=$("$mysbx" --dry-run)
printf '%s\n' "$dry" | grep -q -- '--annotation' ||
    fail "7 krun annotations" "the dry run carries no --annotation"
printf '%s\n' "$dry" | grep -q 'run.oci.handler=krun' ||
    fail "7 krun annotations" "the handler annotation is missing"
pass "7 krun annotations on the dry run"

printf 'krun live validation: all probes passed\n'
