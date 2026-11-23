#!/usr/bin/env bash
# microvm-ch-live-validation — the cloud-hypervisor twin of
# microvm-qemu-live-validation.sh (bd myconfig-9wkp.9, the scripted
# half of the D11 runbook).
#
# Run ON a host with rw /dev/kvm from the repo checkout:
#
#   MYSBX=<path-to-mysbx> ./nix/microvm-ch-live-validation.sh <repo>
#
# Each probe prints PASS/FAIL and names the D11 finding it
# validates; the first FAIL exits non-zero.
#
#   1 boot                — a one-shot true boots the VM (the shared
#                           guest side, bd myconfig-9wkp.3)
#   2 guest kernel        — uname -r is the PINNED kernel, not the host's
#   3 exit code           — sh -c "exit 7" surfaces as 7 (the manifest
#                           argv + the 16550 serial channel, bd 9wkp.4)
#   4 console             — the virtio console carries the boot log
#                           (--console tty + console=hvc0)
#   5 live-repo edit      — the workspace bind reaches the host tree
#   6 ro rootfs           — a write to / is refused
#   7 network shared      — the passt NIC reaches outside (bd 9wkp.5)
#   9 dry-run surface     — the launcher argv names the CH pins
#                           (--kernel, --exit-channel; NO --krun-overlay)
#  10 guest bootstrap     — devpts, /dev/shm, /dev/fd, lo up
#  11 nested=off          — the default CPU carries no vmx/svm
#                           (D27 on CH: the launcher FORCES nested=off —
#                           CH defaults it ON, the exact inverse of qemu)
set -euo pipefail

repo=${1:?usage: microvm-ch-live-validation.sh <repo>}
mysbx=${MYSBX:-mysbx}

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    printf '  see docs/design/backends.md D11 (backends.md runbook)\n' >&2
    exit 1
}

pass() {
    printf 'PASS %s\n' "$1"
}

run() {
    (cd "$repo" && "$mysbx" run --backend microvm-ch -- "$@")
}

cd "$repo"

# 1. boot: a one-shot true — the launcher spawns the virtiofsd +
# passt daemons inside the bwrap chain, CH boots the pinned kernel
# over the virtiofs root, the shared init runs the payload from the
# manifest's argv records.
run true
pass "1 boot: a one-shot true"

# The dry-run argv every later probe audits (9) and probe 2 reads
# the --kernel pin from — captured once, before the first use.
dry=$("$mysbx" run --backend microvm-ch --dry-run -- true)

# 2. guest kernel proof: uname -r inside the VM is the PINNED nixpkgs
# kernel, never the host's. The version rides a marker-delimited
# payload line: --console tty makes the caller's stdout the guest
# console, so a bare `run uname -r` captures the kernel boot log —
# contamination the marker avoids.
dry_kernel=$(printf '%s\n' "$dry" | grep -A1 -- '^--kernel$' | tail -n 1 | tr -d '\r')
case $dry_kernel in
    /nix/store/*) : ;;
    *) fail "2 guest kernel" "the dry run carries no --kernel path" ;;
esac
expected_kernel=$(basename "$(dirname "$dry_kernel")")
case $expected_kernel in
    *-linux-*) expected_kernel=${expected_kernel#*-linux-} ;;
    *) fail "2 guest kernel" "the pinned kernel path is not a store kernel: $dry_kernel" ;;
esac
guest_kernel=$(run sh -c 'echo "KVM-START$(uname -r)KVM-END"' |
    sed -n 's/.*KVM-START\(.*\)KVM-END.*/\1/p' | tail -n 1)
[ -n "$guest_kernel" ] ||
    fail "2 guest kernel" "no KVM-START..KVM-END marker reached the console — the payload never ran"
[ "$guest_kernel" = "$expected_kernel" ] ||
    fail "2 guest kernel" "uname -r inside the sandbox is '$guest_kernel', not the pinned $expected_kernel ($dry_kernel)"
pass "2 guest kernel: $guest_kernel (the pinned $dry_kernel)"

# 3. exit code propagation through the manifest argv + the 16550
# serial channel (bd 9wkp.4): sh -c "exit 7" must surface as 7.
run sh -c 'exit 7' && rc=0 || rc=$?
[ "$rc" -eq 7 ] || fail "3 exit code propagation" "payload exit 7 arrived as $rc"
pass "3 exit code propagation: 7 (manifest argv + the 16550 exit channel)"
# a signal death propagates too: killed -9 → 137
run sh -c 'kill -9 0' && rc=0 || rc=$?
[ "$rc" -eq 137 ] || fail "3 exit code propagation (signal)" "a SIGKILL death arrived as $rc, not 137"
pass "3b signal death: 137"

# 4. console (D11): the verbose run's boot log reaches the caller's
# stdout — --console tty wires the virtio console to CH's stdio and
# the kernel cmdline names console=hvc0.
console_log=$(cd "$repo" && { "$mysbx" run --backend microvm-ch --verbose -- true 2>&1 || true; })
# CH's virtio console carries the kernel log from the driver's
# bring-up onward (no early "Linux version" banner — the qemu arm's
# 16550 carries the whole log; the virtio console starts when the
# hvc driver registers). Any kernel log line proves the path.
printf '%s\n' "$console_log" | grep -qE '\[ *[0-9]+\.[0-9]+\]' ||
    fail "4 console" "the verbose run carries no kernel log line — the virtio console is mute"
pass "4 console: the kernel log reaches the caller's stdio"

# 5. live-repo edit: a write through the workspace bind reaches the
# host tree (the staged share model).
marker=$(mktemp -u microvm-ch-marker.XXXXXX)
run touch "$repo/$marker"
[ -e "$repo/$marker" ] || fail "5 live-repo edit" "the sandbox write did not reach the host repo tree"
rm -f "$repo/$marker"
pass "5 live-repo edit through the workspace bind"

# 6. ro rootfs: / is not writable (virtiofsd --readonly on the root
# device — the SERVER enforces the mode). The guest decides, so a
# failed VM run fails the probe instead of passing it.
run sh -c 'if : >/PROBE 2>/dev/null; then exit 1; fi' ||
    fail "6 ro rootfs" "a write to / succeeded (or the run failed) — the read-only contract is broken"
pass "6 ro rootfs"

# 7. network shared (bd 9wkp.5): the passt NIC exists, carries the
# pinned MAC, and the static SLiRP plan is applied — the same plan
# the guest init's MYSBX_MICROVM_NET branch configures. EGRESS
# through passt is host-conditional (a host interface inside
# 10.0.2.0/24 collides with passt's SNAT — the spike's documented
# artifact; this validation host IS one), so the TCP probe runs
# ONLY when the host's own eth0 is outside the guest plan.
host_no_collision() {
    # this validation host may itself be a SLiRP guest (the spike's
    # nested-sandbox case): any host address inside 10.0.2.0/24 in
    # /proc/net/fib_trie is a collision
    grep -qE "32 host 10\.0\.2\.[0-9]+|-- 10\.0\.2\." /proc/net/fib_trie 2>/dev/null && return 1
    return 0
}
# /bin/busybox carries the ip/route applets (the guest PATH's
# tools tree is not pinned in this minimal validation run, so the
# applets are spelled through busybox explicitly)
# shellcheck disable=SC2016 # $BB expands in the guest shell
run /bin/busybox sh -c 'BB=/bin/busybox;
    $BB ip -o link | grep -q "link/ether 9a:55:9a:55:9a:55" &&
    $BB ip -o addr | grep -qw "10.0.2.15" &&
    $BB ip route | grep -qw "via 10.0.2.2"' ||
    fail "7 network shared" "the passt NIC is missing, or the static SLiRP plan is not applied"
pass "7 network shared: NIC up, the SLiRP plan applied (pinned MAC, 10.0.2.15/24, gw 10.0.2.2)"
if host_no_collision; then
    run /bin/busybox sh -c 'printf "nameserver 10.0.2.3\n" > /etc/resolv.conf; wget -q -O /dev/null -T 10 https://cache.nixos.org/' ||
        fail "7 network shared (egress)" "the guest cannot reach cache.nixos.org through the passt NIC"
    pass "7b egress: cache.nixos.org through passt"
else
    printf 'SKIP 7b egress — this host interface sits inside 10.0.2.0/24 (the spike SNAT collision; live acceptance on a clean host)\n'
fi

# 9. dry-run surface (the qemu suite's numbering; its 8 is
# network=false): the launcher argv — the CH pins (--kernel,
# --exit-channel), NO --krun-overlay (the launcher refuses it).
printf '%s\n' "$dry" | grep -qx -- '--kernel' ||
    fail "9 dry-run surface" "the dry run does not name --kernel"
printf '%s\n' "$dry" | grep -qx -- '--exit-channel' ||
    fail "9 dry-run surface" "the dry run does not name --exit-channel"
if printf '%s\n' "$dry" | grep -q -- '--krun-overlay'; then
    fail "9 dry-run surface" "the dry run carries a --krun-overlay flag"
fi
if ! printf '%s\n' "$dry" | grep -qx -- '/bin/mysbx-init'; then
    fail "9 dry-run surface" "the dry run does not pin the guest init"
fi
printf '%s\n' "$dry" | grep -q -- '--manifest' ||
    fail "9 dry-run surface" "the dry run does not carry the manifest pointer"
pass "9 dry-run surface: the CH pins, no overlay flags"

# 10. guest bootstrap (bd myconfig-af0j): devpts, /dev/shm, /dev/fd,
# lo up — the shared init's PID-1 bootstrap, backend-agnostic.
# /proc/mounts and the busybox applets — the guest PATH carries no
# standalone tools in this minimal run (they ride the store share)
# shellcheck disable=SC2016 # $BB expands in the guest shell
run /bin/busybox sh -c 'BB=/bin/busybox;
    grep -q devpts /proc/mounts &&
    [ -d /dev/shm ] && [ -e /dev/fd ] &&
    $BB ip link show lo | grep -qE "state UNKNOWN|UP"' ||
    fail "10 guest bootstrap" "devpts//dev/shm//dev/fd/lo missing in the guest"
pass "10 guest bootstrap: the shared PID-1 bootstrap"

# 11. nested=off (D27 on CH): the default CPU carries NO vmx/svm —
# CH defaults nested ON, so this probe pins the launcher's forced
# inversion. A nested-virt run MUST carry vmx (the nested half of
# the contract is the qemu runbook's, live-accepted there). The
# guest decides, so a failed VM run fails the probe.
run sh -c '! grep -qwE "vmx|svm" /proc/cpuinfo' ||
    fail "11 nested=off" "the DEFAULT run exposes VMX/SVM (or the run failed) — the launcher's forced nested=off is broken (D27)"
pass "11 nested=off: the default CPU carries no vmx/svm (D27's inversion)"

echo
echo "=== microvm-ch live validation: all probes passed (docs/design/backends.md D11) ==="
