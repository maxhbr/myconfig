#!/usr/bin/env bash
# microvm-qemu-live-validation — the qemu twin of
# krun-live-validation.sh (bd myconfig-6x9c, the scripted half of bd
# myconfig-26z4's runbook, docs/design/backends.md D9).
#
# Run ON a host with rw /dev/kvm from the repo checkout:
#
#   MYSBX=<path-to-mysbx> ./nix/microvm-qemu-live-validation.sh <repo>
#
# Each probe prints PASS/FAIL and names the bd decision it validates;
# the first FAIL exits non-zero. Plain bash + the sandbox itself, no
# nix needed beyond the wrapped mysbx on PATH (the rebuilt switch is
# a precondition — the wrapper must pin
# MYSBX_MICROVM_QEMU_{LAUNCHER,ROOTFS,KERNEL,QEMU,VIRTIOFSD}).
#
# The probes mirror the krun runbook's 1-7 plus the qemu-only
# findings of the external review:
#
#   1 boot                — a one-shot true boots the VM (bd 2l9b)
#   2 guest kernel        — uname -r is the PINNED kernel, not the host's
#   3 exit code           — sh -c "exit 7" surfaces as 7 (bd hq7k: the
#                           argv rides the manifest; a lossy kernel
#                           split would exit 0)
#   4 console             — the serial console carries the boot log
#                           (bd mbib: -serial mon:stdio + console=ttyS0)
#   5 live-repo edit      — the workspace bind reaches the host tree
#   6 ro rootfs           — a write to / is refused
#   7 network control     — default network reaches cache.nixos.org
#                           through the SLiRP NIC (bd k106)
#   8 network=false       — both URLs denied (no netdev at all)
#   9 dry-run surface     — the launcher argv names the qemu pins
#                           (--kernel, --exit-channel; NO --krun-overlay,
#                           bd govd)
set -euo pipefail

repo=${1:?usage: microvm-qemu-live-validation.sh <repo>}
mysbx=${MYSBX:-mysbx}

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    printf '  see docs/design/backends.md D9 (backends.md runbook)\n' >&2
    exit 1
}

pass() {
    printf 'PASS %s\n' "$1"
}

# The sandbox form every probe uses: a one-shot run in the repo.
run() {
    (cd "$repo" && "$mysbx" run --backend microvm-qemu -- "$@")
}

cd "$repo"

# 1. boot: a one-shot true — the launcher spawns the virtiofsd
# daemons inside the bwrap chain (bd ya1o's /dev/null + tmpfs /tmp),
# qemu boots the pinned kernel over the virtiofs root (bd 7scm: no
# rootwait hang), the shared init runs the payload from the
# manifest's argv records.
run true
pass "1 boot: a one-shot true"

# 2. guest kernel proof: uname -r inside the VM is the PINNED nixpkgs
# kernel (the microvm-qemu.kernel build), never the host's.
host_kernel=$(uname -r)
guest_kernel=$(run uname -r | tr -d '\r\n')
if [ "$guest_kernel" = "$host_kernel" ]; then
    fail "2 guest kernel" "uname -r inside the sandbox equals the host's ($host_kernel) — the VM did not boot a guest kernel"
fi
pass "2 guest kernel: $guest_kernel (host: $host_kernel)"

# 3. exit code propagation through the manifest argv + the
# virtio-serial channel (bd hq7k + dnmk): sh -c "exit 7" must
# surface as 7 — the lossy kernel cmdline split of the review would
# have run `sh -c exit 7` (a new command 7) and exited 0, and the
# unresolved exit port would have swallowed every code into 125.
run sh -c 'exit 7' && rc=0 || rc=$?
[ "$rc" -eq 7 ] || fail "3 exit code propagation" "payload exit 7 arrived as $rc"
pass "3 exit code propagation: 7 (manifest argv + exit channel)"

# 4. console (bd mbib): the verbose run's boot log reaches the
# caller's stdout — -nodefaults killed the default serial backend,
# so the argv wires -serial mon:stdio and the kernel cmdline names
# console=ttyS0. The kernel's own log line proves the serial path
# end to end.
console_log=$(cd "$repo" && "$mysbx" run --backend microvm-qemu --verbose -- true 2>&1 || true)
printf '%s\n' "$console_log" | grep -qi 'Linux version' ||
    fail "4 console" "the verbose run carries no kernel log line — the serial console is mute"
pass "4 console: the kernel log reaches the caller's stdio"

# 5. live-repo edit: a write through the workspace bind reaches the
# host tree (the staged share model, bd govd's stage slots).
marker=$(mktemp -u microvm-qemu-marker.XXXXXX)
run touch "$repo/$marker"
[ -e "$repo/$marker" ] || fail "5 live-repo edit" "the sandbox write did not reach the host repo tree"
rm -f "$repo/$marker"
pass "5 live-repo edit through the workspace bind"

# 6. ro rootfs: / is not writable (virtiofsd --readonly on the root
# device — the SERVER enforces the mode).
if run sh -c ': > /PROBE' 2>/dev/null; then
    fail "6 ro rootfs" "a write to / succeeded — the read-only contract is broken"
fi
pass "6 ro rootfs: / is not writable"

# 7. network control (bd k106): the default network reaches the
# internet through the SLiRP NIC the init configures statically
# (10.0.2.15/24, default route 10.0.2.2) — the positive control
# first, so a host without egress does not pass as enforcement.
curl_rc() {
    local dir=$1 url=$2
    # shellcheck disable=SC2016 # $1/$? expand inside the sandbox
    (cd "$dir" && "$mysbx" run --backend microvm-qemu -- sh -c \
        'curl -sS --max-time 10 -o /dev/null "$1" 2>/dev/null; echo "curl-rc=$?"' \
        sh "$url") | sed -n 's/^curl-rc=\([0-9]*\).*/\1/p' | tail -n 1
}
rc=$(curl_rc "$repo" https://cache.nixos.org/nix-cache-info || true)
[ -n "$rc" ] || fail "7 network control" "the default-network run did not start"
[ "$rc" -eq 0 ] ||
    fail "7 network control" "the default-network run has no egress (curl exit $rc) — the SLiRP NIC step or the denial probe proves nothing"
pass "7 network control: SLiRP NIC reaches cache.nixos.org"

# 8. network=false: no netdev at all (the launcher's --network none)
# plus the bwrap --unshare-net around the chain — both URLs must be
# denied, from a scratch repo whose sidecar sets network = false so
# the repo's own configuration is not touched.
cache=${XDG_CACHE_HOME:-$HOME/.cache}
mkdir -p "$cache"
scratch=$(mktemp -d "$cache/mysbx-microvm-qemu-net-probe.XXXXXX")
trap 'rm -rf "$scratch"' EXIT
netrepo="$scratch/net"
git init -q "$netrepo"
git -C "$netrepo" -c user.name=probe -c user.email=probe@invalid \
    -c commit.gpgsign=false commit -q --no-verify --allow-empty -m probe
(cd "$netrepo" && "$mysbx" init >/dev/null)
printf 'backend = "microvm-qemu"\nnetwork = false\n' >"$netrepo.mysbx/config.toml"
for url in https://cache.nixos.org/nix-cache-info http://1.1.1.1/; do
    rc=$(curl_rc "$netrepo" "$url" || true)
    [ -n "$rc" ] || fail "8 network=false" "the network=false run did not start ($url)"
    [ "$rc" -ne 0 ] || fail "8 network=false" "$url was reachable with network = false"
    pass "8 network=false: $url denied (curl exit $rc)"
done

# 9. the dry-run audit surface: the launcher argv names the qemu
# pins, and NO --krun-overlay rides (bd govd — a builder that
# emits one makes every real run exit 125 before the boot).
dry=$("$mysbx" run --backend microvm-qemu --dry-run -- true)
printf '%s\n' "$dry" | grep -q -- '--kernel' ||
    fail "9 dry-run surface" "the dry run carries no --kernel"
printf '%s\n' "$dry" | grep -q -- '--exit-channel' ||
    fail "9 dry-run surface" "the dry run carries no --exit-channel"
printf '%s\n' "$dry" | grep -q -- '--rootfs' ||
    fail "9 dry-run surface" "the dry run carries no --rootfs"
if printf '%s\n' "$dry" | grep -q -- '--krun-overlay'; then
    fail "9 dry-run surface" "the dry run carries a --krun-overlay flag — the launcher refuses it (bd govd)"
fi
pass "9 dry-run surface: kernel + exit-channel pinned, no overlay flags"

# 10. the guest bootstrap (bd myconfig-af0j): the PID-1 init must
# mount devpts (tmux/mux sessions allocate PTYs in the guest), a
# tmpfs /dev/shm, link /dev/fd, and bring up lo (the baked
# /etc/hosts localhost mapping is dialable). Each probe is one
# one-shot run inside the sandbox.
run sh -c '[ -d /dev/pts ] && [ -c /dev/ptmx ]' ||
    fail "10 guest bootstrap" "no devpts mount — tmux cannot allocate a PTY"
run test -d /dev/shm ||
    fail "10 guest bootstrap" "no /dev/shm tmpfs"
run test -e /dev/fd/0 ||
    fail "10 guest bootstrap" "no /dev/fd link"
run sh -c 'ip link show lo | grep -q "state UP"' ||
    fail "10 guest bootstrap" "lo is not up — 127.0.0.1 is not bindable"
pass "10 guest bootstrap: devpts + shm + fd + lo up"

printf 'microvm-qemu live validation: all probes passed\n'
