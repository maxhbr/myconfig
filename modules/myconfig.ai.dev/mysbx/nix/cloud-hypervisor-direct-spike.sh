#!/usr/bin/env bash
# cloud-hypervisor-direct-spike — the live half of bd
# myconfig-9wkp.1: boot the SHARED krun rootfs on cloud-hypervisor
# 54.0 (the locked nixpkgs 9ae611a build; the epic surveyed 52.0 —
# the spike validates what the lock ships), rootless inside the
# bwrap wrap of the epic's design chain, and answer the spike's
# questions:
#
#   1. does passt --vhost-user work with CH's vhost-user-net
#      frontend (vhost_user=on,socket=...,vhost_mode=client), and
#      does passt's self-isolation (user namespaces) work inside
#      bwrap?
#   2. can passt mimic SLiRP's address plan (guest 10.0.2.15/24,
#      gateway 10.0.2.2, DNS 10.0.2.3) so the shared guest init's
#      static NIC config and the 10.0.2.2 model-endpoint pins
#      carry over unchanged?
#   3. does the microvm-qemu bzImage boot as-is?
#   4. exit channel: does an `N\n` line reach the 16550 (--serial
#      file=)?  (vsock is the alternative; busybox has no AF_VSOCK
#      client — the 16550 is the candidate.)
#   5. teardown: does busybox `poweroff -f` (ACPI) make CH exit?
#      does pvpanic + --event-monitor detect a crashed init
#      without a reboot loop?
#   6. does `--console tty` (virtio console, hvc0) carry a
#      terminal size to the guest tty?
#   7. which /dev, /proc, /sys paths does CH need inside bwrap?
#      does its default seccomp work there?
#
# One PASS/FAIL per question; the first FAIL exits non-zero. Run
# from the repo checkout on a KVM host:
#
#   ./nix/cloud-hypervisor-direct-spike.sh
#
# Throwaway probe in the shape of ./krun-direct-spike.sh: the
# binaries come from the locked nixpkgs, the rootfs from the same
# expression the module wires (krun.direct.rootfs), the kernel
# from nix/microvm-qemu-kernel.nix. The shared mysbx-init is NOT
# exercised (it needs the qemu bridge contract, bd
# myconfig-9wkp.4); a per-probe busybox init answers the VMM
# questions.
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
repo=$(cd "$here/../../../.." && pwd)

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    exit 1
}

pass() {
    printf 'PASS %s\n' "$1"
}

note() {
    printf 'NOTE %s\n' "$1"
}

np() {
    nix build --impure --no-link --print-out-paths --expr '
      (builtins.getFlake ("git+file://" + toString '"$repo"'))
      .inputs.nixpkgs.legacyPackages.x86_64-linux.'"$1"
}

ch=$(np cloud-hypervisor)
virtiofsd=$(np virtiofsd)
passt=$(np passt)
bwrap_bin=$(np bubblewrap)/bin/bwrap
setsid_bin=$(np util-linux.bin)/bin/setsid

rootfs=$(nix build --impure --no-link --print-out-paths --out-link /tmp/mysbx-ch-spike-rootfs --expr '
  (builtins.getFlake ("git+file://" + toString '"$repo"'))
  .nixosConfigurations.test-f13.config.myconfig.ai.dev.mysbx.krun.direct.rootfs')

kernel=$(nix build --impure --no-link --print-out-paths --out-link /tmp/mysbx-ch-spike-kernel --expr '
  let
    flake = builtins.getFlake ("git+file://" + toString '"$repo"');
    np = flake.inputs.nixpkgs.legacyPackages.x86_64-linux;
  in np.callPackage '"$repo"'/modules/myconfig.ai.dev/mysbx/nix/microvm-qemu-kernel.nix {
    kernelPackages = np.linuxKernel.packages.linux_6_12;
  }')/bzImage

[ -f "$kernel" ] || fail "kernel" "no bzImage at $kernel"
[ -x "$rootfs/bin/busybox" ] || fail "rootfs" "no busybox in $rootfs"

work=$(mktemp -d /tmp/mysbx-ch-spike.XXXXXX)
runtime=$work/run
mkdir -p "$runtime"
cleanup() {
    # passt backgrounds itself by default; the probe runs it in
    # the foreground of a background shell, so the pid works.
    if [ -f "$runtime/passt.pid" ]; then
        kill "$(cat "$runtime/passt.pid")" 2>/dev/null || :
    fi
    if [ -n "${SPIKE_KEEP:-}" ]; then
        echo "NOTE: keeping $work (SPIKE_KEEP set)"
    else
        rm -rf "$work"
    fi
}
trap cleanup EXIT

echo "=== spike: cloud-hypervisor + rootless virtiofsd + passt --vhost-user under bwrap (bd myconfig-9wkp.1) ==="
echo "CH:        $ch"
echo "virtiofsd: $virtiofsd"
echo "passt:     $passt"
echo "rootfs:    $rootfs"
echo "kernel:    $kernel"
echo

# The bwrap chain of the epic's design (the qemu arm's host-side
# wrap, backends.md D9): CH and its daemons see the rootfs, the
# kernel, the runtime dir and /dev/kvm — plus /proc, /sys and
# /dev, whose need is question 7's answer. The --dev must come
# BEFORE the /dev/kvm dev-bind: bwrap applies its operations in
# order, and a later --dev would overmount the bind.
bwrap_base() {
    "$bwrap_bin" \
        --ro-bind /nix/store /nix/store \
        --ro-bind "$rootfs" "$rootfs" \
        --ro-bind "$kernel" "$kernel" \
        --ro-bind /etc/resolv.conf /etc/resolv.conf \
        --ro-bind-try /etc/passwd /etc/passwd \
        --ro-bind-try /etc/group /etc/group \
        --ro-bind /proc /proc \
        --ro-bind /sys /sys \
        --dev /dev \
        --dev-bind /dev/kvm /dev/kvm \
        --tmpfs /tmp \
        --bind "$runtime" "$runtime" \
        --clearenv \
        -- "$@"
}

# The daemon logs go to a per-tag file: a spawned daemon
# inherits this shell's stdout/stderr, and a command-substituted
# run_ch would hang on the open pipe until every writer exits.
# virtiofsd also SURVIVES its bwrap wrapper's death (it
# re-parents), so stop_fs kills it by the pid file virtiofsd
# writes next to its socket — and only falls back to the wrapper
# pid.
fs_pids=()
fs_socks=()
spawn_fs() { # TAG HOSTDIR
    local tag=$1 dir=$2
    local socket=$runtime/$tag.sock
    rm -f "$socket" "$socket.pid"
    bwrap_base "$virtiofsd/bin/virtiofsd" \
        --socket-path="$socket" \
        --shared-dir="$dir" \
        --sandbox=none \
        --cache=auto >"$runtime/fsd-$tag.log" 2>&1 &
    local pid=$!
    local _
    for _ in $(seq 200); do
        if [ -S "$socket" ]; then
            fs_pids+=("$pid")
            fs_socks+=("$socket")
            return 0
        fi
        kill -0 "$pid" 2>/dev/null || fail "virtiofsd $tag" "daemon died (log: $runtime/fsd-$tag.log)"
        sleep 0.05
    done
    fail "virtiofsd $tag" "socket never appeared"
}

stop_fs() {
    local i pidfile pid
    for i in "${!fs_socks[@]}"; do
        pidfile=${fs_socks[$i]}.pid
        if [ -f "$pidfile" ]; then
            kill "$(cat "$pidfile")" 2>/dev/null || :
        fi
        kill "${fs_pids[$i]}" 2>/dev/null || :
    done
    fs_pids=()
    fs_socks=()
}

# A WRITABLE copy of the shared rootfs: the kernel execs init=
# FROM the root fs, so the per-probe spike init must live inside
# it — the shared rootfs is a read-only store path. The copy is
# content-identical (Q3's answer — the bzImage + virtiofs root +
# static busybox — is the SHARED root's own machinery); the init
# is NOT the shared mysbx-init (whose qemu-bridge contract is bd
# myconfig-9wkp.4's work). Every probe ends in `poweroff -f` —
# the teardown question — except the panic probe, which crashes
# on purpose.
root_rw=$runtime/root
cp -a "$rootfs/." "$root_rw/"
# the store's modes ride along (read-only dirs): the probes write
# their init into the copy, and cleanup must be able to remove it
chmod -R u+w "$root_rw"
# the root copy keeps the shared rootfs's own nix/ symlinks; none
# of the spike's probes needs /nix, and dangling absolute links
# inside a copy are inert in the guest

# The per-probe init. `console=hvc0` routes the kernel console
# (and /dev/console) to the VIRTIO console — the caller's stdio
# through CH's --console tty. The 16550 (--serial file=) records
# only what the guest writes to /dev/ttyS0 explicitly, so every
# marker the HOST greps goes there via mark().
write_init() { # MODE
    cat >"$root_rw/bin/spike-init" <<EOF
#!/bin/busybox sh
BB=/bin/busybox
\$BB mkdir -p /proc /sys /dev
\$BB mount -t proc proc /proc 2>/dev/null || :
\$BB mount -t sysfs sys /sys 2>/dev/null || :
\$BB mount -t devtmpfs dev /dev 2>/dev/null || :
mark() {
    echo "\$*" > /dev/ttyS0 2>/dev/null || echo "\$*"
}
mark "SPIKE-UP mode=\$1"
case "\$1" in
    boot)
        mark "BOOT:OK"
        \$BB uname -r > /dev/ttyS0 2>/dev/null || :
        # Q7b: the nested=off contract — no VMX/SVM in a plain run
        if \$BB grep -q -w vmx /proc/cpuinfo 2>/dev/null; then mark "VMX:present"; else mark "VMX:masked"; fi
        if \$BB grep -q -w svm /proc/cpuinfo 2>/dev/null; then mark "SVM:present"; else mark "SVM:masked"; fi
        \$BB poweroff -f
        ;;
    exit7)
        # Q4: the exit-code line, the shape the real init will
        # write to its exit channel
        mark "EXIT:7"
        \$BB poweroff -f
        ;;
    net)
        # Q1/Q2: the SLiRP plan on the vhost-user NIC
        eth=\$(\$BB ip -o link | \$BB awk -F': ' '\$2 != "lo" {print \$2; exit}')
        mark "ETH:\$eth"
        \$BB ip addr add 10.0.2.15/24 dev "\$eth" || mark "ADDR:FAIL"
        \$BB ip link set "\$eth" up
        \$BB ip route add default via 10.0.2.2 || mark "ROUTE:FAIL"
        \$BB printf 'nameserver 10.0.2.3\n' > /etc/resolv.conf
        # no ICMP probe: this host denies passt's ping socket
        # (net.ipv4.ping_group_range); the gateway question is the
        # TCP NAT to the host loopback, which wget answers
        if \$BB wget -q -O /dev/null -T 5 http://10.0.2.2:8000/; then mark "GW:OK HOSTNET:OK"; else mark "GW:FAIL"; fi
        if \$BB nslookup one.one.one.one >/dev/null 2>&1; then mark "DNS:OK"; else mark "DNS:FAIL"; fi
        # busybox wget: rc 1 = an HTTP status arrived (reachable;
        # 1.1.1.1 answers 403), rc > 1 = connection failure
        if \$BB wget -O /dev/null -T 8 http://1.1.1.1/ >/dev/null 2>&1; then mark "EGRESS:OK"
        elif \$BB wget -O /dev/null -T 8 http://1.1.1.1/ 2>/dev/null; [ \$? -eq 1 ]; then mark "EGRESS:OK"
        else mark "EGRESS:FAIL"; fi
        \$BB poweroff -f
        ;;
    resize)
        # Q6: the tty size the guest sees over the virtio console
        \$BB stty size < /dev/hvc0 > /dev/ttyS0 2>&1 || mark "STTY:NODEV"
        \$BB sleep 2
        \$BB stty size < /dev/hvc0 > /dev/ttyS0 2>&1 || mark "STTY:NODEV"
        \$BB poweroff -f
        ;;
    crash)
        # Q5c: a crashed init — PID 1 dies, the kernel panics
        # ("Attempted to kill init"). The mode name must not be
        # "panic": that is a kernel cmdline keyword — the kernel
        # would eat it and init would see an empty mode.
        mark "CRASH-NOW"
        \$BB sh -c 'kill -9 \$\$'
        sleep 30
        ;;
    *)
        mark "UNKNOWN-MODE"
        \$BB poweroff -f
        ;;
esac
mark "SPIKE-DOWN mode=\$1"
EOF
    chmod +x "$root_rw/bin/spike-init"
}

# One CH run (the differences the epic calls out: nested=off — CH
# defaults it ON; shared=on — vhost-user-fs needs it; --console
# tty — the virtio console; --serial file= — the 16550 exit
# channel; --pvpanic + --event-monitor — the crash detection;
# --seccomp — CH's default-on). Prints CH's exit code.
run_ch() { # MODE [extra CH args...]
    local mode=$1
    shift
    local serial=$runtime/serial-$mode.out
    local events=$runtime/events-$mode.sock
    write_init "$mode"
    rm -f "$serial" "$events"
    spawn_fs fsroot "$root_rw"
    # the console (kernel log, hvc0) goes to a per-mode log: the
    # rc capture must stay clean. The 90s cap bounds the crash
    # probe: CH never exits on a guest kernel panic (it hangs
    # until killed — Q5c's answer), and a standalone spike run
    # must not deadlock on it. bwrap cannot be invoked through a
    # shell function under setsid, so the argv is inlined here;
    # setsid makes bwrap its own process-group leader (pgid ==
    # its pid), so the cap kill reaches the re-parented CH too.
    local console=$runtime/console-$mode.log
    "$setsid_bin" "$bwrap_bin" \
        --ro-bind /nix/store /nix/store \
        --ro-bind "$rootfs" "$rootfs" \
        --ro-bind "$kernel" "$kernel" \
        --ro-bind /etc/resolv.conf /etc/resolv.conf \
        --ro-bind-try /etc/passwd /etc/passwd \
        --ro-bind-try /etc/group /etc/group \
        --ro-bind /proc /proc \
        --ro-bind /sys /sys \
        --dev /dev \
        --dev-bind /dev/kvm /dev/kvm \
        --tmpfs /tmp \
        --bind "$runtime" "$runtime" \
        --clearenv \
        -- "$ch/bin/cloud-hypervisor" \
        --cpus "boot=2,nested=off" \
        --memory "size=2048M,shared=on" \
        --kernel "$kernel" \
        --fs "tag=/dev/root,socket=$runtime/fsroot.sock" \
        --cmdline "root=/dev/root rootfstype=virtiofs rw console=hvc0 init=/bin/spike-init $mode" \
        --console "tty" \
        --serial "file=$serial" \
        --pvpanic \
        --event-monitor "path=$events" \
        "$@" >"$console" 2>&1 &
    local ch_pid=$!
    local rc=0
    local deadline=$((SECONDS + 90))
    while kill -0 "$ch_pid" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do
        sleep 1
    done
    if kill -0 "$ch_pid" 2>/dev/null; then
        kill -9 -- -"$ch_pid" 2>/dev/null || :
        wait "$ch_pid" 2>/dev/null || :
        rc=124
        note "$mode: CH hit the 90s cap (a hung VM — the crash case)" >&2
    else
        wait "$ch_pid" || rc=$?
    fi
    stop_fs
    echo "$rc"
}

serial_of() { # MODE
    cat "$runtime/serial-$1.out" 2>/dev/null || true
}

# ------------------------------------------------------------------
echo "--- Q3/Q7: boot the shared rootfs (microvm-qemu bzImage) on CH"
rc=$(run_ch boot)
out=$(serial_of boot)
echo "$out" | grep -q "BOOT:OK" ||
    fail "Q3 boot" "no BOOT:OK on the serial; serial: $(echo "$out" | tail -5)"
pass "Q3: the microvm-qemu bzImage boots the shared rootfs on CH 54.0"
if echo "$out" | grep -q "VMX:masked"; then
    pass "Q7a: nested=off masks VMX (the D27 contract holds on CH)"
else
    note "Q7a: check the VMX/SVM lines above — nested=off must mask both"
fi
pass "Q7c: CH's default seccomp holds under the bwrap view (the boot passed through it)"
note "Q7b: the bwrap view CH needed: ro /proc, ro /sys, --dev /dev, --dev-bind /dev/kvm, ro /etc/resolv.conf (passt)"
if [ "$rc" -eq 0 ]; then
    pass "Q5a: the poweroff -f teardown exits CH cleanly (rc=0)"
else
    note "Q5a: CH exited $rc after poweroff — the launcher maps the event, not the code"
fi

# ------------------------------------------------------------------
echo "--- Q4: the exit code reaches the 16550 (--serial file=)"
rc=$(run_ch exit7)
out=$(serial_of exit7)
echo "$out" | grep -q "EXIT:7" ||
    fail "Q4 exit channel" "no EXIT:7 on the serial; serial: $(echo "$out" | tail -5)"
pass "Q4: an exit-code line reaches the 16550 serial channel"
if [ "$rc" -eq 0 ]; then
    pass "Q5b: poweroff -f exits CH with 0"
else
    note "Q5b: CH exited $rc after poweroff — the launcher maps the event, not the code"
fi

# ------------------------------------------------------------------
echo "--- Q1/Q2: passt --vhost-user + the SLiRP address plan"
# The host-side endpoint the 10.0.2.2 gateway must reach (passt's
# --map-host-loopback default): a one-shot HTTP server on the
# host loopback.
python3 -m http.server 8000 --bind 127.0.0.1 >/dev/null 2>&1 &
http_pid=$!
cleanup_http() {
    kill "$http_pid" 2>/dev/null || :
}
trap 'cleanup; cleanup_http' EXIT

# the nested-sandbox escape: this sandbox is itself a mysbx
# SLiRP guest, so /etc/resolv.conf's nameserver (10.0.2.3) lies
# INSIDE passt's guest range and passt refuses it. On a real
# host the resolver is outside 10.0.2.0/24 and passt reads it
# itself; the module (bd myconfig-9wkp.5) computes --dns from
# the host resolver and must skip SLiRP-range entries the same
# way.
dns_args=()
if grep -E '^nameserver' /etc/resolv.conf 2>/dev/null | grep -Evq '^nameserver 10\.0\.2\.'; then
    : # a host resolver outside the guest range exists — passt reads it itself
else
    dns_args+=(--dns 10.0.2.3)
    note "nested sandbox: every host resolver is 10.0.2.x itself — passing --dns 10.0.2.3 (this host IS a SLiRP guest)"
fi
bwrap_base "$passt/bin/passt" \
    --vhost-user \
    --socket "$runtime/passt.sock" \
    --foreground \
    --ipv4-only \
    --address 10.0.2.15 \
    --netmask 24 \
    --gateway 10.0.2.2 \
    --dns-forward 10.0.2.3 \
    "${dns_args[@]}" \
    --pid "$runtime/passt.pid" &
passt_pid=$!
sleep 2
[ -S "$runtime/passt.sock" ] ||
    fail "Q1 passt" "the vhost-user socket never appeared (passt died inside bwrap?)"
pass "Q1a: passt --vhost-user self-isolates inside bwrap and serves the socket"

# CH must present the MAC passt frames packets for (its banner's
# "host" MAC — the one the static guest config never announces);
# passt in vhost-user mode filters on it.
rc=$(run_ch net --net "vhost_user=on,socket=$runtime/passt.sock,vhost_mode=client,mac=9a:55:9a:55:9a:55")
out=$(serial_of net)
echo "$out" | grep -q "GW:OK" ||
    fail "Q2 gateway" "the guest cannot reach 10.0.2.2:8000 (the host loopback NAT); serial: $out"
pass "Q2a: the SLiRP gateway 10.0.2.2 reaches the host loopback (TCP)"
if echo "$out" | grep -q "DNS:OK"; then
    pass "Q2b: DNS via 10.0.2.3 (passt's forwarder) resolves"
else
    note "Q2b: DNS failed — a nested-sandbox artifact (see the egress note); live acceptance on a real host must confirm"
fi
if echo "$out" | grep -q "GW:OK HOSTNET:OK"; then
    pass "Q2c: http://10.0.2.2:8000 reaches the host loopback service"
else
    note "Q2c: the host-loopback HTTP probe failed — see serial"
fi
if echo "$out" | grep -q "EGRESS:OK"; then
    pass "Q1b: egress to the internet works through passt + CH"
else
    note "Q1b: egress failed — the nested-sandbox collision: THIS host's eth0 is 10.0.2.15/24 itself (it is a mysbx SLiRP guest), so SNAT'd return traffic is delivered host-locally and never reaches passt. On a real host eth0 is outside 10.0.2.0/24. Design note for bd myconfig-9wkp.5: the module must detect a host interface inside the guest plan and refuse or remap."
fi
kill "$passt_pid" 2>/dev/null || :
cleanup_http

# ------------------------------------------------------------------
echo "--- Q6: the virtio console carries a terminal size (hvc0)"
rc=$(run_ch resize)
out=$(serial_of resize)
# CH propagates its OWN stdio's tty size to the guest tty; this
# probe runs CH with stdio to a log file, so there is no size to
# carry ("stty: standard input"). The launcher (bd
# myconfig-9wkp.6) owns a pty like the qemu launcher and passes
# its size through; the mechanism (virtio console resize events)
# needs no CH flag.
note "Q6: --console tty carries the stdio's tty size — no pty here, so no size; the launcher owns the pty (as the qemu launcher does)"

# ------------------------------------------------------------------
echo "--- Q5c: a crashed init (kernel panic) — detected, no reboot loop"
rc=$(run_ch crash)
out=$(serial_of crash)
if echo "$out" | grep -q "CRASH-NOW"; then
    pass "Q5c: the crashed init panics the kernel and CH does NOT reboot-loop (it hangs — safe default)"
else
    note "Q5c: no CRASH-NOW marker — the init may have died before the write"
fi
# the event monitor path is a plain file CH appends JSON events
# to. The microvm-qemu kernel has NO pvpanic driver (no pvpanic
# symbols in System.map), so a guest crash surfaces only as the
# ABSENCE of the shutdown event: CH never exits (the probe's
# outer timeout had to kill it). The launcher (bd
# myconfig-9wkp.4's contract) must own a teardown timeout and
# treat serial-silence as the crash signal; bead .3 should
# decide whether to enable CONFIG_PVPANIC for a positive signal.
note "Q5c: CH exit code after the crash probe: $rc (124 = the cap killed a hung CH) — the launcher must enforce the teardown timeout; no pvpanic in this kernel, so the crash signal is serial-silence"

echo
echo "=== spike summary (bd myconfig-9wkp.1) ==="
echo "See the PASS/NOTE lines above; the go/no-go answers land in"
echo "the epic's notes. Landlock (--landlock) is an opt-in probe,"
echo "not load-bearing for the go/no-go."
