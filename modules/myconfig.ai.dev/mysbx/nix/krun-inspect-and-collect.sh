#!/usr/bin/env bash
# mysbx-krun-inspect-and-collect — triage a wedged (or live) krun
# sandbox from the HOST side (bd myconfig-dj4).
#
# The defect under investigation: libkrun's virtiofs passthrough
# server runs inside the mysbx-krun LAUNCHER process and pins one
# O_PATH host fd per visited inode (lookup() openat + inode table
# insert), released only on guest FUSE FORGET — with no eviction, so
# a long-lived session that walks the shared /nix/store (find, grep,
# rg, nix evals) accumulates host fds monotonically until EMFILE
# wedges the whole guest (bd myconfig-dj4, recurrence 2026-10-07:
# 524246 fds at the 524288 ceiling).
#
# This script collects, on the host, exactly the evidence the bead's
# design section asks for on a live wedge:
#   - the launcher's /proc/<pid>/limits (the ceiling that stands —
#     separates a stale/low ceiling from genuine unbounded growth)
#   - the fd count and an fd TARGET HISTOGRAM grouped by parent
#     directory (the inode-table accumulation signature: O_PATH fds
#     into the ro store share, dominated by nixpkgs source trees)
#   - the flags/heap/sched state and the cmdline of the launcher
#
# Modes:
#   mysbx-krun-inspect-and-collect              # snapshot every launcher found
#   mysbx-krun-inspect-and-collect --watch N    # additionally sample the fd
#                                               # count every N seconds until
#                                               # interrupted, then snapshot
#
# The report is written to
#   ~/myconfig/myconfig/_mysbx-krun-inspect-and-collect.<timestamp>.log
# and everything it does is READ-ONLY: it never sends a signal, never
# writes outside the report file, and never reads fd CONTENTS (only
# /proc metadata and readlink targets).
#
# Plain bash + /proc only, so it runs on any host without nix or
# root — the same discipline as krun-live-validation.sh. Run it ON
# THE HOST (f13), not inside the guest: the wedged launcher is a
# host process.
set -euo pipefail

report_dir=${REPORT_DIR:-"$HOME/myconfig/myconfig"}
watch_seconds=${WATCH_SECONDS:-0}

usage() {
    cat >&2 <<'EOF'
usage: mysbx-krun-inspect-and-collect [--watch SECONDS]

Collects a read-only triage report of every mysbx-krun launcher on
this host into ~/myconfig/myconfig/_mysbx-krun-inspect-and-collect.<timestamp>.log
(bd myconfig-dj4). --watch SECONDS samples each launcher's fd count
every SECONDS until interrupted (Ctrl-C), then snapshots.
EOF
}

while [ $# -gt 0 ]; do
    case $1 in
        --watch)
            [ $# -ge 2 ] || {
                usage
                exit 2
            }
            watch_seconds=$2
            shift 2
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        *)
            usage
            exit 2
            ;;
    esac
done

mkdir -p "$report_dir"
timestamp=$(date +%Y%m%d-%H%M%S)
report="$report_dir/_mysbx-krun-inspect-and-collect.${timestamp}.log"

log() { printf '%s\n' "$*" | tee -a "$report"; }

log "mysbx-krun-inspect-and-collect — $(date -Is)"
log "host: $(uname -srm), uptime: $(uptime -p 2>/dev/null || true)"
log "report: $report"
if [ "$watch_seconds" -gt 0 ]; then
    log "mode: snapshot + watch every ${watch_seconds}s (Ctrl-C to stop)"
else
    log "mode: snapshot"
fi
log ""

# Every mysbx-krun launcher on the host: the binary is mysbx-krun or
# .mysbx-krun-wrapped (the wrapper script) and its argv carries
# --rootfs — the same identity rule as the find_launcher() of
# krun-fd-stress.py. A wedged guest's launcher is still alive (the
# guest kernel runs on), so a plain /proc walk finds it; no
# process-tree ancestry is needed.
# The identity rule of krun-fd-stress.py: argv[0]'s basename is
# mysbx-krun or .mysbx-krun-wrapped AND --rootfs is among the argv —
# content matching on the whole cmdline would also catch a parent
# shell whose -c string names the launcher.
launchers=$(
    for proc in /proc/[0-9]*; do
        pid=${proc#/proc/}
        [ -r "$proc/cmdline" ] || continue
        first=$(tr '\0' '\n' <"$proc/cmdline" 2>/dev/null | head -1)
        name=${first##*/}
        if [ "$name" = mysbx-krun ] || [ "$name" = .mysbx-krun-wrapped ]; then
            if tr '\0' '\n' <"$proc/cmdline" 2>/dev/null | grep -qx -- '--rootfs'; then
                printf '%s\n' "$pid"
            fi
        fi
    done
)

if [ -z "$launchers" ]; then
    log "NO mysbx-krun launcher found — nothing to inspect."
    log "If the session already died, the launcher exited with it;"
    log "reproduce the wedge first (docs/krun-fd-stress.py can drive one)."
    exit 1
fi

# shellcheck disable=SC2086  # a newline-separated pid list; the word split is the loop
log "launchers found: $(printf '%s ' $launchers)"
log ""

snapshot_launcher() {
    # $1: pid
    pid=$1
    proc=/proc/$pid
    log "=============================================================="
    log "launcher pid $pid"
    log "=============================================================="
    if [ ! -d "$proc" ]; then
        log "  (gone — the launcher exited between discovery and here)"
        log ""
        return
    fi

    # The identity, for the record: full argv and the binary behind it.
    log '-- cmdline (\0-separated, one arg per line):'
    tr '\0' '\n' <"$proc/cmdline" 2>/dev/null | sed 's/^/  /' | tee -a "$report" || true
    log "-- exe: $(readlink "$proc/exe" 2>/dev/null || echo unreadable)"
    log "-- started: $(ps -o lstart= -p "$pid" 2>/dev/null || echo unknown)"
    log "-- ppid: $(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ' || echo unknown)"
    log ""

    # The ceiling that stands — bd myconfig-dj4's triage (a)/(c):
    # a low hard limit means the raise failed or was constrained
    # (launcher stderr would carry the WARNING lines), the raised
    # 524288/1048576 pair means genuine unbounded growth (b).
    log "-- /proc/$pid/limits (the fd ceiling the launcher raised or inherited):"
    sed 's/^/  /' "$proc/limits" 2>/dev/null | tee -a "$report" || log "  (unreadable)"
    soft=$(awk '/^Max open files/ { print $4 }' "$proc/limits" 2>/dev/null || echo '?')
    log "  => Max open files: soft=$soft"
    log ""

    # The fd count and its pressure against the ceiling.
    fd_count=$(find "$proc/fd" -maxdepth 1 2>/dev/null | wc -l)
    log "-- fd count: $fd_count (soft ceiling: $soft)"
    if [ "$soft" != '?' ] && [ "$soft" -gt 0 ] 2>/dev/null; then
        log "   pressure: $((fd_count * 100 / soft))% of the soft ceiling"
    fi
    log ""

    # The fd TARGET HISTOGRAM, grouped by parent directory — the
    # signature of the accumulation: O_PATH fds into the ro store
    # share, clustered inside nixpkgs source trees. Top 30 parents,
    # then the totals by top-level scope, like the recurrence's
    # evidence table (bd myconfig-dj4 notes 2026-10-07).
    log "-- fd targets by parent directory (top 30):"
    for fd in "$proc"/fd/*; do
        readlink "$fd" 2>/dev/null || echo '(unreadable)'
    done |
        awk '{
        # strip the file name: the parent directory of the target
        n = split($0, parts, "/")
        dir = ""
        for (i = 1; i < n; i++) dir = dir (i > 1 ? "/" : "") parts[i]
        if (dir == "") {
            print "(no-directory)"
            next
        }
        print dir
    }' |
        sort | uniq -c | sort -rn |
        awk '{ printf "  %7d %s\n", $1, $2 }' |
        head -30 |
        tee -a "$report"
    log ""

    log "-- fd targets by top-level scope:"
    for fd in "$proc"/fd/*; do
        readlink "$fd" 2>/dev/null || echo '(unreadable)'
    done |
        awk '{
        t = $0
        if (t ~ /^\/nix\/store(\/|$)/) print "/nix/store"
        else if (t ~ /^\/mysbx-krun-stage(\/|$)/) print "/mysbx-krun-stage"
        else if (t ~ /^\/tmp\/mysbx-shares(\/|$)/) print "/tmp/mysbx-shares"
        else if (t ~ /^(socket|pipe|anon_inode)/) print "(socket/pipe/anon)"
        else if (t ~ /^\//) print "(other-absolute)"
        else print "(other)"
    }' |
        sort | uniq -c | sort -rn |
        awk '{ printf "  %7d %s\n", $1, $2 }' |
        tee -a "$report"
    log ""

    # open fd flags: the inode-table pins are O_PATH (no read/write
    # intent), fdinfo carries the flags field per descriptor.
    opath_count=0
    handle_count=0
    for fd in "$proc"/fd/*; do
        flags=$(awk '/^flags:/ { print $2 }' "$proc/fdinfo/${fd##*/}" 2>/dev/null || echo '')
        [ -n "$flags" ] || continue
        if [ $((8#${flags} & 0x200000)) -ne 0 ] 2>/dev/null; then
            opath_count=$((opath_count + 1))
        else
            handle_count=$((handle_count + 1))
        fi
    done
    log "-- fd kinds by flags: O_PATH=$opath_count, handle=$handle_count"
    log "   (the inode-table pins of the virtiofs passthrough are O_PATH;"
    log "    a dominating O_PATH count over the store share is case (b),"
    log "    the no-eviction accumulation — bd myconfig-dj4)"
    log ""

    # Memory and scheduler state: the inode table itself lives in the
    # launcher's heap, so its growth shows in RSS/VmSwap too.
    log "-- memory (kB):"
    awk '/^(VmPeak|VmSize|VmRSS|VmData|VmSwap):/ { printf "  %s\n", $0 }' "$proc/status" 2>/dev/null | tee -a "$report" || log "  (unreadable)"
    log "-- threads: $(awk '/^Threads:/ { print $2 }' "$proc/status" 2>/dev/null || echo '?')"
    log "-- state: $(awk '/^State:/ { print $2, $3 }' "$proc/status" 2>/dev/null || echo '?')"
    log ""

    # The launcher's stderr is gone with its parent terminal, but the
    # kernel ring may hold the fallout of the exhaustion if anything
    # logged it; grab only the lines that name this pid or fd limits.
    log "-- kernel log lines naming fd exhaustion or this pid (dmesg):"
    if dmesg 2>/dev/null | grep -iE 'emfile|too many open|fd limit' | tail -10 | tee -a "$report"; then :; else
        log "  (dmesg unreadable without root, or no matches)"
    fi
    log ""
}

for pid in $launchers; do
    snapshot_launcher "$pid"
done

# --watch: the bead's "measure over time" follow-up — sample the fd
# count of every launcher every N seconds and log the delta; Ctrl-C
# (SIGINT) breaks the loop and takes a final snapshot per launcher.
if [ "$watch_seconds" -gt 0 ]; then
    log "=============================================================="
    log "watch mode: sampling fd counts every ${watch_seconds}s — Ctrl-C to stop"
    log "=============================================================="
    # Ctrl-C (SIGINT) breaks the sampling loop; the trap keeps the
    # script alive long enough to fall through to the final section.
    watch_interrupted=0
    trap 'watch_interrupted=1' INT
    while [ "$watch_interrupted" -eq 0 ]; do
        line="$(date -Is)"
        for pid in $launchers; do
            if [ -d "/proc/$pid" ]; then
                count=$(find "/proc/$pid/fd" -maxdepth 1 2>/dev/null | wc -l)
                line="$line pid=$pid fd_count=$count"
            else
                line="$line pid=$pid (gone)"
            fi
        done
        log "$line"
        # the interrupt also breaks the sleep; its non-zero return
        # must not trip set -e — the loop condition is the flag
        sleep "$watch_seconds" || true
    done
    trap - INT
    log "watch stopped — the full snapshot follows below"
fi

log "=============================================================="
log "done. Report: $report"
log "Next steps: attach the report to bd myconfig-dj4; if the fd"
log "histogram is dominated by O_PATH fds into the store share with a"
log "raised ceiling, the case is (b) — the no-eviction accumulation."
