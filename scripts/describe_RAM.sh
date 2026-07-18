#!/usr/bin/env nix-shell
#! nix-shell -i bash -p procps util-linux gawk lsof
# shellcheck shell=bash
set -euo pipefail

# Describe how RAM is being used on this system:
#   1. Top 5 processes by memory consumption
#   2. Memory consumed by tmpfs mounts
#   3. Memory used by the GPU
#
# The three blocks above are reported separately rather than reconciled
# into one "used" total, because the numbers are measured differently
# (RSS double-counts shared libraries; tmpfs usage from `findmnt` includes
# deleted-but-open files; GPU VRAM on integrated GPUs is carved out of
# system RAM). `free`'s "used" column is the authoritative total.

echo "==================== RAM overview ===================="
free -h
echo

echo "==================== Top 5 processes by RSS ===================="
ps -eo pid,user,rss,comm --sort=-rss | head -n 6 |
    awk 'NR==1 {print $0} NR>1 {
    printf "%-8s %-10s %10.1f MB  %s\n", $1, $2, ($3/1024), $4
  }'
echo
echo "(RSS double-counts shared libraries across processes; treat the"
echo " sum as an upper bound on process memory, not an exact figure.)"
echo

echo "==================== tmpfs mounts ===================="
if command -v findmnt >/dev/null 2>&1; then
    printf "%-45s %10s %10s %10s %6s\n" "TARGET" "SIZE" "USED" "AVAIL" "USE%"
    findmnt -t tmpfs -n -o TARGET,SIZE,USED,AVAIL,USE% 2>/dev/null |
        awk '{printf "%-45s %10s %10s %10s %6s\n", $1, $2, $3, $4, $5}'
    # Surface deleted-but-open files held on tmpfs. Invisible to `du` but
    # still consume RAM — explains why `df`/`findmnt` > `du` on a tmpfs.
    if command -v lsof >/dev/null 2>&1; then
        deleted=$(lsof +L1 2>/dev/null | awk 'NF>0 && $NF ~ "^/tmp" {print $1, $2, $NF}' | sort -u)
        if [[ -n $deleted ]]; then
            echo
            echo "Deleted-but-open files on /tmp (invisible to du, still using RAM):"
            echo "$deleted" | awk '{printf "  %-20s pid=%-8s %s\n", $1, $2, $3}'
        fi
    fi
    # Top subdirectories under each large tmpfs mount (>200 MiB).
    if command -v du >/dev/null 2>&1; then
        echo
        echo "Top entries under each large tmpfs mount (visible to du only):"
        findmnt -t tmpfs -n -o TARGET,USED 2>/dev/null |
            awk '{
                u=$2; mult=1
                if (u ~ /G$/) {mult=1024; gsub(/G$/,"",u)}
                else if (u ~ /M$/) {mult=1; gsub(/M$/,"",u)}
                else if (u ~ /K$/) {mult=0.000976562; gsub(/K$/,"",u)}
                mib=(u+0)*mult
                if (mib >= 200) printf "%s %.1f\n", $1, mib
            }' |
            while read -r target mib; do
                echo "  -- $target (~${mib}MiB reported by df) --"
                # `du` may hit permission errors and `head` truncates the
                # pipe (SIGPIPE) — both become non-zero under pipefail, so
                # guard the whole pipeline.
                { du -x --max-depth=1 -h "$target" 2>/dev/null |
                    sort -rh | head -n 6 | sed 's/^/    /'; } || true
            done
    fi
elif command -v df >/dev/null 2>&1; then
    printf "%-45s %12s %12s %6s\n" "TARGET" "SIZE" "USED" "USE%"
    df -t tmpfs -P 2>/dev/null |
        awk 'NR==1 {next}
             {printf "%-45s %10.1f MB %10.1f MB %6s\n", $6, ($2/1024), ($3/1024), int($5+0)"%"}'
else
    echo "Neither findmnt nor df available; cannot list tmpfs mounts."
fi
echo

echo "==================== GPU memory ===================="
gpu_reported=0

# NVIDIA
if command -v nvidia-smi >/dev/null 2>&1; then
    gpu_reported=1
    echo "-- NVIDIA --"
    nvidia-smi --query-gpu=name,memory.total,memory.used,memory.free \
        --format=csv,noheader 2>/dev/null || true
    echo
fi

# AMD / Intel (amdgpu / i915) via sysfs. On integrated GPUs this VRAM is
# carved out of system RAM, so it counts toward "used" RAM.
shopt -s nullglob
for card in /sys/class/drm/card*/device; do
    if [[ -r "$card/mem_info_vram_used" ]]; then
        gpu_reported=1
        name=$(cat "$card/device" 2>/dev/null || true)
        [[ -z $name ]] && name=$(basename "$(dirname "$card")")
        used=$(($(cat "$card/mem_info_vram_used") / 1024))
        total=$(cat "$card/mem_info_vram_total" 2>/dev/null || echo 0)
        total=$((total / 1024))
        echo "$name: VRAM used ${used} KiB / total ${total} KiB"
    fi
    if [[ -r "$card/i915_memory_region_total_bytes" ]]; then
        gpu_reported=1
        echo "Intel i915 ($(basename "$(dirname "$card")")):"
        awk '{printf "  %s: %.1f MiB\n", $1, ($2/1048576)}' "$card/i915_memory_region_total_bytes" 2>/dev/null || true
    fi
done
shopt -u nullglob

if [[ $gpu_reported -eq 0 ]]; then
    echo "No GPU memory info found (no nvidia-smi, amdgpu VRAM sysfs, or i915 sysfs)."
fi
