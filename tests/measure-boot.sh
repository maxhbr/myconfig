#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# REAL-KVM measurement harness for the `myconfig.ai.microvm` agent-sandbox tier
# (lightweight plan phase 0, "add a repeatable benchmark script", and the phase-6
# acceptance measurements).
#
# It is the OUT-OF-CI half of the measurement story. Everything that can be
# measured from an evaluated configuration or a built closure — guest closure
# size, runner closure size, the number of generated guest units, the number of
# virtiofs shares, the number of host helper units per VM — is measured here too
# (with `--static-only` it is ALL this script does, and it then needs neither
# root nor /dev/kvm). What genuinely needs a booted guest, and therefore
# /dev/kvm plus root, is:
#
#   * launch-to-ready LATENCY (host `agent-microvm run --detach` -> the control
#     channel answers), and
#   * IDLE RSS per running slot (the hypervisor cgroup plus its virtiofsd), and
#   * the RUNNING host helper process count and the guest process count.
#
# Those three are reported as `null` unless the script is run on a KVM host as
# root. They are NEVER estimated or filled in from another machine: an invented
# number would be worse than a missing one (see
# docs/agent-microvm-runtime-validation.md).
#
# USAGE
#   sudo tests/measure-boot.sh [--slot <slot>] [--repeat <n>] [--static-only]
#                              [--host <nixos-configuration>] [--json <file>]
#
# The output is ONE machine-readable JSON document (plan phase 0's "suggested
# benchmark output", extended with the fields phases 4/5/6 made measurable), so
# two runs can be diffed and a later phase can be compared against a recorded
# baseline.
set -euo pipefail

PROG="${0##*/}"
REPO_ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
HOST_ATTR="test-f13"
SLOT=""
REPEAT=3
STATIC_ONLY=0
JSON_OUT=""
LAUNCHER="${AGENT_MICROVM:-agent-microvm}"

die() {
    printf '%s: error: %s\n' "$PROG" "$*" >&2
    exit 1
}
log() { printf '%s: %s\n' "$PROG" "$*" >&2; }

while (($#)); do
    case "$1" in
        --slot)
            SLOT="${2-}"
            shift 2
            ;;
        --repeat)
            REPEAT="${2-}"
            shift 2
            ;;
        --host)
            HOST_ATTR="${2-}"
            shift 2
            ;;
        --json)
            JSON_OUT="${2-}"
            shift 2
            ;;
        --static-only)
            STATIC_ONLY=1
            shift
            ;;
        -h | --help)
            sed -n '5,35p' "${BASH_SOURCE[0]}"
            exit 0
            ;;
        *) die "unknown argument '$1' (try --help)" ;;
    esac
done

command -v nix >/dev/null 2>&1 || die "nix is required"
command -v jq >/dev/null 2>&1 || die "jq is required"

# --- the STATIC half: evaluated configuration + built closures -------------
# Read from the SAME evaluated NixOS configuration the host is built from, so
# the numbers describe the shipped shape and not a hand-written model of it.
static_json() {
    nix eval --raw --impure --expr "
      let
        flake = builtins.getFlake (\"git+file://\" + toString ${REPO_ROOT});
        lib = flake.inputs.nixpkgs.lib;
        cfg = flake.nixosConfigurations.${HOST_ATTR}.config;
        net = flake.nixosConfigurations.${HOST_ATTR}._module.args.agentNetwork;
        vmNames = builtins.attrNames cfg.microvm.vms;
        vm = n: cfg.microvm.vms.\${n}.config.config;
        perVmHostUnits = n:
          lib.filter (u: lib.hasInfix n u) (builtins.attrNames cfg.systemd.services);
        perVmHostSockets = n:
          lib.filter (u: lib.hasInfix n u) (builtins.attrNames cfg.systemd.sockets);
        per = n: let g = vm n; in {
          toplevel = g.system.build.toplevel.outPath;
          runner = g.microvm.declaredRunner.outPath;
          vcpu = g.microvm.vcpu;
          memory_mib = g.microvm.mem;
          guest_unit_count = builtins.length (builtins.attrNames g.systemd.services);
          guest_socket_count = builtins.length (builtins.attrNames g.systemd.sockets);
          virtiofs_share_count = builtins.length g.microvm.shares;
          network_interface_count = builtins.length g.microvm.interfaces;
          host_helper_unit_count = builtins.length (perVmHostUnits n);
          host_helper_units = lib.sort (a: b: a < b) (perVmHostUnits n);
          host_helper_socket_count = builtins.length (perVmHostSockets n);
          host_helper_sockets = lib.sort (a: b: a < b) (perVmHostSockets n);
        };
      in builtins.toJSON {
        host = \"${HOST_ATTR}\";
        network_profile = net.profile;
        model_transport = net.transport;
        capabilities = cfg.myconfig.ai.microvm.capabilities;
        enabled_agents = cfg.myconfig.ai.microvm.enabledAgents;
        slots = lib.genAttrs vmNames per;
      }
    "
}

# Closure size of a store path, realising it first (the number is meaningless
# for an unbuilt derivation).
closure_size() {
    local path="$1"
    nix build --no-link "$path" >/dev/null 2>&1 || return 1
    nix path-info -S --json "$path" 2>/dev/null | jq -r 'to_entries[0].value.closureSize'
}

# --- the RUNTIME half: needs /dev/kvm + root -------------------------------
runtime_possible() {
    [[ -e /dev/kvm ]] || return 1
    [[ $EUID -eq 0 ]] || return 1
    command -v "$LAUNCHER" >/dev/null 2>&1 || return 1
    return 0
}

# Launch-to-ready latency in milliseconds: start a detached session and poll the
# launcher's own readiness view (`status` reports `ssh: ready` once the control
# channel — TAP or VSOCK — answers), then tear it down again.
measure_launch_ms() {
    local slot="$1" task="$2" start end
    "$LAUNCHER" destroy "$slot" >/dev/null 2>&1 || true
    start="$(date +%s%3N)"
    "$LAUNCHER" run --task "$task" --slot "$slot" --detach >/dev/null 2>&1 ||
        return 1
    while :; do
        if "$LAUNCHER" status "$slot" 2>/dev/null | grep -q '^  ssh:  *ready$'; then
            break
        fi
        (($(date +%s%3N) - start > 120000)) && return 1
        sleep 0.1
    done
    end="$(date +%s%3N)"
    printf '%s' "$((end - start))"
}

# Idle RSS and process counts of a RUNNING slot, from the hypervisor's cgroup
# (which contains cloud-hypervisor, its threads and the slot's virtiofsd
# instances — that is exactly the "host processes per running slot" the plan
# asks for).
measure_running() {
    local slot="$1"
    local unit="microvm@$slot.service"
    local mem procs guest_procs
    mem="$(systemctl show -p MemoryCurrent --value "$unit" 2>/dev/null || echo '')"
    [[ $mem =~ ^[0-9]+$ ]] || mem="null"
    procs="$(systemctl show -p TasksCurrent --value "$unit" 2>/dev/null || echo '')"
    [[ $procs =~ ^[0-9]+$ ]] || procs="null"
    # virtiofsd runs in its own unit per VM, so count it separately.
    local vio
    vio="$(systemctl show -p TasksCurrent --value "microvm-virtiofsd@$slot.service" 2>/dev/null || echo '')"
    [[ $vio =~ ^[0-9]+$ ]] || vio="null"
    guest_procs="$("$LAUNCHER" ssh "$slot" -- ps -e --no-headers 2>/dev/null | wc -l || echo '')"
    [[ $guest_procs =~ ^[0-9]+$ ]] && ((guest_procs > 0)) || guest_procs="null"
    printf '{"idle_rss_bytes":%s,"hypervisor_tasks":%s,"virtiofsd_tasks":%s,"guest_process_count":%s}' \
        "$mem" "$procs" "$vio" "$guest_procs"
}

# --- run ------------------------------------------------------------------
log "collecting the static (evaluated + built) measurements for $HOST_ATTR"
static="$(static_json)"

slots_json="{}"
while IFS= read -r s; do
    top="$(jq -r --arg s "$s" '.slots[$s].toplevel' <<<"$static")"
    run="$(jq -r --arg s "$s" '.slots[$s].runner' <<<"$static")"
    log "measuring closure sizes of $s (this realises the closures)"
    top_size="$(closure_size "$top" || echo null)"
    run_size="$(closure_size "$run" || echo null)"
    slots_json="$(jq --arg s "$s" \
        --argjson top "${top_size:-null}" --argjson run "${run_size:-null}" \
        '.[$s] = { guest_closure_bytes: $top, runner_closure_bytes: $run }' \
        <<<"$slots_json")"
done < <(jq -r '.slots | keys[]' <<<"$static")

runtime_json='null'
runtime_reason="measured"
if ((STATIC_ONLY)); then
    runtime_reason="skipped: --static-only"
elif ! runtime_possible; then
    if [[ ! -e /dev/kvm ]]; then
        runtime_reason="PENDING: no /dev/kvm on this machine (boot latency and idle RSS are NOT estimated)"
    elif [[ $EUID -ne 0 ]]; then
        runtime_reason="PENDING: must run as root (the launcher mounts and drives systemd)"
    else
        runtime_reason="PENDING: the agent-microvm launcher is not on PATH"
    fi
    log "$runtime_reason"
else
    [[ -n $SLOT ]] || SLOT="$(jq -r '.slots | keys[0]' <<<"$static")"
    log "measuring the runtime numbers on slot $SLOT ($REPEAT launches)"
    lat="[]"
    for i in $(seq 1 "$REPEAT"); do
        ms="$(measure_launch_ms "$SLOT" "measure-boot-$i" || echo '')"
        if [[ -n $ms ]]; then
            lat="$(jq --argjson ms "$ms" '. + [$ms]' <<<"$lat")"
            if ((i == REPEAT)); then
                running="$(measure_running "$SLOT")"
            fi
        else
            log "launch $i did not become ready within the timeout"
        fi
        "$LAUNCHER" destroy "$SLOT" >/dev/null 2>&1 || true
    done
    runtime_json="$(jq -n --arg slot "$SLOT" --argjson lat "$lat" \
        --argjson running "${running:-null}" \
        '{ slot: $slot, launch_to_ready_ms: $lat, running: $running }')"
fi

result="$(jq -n --argjson static "$static" --argjson sizes "$slots_json" \
    --argjson runtime "$runtime_json" --arg reason "$runtime_reason" \
    --arg rev "$(git -C "$REPO_ROOT" rev-parse HEAD 2>/dev/null || echo unknown)" \
    '{
       revision: $rev,
       static: ($static | .slots |= with_entries(.value += ($sizes[.key] // {}))),
       runtime: $runtime,
       runtime_status: $reason
     }')"

if [[ -n $JSON_OUT ]]; then
    printf '%s\n' "$result" >"$JSON_OUT"
    log "wrote $JSON_OUT"
else
    printf '%s\n' "$result"
fi
