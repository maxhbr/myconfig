#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — REAL-KVM runtime validation suite (improvement ticket 6
# part A).
#
# The `nix flake check` suite (tests/microvm.nix) is EVAL/BUILD only: it proves
# the module evaluates, that the pool is well-formed and that the shell code
# passes shellcheck. It can neither boot a guest nor move a packet. This script
# is the other half: it must be run BY AN OPERATOR, as root, ON A HOST WITH
# /dev/kvm and the feature enabled (f13), and it exercises the properties that
# only exist at runtime.
#
# It is deliberately NOT part of `nix flake check` (no KVM in CI, and it starts
# real VMs, mounts filesystems and sends packets).
#
# Usage:
#   sudo ./modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh \
#        --repository /home/mhuber/some-git-repo \
#        [--section all|boot|net|l2|creds|lifecycle|malrepo|forgery|seed]
#
# Sections are also gated on the CAPABILITIES of the host under test
# (`myconfig.ai.microvm.capabilities`, lightweight plan phase 5): every section
# that drives a guest over SSH needs `interactive`, `lifecycle` and `forgery`
# additionally need `batch`, and `seed` needs neither. A section whose
# capability the host does not select is SKIPPED (under `--section all`) or
# HARD-ABORTS (when asked for explicitly) instead of reporting vacuous passes.
# Within `creds`, the batch-worker environment subtest additionally needs
# `batch` and reports the capability when it is absent (the unit it inspects
# does not exist there).
#
# The set is READ from `agent-microvm capabilities` (machine-readable, needs no
# root, starts nothing); an answer that cannot be parsed HARD-ABORTS the run
# rather than defaulting to "this host has everything".
#
# CONSEQUENCE, recorded here because it is a real coverage hole: on a host that
# selects ONLY `batch`, seven of the eight sections need `interactive` (the
# control channel IS ssh) and only `seed` runs. Restoring coverage there needs a
# guest transport that does not require sshd — which is exactly what phase 6
# (VSOCK) is for. Until then a batch-only host is validated by the eval/build
# tier (`checks.microvm-capabilities`) plus `--section seed` only.
#
# Every check prints exactly one line: `PASS`, `FAIL` or `SKIP` plus a reason.
# The script exits non-zero if any check FAILED. SKIPs are honest: they mark
# things this environment cannot decide (e.g. only one slot in a class), never
# silent passes.
#
# The procedure and the meaning of each section are documented in
# docs/agent-microvm-runtime-validation.md.
set -euo pipefail

PROG="agent-microvm-runtime-validation"

# --- configuration (must match the module options of the host under test) ----
GATEWAY="${AGENT_GATEWAY:-192.168.83.1}"
LITELLM_PORT="${AGENT_LITELLM_PORT:-4000}"
BRIDGE="${AGENT_BRIDGE:-agentbr0}"
RUNTIME_ROOT="${AGENT_RUNTIME_ROOT:-/var/lib/agent-microvms}"
# The generated, policy-BAKED host-side config stager (config-seed.nix). It is
# a host `systemPackages` entry of every host that enables the feature; the
# `seed` section SKIPs when it is not on PATH.
STAGER="${AGENT_STAGER:-agent-microvm-stage-config}"
WORKSPACE_ROOT="$RUNTIME_ROOT/workspaces"
LAUNCHER="${AGENT_LAUNCHER:-agent-microvm}"
# How long a freshly started guest may take to accept SSH. A DETACHED `run`
# returns as soon as systemd accepted the VM — launcher.nix only calls its own
# `wait_ready` under `--attach` — so every start path in this suite has to wait
# for readiness itself (see wait_guest_ready).
READY_TIMEOUT="${AGENT_RTV_READY_TIMEOUT:-120}"
READY_INTERVAL="${AGENT_RTV_READY_INTERVAL:-3}"
# Placeholder value guest.nix gives the model-API keys (no real credential ever
# enters a guest; the real one lives only in the host LiteLLM proxy).
KEY_PLACEHOLDER="${AGENT_RTV_KEY_PLACEHOLDER:-not-needed}"
RESULTS_DIR="$RUNTIME_ROOT/results"

# --- SHARE LAYOUT (lightweight plan phase 4) ---------------------------------
# The module has exactly ONE share layout: ONE writable virtiofs share per slot
# plus ONE read-only share (session.nix).
#
#   <runtimeRoot>/sessions/<slot>              job data (the writable share)
#   <runtimeRoot>/sessions/<slot>/workspace    clone bind
#   <runtimeRoot>/sessions/<slot>/state        agent-state bind
#   <runtimeRoot>/sessions-ro/<slot>/hostkeys  the slot's SSH host identity
#   <runtimeRoot>/sessions-ro/<slot>/config-seed  staged host configuration
#     guest: /run/agent-session /run/agent-session-ro (+ /workspace, a BIND of
#            the session share, hence not of type virtiofs)
#
# Every path constant below feeds a `test -e`, a `find` or a `grep -E` whose
# *absence* of a match is reported as a PASS, so pointing them at a
# non-existent tree would turn the whole suite into silent, vacuous green —
# exactly the failure mode `check_denied`/`check_reason` exist to rule out
# everywhere else. The `seed` section therefore hard-FAILs when the staged
# payload is missing after staging (see section_seed), and `shares` asserts
# EXPECTED_SHARES for EQUALITY.
#
# EXPECTED_SHARES is the COMPLETE set of virtiofs mount targets a guest may
# have (sorted, space-separated).
SESSION_ROOT="$RUNTIME_ROOT/sessions"
SESSION_RO_ROOT="$RUNTIME_ROOT/sessions-ro"
JOBS_ROOT="$SESSION_ROOT"
GUEST_JOB_DIR="/run/agent-session"
GUEST_RO_DIR="/run/agent-session-ro"
# /workspace is a BIND of "$GUEST_JOB_DIR/workspace", so `findmnt -t virtiofs`
# does not list it.
EXPECTED_SHARES="$GUEST_JOB_DIR $GUEST_RO_DIR"
WORKSPACE_MOUNT_RE="^${SESSION_ROOT}/[^/]+/workspace\$"
STATE_MOUNT_RE="^${SESSION_ROOT}/[^/]+/state\$"
# <runtimeRoot>/sessions-ro/<slot>/config-seed — a share virtiofsd mounts
# `--readonly`, hence NOT under the writable session tree.
config_seed_payload() { printf '%s' "$SESSION_RO_ROOT/$1/config-seed"; }
# The slot name of a bind-mount TARGET matched by the two regexes above. Both
# end in `<slot>/workspace` or `<slot>/state`, so stripping the known trailing
# component and taking the basename is exact (slot names are
# `agent-<class>-<i>`, never `workspace`/`state`).
slot_of_mount() {
    local mp="$1"
    mp="${mp%/workspace}"
    mp="${mp%/state}"
    printf '%s' "${mp##*/}"
}
# Batch job share (job.nix): the subdirectory names the guest sees under
# $GUEST_JOB_DIR.
GUEST_INPUT_DIR="$GUEST_JOB_DIR/input"
GUEST_CTRL_DIR="$GUEST_JOB_DIR/controller"
GUEST_WORKER_DIR="$GUEST_JOB_DIR/worker"
# ROOT-owned: the guest's systemd opens the worker's stdout/stderr in here as
# root, so the worker must not be able to write, replace or rename any of it.
GUEST_WORKER_LOGS_DIR="$GUEST_JOB_DIR/worker-logs"

REPO=""
SECTION="all"

PASS=0
FAIL=0
SKIP=0

pass() {
    printf 'PASS  %s\n' "$*"
    PASS=$((PASS + 1))
}
fail() {
    printf 'FAIL  %s\n' "$*"
    FAIL=$((FAIL + 1))
}
skip() {
    printf 'SKIP  %s\n' "$*"
    SKIP=$((SKIP + 1))
}
info() { printf '#     %s\n' "$*"; }
section() { printf '\n=== %s ===\n' "$*"; }

die() {
    printf '%s: error: %s\n' "$PROG" "$*" >&2
    exit 1
}

# `check <description> <command...>` — PASS when the command succeeds.
check() {
    local desc="$1"
    shift
    if "$@" >/dev/null 2>&1; then
        pass "$desc"
    else
        fail "$desc"
    fi
}

# `check_fails <description> <command...>` — PASS when the command FAILS.
#
# ONLY for commands that run on the HOST (`test -e /tmp/...`, a launcher
# invocation that must be rejected). Any non-zero exit counts, so using it for a
# command executed IN A GUEST makes the check pass vacuously whenever the SSH
# channel is merely not up yet — "the attack failed" and "the connection
# failed" must never look the same. Every guest-side denial uses `check_denied`
# below.
check_fails() {
    local desc="$1"
    shift
    if "$@" >/dev/null 2>&1; then
        fail "$desc (expected failure, but it succeeded)"
    else
        pass "$desc"
    fi
}

# `check_denied <description> <slot> <cmd...>` — the guest-side variant of
# check_fails that cannot pass vacuously. THREE ways a guest-side denial can be
# fake, all of them handled here:
#
#   1. the SSH channel is dead        -> SKIP (re-proved after every denial)
#   2. the command never reached the guest intact, because
#      `agent-microvm ssh -- <argv>` flattens argv and the guest LOGIN SHELL
#      re-parses it (see the transport block below)  -> SKIP
#   3. the command really ran and was refused  -> PASS
#
# "the attack failed", "the connection failed" and "the command was mangled"
# must never look the same.
check_denied() {
    local desc="$1" slot="$2"
    shift 2
    if ! guest_transport_ok "$slot"; then
        skip "$desc (the guest command transport is unusable — the denial could not be decided)"
        return
    fi
    if guest "$slot" "$@" >/dev/null 2>&1; then
        fail "$desc (expected denial, but it SUCCEEDED)"
        return
    fi
    if guest_channel_ok "$slot"; then
        pass "$desc"
    else
        skip "$desc (the guest channel died — the denial could not be decided)"
    fi
}

usage() {
    cat >&2 <<EOF
Usage: sudo $0 --repository <git-repo> [--section all|boot|net|l2|creds|lifecycle|malrepo|forgery|seed]

Sections:
  boot       per-class boot, readiness, /workspace, persistence, shares
  net        proxy-only allow/deny matrix (metadata, RFC1918, DNS, internet)
  l2         two-guest layer-2 isolation (ARP spoofing, direct ethernet)
  creds      absence of host credentials/sockets inside a guest
  lifecycle  forced failures at every stage + slot reusability
  malrepo    hostile repository fixture (hooks, flake.nix, direnv, symlinks)
  forgery    batch RESULT CHANNEL: a hostile worker must not be able to write,
             replace, delete or shadow the authoritative result, stale results
             must be rejected, timeouts must kill the whole worker cgroup, and
             cancellation must be bound to the allocation token
  seed       runtime configuration staging: the stager is
             run against real fixtures in the host home and must stage the
             allowlisted file while refusing credential-shaped names, benignly
             NAMED symlinks onto credentials, host-home escapes, FIFOs, setuid
             and over-budget files, keep the tree root-only and clean it before
             every launch. Starts no VM; SKIPs unless the host stages at all.
EOF
    exit 2
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --repository)
            REPO="${2-}"
            shift 2
            ;;
        --section)
            SECTION="${2-}"
            shift 2
            ;;
        -h | --help) usage ;;
        *) die "unknown argument '$1'" ;;
    esac
done

[[ "$(id -u)" -eq 0 ]] || die "must run as root (it starts VMs and inspects mounts)"
[[ -e /dev/kvm ]] || die "/dev/kvm is missing — this suite requires real KVM"
command -v "$LAUNCHER" >/dev/null || die "$LAUNCHER is not on PATH (is the feature enabled on this host?)"
[[ -n $REPO ]] || usage
[[ -d "$REPO/.git" ]] || die "--repository must be a git repository: $REPO"

# --- helpers ---------------------------------------------------------------
# `agent-microvm list` prints one line per slot of the CURRENT pool:
#   <slot> <class> <running|stopped> <ip> <task|"<free>">
all_classes() {
    "$LAUNCHER" list | awk '{ print $2 }' | sort -u
}
current_slots() { "$LAUNCHER" list | awk '{ print $1 }'; }
running_slots() { "$LAUNCHER" list | awk '$3 == "running" { print $1 }'; }

# Is <name> a slot of the pool this generation defines? Per-slot state under a
# name that is NOT in the pool is left over from an earlier slot naming and
# must be reported separately (see check_no_residue) rather than counted as
# residue of the run under test.
# The pool is fixed for the lifetime of a host generation, so ask the launcher
# ONCE instead of once per candidate (check_no_residue calls this in two loops).
CURRENT_SLOTS_CACHE=""
is_current_slot() {
    local want="$1" s
    [[ -n $CURRENT_SLOTS_CACHE ]] || CURRENT_SLOTS_CACHE="$(current_slots)"
    while read -r s; do
        [[ $s == "$want" ]] && return 0
    done <<<"$CURRENT_SLOTS_CACHE"
    return 1
}

# The slot currently holding <task>, or the empty string.
slot_of_task() {
    "$LAUNCHER" list | awk -v t="$1" '$5 == t { print $1 }'
}

# A slot's host-side TAP name. slots.nix derives both names from the same
# position in the pool: the slot is `agent-<class>-<i>` and its TAP is
# `vm-<class>-<i>` (the short form, because IFNAMSIZ caps an interface name at
# 15 characters), so the mapping is a pure prefix swap.
slot_tap() { printf 'vm-%s' "${1#agent-}"; }

# --- the guest command TRANSPORT -------------------------------------------
#
# `agent-microvm ssh <slot> -- <argv...>` CANNOT preserve argument boundaries:
# OpenSSH joins the remaining argv with single spaces into one string and the
# guest's LOGIN SHELL re-parses it — bash today (guest.nix gives the agent user
# `shell = pkgs.bashInteractive`; it was fish before the lightweight guest
# dropped it). Sending
#
#     ssh <slot> -- sh -c "timeout 5 sh -c '</dev/tcp/GW/22'"
#
# therefore made the guest run `sh -c timeout 5 sh -c '...'`, i.e. `sh -c` got
# only the word `timeout` (`5` became $0) — "timeout: missing operand", non-zero,
# and every `check_denied` built on such a payload passed VACUOUSLY. Payloads
# containing `${VAR:-}` fared even worse: the login shell expanded (or, under
# fish, rejected) them before the inner shell saw them, so the environment
# assertions could never have run at all.
#
# The fix has three parts and none of them may be dropped:
#   * guest_sh_as sends the ENTIRE script as ONE token, escaped for the guest's
#     login shell;
#   * the escaping dialect is DETECTED per slot (fish and POSIX single-quoting
#     are mutually incompatible; today's guest answers `posix`, and the fish
#     dialect is kept so an operator can point the suite at an older guest) with
#     a probe that only succeeds when word
#     boundaries, embedded single quotes, `${VAR:-}` expansion AND the exit code
#     all survive the round trip;
#   * every guest-side assertion consults that probe first, so a transport that
#     is broken again in the future produces SKIPs and a loud FAIL of the
#     transport control itself — never a green suite.
declare -A GUEST_QUOTE_MODE=()

# Quote one argument for the /bin/sh that will run INSIDE the guest.
sh_quote() { printf "'%s'" "${1//\'/\'\\\'\'}"; }

# Quote a whole script into ONE token for the guest's login shell. fish and
# POSIX shells disagree about backslashes inside single quotes, so both
# dialects exist and the probe below decides which one this guest speaks.
quote_for_fish() {
    local s=${1//\\/\\\\}
    printf "'%s'" "${s//\'/\\\'}"
}
quote_for_posix() { sh_quote "$1"; }

guest_sh_as() {
    local slot="$1" mode="$2" script="$3" quoted
    case "$mode" in
        fish) quoted="$(quote_for_fish "$script")" ;;
        *) quoted="$(quote_for_posix "$script")" ;;
    esac
    "$LAUNCHER" ssh "$slot" -- /bin/sh -c "$quoted"
}

# Is the plain control channel alive? Deliberately a payload that survives ANY
# re-parsing (a single word), so it answers "is the guest reachable", nothing
# else.
guest_channel_ok() { "$LAUNCHER" ssh "$1" -- true >/dev/null 2>&1; }

# The transport probe. It fails unless ALL of these survive the trip:
#   * argument boundaries  (a flattened `printf %s 'a b'` prints nothing)
#   * an embedded single quote
#   * `${VAR:-}` parameter expansion (a fish syntax error if it is re-parsed)
#   * the exit code (7, not 0 and not 1)
TRANSPORT_PROBE=$'printf %s \'a b\'; test -n "${HOME:-}" || exit 3; exit 7'

# Run TRANSPORT_PROBE through one of the production entry points and check both
# its output and its exit code. `guest_sh <slot> <script>` and
# `guest <slot> sh -c <script>` must both reproduce it.
probe_via() {
    local how="$1" slot="$2" out rc=0
    case "$how" in
        guest_sh) out="$(guest_sh "$slot" "$TRANSPORT_PROBE" 2>/dev/null)" || rc=$? ;;
        *) out="$(guest "$slot" sh -c "$TRANSPORT_PROBE" 2>/dev/null)" || rc=$? ;;
    esac
    out="${out//$'\r'/}"
    out="${out//$'\n'/}"
    [[ $out == "a b" ]] && ((rc == 7))
}

# Echo the quoting dialect that works for <slot>, or return 1. Memoised: a
# POSITIVE verdict always, a NEGATIVE one only when the channel itself is alive
# (otherwise a probe issued while the guest is still booting would poison the
# cache for the whole run).
guest_transport_mode() {
    local slot="$1" mode out rc
    if [[ -n ${GUEST_QUOTE_MODE[$slot]:-} ]]; then
        [[ ${GUEST_QUOTE_MODE[$slot]} == none ]] && return 1
        printf '%s' "${GUEST_QUOTE_MODE[$slot]}"
        return 0
    fi
    for mode in fish posix; do
        rc=0
        out="$(guest_sh_as "$slot" "$mode" "$TRANSPORT_PROBE" 2>/dev/null)" || rc=$?
        out="${out//$'\r'/}"
        out="${out//$'\n'/}"
        if [[ $out == "a b" ]] && ((rc == 7)); then
            GUEST_QUOTE_MODE["$slot"]="$mode"
            # Re-run the SAME probe through the two entry points the checks
            # really use (guest_sh for scripts, guest for argv), so the control
            # can never validate a code path no assertion goes through.
            if probe_via guest_sh "$slot" && probe_via guest "$slot"; then
                printf '%s' "$mode"
                return 0
            fi
            GUEST_QUOTE_MODE["$slot"]=""
        fi
    done
    guest_channel_ok "$slot" && GUEST_QUOTE_MODE["$slot"]=none
    return 1
}

guest_transport_ok() { guest_transport_mode "$1" >/dev/null; }

# Report the transport verdict for a slot as its OWN check line, so a mangled
# transport is visible as a FAIL instead of silently turning the slot's whole
# assertion block into SKIPs.
assert_transport() {
    local slot="$1" desc="$2" mode
    if mode="$(guest_transport_mode "$slot")"; then
        pass "$desc: the guest command transport preserves argument boundaries, quoting, \${VAR} expansion and exit codes ($mode quoting)"
    else
        fail "$desc: the guest command transport is BROKEN — commands do not reach the guest as written, so every guest-side assertion on $slot can only be SKIPPED"
    fi
}

# POSITIVE CONTROL for every `</dev/tcp/host/port>` reachability probe: the
# very same mechanism must SUCCEED against the ONE endpoint the policy allows
# (LiteLLM on the bridge gateway). A guest shell without /dev/tcp support would
# otherwise refuse every connection for a reason that has nothing to do with
# the firewall, and the whole matrix would pass vacuously. Memoised per slot;
# reports itself once as a PASS/FAIL line.
declare -A TCP_PROBE_STATE=()
tcp_probe_works() {
    local slot="$1"
    if [[ -z ${TCP_PROBE_STATE[$slot]:-} ]]; then
        if guest "$slot" bash -c "timeout 5 bash -c '</dev/tcp/$GATEWAY/$LITELLM_PORT'" >/dev/null 2>&1; then
            TCP_PROBE_STATE["$slot"]=ok
            pass "the /dev/tcp probe really works in $slot (it reaches the ALLOWED $GATEWAY:$LITELLM_PORT)"
        else
            TCP_PROBE_STATE["$slot"]=broken
            fail "the /dev/tcp probe cannot reach the ALLOWED endpoint $GATEWAY:$LITELLM_PORT from $slot — every /dev/tcp denial would be vacuous"
        fi
    fi
    [[ ${TCP_PROBE_STATE[$slot]} == ok ]]
}

# Run a SCRIPT inside a guest slot, as the unprivileged agent user, with the
# script intact. Returns 125 when the transport is unusable (a value no payload
# here produces), so callers can tell "could not ask" from "the guest said no".
guest_sh() {
    local slot="$1" mode
    shift
    mode="$(guest_transport_mode "$slot")" || return 125
    guest_sh_as "$slot" "$mode" "$*"
}

# Run a command INSIDE a guest slot, as the unprivileged agent user. Each
# argument is quoted for the guest's /bin/sh first, so `guest <slot> sh -c
# '<script>'` and every other multi-word payload arrives exactly as written.
guest() {
    local slot="$1" a script=""
    shift
    for a in "$@"; do script+=" $(sh_quote "$a")"; done
    guest_sh "$slot" "${script# }"
}

# Run a command inside a guest in a LOGIN shell. The guest's
# `environment.variables` (guest.nix) reach a process only via
# /etc/set-environment, which /etc/profile sources — i.e. LOGIN SHELLS ONLY.
# `agent-microvm ssh <slot> -- <cmd>` is neither a login nor an interactive
# shell, so every environment assertion MUST go through this helper; otherwise
# no variable is set at all and "the variable is absent" checks pass vacuously.
guest_login() {
    local slot="$1"
    shift
    guest "$slot" sh -lc "$*"
}

# THE readiness gate for every start path in this suite.
#
# A DETACHED `run` deliberately does not wait for the guest: launcher.nix calls
# `wait_ready` only under `--attach`. Issuing a guest command immediately after
# `run` returns therefore fails because sshd is not up yet — which is how this
# suite used to report spurious FAILs ("guest is SSH-ready", "/workspace is a
# mount point") and, far worse, VACUOUS PASSES for every
# "the guest must NOT be able to ..." check. Poll the real control channel; do
# not sleep and hope.
wait_guest_ready() {
    local slot="$1" waited=0
    while ((waited < READY_TIMEOUT)); do
        guest_channel_ok "$slot" && return 0
        sleep "$READY_INTERVAL"
        waited=$((waited + READY_INTERVAL))
    done
    return 1
}

# `start_task <task> <class> [agent] [extra `run` args...]` — start a detached
# slot and echo its name once the guest ACCEPTS SSH.
#
# Exit codes: 0 = ready, 1 = could not start / no slot was allocated,
# 2 = a slot was allocated but the guest never became SSH-ready (the slot name
# is still echoed so the caller can clean it up). Reporting is left to the
# caller via report_start_failure, because pass/fail counters incremented inside
# a command substitution would be lost with the subshell.
start_task() { start_task_in "$REPO" "$@"; }

# The same, for a task whose source repository is NOT the suite's throwaway one
# (the hostile fixture in section_malrepo). ONE start path, ONE readiness gate.
start_task_in() {
    local repo="$1" task="$2" class="$3" agent="${4:-pi}" slot
    shift 3
    (($# > 0)) && shift # the agent, if it was given; the rest are `run` args
    "$LAUNCHER" run --name "$task" --repository "$repo" --agent "$agent" \
        --resource-class "$class" "$@" >/dev/null 2>&1 || return 1
    slot="$(slot_of_task "$task")"
    [[ -n $slot ]] || return 1
    printf '%s' "$slot"
    wait_guest_ready "$slot" || return 2
}

# Wait for a BACKGROUND `submit` to allocate a slot for <task> and for the guest
# on it to accept SSH; echo the slot. Exit codes as for start_task (1 = no slot
# appeared, 2 = a slot appeared but the guest never became reachable). Shares
# wait_guest_ready with every other start path so the two cannot drift apart.
wait_task_slot_ready() {
    local task="$1" waited=0 slot=""
    while ((waited < READY_TIMEOUT)); do
        slot="$(slot_of_task "$task")"
        if [[ -n $slot ]]; then
            printf '%s' "$slot"
            wait_guest_ready "$slot" || return 2
            return 0
        fi
        sleep "$READY_INTERVAL"
        waited=$((waited + READY_INTERVAL))
    done
    return 1
}

# Wait until <task> has been ALLOCATED a slot (readiness NOT required) and echo
# it; return 1 if none appeared within READY_TIMEOUT. Used where the point is to
# plant something on the HOST side while the job runs. Sequencing those subtests
# by `sleep N` (and then asserting anyway) is what let them "decide" properties
# of a job that had not started yet — invariant 1 of the suite: never sleep and
# hope, and never assert against a fixture that did not run.
wait_task_slot() {
    local task="$1" waited=0 slot=""
    while ((waited < READY_TIMEOUT)); do
        slot="$(slot_of_task "$task")"
        if [[ -n $slot ]]; then
            printf '%s' "$slot"
            return 0
        fi
        sleep "$READY_INTERVAL"
        waited=$((waited + READY_INTERVAL))
    done
    return 1
}

# Report a failed start_task. MUST run in the caller's shell (see above).
report_start_failure() {
    local rc="$1" ctx="$2"
    if ((rc == 2)); then
        fail "$ctx: a slot was allocated but the guest never accepted SSH within ${READY_TIMEOUT}s"
    else
        fail "$ctx: could not start a guest"
    fi
}

cleanup_task() {
    local task="$1"
    "$LAUNCHER" stop "$task" >/dev/null 2>&1 || true
    "$LAUNCHER" workspace-remove "$task" --force >/dev/null 2>&1 || true
}

# --- (1) boot / filesystem -------------------------------------------------
section_boot() {
    section "boot + filesystem (ticket 6 A.1)"
    local class slot task rc shares
    for class in $(all_classes); do
        task="rtv-boot-$class"
        cleanup_task "$task"
        info "class $class: starting $task"
        rc=0
        slot="$(start_task "$task" "$class")" || rc=$?
        if ((rc != 0)) || [[ -z $slot ]]; then
            report_start_failure "$rc" "class $class boots"
            cleanup_task "$task"
            continue
        fi
        pass "class $class boots and becomes SSH-ready ($slot)"
        # POSITIVE CONTROL for everything below: the channel the assertions run
        # over really works, so a denial below means denial, not "not up yet".
        check "class $class: guest is SSH-ready" guest "$slot" true
        # ... and the commands really arrive as written (see the transport
        # block): without this, a quoting slip turns every denial into a
        # vacuous PASS.
        assert_transport "$slot" "class $class"
        check "class $class: /workspace is a mount point" \
            guest "$slot" findmnt -n /workspace
        check "class $class: /workspace is writable" \
            guest "$slot" sh -c 'echo runtime-validation > /workspace/.rtv && rm -f /workspace/.rtv'
        # The host /nix/store must NEVER be shared in.
        check_denied "class $class: host /nix/store is not shared" "$slot" \
            findmnt -n -S /nix/store
        # Exactly the four expected shares, nothing else. An EMPTY answer is not
        # a pass and not a fail of the property — it means the enumeration
        # itself did not run, so say that instead of guessing.
        shares="$(guest "$slot" sh -c 'findmnt -t virtiofs -o TARGET -n | sort | tr "\n" " "' 2>/dev/null || true)"
        # EQUALITY, not a substring match: a substring match accepted any
        # ADDITIONAL share that happens to sort after /workspace.
        local shares_norm
        shares_norm="$(printf '%s' "$shares" | tr -s '[:space:]' ' ' | sed 's/^ //; s/ $//')"
        if [[ -z ${shares//[[:space:]]/} ]]; then
            fail "class $class: could not enumerate the guest's virtiofs shares (empty answer)"
        elif [[ $shares_norm == "$EXPECTED_SHARES" ]]; then
            pass "class $class: exactly the expected virtiofs shares"
        else
            fail "class $class: unexpected virtiofs share set: $shares"
        fi

        # Workspace changes must survive shutdown; the guest's own filesystem
        # must not.
        #
        # The markers are placed where the UNPRIVILEGED agent can really create
        # them ($HOME and /tmp) and their existence is ASSERTED here, in the
        # first run. The suite used to write `/root-marker`, which the agent
        # user cannot create at all (`/` is root-owned, the agent has no sudo
        # and no groups, guest.nix:292-301) — so "it did not persist" was true
        # of a file that never existed.
        guest "$slot" sh -c 'echo persisted > /workspace/rtv-persisted.txt' >/dev/null 2>&1 || true
        # shellcheck disable=SC2016  # $HOME must expand in the GUEST, not here
        guest "$slot" sh -c 'echo ephemeral > "$HOME/rtv-home-marker"; echo ephemeral > /tmp/rtv-fs-marker' >/dev/null 2>&1 || true
        local markers_planted=0
        # shellcheck disable=SC2016  # $HOME must expand in the GUEST, not here
        if guest "$slot" sh -c 'test -f "$HOME/rtv-home-marker" && test -f /tmp/rtv-fs-marker' >/dev/null 2>&1; then
            markers_planted=1
            pass "class $class: the ephemerality markers were really created in the first run"
        else
            fail "class $class: could not create the ephemerality markers in the guest, so 'they did not persist' would prove nothing"
        fi
        "$LAUNCHER" stop "$task" >/dev/null 2>&1 || true
        if [[ -f "$WORKSPACE_ROOT/$task/rtv-persisted.txt" ]]; then
            pass "class $class: workspace changes persist in the clone"
        else
            fail "class $class: workspace changes were lost"
        fi
        # Restart the same task on the same slot and check the guest's own state.
        rc=0
        slot="$(start_task "$task-again" "$class")" || rc=$?
        if ((rc == 0)) && [[ -n $slot ]] && ((markers_planted)); then
            assert_transport "$slot" "class $class (restart)"
            check_denied "class $class: guest home does not persist between runs" "$slot" \
                test -f /home/agent/rtv-home-marker
            check_denied "class $class: the guest filesystem outside /workspace does not persist between runs" "$slot" \
                test -f /tmp/rtv-fs-marker
            cleanup_task "$task-again"
        elif ((rc == 0)) && [[ -n $slot ]]; then
            skip "class $class: the markers were never planted, so persistence cannot be decided"
            cleanup_task "$task-again"
        else
            skip "class $class: could not restart for the persistence check (rc $rc)"
            cleanup_task "$task-again"
        fi
        cleanup_task "$task"
    done

    section "task isolation + agent-state persistence (ticket 6 A.7/A.8)"
    local class_a slot_a slot_b
    class_a="$(all_classes | head -1)"
    cleanup_task rtv-iso-a
    cleanup_task rtv-iso-b
    rc=0
    # A SECOND workspace must really EXIST on the host while A is asked about
    # it: `rtv-iso-b` used to be cleaned up and never started, so the path
    # `$WORKSPACE_ROOT/rtv-iso-b` did not exist anywhere and "A cannot see it"
    # was unconditionally true. Start B if the pool allows it; otherwise create
    # the host path directly (that alone is enough for the property under test:
    # the guest must not be able to reach ANY host workspace path).
    local iso_b_started=0
    rc=0
    slot_b="$(start_task rtv-iso-b "$class_a" 2>/dev/null)" || rc=$?
    if ((rc == 0)) && [[ -n $slot_b ]]; then
        iso_b_started=1
    else
        cleanup_task rtv-iso-b
        mkdir -p "$WORKSPACE_ROOT/rtv-iso-b"
        printf 'other task data\n' >"$WORKSPACE_ROOT/rtv-iso-b/secret.txt"
        info "task isolation: could not start a second guest; created the host path $WORKSPACE_ROOT/rtv-iso-b directly"
    fi
    rc=0
    slot_a="$(start_task rtv-iso-a "$class_a" hermes)" || rc=$?
    if ((rc == 0)) && [[ -n $slot_a ]]; then
        assert_transport "$slot_a" "task isolation"
        # The path really is there on the host — stated, not assumed.
        if [[ -e "$WORKSPACE_ROOT/rtv-iso-b" ]]; then
            pass "task isolation: the other task's workspace really exists on the host ($WORKSPACE_ROOT/rtv-iso-b)"
            # Task A must not see it (there is no path to it at all).
            check_denied "task A cannot see another task's workspace" "$slot_a" \
                test -e "$WORKSPACE_ROOT/rtv-iso-b"
        else
            skip "task A cannot see another task's workspace (the other workspace could not be created, so the denial would prove nothing)"
        fi
        check_denied "task A cannot see the host workspace root" "$slot_a" \
            test -d "$WORKSPACE_ROOT"
        cleanup_task rtv-iso-a
    else
        report_start_failure "$rc" "task isolation (rtv-iso-a)"
        cleanup_task rtv-iso-a
    fi
    if ((iso_b_started)); then
        cleanup_task rtv-iso-b
    else
        rm -rf "${WORKSPACE_ROOT:?}/rtv-iso-b"
    fi
    # Declared-path-only persistence. Uses the SAME start path (and therefore
    # the same readiness gate) as everything else — it used to inline `run`
    # without waiting, which is why the two checks below failed spuriously.
    cleanup_task rtv-persist
    rc=0
    slot_b="$(start_task rtv-persist "$class_a" hermes --persist-agent-state)" || rc=$?
    if ((rc == 0)) && [[ -n $slot_b ]]; then
        assert_transport "$slot_b" "agent-state persistence"
        check "persisted state: ~/.hermes is a symlink into the share" \
            guest "$slot_b" test -L /home/agent/.hermes
        guest "$slot_b" sh -c 'mkdir -p ~/.hermes && echo kept > ~/.hermes/rtv' >/dev/null 2>&1 || true
        guest "$slot_b" sh -c 'echo notkept > ~/rtv-undeclared' >/dev/null 2>&1 || true
        "$LAUNCHER" stop rtv-persist >/dev/null 2>&1 || true
        if [[ -f "$RUNTIME_ROOT/state/tasks/rtv-persist/hermes/.hermes/rtv" ]]; then
            pass "persisted state: declared path survived on the host"
        else
            fail "persisted state: declared path did not survive"
        fi
        if find "$RUNTIME_ROOT/state/tasks/rtv-persist" -name rtv-undeclared | grep -q .; then
            fail "persisted state: an UNDECLARED path was persisted"
        else
            pass "persisted state: undeclared paths were not persisted"
        fi
        cleanup_task rtv-persist
    else
        report_start_failure "$rc" "agent-state persistence (rtv-persist)"
        cleanup_task rtv-persist
    fi
}

# Assert the guest rendered its agent model configs from the LIVE endpoint.
# Property checked: the number of models the generated configs advertise equals
# the number the endpoint actually serves — a stale (build-time) config would
# almost always disagree, and an unreachable endpoint yields SKIP, never a pass.
assert_model_config() {
    local slot="$1" state endpoint_count opencode_count pi_file opencode_file got_cfg
    opencode_file=/run/agent-model-config/opencode.json
    pi_file=/home/agent/.pi/agent/extensions/zz-microvm-models.ts

    state="$(guest "$slot" sh -c "systemctl show agent-model-config.service --property=Result --value" 2>/dev/null || true)"
    state="${state//$'\r'/}"
    if [[ -z $state ]]; then
        skip "boot-time model discovery: agent-model-config.service is not readable in $slot"
        return
    fi
    if [[ $state == success ]]; then
        pass "agent-model-config.service completed successfully in $slot"
    else
        fail "agent-model-config.service did not succeed in $slot (Result=$state)"
    fi

    check "the generated opencode overlay config exists and lists models" \
        guest "$slot" sh -c "jq -e '.provider.litellm.models | length > 0' $opencode_file >/dev/null"
    check "the generated pi provider extension exists and registers the provider" \
        guest "$slot" sh -c "grep -q '\"litellm\"' $pi_file"

    # The rendered lists must match the LIVE endpoint, not the frozen host copy.
    endpoint_count="$(guest "$slot" sh -c "curl -fsS -m 10 http://127.0.0.1:$LITELLM_PORT/v1/models | jq -r '[.data[].id] | unique | length'" 2>/dev/null || true)"
    opencode_count="$(guest "$slot" sh -c "jq -r '.provider.litellm.models | length' $opencode_file" 2>/dev/null || true)"
    endpoint_count="${endpoint_count//$'\r'/}"
    opencode_count="${opencode_count//$'\r'/}"
    if [[ ! $endpoint_count =~ ^[0-9]+$ ]] || ((endpoint_count == 0)); then
        skip "boot-time model discovery: the endpoint reported no usable model count, so freshness cannot be decided"
    elif [[ $opencode_count == "$endpoint_count" ]]; then
        pass "the generated agent configs list exactly the $endpoint_count model(s) the endpoint serves"
    else
        fail "the generated agent configs list $opencode_count model(s) but the endpoint serves $endpoint_count (stale config)"
    fi

    # The overlay only takes effect if opencode is actually pointed at it.
    # shellcheck disable=SC2016  # the variable must expand in the GUEST
    got_cfg="$(guest_login "$slot" 'printf %s "${OPENCODE_CONFIG-}"' 2>/dev/null || true)"
    got_cfg="${got_cfg//$'\r'/}"
    if [[ $got_cfg == "$opencode_file" ]]; then
        pass "the guest login environment points opencode at $opencode_file"
    else
        fail "OPENCODE_CONFIG is '$got_cfg', expected '$opencode_file' (the generated overlay would be ignored)"
    fi
}

# --- (2) network (proxy-only) ---------------------------------------------
section_net() {
    section "network: proxy-only allow/deny matrix (ticket 6 A.2)"
    local class slot rc=0
    class="$(all_classes | head -1)"
    cleanup_task rtv-net
    slot="$(start_task rtv-net "$class")" || rc=$?
    if ((rc != 0)) || [[ -z $slot ]]; then
        report_start_failure "$rc" "network"
        cleanup_task rtv-net
        return
    fi
    assert_transport "$slot" "network"
    # ALLOWED: the bridge-only LiteLLM endpoint, reached via the guest's own
    # loopback forwarder AND directly at the gateway.
    check "guest reaches the LiteLLM endpoint via loopback" \
        guest "$slot" curl -fsS -m 10 -o /dev/null "http://127.0.0.1:$LITELLM_PORT/v1/models"
    check "guest reaches the LiteLLM endpoint on the bridge gateway" \
        guest "$slot" curl -fsS -m 10 -o /dev/null "http://$GATEWAY:$LITELLM_PORT/v1/models"

    # Boot-time model discovery (guest-model-config.nix): the guest must have
    # turned the LIVE /v1/models answer into pi + opencode config, replacing the
    # build-time model lists copied from the host dotfiles. Only decidable while
    # the endpoint above is reachable, which the two checks just proved.
    assert_model_config "$slot"

    # DENIED — every one of these succeeding is a security failure, so all of
    # them use check_denied: the SSH channel is re-proved after each denial and a
    # dead channel yields SKIP, never a pass. (With check_fails the whole matrix
    # passed vacuously whenever the guest was not up yet.)
    check_denied "guest cannot reach the cloud-metadata endpoint" "$slot" \
        curl -fsS -m 5 -o /dev/null http://169.254.169.254/
    if tcp_probe_works "$slot"; then
        check_denied "guest cannot reach host SSH on the gateway" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/$GATEWAY/22'"
        check_denied "guest cannot reach an arbitrary host port (8080)" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/$GATEWAY/8080'"
        check_denied "guest cannot reach RFC1918 10.0.0.0/8" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/10.0.0.1/80'"
        check_denied "guest cannot reach RFC1918 172.16.0.0/12" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/172.16.0.1/80'"
        check_denied "guest cannot reach RFC1918 192.168.0.0/16 (outside the agent subnet)" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/192.168.1.1/80'"
        check_denied "guest cannot reach a public IP" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/1.1.1.1/80'"
        check_denied "guest cannot reach a public DNS server" "$slot" \
            bash -c "timeout 5 bash -c '</dev/tcp/8.8.8.8/53'"
    else
        skip "the seven /dev/tcp reachability denials (the probe cannot even reach the ALLOWED endpoint, so a refusal would prove nothing)"
    fi
    check_denied "guest cannot resolve public DNS names" "$slot" \
        sh -c "timeout 5 getent hosts example.com"
    # IPv6 must not provide a bypass: the bridge has IPv6 disabled.
    check_denied "guest has no IPv6 route that bypasses the IPv4 policy" "$slot" \
        sh -c "ip -6 route show default | grep -q ."
    cleanup_task rtv-net
}

# --- (3) layer 2 ----------------------------------------------------------
section_l2() {
    section "layer 2 isolation between two guests (ticket 6 A.3)"
    local class slots slot_a slot_b ip_a ip_b
    # Two simultaneous guests are required; they may come from different classes.
    mapfile -t slots < <("$LAUNCHER" list | awk '{ print $1 }')
    if ((${#slots[@]} < 2)); then
        skip "layer 2: fewer than two slots in the pool"
        return
    fi
    cleanup_task rtv-l2a
    cleanup_task rtv-l2b
    class="$(all_classes | head -1)"
    local rc=0
    slot_a="$(start_task rtv-l2a "$class")" || rc=$?
    if ((rc != 0)) || [[ -z $slot_a ]]; then
        report_start_failure "$rc" "layer 2 (guest A)"
        cleanup_task rtv-l2a
        return
    fi
    # Prefer a different class for B if the first one has a single slot. Stop at
    # the first class whose allocation SUCCEEDED (rc 0) or whose guest came up
    # but stayed unreachable (rc 2) — in the latter case the clone already
    # exists, so retrying another class could only fail on it.
    slot_b=""
    for class in $(all_classes); do
        rc=0
        slot_b="$(start_task rtv-l2b "$class" 2>/dev/null)" || rc=$?
        ((rc != 1)) && break
    done
    if ((rc != 0)) || [[ -z $slot_b ]]; then
        if ((rc == 2)); then
            report_start_failure "$rc" "layer 2 (guest B)"
        else
            skip "layer 2: could not start two guests simultaneously"
        fi
        cleanup_task rtv-l2a
        cleanup_task rtv-l2b
        return
    fi
    assert_transport "$slot_a" "layer 2 (guest A)"
    assert_transport "$slot_b" "layer 2 (guest B)"
    ip_a="$("$LAUNCHER" status "$slot_a" | awk '/^  ip:/ { print $2 }')"
    ip_b="$("$LAUNCHER" status "$slot_b" | awk '/^  ip:/ { print $2 }')"
    info "guest A=$slot_a ($ip_a)  guest B=$slot_b ($ip_b)"

    # Bridge-port isolation must be visible on EVERY running guest's TAP.
    #
    # `bridge link show` does NOT print per-port flags — only the DETAILED form
    # `bridge -d link show` does, so the old `grep -qv "isolated on"` could never
    # see the flag and always reported a FAIL. Iterate the KNOWN taps of the
    # running slots instead of grepping for a substring, and treat a MISSING tap
    # as a failure of its own (a running slot without its TAP is broken
    # differently, not "isolated").
    local running=() s tap bad=0
    mapfile -t running < <(running_slots)
    if ((${#running[@]} < 2)); then
        fail "bridge-port isolation: fewer than two slots report 'running' (${running[*]-none}); the introspection cannot be trusted"
    else
        for s in "${running[@]}"; do
            tap="$(slot_tap "$s")"
            if ! ip link show "$tap" >/dev/null 2>&1; then
                info "slot $s is running but its TAP $tap does not exist"
                bad=1
                continue
            fi
            if ! bridge -d link show dev "$tap" 2>/dev/null | grep -q "isolated on"; then
                info "TAP $tap ($s) does not report 'isolated on':"
                bridge -d link show dev "$tap" 2>&1 | sed 's/^/#     /'
                bad=1
            fi
        done
        if ((bad)); then
            fail "every running guest TAP reports 'isolated on' (bridge -d link show)"
        else
            pass "every running guest TAP reports 'isolated on' (${#running[@]} taps)"
        fi
    fi

    check_denied "guest A cannot ping guest B (IPv4)" "$slot_a" \
        ping -c 2 -W 2 "$ip_b"
    if tcp_probe_works "$slot_a"; then
        check_denied "guest A cannot open a TCP connection to guest B" "$slot_a" \
            bash -c "timeout 5 bash -c '</dev/tcp/$ip_b/22'"
    else
        skip "guest A cannot open a TCP connection to guest B (the /dev/tcp probe does not work at all here)"
    fi
    check_denied "guest A cannot reach guest B over IPv6 link-local" "$slot_a" \
        sh -c "ping -6 -c 2 -W 2 ff02::1%eth0 2>/dev/null | grep -q 'bytes from'"
    # ARP: after an explicit request, guest B's MAC must never appear in A's
    # neighbour table as reachable. (iptables cannot filter ARP, so this measures
    # the bridge's port isolation directly.)
    guest "$slot_a" sh -c "ping -c 1 -W 1 $ip_b >/dev/null 2>&1 || true" >/dev/null 2>&1 || true
    check_denied "guest A cannot learn guest B's MAC (ARP is blocked at L2)" "$slot_a" \
        sh -c "ip neigh show $ip_b | grep -q 'lladdr'"

    # Impersonation: adding B's IP to A must not let A answer for it. Verified
    # from the HOST, which must still reach the real B.
    #
    # This goes through the launcher's own `ssh` subcommand, which is THE host's
    # channel to a guest: strict host-key verification against $KNOWN_HOSTS plus
    # the identity from $AGENT_MICROVM_SSH_KEY. A raw `ssh -o BatchMode=yes`
    # without an identity (what this check used to do) fails for AUTH reasons
    # whether or not the impersonation worked, which made the result meaningless.
    #
    # POSITIVE CONTROL first: if the host cannot reach B BEFORE the impersonation
    # is set up, the check is undecidable — SKIP instead of blaming the guest.
    #
    # ... and the ATTACK ITSELF must be proved to have HAPPENED. `ip addr add`
    # needs CAP_NET_ADMIN, and the guest agent is an unprivileged user with no
    # groups and no sudo (guest.nix:292-301), so the add is normally rejected
    # with EPERM — the suite used to swallow that with `|| true` and then
    # "prove" that the host still reached B, which it trivially did because
    # nothing had been done to it. So: assert B's address is really on A's
    # eth0; if it is not, the result is UNDECIDABLE, not a pass.
    if ! guest_channel_ok "$slot_b"; then
        skip "impersonation: the host cannot reach guest B even before the attack (baseline failed)"
    else
        pass "impersonation baseline: the host reaches the real guest B"
        guest "$slot_a" sh -c "ip addr add $ip_b/24 dev eth0 2>/dev/null || true" >/dev/null 2>&1 || true
        if guest "$slot_a" sh -c "ip -o addr show dev eth0 | grep -q ' $ip_b/'" >/dev/null 2>&1; then
            pass "impersonation: guest A really did take guest B's address ($ip_b) on eth0"
            if guest_channel_ok "$slot_b"; then
                pass "host still reaches the REAL guest B while A impersonates its IP"
            else
                fail "host lost/redirected its connection to guest B during impersonation"
            fi
        else
            skip "impersonation: the guest agent cannot configure addresses (no CAP_NET_ADMIN), so the impersonation could not be attempted at all"
        fi
        guest "$slot_a" sh -c "ip addr del $ip_b/24 dev eth0 2>/dev/null || true" >/dev/null 2>&1 || true
    fi
    cleanup_task rtv-l2a
    cleanup_task rtv-l2b
}

# The BATCH half of the `creds` section, in its own function so the section can
# DECIDE whether to run it: it inspects `agent-job-worker@pi.service`, which
# only EXISTS on a host that selects the `batch` capability (lightweight plan
# phase 5). Takes the slot of an already-running session, because the unit is
# read over the same SSH control channel the rest of the section uses.
creds_batch_worker_env() {
    local slot="$1"
    # The BATCH path gets its environment from the systemd WORKER UNIT, not from
    # a login profile, so the shell type above says nothing about it. Assert the
    # same facts against what `agent-job-worker@<agent>` would inherit: the
    # systemd manager environment plus the unit's declared Environment=.
    # The two halves are read SEPARATELY and BOTH must carry a PATH= of their
    # own. Concatenating them was a vacuity hole: `systemctl show-environment`
    # alone always prints PATH, so a unit read that returned nothing at all (a
    # renamed unit, an unknown instance — `systemctl show` exits 0 with empty
    # properties for those) still satisfied the control, and all thirteen
    # assertions about the UNIT's Environment= passed against an empty string.
    # The worker template really does set PATH via `path = workerPackages`
    # (job.nix), so requiring it of each half is a property, not a formality.
    local manager_env unit_env batch_env
    manager_env="$(guest "$slot" sh -c "systemctl show-environment" 2>/dev/null || true)"
    # `pi` is the agent every other section in this suite submits with; the unit
    # is a template, so its Environment= is the same for every instance.
    unit_env="$(guest "$slot" sh -c "systemctl show 'agent-job-worker@pi.service' --property=Environment --value" 2>/dev/null || true)"
    batch_env="$manager_env
$unit_env"
    if [[ $manager_env != *PATH=* ]]; then
        skip "batch worker environment: the guest's systemd MANAGER environment could not be read (no PATH in the answer)"
    elif [[ $unit_env != *PATH=* ]]; then
        skip "batch worker environment: agent-job-worker@pi.service reported no Environment= containing PATH (the unit was not readable, so its environment cannot be decided)"
    else
        pass "both halves of the batch worker's environment could be read (positive control: each contains PATH)"
        # POSITIVE CONTROL for the endpoint plumbing: the batch worker is a
        # non-login oneshot, so it gets its endpoint vars ONLY from the unit's
        # `Environment=` (NixOS puts `environment.variables` in login profiles
        # only). Without an explicit `environment=` on the unit these were ABSENT
        # — the worker had no model endpoint and died in ~2s — yet the old
        # check passed, because it only asserted secrets-absent, never
        # endpoint-present. Assert each endpoint var is present in the UNIT's
        # own Environment= (not the manager's) with its expected loopback value.
        local want_base="http://127.0.0.1:$LITELLM_PORT/v1" want_anth="http://127.0.0.1:$LITELLM_PORT"
        if grep -qE "(^|[[:space:]])OPENAI_BASE_URL=$want_base([[:space:]]|$)" <<<"$unit_env"; then
            pass "the batch worker's unit Environment= carries OPENAI_BASE_URL=$want_base"
        else
            fail "the batch worker's unit Environment= does NOT carry OPENAI_BASE_URL=$want_base (the batch path has no model endpoint)"
        fi
        if grep -qE "(^|[[:space:]])OPENROUTER_BASE_URL=$want_base([[:space:]]|$)" <<<"$unit_env"; then
            pass "the batch worker's unit Environment= carries OPENROUTER_BASE_URL=$want_base"
        else
            fail "the batch worker's unit Environment= does NOT carry OPENROUTER_BASE_URL=$want_base (hermes batch jobs would have no endpoint)"
        fi
        if grep -qE "(^|[[:space:]])ANTHROPIC_BASE_URL=$want_anth([[:space:]]|$)" <<<"$unit_env"; then
            pass "the batch worker's unit Environment= carries ANTHROPIC_BASE_URL=$want_anth"
        else
            fail "the batch worker's unit Environment= does NOT carry ANTHROPIC_BASE_URL=$want_anth (claude-code batch jobs would have no endpoint)"
        fi
        for var in OPENROUTER_API_KEY GITHUB_TOKEN GH_TOKEN GITLAB_TOKEN AWS_ACCESS_KEY_ID \
            AWS_SECRET_ACCESS_KEY GOOGLE_APPLICATION_CREDENTIALS AZURE_CLIENT_SECRET \
            KUBECONFIG SSH_AUTH_SOCK GPG_AGENT_INFO; do
            if grep -qE "(^|[[:space:]])$var=[^[:space:]]" <<<"$batch_env"; then
                fail "the batch worker's environment carries $var"
            else
                pass "the batch worker's environment does not carry $var"
            fi
        done
        for var in OPENAI_API_KEY ANTHROPIC_API_KEY; do
            if ! grep -qE "(^|[[:space:]])$var=[^[:space:]]" <<<"$batch_env"; then
                # Absent is at least as good as a placeholder: no key at all.
                pass "the batch worker's environment carries no $var at all"
            elif grep -qE "(^|[[:space:]])$var=$KEY_PLACEHOLDER([[:space:]]|$)" <<<"$batch_env"; then
                pass "the batch worker's $var is the placeholder, not a real key"
            else
                fail "the batch worker's $var is set to something other than the placeholder"
            fi
        done
    fi
}

# --- (4) credential leakage ----------------------------------------------
section_creds() {
    section "credential boundary (ticket 6 A.4)"
    local class slot rc=0
    class="$(all_classes | head -1)"
    cleanup_task rtv-creds
    slot="$(start_task rtv-creds "$class")" || rc=$?
    if ((rc != 0)) || [[ -z $slot ]]; then
        report_start_failure "$rc" "credentials"
        cleanup_task rtv-creds
        return
    fi
    assert_transport "$slot" "credential boundary"
    # NOTE: only names/paths are ever printed, never values.
    #
    # Every environment assertion runs in a LOGIN shell (guest_login): the guest's
    # `environment.variables` are exported by /etc/profile only, so a plain
    # `agent-microvm ssh <slot> -- sh -c ...` sees NO variable at all. That is why
    # the placeholder check used to FAIL while the twelve "does not contain"
    # checks below it used to pass VACUOUSLY — nothing was set either way.
    local var
    # POSITIVE CONTROL: a variable that MUST be set and MUST have this exact
    # value. If it is not there the profile was not loaded, so the whole
    # environment block is undecidable and is SKIPPED rather than passed.
    local want_base="http://127.0.0.1:$LITELLM_PORT/v1" got_base
    # shellcheck disable=SC2016  # the variable must expand in the GUEST
    got_base="$(guest_login "$slot" 'printf %s "${OPENAI_BASE_URL-}"' 2>/dev/null || true)"
    # The launcher's ssh allocates a pty (-t), so strip any CR the tty layer
    # added before comparing.
    got_base="${got_base//$'\r'/}"
    if [[ $got_base == "$want_base" ]]; then
        pass "guest environment is loaded (OPENAI_BASE_URL=$want_base)"
        for var in OPENAI_API_KEY ANTHROPIC_API_KEY; do
            if guest_login "$slot" "test \"\${$var-}\" = $KEY_PLACEHOLDER" >/dev/null 2>&1; then
                pass "guest $var is the placeholder, not a real key"
            else
                fail "guest $var is not the expected placeholder"
            fi
        done
        for var in OPENROUTER_API_KEY GITHUB_TOKEN GH_TOKEN GITLAB_TOKEN AWS_ACCESS_KEY_ID \
            AWS_SECRET_ACCESS_KEY GOOGLE_APPLICATION_CREDENTIALS AZURE_CLIENT_SECRET \
            KUBECONFIG SSH_AUTH_SOCK GPG_AGENT_INFO; do
            check_denied "guest environment does not contain $var" "$slot" \
                sh -lc "test -n \"\${$var:-}\""
        done
    else
        skip "guest environment assertions: OPENAI_BASE_URL is '$got_base', expected '$want_base' — the login profile did not load, so nothing about the environment can be decided"
    fi

    # The BATCH path gets its environment from the systemd WORKER UNIT, not from
    # a login profile, so the shell type above says nothing about it. Asserted
    # only where that unit exists: on an interactive-only host `systemctl show`
    # answers with EMPTY properties for an unknown unit, so the subtest would
    # report "the unit was not readable" — a wrong reason, and exactly the
    # dishonesty the capability dispatch exists to remove. The reason given is
    # therefore the capability itself, and no batch property is claimed.
    if ((BATCH_CAPABLE)); then
        creds_batch_worker_env "$slot"
    else
        skip "batch worker environment: this host does not select the 'batch' capability, so no agent-job-worker@ unit exists here; the worker's endpoint and credential-denylist assertions are a batch host's property"
    fi

    local path
    for path in /home/agent/.ssh/id_ed25519 /home/agent/.ssh/id_rsa \
        /home/agent/.aws /home/agent/.config/gcloud /home/agent/.kube \
        /home/agent/.password-store /home/agent/.gnupg \
        /var/run/docker.sock /run/docker.sock /run/podman/podman.sock \
        /nix/var/nix/daemon-socket/socket; do
        check_denied "guest has no $path" "$slot" test -e "$path"
    done
    # /run/dbus/system_bus_socket is deliberately NOT in the list above: the
    # guest runs its OWN systemd and therefore its OWN system bus, so the socket
    # EXISTING is expected and is not a leak. The property that matters is that
    # no HOST bus is shared in — which the share-set assertion in section_boot
    # already covers (exactly four virtiofs shares, none of them /run/dbus).
    # Assert the cheap positive fact here instead: if the socket exists it lives
    # on a guest-local filesystem, not on a virtiofs share.
    # shellcheck disable=SC2016  # $p / the command substitution run in the GUEST
    check "the guest's D-Bus socket is guest-local (not a shared-in host bus)" \
        guest "$slot" sh -c 'p=/run/dbus/system_bus_socket; [ -e "$p" ] || exit 0; [ "$(findmnt -n -o FSTYPE -T "$p")" != virtiofs ]'
    check_denied "guest has no git credential helper configured" "$slot" \
        sh -c "git config --get credential.helper"
    check_denied "guest cannot read the host operator's home" "$slot" \
        sh -c "ls /home | grep -qv '^agent$'"
    cleanup_task rtv-creds
}

# --- (5) lifecycle failures ----------------------------------------------
section_lifecycle() {
    section "lifecycle failure handling (ticket 6 A.5)"
    local class slot
    class="$(all_classes | head -1)"

    # (a) clone creation fails (workspace already exists).
    cleanup_task rtv-life
    mkdir -p "$WORKSPACE_ROOT/rtv-life"
    if "$LAUNCHER" run --name rtv-life --repository "$REPO" --agent pi \
        --resource-class "$class" >/dev/null 2>&1; then
        fail "clone-creation failure is detected"
    else
        pass "clone-creation failure is detected"
    fi
    check_no_residue "after a failed clone"
    rmdir "$WORKSPACE_ROOT/rtv-life" 2>/dev/null || rm -rf "$WORKSPACE_ROOT/rtv-life"

    # (b) repository validation fails.
    check_fails "a non-repository is rejected" \
        "$LAUNCHER" run --name rtv-life2 --repository /tmp --agent pi --resource-class "$class"
    check_fails "the workspace root itself is rejected as a repository" \
        "$LAUNCHER" run --name rtv-life3 --repository "$WORKSPACE_ROOT" --agent pi --resource-class "$class"
    check_no_residue "after a rejected repository"

    # (c) launcher termination mid-run: SIGKILL the launcher, then recover.
    #
    # The prompt file must be a real, NON-EMPTY regular file: the launcher
    # rejects `--prompt-file /dev/null` twice over (not a regular file, and
    # empty), so the submit this subtest used to start died in milliseconds and
    # the kill 15s later hit nothing at all — no slot was ever allocated, no
    # clone was ever created, and both assertions below tested nothing.
    cleanup_task rtv-kill
    local kill_prompt kslot="" waited=0
    kill_prompt="$(mktemp /tmp/rtv-kill-prompt.XXXXXX)"
    printf 'Print the word ok and exit.\n' >"$kill_prompt"
    "$LAUNCHER" submit --name rtv-kill --repository "$REPO" --agent pi \
        --prompt-file "$kill_prompt" --timeout 60 >/tmp/rtv-kill-submit.log 2>&1 &
    local pid=$!
    # Kill the launcher only once it REALLY got as far as allocating a slot and
    # creating the clone — otherwise there is nothing to recover and the
    # subtest must say so instead of silently passing/failing.
    while ((waited < 120)); do
        kslot="$(slot_of_task rtv-kill)"
        [[ -n $kslot && -d "$WORKSPACE_ROOT/rtv-kill/.git" ]] && break
        kill -0 "$pid" 2>/dev/null || break
        sleep 3
        waited=$((waited + 3))
    done
    if [[ -z $kslot || ! -d "$WORKSPACE_ROOT/rtv-kill/.git" ]]; then
        kill -9 "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
        skip "killed launcher: the submit never allocated a slot and created a clone (see /tmp/rtv-kill-submit.log), so killing it would have tested nothing"
    else
        pass "killed launcher: the submit allocated $kslot and created the clone before the kill"
        kill -9 "$pid" 2>/dev/null || true
        wait "$pid" 2>/dev/null || true
        info "launcher killed; recovering"
        "$LAUNCHER" recover --dry-run | sed 's/^/#     /'
        "$LAUNCHER" recover | sed 's/^/#     /'
        check_no_residue "after a killed launcher + recover"
        if [[ -d "$WORKSPACE_ROOT/rtv-kill" ]]; then
            pass "the workspace clone survived the killed launcher"
        else
            fail "the workspace clone was lost"
        fi
    fi
    rm -f "$kill_prompt"
    cleanup_task rtv-kill

    # (d) guest crash: hard-stop the VM under a running slot.
    cleanup_task rtv-crash
    local rc=0
    slot="$(start_task rtv-crash "$class")" || rc=$?
    if ((rc == 0)) && [[ -n $slot ]]; then
        systemctl kill --signal=SIGKILL "microvm@$slot.service" || true
        sleep 5
        "$LAUNCHER" recover | sed 's/^/#     /'
        check_no_residue "after a guest crash + recover"
        cleanup_task rtv-crash
    else
        report_start_failure "$rc" "guest crash"
        cleanup_task rtv-crash
    fi

    # (e) the slot must be reusable afterwards.
    cleanup_task rtv-reuse
    rc=0
    slot="$(start_task rtv-reuse "$class")" || rc=$?
    if ((rc == 0)) && [[ -n $slot ]]; then
        pass "a slot is reusable after the failure battery ($slot)"
        cleanup_task rtv-reuse
    else
        report_start_failure "$rc" "slot reuse after the failure battery"
        cleanup_task rtv-reuse
    fi
}

# No slot may stay falsely allocated, no VM may stay up, no bind mount and no
# runtime job data may remain.
#
# SCOPING (this used to be the bug): `list`/`status`/`recover` iterate the
# CURRENT slot pool only, so globbing $RUNTIME_ROOT/slots/*/ conflated two very
# different situations — residue the run under test left behind (a real FAIL),
# and per-slot state under a slot name from an EARLIER generation's naming (e.g.
# `agent-0` before the `agent-<class>-<i>` rename), which the launcher never
# looks at. That produced FAIL lines immediately followed by a listing showing
# every slot `<free>` and `recover: nothing to recover`. The two cases are now
# reported separately: current-slot residue FAILS, foreign-slot state is a
# clearly labelled diagnostic.
check_no_residue() {
    local when="$1" residue=0 f slot mp
    local -a foreign_state=() foreign_mounts=()
    for f in "$RUNTIME_ROOT"/slots/*/session.json; do
        [[ -e $f ]] || continue
        slot="$(basename -- "$(dirname -- "$f")")"
        if is_current_slot "$slot"; then
            residue=1
            info "allocation marker still present for current slot $slot"
        else
            foreign_state+=("$slot")
        fi
    done
    if ((residue)); then
        fail "no slot stays allocated $when"
        "$LAUNCHER" list | sed 's/^/#     /'
    else
        pass "no slot stays allocated $when"
    fi

    residue=0
    while read -r mp; do
        [[ -n $mp ]] || continue
        slot="$(slot_of_mount "$mp")"
        if is_current_slot "$slot"; then
            residue=1
            info "workspace bind mount still present for current slot $slot ($mp)"
        else
            foreign_mounts+=("$mp")
        fi
    done < <(findmnt -rn -o TARGET | grep -E "$WORKSPACE_MOUNT_RE" || true)
    if ((residue)); then
        fail "no stale workspace bind mount remains $when"
    else
        pass "no stale workspace bind mount remains $when"
    fi

    # The SECOND bind a slot can hold: the agent-state share
    # (<runtimeRoot>/state/slots/<slot>, ticket 5 B). It is held by the same
    # per-slot virtiofsd as the workspace bind, so a SIGKILLed guest leaks it
    # the same way — and scanning only for `<stateRoot>/<slot>/workspace` made
    # that leak invisible to this suite exactly as it was invisible to
    # `recover`.
    residue=0
    while read -r mp; do
        [[ -n $mp ]] || continue
        slot="$(slot_of_mount "$mp")"
        if is_current_slot "$slot"; then
            residue=1
            info "agent-state bind mount still present for current slot $slot ($mp)"
        else
            foreign_mounts+=("$mp")
        fi
    done < <(findmnt -rn -o TARGET | grep -E "$STATE_MOUNT_RE" || true)
    if ((residue)); then
        fail "no stale agent-state bind mount remains $when"
    else
        pass "no stale agent-state bind mount remains $when"
    fi

    if find "$JOBS_ROOT" -name spec.json 2>/dev/null | grep -q .; then
        fail "no stale job spec remains $when"
    else
        pass "no stale job spec remains $when"
    fi

    # Per-slot state whose slot name is NOT in the current pool: left over from a
    # generation with a different slot naming, never residue of the run under
    # test, and never iterated by any launcher command — hence a separate,
    # clearly labelled diagnostic instead of a FAIL.
    if ((${#foreign_state[@]} + ${#foreign_mounts[@]})); then
        info "FOREIGN SLOT STATE $when (slot names that are not in the current pool):"
        for slot in "${foreign_state[@]+"${foreign_state[@]}"}"; do
            info "  allocation marker under an unknown slot name: $RUNTIME_ROOT/slots/$slot"
        done
        for mp in "${foreign_mounts[@]+"${foreign_mounts[@]}"}"; do
            info "  workspace bind mount under an unknown slot name: $mp"
        done
        info "  this is NOT residue of the run under test: no launcher command"
        info "  ITERATES these names — but 'recover' must still REPORT them."
        # Cross-check the launcher's own reporting: state the launcher cannot see
        # is state nobody will ever clean up, so its absence from `recover` is a
        # real finding (this is the property the launcher's --prune-foreign path
        # exists for).
        local reported
        reported="$("$LAUNCHER" recover --dry-run 2>/dev/null | grep '^foreign:' || true)"
        if [[ -n $reported ]]; then
            pass "the launcher reports the foreign slot state ($LAUNCHER recover)"
            printf '%s\n' "$reported" | sed 's/^/#     /'
            info "  remove it with: $LAUNCHER recover --prune-foreign"
        else
            fail "the launcher does not report the foreign slot state (recover --dry-run said nothing about it)"
        fi
    fi
}

# --- (6) malicious repository --------------------------------------------
section_malrepo() {
    section "hostile repository fixture (ticket 6 A.6)"
    local dir class slot
    dir="$(mktemp -d /tmp/rtv-malrepo.XXXXXX)"
    git -C "$dir" init -q
    # A hook that would run on clone/checkout if hooks were ever honoured.
    mkdir -p "$dir/.git/hooks"
    cat >"$dir/.git/hooks/post-checkout" <<'HOOK'
#!/bin/sh
touch /tmp/rtv-HOOK-RAN
HOOK
    chmod +x "$dir/.git/hooks/post-checkout"
    # Nix / direnv / MCP files that must never be evaluated by the HOST.
    #
    # NOTE on flake.nix: a Nix EVALUATION cannot create a marker file (there is
    # no such builtin), so "the host never evaluated flake.nix" has no direct
    # observable. The fixture keeps a flake that would `throw` — so an
    # evaluation would be LOUD in the launcher output — but this suite makes no
    # assertion about it any more: the assertion that used to carry that name
    # actually tested the DIRENV marker (see below) and is now named after what
    # it measures.
    printf '{ outputs = _: builtins.throw "rtv-flake-evaluated"; }\n' >"$dir/flake.nix"
    printf 'touch /tmp/rtv-DIRENV-RAN\n' >"$dir/.envrc"
    printf 'export RTV_DIRENV=1\n' >>"$dir/.envrc"
    printf '{"mcpServers":{"evil":{"command":"touch","args":["/tmp/rtv-MCP-RAN"]}}}\n' >"$dir/.mcp.json"
    # Symlink escapes and a nested repository.
    ln -s /etc/shadow "$dir/escape-shadow"
    ln -s / "$dir/escape-root"
    git -C "$dir" init -q nested
    # A .git FILE pointing outside the workspace.
    mkdir -p "$dir/gitfile-escape"
    printf 'gitdir: /etc\n' >"$dir/gitfile-escape/.git"
    git -C "$dir" add -A >/dev/null 2>&1 || true
    git -C "$dir" -c user.email=rtv@example.invalid -c user.name=rtv \
        commit -qm "hostile fixture" >/dev/null 2>&1 || true
    rm -f /tmp/rtv-HOOK-RAN /tmp/rtv-DIRENV-RAN /tmp/rtv-MCP-RAN

    class="$(all_classes | head -1)"
    cleanup_task rtv-mal
    # The HOSTILE fixture must be the source repository — this used to start the
    # guest from $REPO, so nothing of the fixture ever reached the guest and the
    # symlink assertion below passed vacuously (there was no such path at all).
    local rc=0
    slot="$(start_task_in "$dir" rtv-mal "$class")" || rc=$?
    if ((rc != 0)) || [[ -z $slot ]]; then
        report_start_failure "$rc" "hostile repository"
        cleanup_task rtv-mal
        rm -rf "$dir"
        return
    fi
    # These three run on the HOST, so check_fails is the right tool.
    check_fails "the host never ran the repository's git hook" test -e /tmp/rtv-HOOK-RAN
    check_fails "the host never sourced the repository's .envrc (direnv)" test -e /tmp/rtv-DIRENV-RAN
    check_fails "the host never ran the repository's MCP command" test -e /tmp/rtv-MCP-RAN
    assert_transport "$slot" "hostile repository"
    # POSITIVE CONTROL: the fixture's symlink really is in the guest's workspace.
    # Without it, "the guest cannot read /etc/shadow through it" says nothing.
    if guest "$slot" test -L /workspace/escape-shadow >/dev/null 2>&1; then
        pass "the hostile fixture reached the guest (/workspace/escape-shadow is a symlink)"
        # Inside the guest the symlink must not become a path out of the workspace.
        check_denied "a symlink in the repo does not expose /etc/shadow to the guest" "$slot" \
            sh -c "cat /workspace/escape-shadow"
    else
        skip "hostile repository: the fixture's symlink is not in the guest workspace, so the escape check is undecidable"
    fi
    # Guest limits must contain a fork bomb and a disk filler; both must leave
    # the VM (and the host) alive.
    guest "$slot" sh -c ':(){ :|:& };: 2>/dev/null &' >/dev/null 2>&1 || true
    sleep 5
    check "the guest is still reachable after a fork bomb" guest "$slot" true
    # The workspace is a BIND MOUNT of a host directory, so this writes onto the
    # HOST filesystem. Writing a fixed 20 GiB could wedge the host under test on
    # a small /var — which the very next check then reports as a host-health
    # failure caused by the suite itself. Bound the attempt by what the host can
    # spare (at most 2 GiB, and only if 4 GiB remain free afterwards).
    local avail_mb fill_mb=0
    avail_mb="$(df -Pm --output=avail "$WORKSPACE_ROOT" 2>/dev/null | tail -1 | tr -dc '0-9')"
    if [[ -n ${avail_mb:-} ]] && ((avail_mb > 6144)); then
        fill_mb=2048
        ((avail_mb - fill_mb > 4096)) || fill_mb=$((avail_mb - 4096))
    fi
    if ((fill_mb > 0)); then
        guest "$slot" sh -c "dd if=/dev/zero of=/workspace/rtv-fill bs=1M count=$fill_mb 2>/dev/null || true" >/dev/null 2>&1 || true
        check "the guest is still reachable after a disk-filling attempt (${fill_mb} MiB)" guest "$slot" true
    else
        skip "disk-filling attempt: the host has too little free space under $WORKSPACE_ROOT (${avail_mb:-unknown} MiB) to run it safely"
    fi
    check "the host is still healthy (systemd is running)" systemctl is-system-running --quiet
    guest "$slot" sh -c 'rm -f /workspace/rtv-fill' >/dev/null 2>&1 || true
    check_denied "the guest cannot enumerate host block devices" "$slot" \
        sh -c "ls /dev/sd* /dev/nvme* 2>/dev/null | grep -q ."
    cleanup_task rtv-mal
    rm -rf "$dir"
}

# --- (7) batch result channel: forgery, stale results, timeout, cancel -----
# THIS is the section that measures the trust split introduced with spec v2:
# the guest CONTROLLER (root) is the only writer of
# /run/agent-job/controller/result.json, and the WORKER (uid 1000, the same
# identity as the coding agent and every repository process) must not be able to
# influence it in any way.
#
# Everything here needs a booted guest, because it is the guest KERNEL that
# enforces the ownership/permission split over virtiofs. The eval/build suite
# can only prove the layout and the validators (see
# tests/microvm-batch-result-integrity.sh).
section_forgery() {
    section "batch result channel: forgery + stale results (ticket 7)"
    local class slot task prompt
    class="$(all_classes | head -1)"
    prompt="$(mktemp /tmp/rtv-prompt.XXXXXX)"
    # A prompt whose agent run is irrelevant: what matters is what the guest
    # user can do to the result channel WHILE a job is running.
    printf 'Print the word ok and exit.\n' >"$prompt"

    # A long-running batch job, so the forgery attempts happen while the
    # controller is still supervising the worker.
    task=rtv-forge
    cleanup_task "$task"
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 300 --resource-class "$class" \
        >/tmp/rtv-forge-submit.log 2>&1 &
    local submit_pid=$!
    # Wait for the slot to appear and the guest to be reachable (shared gate).
    local waited=0 slot="" rc=0
    slot="$(wait_task_slot_ready "$task")" || rc=$?
    if ((rc != 0)) || [[ -z $slot ]]; then
        if ((rc == 2)); then
            skip "forgery: slot $slot came up for $task but the guest never accepted SSH within ${READY_TIMEOUT}s"
        else
            skip "forgery: no slot came up for $task"
        fi
        kill "$submit_pid" 2>/dev/null || true
        wait "$submit_pid" 2>/dev/null || true
        rm -f "$prompt"
        return
    fi
    info "forgery target: slot $slot (task $task)"
    assert_transport "$slot" "batch result channel"

    # --- (A) direct result forgery ---------------------------------------
    # check_denied (not check_fails): a dead SSH channel must not be mistaken
    # for a successful denial.
    check_denied "the guest agent cannot write the authoritative result" "$slot" \
        sh -c "echo '{\"version\":2,\"state\":\"completed\"}' > $GUEST_CTRL_DIR/result.json"
    check_denied "the guest agent cannot even LIST the controller directory" "$slot" \
        sh -c "ls $GUEST_CTRL_DIR"
    check_denied "the guest agent cannot read the controller result" "$slot" \
        sh -c "cat $GUEST_CTRL_DIR/result.json"
    check_denied "the guest agent cannot read the job spec (it holds the allocation token)" "$slot" \
        sh -c "cat $GUEST_INPUT_DIR/spec.json"
    check_denied "the guest agent cannot delete the controller result" "$slot" \
        sh -c "rm -f $GUEST_CTRL_DIR/result.json"
    check_denied "the guest agent cannot modify its own job spec" "$slot" \
        sh -c "echo x >> $GUEST_INPUT_DIR/spec.json"
    check_denied "the guest agent cannot modify its own prompt" "$slot" \
        sh -c "echo x >> $GUEST_INPUT_DIR/prompt.md"
    check_denied "the guest agent cannot forge a cancellation request" "$slot" \
        sh -c "echo '{}' > $GUEST_INPUT_DIR/cancel.json"
    # The allocation token must not be readable off a running process either:
    # /proc/<pid>/cmdline is 0444, so nothing may carry the token in its argv.
    # (The controller's processes are additionally hidden from the worker by
    # ProtectProc=invisible.)
    # Search for the SHAPE of a token (64 hex chars) rather than the value, so
    # the check itself never discloses it to the guest.
    check_denied "no 64-hex allocation token is visible in any /proc/*/cmdline" "$slot" \
        sh -c "grep -aoE '[0-9a-f]{64}' /proc/*/cmdline 2>/dev/null | grep -q ."
    check_denied "the guest agent cannot see the controller's processes at all" "$slot" \
        sh -c "grep -a -l agent-job-controller /proc/*/cmdline 2>/dev/null | grep -q ."

    # --- (B) directory replacement ---------------------------------------
    check_denied "the guest agent cannot rename the controller directory" "$slot" \
        sh -c "mv $GUEST_CTRL_DIR $GUEST_JOB_DIR/stolen"
    check_denied "the guest agent cannot remove the controller directory" "$slot" \
        sh -c "rmdir $GUEST_CTRL_DIR"
    check_denied "the guest agent cannot shadow the controller directory with a symlink" "$slot" \
        sh -c "ln -sfn $GUEST_WORKER_DIR $GUEST_JOB_DIR/controller"
    check_denied "the guest agent cannot create anything in the job share root" "$slot" \
        sh -c "touch $GUEST_JOB_DIR/x"
    check_denied "the guest agent cannot rename the input directory" "$slot" \
        sh -c "mv $GUEST_INPUT_DIR $GUEST_JOB_DIR/stolen-input"

    # --- (B2) the worker's own LOG files are root-owned -------------------
    # systemd opens them as root and follows symlinks, so the worker must not be
    # able to write them, replace them with a symlink, or rename their
    # directory (which is why they do NOT live inside the agent-owned worker/).
    check_denied "the guest agent cannot overwrite its own stdout log" "$slot" \
        sh -c "echo forged > $GUEST_WORKER_LOGS_DIR/stdout.log"
    check_denied "the guest agent cannot replace a log file with a symlink" "$slot" \
        sh -c "ln -sfn /etc/passwd $GUEST_WORKER_LOGS_DIR/stdout.log"
    check_denied "the guest agent cannot rename the worker log directory" "$slot" \
        sh -c "mv $GUEST_WORKER_LOGS_DIR $GUEST_JOB_DIR/stolen-logs"
    check_denied "the guest agent cannot create a new file in the worker log directory" "$slot" \
        sh -c "touch $GUEST_WORKER_LOGS_DIR/x"
    if [[ "$(stat -c '%u %a' "$JOBS_ROOT/$slot/worker-logs")" == "0 755" ]]; then
        pass "the worker log directory is root:root 0755 on the host side"
    else
        fail "the worker log directory changed owner/mode: $(stat -c '%u %a' "$JOBS_ROOT/$slot/worker-logs")"
    fi

    # --- (C) worker-side fake results ------------------------------------
    # The worker MAY write these; the host must ignore them entirely.
    guest "$slot" sh -c "echo '{\"version\":2,\"state\":\"completed\",\"exitCode\":0}' > $GUEST_WORKER_DIR/result.json" \
        >/dev/null 2>&1 || true
    guest "$slot" sh -c "echo '{\"version\":2,\"state\":\"completed\",\"exitCode\":0}' > /workspace/result.json" \
        >/dev/null 2>&1 || true
    check "a worker-written result in worker/ is possible (it is untrusted output)" \
        test -f "$JOBS_ROOT/$slot/worker/result.json"
    # ... and the controller-owned result is still absent or controller-owned.
    if [[ -e "$JOBS_ROOT/$slot/controller/result.json" ]]; then
        if [[ "$(stat -c %u "$JOBS_ROOT/$slot/controller/result.json")" == "0" ]]; then
            pass "the authoritative result is still owned by the guest controller (uid 0)"
        else
            fail "the authoritative result is NOT root-owned any more"
        fi
    else
        pass "no authoritative result exists yet (the controller is still supervising)"
    fi
    if [[ "$(stat -c '%u %a' "$JOBS_ROOT/$slot/controller")" == "0 700" ]]; then
        pass "the controller directory is still root:root 0700"
    else
        fail "the controller directory changed owner/mode: $(stat -c '%u %a' "$JOBS_ROOT/$slot/controller")"
    fi

    # --- (G) forged early completion must not end the job ----------------
    sleep 5
    if kill -0 "$submit_pid" 2>/dev/null; then
        pass "the host is still waiting despite the forged worker results"
    else
        fail "the host stopped waiting after a forged worker result"
    fi

    # Let the real job finish and check the outcome the HOST reports.
    wait "$submit_pid" 2>/dev/null || true
    if [[ -f "$RESULTS_DIR/$task.json" ]]; then
        local src state
        src="$(jq -r '.source // ""' "$RESULTS_DIR/$task.json")"
        state="$(jq -r '.state // ""' "$RESULTS_DIR/$task.json")"
        info "archived result: state=$state source=$src"
        if [[ $src == "controller" ]]; then
            pass "the archived result came from the guest CONTROLLER"
        else
            fail "the archived result did not come from the controller (source=$src)"
        fi
        if [[ $state == "completed" || $state == "failed" || $state == "timed-out" ]]; then
            pass "the archived state reflects a real controller verdict ($state)"
        else
            fail "unexpected archived state '$state'"
        fi
    else
        fail "no archived result for $task"
    fi
    cleanup_task "$task"

    # --- (D/E/F) stale, foreign and malformed results ---------------------
    # Plant documents in the CONTROLLER directory as root (i.e. give the
    # attacker strictly more power than a guest worker has) and check that the
    # host still refuses to accept them for the RUNNING allocation.
    #
    # The document MUST be planted AFTER the slot has been allocated: the
    # launcher's prepare_job deletes controller/result.json before it starts the
    # VM, so anything planted beforehand is gone by the time the host reads it
    # (and which slot the submit picks is not known in advance).
    task=rtv-stale
    cleanup_task "$task"
    local rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 120 --resource-class "$class" \
        >/tmp/rtv-stale-submit.log 2>&1 &
    submit_pid=$!
    slot="$(wait_task_slot "$task")" || slot=""
    if [[ -z $slot ]]; then
        skip "stale result: no slot came up for $task"
        kill "$submit_pid" 2>/dev/null || true
        wait "$submit_pid" 2>/dev/null || true
    else
        # A syntactically PERFECT result — but with a foreign allocation token.
        jq -n '{version:2, controllerVersion:1, taskId:"rtv-stale",
                allocationToken:"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
                slot:$slot, agent:"pi", state:"completed", exitCode:0,
                startedAt:"2020-01-01T00:00:00Z", finishedAt:"2020-01-01T00:00:01Z",
                timedOut:false, message:"stale"}' --arg slot "$slot" \
            >"$JOBS_ROOT/$slot/controller/result.json"
        chmod 0600 "$JOBS_ROOT/$slot/controller/result.json"
        rc=0
        wait "$submit_pid" || rc=$?
        local archived_msg archived_src archived_state
        archived_msg="$(jq -r '.message // ""' "$RESULTS_DIR/$task.json" 2>/dev/null || true)"
        archived_src="$(jq -r '.source // ""' "$RESULTS_DIR/$task.json" 2>/dev/null || true)"
        archived_state="$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null || true)"
        if [[ $archived_msg == "stale" ]]; then
            fail "the host ACCEPTED a planted result with a foreign allocation token (exit $rc)"
        elif grep -q "allocation token does not belong" /tmp/rtv-stale-submit.log; then
            pass "a result with a foreign allocation token is REJECTED by name (exit $rc)"
            if ((rc == 70)); then
                pass "the rejection is reported as an infrastructure error (exit 70)"
            else
                fail "a rejected result did not produce exit 70 (got $rc)"
            fi
        elif [[ $archived_src == "controller" ]]; then
            # The real controller finished before the document was planted.
            skip "stale result: the genuine controller result (state=$archived_state) won the race; the planted document was never read"
        else
            fail "the planted result was neither rejected by name nor superseded (exit $rc, source=$archived_src, state=$archived_state)"
        fi
    fi
    cleanup_task "$task"

    # A malformed result must become an INFRASTRUCTURE error (exit 70), never
    # a success.
    task=rtv-malformed
    cleanup_task "$task"
    rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 60 --resource-class "$class" \
        >/tmp/rtv-malformed-submit.log 2>&1 &
    submit_pid=$!
    slot="$(wait_task_slot "$task")" || slot=""
    if [[ -n $slot ]]; then
        printf '{ "version": 2, "state": ' >"$JOBS_ROOT/$slot/controller/result.json"
        chmod 0600 "$JOBS_ROOT/$slot/controller/result.json"
    fi
    rc=0
    wait "$submit_pid" || rc=$?
    if [[ -z $slot ]]; then
        # Nothing was planted, so the exit code says nothing about malformed
        # documents — asserting 70 here would have tested the empty pool.
        skip "malformed result: no slot came up for $task, so no malformed document was ever planted (exit $rc)"
    elif ((rc == 70)); then
        pass "a malformed result becomes an infrastructure error (exit 70)"
    else
        fail "a malformed result did not produce exit 70 (got $rc)"
    fi
    cleanup_task "$task"

    # --- (H) deadline enforcement -----------------------------------------
    # A long-running DOUBLE-FORKED descendant is planted in the guest so the
    # teardown has something to kill that is not the worker's direct child. Its
    # existence is a POSITIVE CONTROL and is asserted; whether it died *with the
    # worker cgroup* specifically is NOT measurable from here, and is reported
    # as such rather than claimed (the host stops the whole VM as part of the
    # timeout path, so "the cgroup was killed" and "the VM was destroyed" have
    # the same observable).
    task=rtv-timeout
    cleanup_task "$task"
    rc=0
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 60 --resource-class "$class" \
        >/tmp/rtv-timeout-submit.log 2>&1 &
    submit_pid=$!
    local planted=0 timeout_slot=""
    rc=0
    timeout_slot="$(wait_task_slot_ready "$task")" || rc=$?
    if ((rc == 0)) && [[ -n $timeout_slot ]]; then
        guest "$timeout_slot" sh -c 'setsid sh -c "sleep 3600 # rtv-orphan" >/dev/null 2>&1 &' >/dev/null 2>&1 || true
        if guest "$timeout_slot" sh -c "pgrep -f 'sleep 3600' >/dev/null" >/dev/null 2>&1; then
            planted=1
            pass "a double-forked descendant really exists in the guest while the job runs"
        else
            fail "the double-forked descendant could not be planted, so nothing about the teardown of orphans can be decided"
        fi
    else
        info "timeout: no reachable slot for $task (rc $rc)"
    fi
    rc=0
    wait "$submit_pid" || rc=$?
    if ((rc == 124)); then
        pass "a job that exceeds its deadline reports timed-out (exit 124)"
    else
        fail "a timed-out job did not produce exit 124 (got $rc)"
    fi
    if [[ -f "$RESULTS_DIR/$task.json" ]] &&
        [[ "$(jq -r '.state' "$RESULTS_DIR/$task.json")" == "timed-out" ]] &&
        [[ "$(jq -r '.source' "$RESULTS_DIR/$task.json")" == "controller" ]]; then
        pass "the timeout verdict came from the guest controller"
    else
        info "archived: $(cat "$RESULTS_DIR/$task.json" 2>/dev/null)"
        fail "the timeout verdict did not come from the controller"
    fi
    if ((planted)); then
        # What IS decidable: after the timeout the slot must be released and its
        # VM gone, so no descendant of the worker survives ON THE HOST.
        if [[ -n "$(slot_of_task "$task")" ]] ||
            "$LAUNCHER" list | awk -v s="$timeout_slot" '$1 == s && $3 == "running"' | grep -q .; then
            fail "the timed-out job left $timeout_slot allocated/running, so its processes outlived the deadline"
        else
            pass "the timed-out job released $timeout_slot and stopped its VM (no worker descendant survives on the host)"
        fi
        skip "whether the timeout killed the worker CGROUP specifically: the host stops the whole VM on the timeout path, so this cannot be distinguished from the VM teardown from outside the guest"
    fi
    cleanup_task "$task"

    # --- (I) cancellation is bound to the allocation token ----------------
    task=rtv-cancel
    local cancel_waited=0 cancel_state="" plant_waited=0
    cleanup_task "$task"
    "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
        --prompt-file "$prompt" --timeout 300 --resource-class "$class" \
        >/tmp/rtv-cancel-submit.log 2>&1 &
    submit_pid=$!
    slot="$(wait_task_slot "$task")" || slot=""
    if [[ -z $slot ]]; then
        skip "cancellation: no slot came up"
        kill "$submit_pid" 2>/dev/null || true
    else
        # The worker must actually be RUNNING before we cancel: cancelling a
        # job whose worker has not started yet exercises the controller's
        # "cancelled before the worker started" path instead, and a cancel that
        # races the very first controller poll is not what test I is about.
        cancel_waited=0
        while ((cancel_waited < 60)); do
            [[ "$(jq -r '.state // ""' "$JOBS_ROOT/$slot/controller/state.json" 2>/dev/null)" == running ]] && break
            sleep 2
            cancel_waited=$((cancel_waited + 2))
        done
        info "cancelling after ${cancel_waited}s (controller phase: $(jq -r '.state // "<none>"' "$JOBS_ROOT/$slot/controller/state.json" 2>/dev/null))"
        # Keep a copy of the cancellation request, then cancel for real.
        "$LAUNCHER" cancel "$task" >/tmp/rtv-cancel.log 2>&1 || true
        cp "$JOBS_ROOT/$slot/input/cancel.json" /tmp/rtv-cancel-request.json 2>/dev/null || true
        rc=0
        wait "$submit_pid" || rc=$?
        cancel_state="$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)"
        if [[ $cancel_state == cancelled ]]; then
            pass "cancellation is recorded as 'cancelled'"
        else
            fail "cancellation was not recorded (got '$cancel_state')"
            # DIAGNOSABILITY: `cleanup_task` below deletes the archived result,
            # so without this dump the next run cannot tell WHO wrote the wrong
            # verdict (the guest controller, the host `cancel`, or the still-
            # waiting `submit` overwriting cancel's archive). Print every
            # artefact that decides it, before it is destroyed.
            info "--- archived result ($RESULTS_DIR/$task.json) ---"
            jq -c . "$RESULTS_DIR/$task.json" 2>/dev/null | sed 's/^/#     /' ||
                info "  (no archived result)"
            info "--- cancel exit=$? log ---"
            sed 's/^/#     /' /tmp/rtv-cancel.log 2>/dev/null | tail -20
            info "--- submit exit=$rc log ---"
            sed 's/^/#     /' /tmp/rtv-cancel-submit.log 2>/dev/null | tail -20
            info "--- guest controller journal ---"
            "$LAUNCHER" ssh "$slot" -- journalctl -u agent-job-controller --no-pager -n 40 \
                2>/dev/null | sed 's/^/#     /' || info "  (guest already gone)"
        fi
        cleanup_task "$task"
        # Now replay the STALE cancellation request against a NEW allocation of
        # the same slot: the token no longer matches, so the new job must run.
        task=rtv-cancel-replay
        cleanup_task "$task"
        "$LAUNCHER" submit --name "$task" --repository "$REPO" --agent pi \
            --prompt-file "$prompt" --timeout 120 --resource-class "$class" \
            >/tmp/rtv-replay-submit.log 2>&1 &
        submit_pid=$!
        local replayed=0
        slot="$(wait_task_slot "$task")" || slot=""
        # `wait_task_slot` returns as soon as the slot is ALLOCATED, which can be
        # before the launcher has populated the job input directory — the reason
        # the previous run could not plant the stale request and had to SKIP.
        # Wait for the directory that must receive it.
        if [[ -n $slot ]]; then
            plant_waited=0
            while ((plant_waited < 60)) && [[ ! -d "$JOBS_ROOT/$slot/input" ]]; do
                sleep 2
                plant_waited=$((plant_waited + 2))
            done
        fi
        if [[ -n $slot && -f /tmp/rtv-cancel-request.json && -d "$JOBS_ROOT/$slot/input" ]]; then
            install -m 0400 -o root -g root /tmp/rtv-cancel-request.json \
                "$JOBS_ROOT/$slot/input/cancel.json"
            replayed=1
        else
            info "could not plant: slot='$slot' request=$([[ -f /tmp/rtv-cancel-request.json ]] && echo yes || echo NO) inputdir=$([[ -n $slot && -d "$JOBS_ROOT/$slot/input" ]] && echo yes || echo NO)"
        fi
        rc=0
        wait "$submit_pid" || rc=$?
        if ((!replayed)); then
            # Without a planted request there was no replay to survive.
            skip "stale cancellation replay: the request could not be planted (slot='$slot'), so the new allocation was never attacked"
        elif [[ "$(jq -r '.state // ""' "$RESULTS_DIR/$task.json" 2>/dev/null)" == "cancelled" ]]; then
            fail "a STALE cancellation request stopped a newly allocated job"
        else
            pass "a stale cancellation request does not affect a new allocation"
        fi
        cleanup_task "$task"
    fi
    rm -f "$prompt" /tmp/rtv-cancel-request.json
}

# --- main -----------------------------------------------------------------
printf '%s: starting real-KVM validation (host %s, bridge %s, session root %s)\n' \
    "$PROG" "$(uname -n)" "$BRIDGE" "$SESSION_ROOT"
"$LAUNCHER" list | sed 's/^/#     /'

# --- runtime configuration staging (lightweight plan phase 3) ---------------
# HOST-ONLY section: it runs the generated stager `agent-microvm-stage-config`
# against REAL fixtures planted in the host home and inspects the staged tree
# plus the manifest. No VM is started.
#
# Why it lives HERE and not in `nix flake check`: the stager writes a root-owned
# tree (`install -o root -g root`), and the Nix build sandbox is not root. The
# eval/build checks can therefore only prove the policy is BAKED IN; whether it
# is ENFORCED is decided here.
#
# The stager is installed by every host that enables the feature; if it is not
# on PATH the whole section SKIPs.
# shellcheck disable=SC2317  # reached via the dispatch below
section_seed() {
    section "runtime configuration staging: allowlist + denylist enforcement"

    if ! command -v "$STAGER" >/dev/null; then
        skip "config-seed: $STAGER is not on PATH (this host does not use runtime config staging)"
        return
    fi
    # Staging REPLACES the slot's staged tree, so never touch a slot that is
    # currently serving a task.
    local slot
    slot="$("$LAUNCHER" list | awk '$3 != "running" { print $1; exit }')"
    if [[ -z $slot ]]; then
        skip "config-seed: every slot is running — refusing to restage a live slot"
        return
    fi

    # The payload lands in `<runtimeRoot>/sessions-ro/<slot>/config-seed` (see
    # the LAYOUT block at the top); the MANIFEST root is
    # `<runtimeRoot>/config-seed/<slot>`, outside every guest share.
    local payload manifest
    payload="$(config_seed_payload "$slot")"
    manifest="$RUNTIME_ROOT/config-seed/$slot/manifest.json"

    if ! "$STAGER" "$slot" >/tmp/rtv-seed-baseline.log 2>&1; then
        fail "config-seed: the stager failed on a clean run (see /tmp/rtv-seed-baseline.log)"
        return
    fi
    # HARD-FAIL, not a skip: every enforcement assertion below is of the form
    # "nothing forbidden is under $payload", so a payload path that does not
    # exist would make ALL of them pass vacuously. Deciding this ONCE, here,
    # makes that structurally impossible.
    if [[ ! -d $payload ]]; then
        fail "config-seed: the staged payload $payload does not exist after staging (see the LAYOUT block)"
        return
    fi
    if [[ ! -f $manifest ]]; then
        fail "config-seed: the staging manifest $manifest does not exist after staging"
        return
    fi
    pass "config-seed: the stager runs and writes $payload"

    # The stager BAKES the host home and the allowlist; read both back from the
    # manifest instead of guessing them here.
    local host_home fixture_root fixture secret_root
    host_home="$(jq -r '.hostHome // ""' "$manifest")"
    if [[ -z $host_home || ! -d $host_home ]]; then
        fail "config-seed: the manifest names no usable host home ('$host_home')"
        return
    fi
    if ! jq -e '.allowlist | index(".agents/skills")' "$manifest" >/dev/null; then
        skip "config-seed: '.agents/skills' is not allowlisted on this host — no directory to plant fixtures in"
        return
    fi
    fixture_root="$host_home/.agents/skills"
    fixture="$fixture_root/rtv-config-seed"
    # A credential-shaped tree OUTSIDE the allowlist, used as the TARGET of
    # benignly named symlinks (the interesting case: the link name passes the
    # denylist, the resolved name must not).
    secret_root="$host_home/.rtv-cfgseed-fixture"

    # Whatever happens below, the operator's home must be left as it was.
    local cleanup="rm -rf -- '$fixture' '$secret_root'"
    if [[ ! -d $fixture_root ]]; then
        mkdir -p "$fixture_root"
        # Only remove what this section created, and only if it stayed empty.
        cleanup="$cleanup; rmdir --ignore-fail-on-non-empty '$fixture_root' 2>/dev/null || true"
    fi
    # shellcheck disable=SC2064  # the paths must be expanded NOW, not on RETURN
    trap "$cleanup" RETURN

    rm -rf -- "$fixture" "$secret_root"
    mkdir -p "$fixture" "$secret_root/tokens"
    # (0) a benign file that MUST be staged (positive control: without it every
    #     negative check below could pass simply because nothing was staged).
    printf 'a skill\n' >"$fixture/good.md"
    # (1) a credential reached under a BENIGN name — the name-only denylist
    #     would stage this.
    printf 'TOKEN\n' >"$secret_root/auth.json"
    ln -s "$secret_root/auth.json" "$fixture/benign-config.md"
    # (2) ... and the same trick with a DIRECTORY.
    printf 'TOKEN\n' >"$secret_root/tokens/t"
    ln -s "$secret_root/tokens" "$fixture/notes"
    # (3) a credential-shaped NAME inside an allowlisted directory.
    printf 'sk-live\n' >"$fixture/prod-api-token.txt"
    # (4) an escape out of the host home.
    ln -s /etc/passwd "$fixture/escape.md"
    # (5) non-regular + setuid files.
    mkfifo "$fixture/pipe"
    printf 'x\n' >"$fixture/setuid.md"
    chmod u+s "$fixture/setuid.md"
    # (6) over the per-file budget.
    head -c 2000000 /dev/zero >"$fixture/big.md"

    if ! "$STAGER" "$slot" >/tmp/rtv-seed-fixture.log 2>&1; then
        fail "config-seed: the stager failed with the fixtures planted (see /tmp/rtv-seed-fixture.log)"
        return
    fi

    # `seed_reason <relative path>` — the manifest's skip reason, or the empty
    # string when the path was not skipped.
    seed_reason() {
        jq -r --arg p "$1" '[.skipped[] | select(.path == $p) | .reason] | first // ""' "$manifest"
    }
    # `check_reason <desc> <path> <expected reason infix>` — the path must have
    # been SKIPPED, and for the STATED reason (never "some reason", which a
    # generic failure would also satisfy).
    check_reason() {
        local desc="$1" path="$2" want="$3" got
        got="$(seed_reason "$path")"
        if [[ -e "$payload/$path" ]]; then
            fail "$desc (it was STAGED to $payload/$path)"
        elif [[ $got == *"$want"* ]]; then
            pass "$desc"
        else
            fail "$desc (not staged, but the manifest reason was '$got', expected '*$want*')"
        fi
    }

    local rel=".agents/skills/rtv-config-seed"
    check "config-seed: an allowlisted regular file IS staged" \
        test -f "$payload/$rel/good.md"
    check_reason "config-seed: a benignly NAMED symlink to a credential is refused" \
        "$rel/benign-config.md" "credential-shaped path"
    check_reason "config-seed: a benignly NAMED symlink to a credential DIRECTORY is refused" \
        "$rel/notes" "credential-shaped path"
    check_reason "config-seed: a credential-shaped file name is refused" \
        "$rel/prod-api-token.txt" "credential denylist"
    check_reason "config-seed: a symlink escaping the host home is refused" \
        "$rel/escape.md" "outside the host home"
    check_reason "config-seed: a setuid file is refused" \
        "$rel/setuid.md" "setuid/setgid"
    check_reason "config-seed: an over-budget file is refused" \
        "$rel/big.md" "larger than"
    check_fails "config-seed: a FIFO never reaches the staged tree" \
        test -e "$payload/$rel/pipe"
    # The credential CONTENT must not appear anywhere in the staged tree, no
    # matter which path it might have been copied under.
    if grep -rqI -- 'TOKEN' "$payload" 2>/dev/null; then
        fail "config-seed: credential content from the fixtures reached the staged tree"
    else
        pass "config-seed: no fixture credential content is anywhere in the staged tree"
    fi

    # The staged tree is root-owned and root-ONLY (the guest agent gets a COPY,
    # and no other host user may read the operator's configuration).
    if [[ -n "$(find "$payload" \! -user root -print -quit)" ]]; then
        fail "config-seed: the staged tree contains non-root-owned entries"
    else
        pass "config-seed: the staged tree is root-owned"
    fi
    if [[ -n "$(find "$payload" -perm /077 -print -quit)" ]]; then
        fail "config-seed: the staged tree is group/other-accessible"
    else
        pass "config-seed: the staged tree is not readable by group/other"
    fi
    # The manifest names the host home and every skipped, credential-SHAPED
    # host file name: it must stay OUTSIDE the share source (the payload).
    check_fails "config-seed: the manifest is not inside the guest-visible payload" \
        test -e "$payload/manifest.json"
    check "config-seed: the manifest is root-only" \
        test "$(stat -c '%U %a' "$manifest")" = "root 400"

    # CLEANED before every launch: removing the fixtures and restaging must
    # leave nothing of them behind.
    rm -rf -- "$fixture" "$secret_root"
    if "$STAGER" "$slot" >/tmp/rtv-seed-clean.log 2>&1; then
        check_fails "config-seed: a previous launch's staged files do not survive restaging" \
            test -e "$payload/$rel"
    else
        fail "config-seed: the stager failed on the cleanup run (see /tmp/rtv-seed-clean.log)"
    fi
}

# --- host-side model-endpoint preflight -------------------------------------
# Sections `net` and `forgery` are MEANINGLESS (and produce dozens of
# misleading FAILs) when the guest cannot reach the model API: a batch worker
# that dies in seconds fails every forgery subtest, and the two `net` endpoint
# checks fail for the same root cause. Probe the SAME bridge endpoint a guest
# would use (the host's `agent-litellm-proxy` socket forwarding to the loopback
# LiteLLM) ONCE, before any VM boots. The endpoint-dependent sections are then
# SKIPPED (under `--section all`) or HARD-ABORTED (when the operator asked for
# just `net`/`forgery`) with a precise reason instead of running them into the
# ground. `boot`/`l2`/`creds`/`lifecycle`/`malrepo` are still meaningful without
# the endpoint, so `--section all` always RUNS them.
# shellcheck disable=SC2317  # reached via the dispatch below
preflight_endpoint() {
    local url="http://$GATEWAY:$LITELLM_PORT/v1/models"
    local attempt
    # A COLD LiteLLM (DB init/migration on the first post-boot request) can
    # take a few seconds to answer even though it is healthy. Retry a bounded
    # number of times so a slow-but-working endpoint is not mistaken for a
    # dead one (matching the launcher's preflight_model_endpoint).
    for attempt in 1 2 3; do
        if curl -fsS -m 3 --connect-timeout 3 -o /dev/null "$url" 2>/dev/null; then
            pass "preflight: the model endpoint answers at $url"
            return 0
        fi
        ((attempt < 3)) && sleep 2
    done
    fail "preflight: the model endpoint is NOT reachable at $url"
    info "the guest agent cannot reach the model API; the endpoint-dependent"
    info "sections would only produce misleading failures. Diagnose on the host:"
    info "  sudo $LAUNCHER doctor"
    info "  systemctl is-active agent-litellm-proxy.socket litellm.service"
    info "  curl -fsS http://127.0.0.1:$LITELLM_PORT/v1/models   (the backend)"
    info "  ip -br addr show $BRIDGE                          (bridge + gateway)"
    return 1
}

# --- CAPABILITY detection (lightweight plan phase 5) ------------------------
# `myconfig.ai.microvm.capabilities` selects whether a host's guests carry the
# INTERACTIVE half (sshd + the per-slot host identity, hence every guest command
# this suite issues over `agent-microvm ssh`) and/or the BATCH half (the job
# controller/worker, hence `agent-microvm submit`). A section that needs a
# capability the host under test does not have would either fail on every
# subtest (no SSH channel) or — much worse — report VACUOUS PASSES for its
# "the guest must NOT be able to ..." checks.
#
# The set is ASKED FOR, not inferred: `agent-microvm capabilities` prints it
# machine-readably on EVERY host (`capabilities: <space-separated>` plus a
# `declared:` line), needs no root and starts nothing. The earlier detection
# grepped the launcher's REFUSAL messages for an English substring and defaulted
# to "this host has everything" — it therefore failed OPEN: rewording
# `require_capability`'s `die`, a `require_root` failure or a future transport
# change would silently restore exactly the vacuous passes this dispatch exists
# to prevent.
#
# So it fails CLOSED instead: an unparseable answer HARD-ABORTS the run. A
# launcher that does not know the subcommand (a host built before this phase)
# is aborted too, on purpose — an operator must not learn about a coverage hole
# from a green run.
INTERACTIVE_CAPABLE=0
BATCH_CAPABLE=0
SELECTED_CAPABILITIES=""
DECLARED_CAPABILITIES=""
detect_capabilities() {
    local out line cap
    if ! out="$("$LAUNCHER" capabilities 2>&1)"; then
        # shellcheck disable=SC2016  # the backticks are PROSE (the command name), not a substitution
        printf '%s: ABORTING: `%s capabilities` failed:\n' "$PROG" "$LAUNCHER" >&2
        printf '%s\n' "$out" >&2
        die "the capability set of the host under test could not be read, so no section can honestly be run or skipped"
    fi
    while IFS= read -r line; do
        case "$line" in
            "capabilities: "*) SELECTED_CAPABILITIES="${line#capabilities: }" ;;
            "declared: "*) DECLARED_CAPABILITIES="${line#declared: }" ;;
        esac
    done <<<"$out"
    [[ -n $DECLARED_CAPABILITIES ]] ||
        die "\`$LAUNCHER capabilities\` printed no 'declared:' line (got: $out) — refusing to guess what this host supports"
    [[ -n $SELECTED_CAPABILITIES ]] ||
        die "\`$LAUNCHER capabilities\` printed no non-empty 'capabilities:' line (got: $out) — the module rejects an empty selection, so this answer cannot be trusted"
    # Every capability THIS SUITE knows about must be declared by the launcher.
    # A missing token means the suite and the module disagree about what exists
    # (a renamed capability, a suite older than the module), which the gating
    # below would silently read as "not selected".
    for cap in interactive batch; do
        case " $DECLARED_CAPABILITIES " in
            *" $cap "*) ;;
            *) die "the host under test does not DECLARE the '$cap' capability this suite gates on (declared: $DECLARED_CAPABILITIES) — the suite and the module disagree about what exists" ;;
        esac
    done
    for cap in $SELECTED_CAPABILITIES; do
        case "$cap" in
            interactive) INTERACTIVE_CAPABLE=1 ;;
            batch) BATCH_CAPABLE=1 ;;
            *) die "the host under test selects the capability '$cap', which this suite does not know how to gate on — update the SECTION_CAPABILITIES table" ;;
        esac
    done
}

# --- section dispatch ------------------------------------------------------
# The eight sections, in the fixed order they always run under `--section all`.
# `net` and `forgery` depend on a reachable model endpoint; the rest do not.
ALL_SECTIONS=(boot net l2 creds lifecycle malrepo forgery seed)
ENDPOINT_SECTIONS=(net forgery)
is_endpoint_section() {
    local s
    for s in "${ENDPOINT_SECTIONS[@]}"; do
        [[ $s == "$1" ]] && return 0
    done
    return 1
}
# Which CAPABILITIES each section needs. Every section that issues a guest
# command needs `interactive` (the control channel IS ssh); `lifecycle` and
# `forgery` additionally submit batch jobs. `seed` exercises the HOST-side
# stager only, so it needs neither.
# `creds` needs only `interactive` because that is what its SECTION needs; its
# batch-worker-environment subtest is gated on `BATCH_CAPABLE` at its own call
# site (`creds_batch_worker_env`) and skips with the capability as the reason.
# A section-level `interactive batch` would have been wrong: it would drop the
# eleven credential-boundary assertions that DO hold on an interactive-only host.
declare -A SECTION_CAPABILITIES=(
    [boot]="interactive"
    [net]="interactive"
    [l2]="interactive"
    [creds]="interactive"
    [lifecycle]="interactive batch"
    [malrepo]="interactive"
    [forgery]="interactive batch"
    [seed]=""
)
# The capabilities <section> needs but the host under test does not have.
missing_capabilities_of() {
    local cap
    for cap in ${SECTION_CAPABILITIES[$1]-}; do
        case "$cap" in
            interactive) ((INTERACTIVE_CAPABLE)) || printf '%s\n' "$cap" ;;
            batch) ((BATCH_CAPABLE)) || printf '%s\n' "$cap" ;;
        esac
    done
}
# Resolve the requested $SECTION into the ordered list of sections to RUN.
resolve_sections() {
    case "$SECTION" in
        all) printf '%s\n' "${ALL_SECTIONS[@]}" ;;
        *) printf '%s\n' "$SECTION" ;;
    esac
}

# Validate the section name ONCE, in the MAIN shell. A `die` inside the process
# substitution below (`< <(resolve_sections)`) would only kill that subshell,
# leaving PLAN empty and the run silently validating nothing (a typo'd
# `--section bogus` would then report "0 passed, 0 failed" and exit 0). The
# name is therefore checked HERE, so an unknown section aborts the whole run.
case "$SECTION" in
    all) ;;
    boot | net | l2 | creds | lifecycle | malrepo | forgery | seed) ;;
    *) die "unknown --section '$SECTION'" ;;
esac

# Read the plan ONCE, before the preflight, so the resolved section list is
# printed up front (Gap 3): the operator can see exactly what is about to run
# instead of inferring it from the output. Greppable: one `info` line.
mapfile -t PLAN < <(resolve_sections)
info "sections: ${PLAN[*]}"

# Endpoint preflight. Under `--section all` an unreachable endpoint SKIPS the
# two endpoint-dependent sections (`net`, `forgery`) with a loud, counted reason
# and still RUNS the other five; the operator asked for everything and a dead
# endpoint must not silently validate nothing (Bug 2). When the operator asked
# for JUST an endpoint-dependent section (`--section net`/`--section forgery`),
# running it would be pointless, so the run HARD-ABORTS instead.
# Capability preflight (lightweight plan phase 5). Detected ONCE, before any VM
# boots, and reported: a section whose capability is absent is SKIPPED under
# `--section all` and HARD-ABORTS when the operator asked for exactly it, for
# the same reason the endpoint preflight does — a run that cannot exercise a
# property must say so, never pass it.
detect_capabilities
info "capabilities of the host under test: $SELECTED_CAPABILITIES (declared: $DECLARED_CAPABILITIES)"
ENDPOINT_DOWN=0
SKIPPED_SECTIONS=()
UNSUPPORTED_SECTIONS=()
if [[ $SECTION != all ]]; then
    mapfile -t missing < <(missing_capabilities_of "$SECTION")
    if ((${#missing[@]})); then
        printf '%s: ABORTING section %q: this host does not select the %s capability;\n' \
            "$PROG" "$SECTION" "${missing[*]}" >&2
        printf '%s: the section cannot exercise anything and its "must NOT be able to"\n' "$PROG" >&2
        printf '%s: checks would pass VACUOUSLY. Run it on a host whose\n' "$PROG" >&2
        printf '%s: myconfig.ai.microvm.capabilities includes it.\n' "$PROG" >&2
        exit 1
    fi
fi
if is_endpoint_section "$SECTION" || [[ $SECTION == all ]]; then
    if ! preflight_endpoint; then
        ENDPOINT_DOWN=1
        if [[ $SECTION == all ]]; then
            for s in "${ENDPOINT_SECTIONS[@]}"; do
                skip "section '$s' SKIPPED: the model endpoint is not reachable (run: sudo $LAUNCHER doctor); its checks would only produce misleading failures"
                SKIPPED_SECTIONS+=("$s")
            done
        else
            printf '%s: ABORTING section %q: the model endpoint is not reachable,\n' \
                "$PROG" "$SECTION" >&2
            printf '%s: running just this section would only produce misleading failures.\n' "$PROG" >&2
            printf '%s: fix the host backend first (run: sudo %s doctor), then re-run.\n' \
                "$PROG" "$LAUNCHER" >&2
            exit 1
        fi
    fi
fi

# Run each planned section in order. Per-section tallies (Gap 3) make a partial
# run diagnosable: the earlier real run that reported only forgery's checks was
# not distinguishable from a run where the other sections produced no output.
# Now each section announces its own pass/fail/skip delta as it completes, and
# the final summary lists the sections that actually RAN.
#
# `section_$s || rc=...` (not a bare call) is deliberate: under `set -e` a
# section function that returned non-zero would otherwise abort the WHOLE suite
# and swallow every later section. A section is expected to return 0 (it uses
# pass/fail/skip, and cleanup_task always succeeds), but a future bug that made
# one return non-zero must surface as a single section-level failure, not a
# silent truncation. This is the `set -e` class of defect the gap asked about.
RAN_SECTIONS=()
for s in "${PLAN[@]}"; do
    # An endpoint-dependent section that was skipped is NOT run.
    if [[ $ENDPOINT_DOWN == 1 ]] && is_endpoint_section "$s"; then
        continue
    fi
    # Neither is a section whose CAPABILITY this host does not have.
    mapfile -t missing < <(missing_capabilities_of "$s")
    if ((${#missing[@]})); then
        skip "section '$s' SKIPPED: this host does not select the ${missing[*]} capability, so its checks would pass vacuously"
        UNSUPPORTED_SECTIONS+=("$s")
        continue
    fi
    before_pass=$PASS before_fail=$FAIL before_skip=$SKIP
    rc=0
    "section_$s" || rc=$?
    if ((rc != 0)); then
        fail "section '$s' returned non-zero (rc $rc) — its checks may be incomplete; see the lines above"
    fi
    RAN_SECTIONS+=("$s")
    info "section $s: $((PASS - before_pass)) passed, $((FAIL - before_fail)) failed, $((SKIP - before_skip)) skipped"
done

# --- final summary ---------------------------------------------------------
# Make it obvious which sections actually RAN and which were SKIPPED, so a
# partial run cannot be mistaken for a complete one (Gap 3).
printf '\n%s: %d passed, %d failed, %d skipped\n' "$PROG" "$PASS" "$FAIL" "$SKIP"
info "sections ran: ${RAN_SECTIONS[*]:-<none>}"
if ((${#SKIPPED_SECTIONS[@]})); then
    info "sections skipped (endpoint down): ${SKIPPED_SECTIONS[*]}"
fi
# A capability the host deliberately does NOT select is a configuration fact,
# not a defect: those sections are skipped, loudly and counted, but they do NOT
# force a non-zero exit (unlike an endpoint that is merely down). Running them
# would be the dishonest option, which is the whole point of the dispatch.
if ((${#UNSUPPORTED_SECTIONS[@]})); then
    info "sections skipped (capability not selected by this host): ${UNSUPPORTED_SECTIONS[*]}"
fi
# Exit status: non-zero if any check FAILED. ALSO non-zero if an endpoint-
# dependent section was SKIPPED (not decided) because the endpoint was down:
# the operator asked for `--section all` to validate everything, and a dead
# endpoint must not let the security-critical `forgery` section pass by simply
# not running (Bug 2). The skip is loud and counted above; this exit code makes
# it impossible to miss in a script's `&&` chain. A normal sub-check SKIP
# (e.g. "fewer than two slots") does NOT force non-zero — it is an honest
# "this environment cannot decide this one check", already counted in $SKIP.
# The endpoint-down preflight itself is a `fail` (so FAIL is already > 0 in
# the common case), but the skipped-sections WARNING prints unconditionally so
# it cannot be lost behind the preflight's own failure line.
if ((${#SKIPPED_SECTIONS[@]})); then
    printf '%s: WARNING: %d section(s) were SKIPPED, not decided (endpoint down; fix it and re-run): %s\n' \
        "$PROG" "${#SKIPPED_SECTIONS[@]}" "${SKIPPED_SECTIONS[*]}" >&2
fi
if ((FAIL > 0)) || ((${#SKIPPED_SECTIONS[@]})); then
    exit 1
fi
