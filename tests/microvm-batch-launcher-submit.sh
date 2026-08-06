#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED harness for the HOST side of a batch submission
# (`agent-microvm submit`), run from the `microvm-batch-launcher-submit` check in
# tests/microvm.nix.
#
# It runs the REAL, unmodified launcher:
#
#   * inside `bwrap` (a fresh tmpfs root, so the absolute runtime/state roots the
#     launcher hard-codes can be created) plus `fakeroot`, because the launcher
#     requires uid 0 and chowns the workspace clone to the guest agent uid;
#   * with `systemctl`, `mount`, `umount` and `findmnt` STUBBED by bind-mounting
#     scripts over the exact store paths the launcher resolves — so the script
#     under test is byte-identical to the installed one;
#   * where the `systemctl start|restart agent-microvm-hostkeys.service` stub
#     PLAYS THE PROVISIONING UNIT with the REAL provisioner from hostkeys.nix,
#     so the launcher's pre-launch host-identity validation (and its
#     self-healing restart) is satisfied by genuine key material or not at all;
#   * where the `systemctl start microvm@<slot>` stub PLAYS THE GUEST: it records
#     the effective ownership/modes of the job share the launcher just laid out,
#     and then plants whatever "result" the scenario asks for (a genuine
#     controller result, one with a foreign allocation token, one for another
#     slot, a malformed one, or only worker-written fakes).
#
# What this establishes: the launcher lays the trust boundary out correctly, and
# it accepts ONLY a controller-authenticated result for the ACTIVE allocation —
# everything else becomes an infrastructure error (exit 70), never a success.
#
# What it does NOT establish: anything about a booted guest (see
# runtime-validation.sh --section forgery).
set -euo pipefail

for v in LAUNCHER BWRAP FAKEROOT BASH_BIN SYSTEMCTL_TARGET MOUNT_TARGET \
    UMOUNT_TARGET FINDMNT_TARGET JQ_TARGET CURL_TARGET RUNTIME_ROOT STATE_ROOT \
    JOBS_ROOT WORKSPACE_SUBDIR HOST_HOME INPUT_SUBDIR \
    CONTROLLER_SUBDIR WORKER_SUBDIR WORKER_LOGS_SUBDIR WORKER_STDERR_NAME SPEC_NAME PROMPT_NAME \
    RESULT_NAME SPEC_VERSION CONTROLLER_VERSION AGENT WORKER_UID \
    PROVISION_HOSTKEYS; do
    [[ -n ${!v:-} ]] || {
        printf 'harness: required environment variable %s is unset\n' "$v" >&2
        exit 2
    }
done

FAILED=0
PASSED=0
pass() {
    printf 'PASS  %s\n' "$*"
    PASSED=$((PASSED + 1))
}
fail() {
    printf 'FAIL  %s\n' "$*"
    FAILED=$((FAILED + 1))
}
expect() {
    if [[ $2 == "$3" ]]; then
        pass "$1"
    else
        fail "$1 (expected '$2', got '$3')"
    fi
}
skip_all() {
    printf 'SKIP  %s\n' "$*"
    printf '\nSKIPPED: this sandbox cannot run the launcher (see the reason above).\n'
    exit 0
}

WORK="$PWD/launchersubmit"
mkdir -p "$WORK"
STUBS="$WORK/stubs"
mkdir -p "$STUBS"

if ! "$BWRAP" --unshare-user --uid 0 --gid 0 --tmpfs / --ro-bind /nix /nix \
    --proc /proc --dev /dev -- "$BASH_BIN" -c true 2>"$WORK/bwrap.err"; then
    skip_all "bwrap cannot create a user namespace here: $(cat "$WORK/bwrap.err")"
fi

# --- a throwaway source repository -----------------------------------------
REPO="$WORK/src"
rm -rf "$REPO"
mkdir -p "$REPO"
git -c init.defaultBranch=main init -q "$REPO"
printf 'hello\n' >"$REPO/a"
git -C "$REPO" add -A
git -C "$REPO" -c user.email=t@example.invalid -c user.name=t commit -qm init

# --- the stubs --------------------------------------------------------------
# `systemctl` doubles as the fake guest: on `start microvm@<slot>` it records the
# job share's effective ownership/modes and plants the scenario's "result".
cat >"$STUBS/systemctl" <<EOF
#!$BASH_BIN
set -u
log="\$STUB_DIR/systemctl.log"
printf 'systemctl %s\n' "\$*" >> "\$log"
active="\$STUB_DIR/active"
calls="\$STUB_DIR/is-active-calls"
# The per-slot job data lives in the ONE writable session share; its root
# comes from the module (job.nix -> session.nix), never from a literal here.
jobs_root="$JOBS_ROOT"

plant() {
    local slot="\$1" dir="\$jobs_root/\$slot" token
    # Record what the launcher actually created (this is the trust boundary).
    for p in "\$dir" "\$dir/$INPUT_SUBDIR" "\$dir/$CONTROLLER_SUBDIR" \\
             "\$dir/$WORKER_SUBDIR" "\$dir/$WORKER_LOGS_SUBDIR" \\
             "\$dir/$INPUT_SUBDIR/$SPEC_NAME" \\
             "\$dir/$INPUT_SUBDIR/$PROMPT_NAME"; do
        if [[ -e \$p ]]; then
            stat -c '%n %u %g %a' -- "\$p" >> "\$STUB_DIR/layout"
        else
            printf '%s MISSING\n' "\$p" >> "\$STUB_DIR/layout"
        fi
    done
    cp -f -- "\$dir/$INPUT_SUBDIR/$SPEC_NAME" "\$STUB_DIR/spec.json" 2>/dev/null || true
    token="\$(jq -r '.allocationToken // ""' "\$dir/$INPUT_SUBDIR/$SPEC_NAME")"
    printf '%s' "\$token" > "\$STUB_DIR/token"
    local task
    task="\$(jq -r '.taskId // ""' "\$dir/$INPUT_SUBDIR/$SPEC_NAME")"
    # The token goes through the ENVIRONMENT here too, so that the argv
    # recorder below only ever sees the LAUNCHER's own jq invocations
    # (/proc/<pid>/cmdline is world-readable, which is the whole point).
    result() {
        ALLOC_TOKEN="\$3" jq -nc --argjson version "\$1" \\
            --argjson controllerVersion "$CONTROLLER_VERSION" \\
            --arg taskId "\$2" --arg slot "\$4" \\
            --arg agent "$AGENT" --arg state "\$5" --argjson exitCode "\$6" \\
            '{version:\$version, controllerVersion:\$controllerVersion,
              taskId:\$taskId, allocationToken:\$ENV.ALLOC_TOKEN, slot:\$slot,
              agent:\$agent, state:\$state, exitCode:\$exitCode,
              startedAt:"2025-01-01T00:00:00Z", finishedAt:"2025-01-01T00:00:05Z",
              timedOut:false, message:""}'
    }
    case "\$STUB_MODE" in
        valid)
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" completed 0 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        wrong-token)
            result "$SPEC_VERSION" "\$task" \\
                deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef \\
                "\$slot" completed 0 > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        wrong-slot)
            result "$SPEC_VERSION" "\$task" "\$token" other-slot completed 0 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        legacy-v1)
            result 1 "\$task" "\$token" "\$slot" completed 0 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        malformed)
            printf '{ "version": 2, "state": ' > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        worker-only)
            # The untrusted worker's own "results" — in its own directory and in
            # the workspace. Neither may be believed.
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" completed 0 \\
                > "\$dir/$WORKER_SUBDIR/$RESULT_NAME"
            chown $WORKER_UID:$WORKER_UID "\$dir/$WORKER_SUBDIR/$RESULT_NAME"
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" completed 0 \\
                > "\$dir/$WORKSPACE_SUBDIR/$RESULT_NAME" 2>/dev/null || true
            ;;
        none) ;;
        # The VM stays "running" and NO result is planted by the stub: the test
        # plants the authoritative controller document itself. Used by
        # run_cancel, which needs service_active to keep saying yes so that
        # cmd_cancel takes its batch confirmation path.
        stay-active) ;;
        cancelled)
            # The guest controller's authoritative CANCELLATION verdict.
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" cancelled 130 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        pre-timed-out)
            # A job that reached its deadline BEFORE any cancellation took
            # effect. cancel must preserve this verdict verbatim instead of
            # rewriting it into a cancellation that never happened.
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" timed-out 124 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            ;;
        failed)
            # A genuinely-failed agent: the controller records 'failed' + a
            # non-zero exit code, and the worker's own stderr (root-owned,
            # opened by systemd) holds the real reason. Plant UNTRUSTED worker
            # stderr containing a terminal-control byte (ESC, 0x1b) that
            # cat -v MUST neutralise -- if it reaches the operator's TTY raw,
            # a hostile worker could clear the screen / forge a launcher line.
            result "$SPEC_VERSION" "\$task" "\$token" "\$slot" failed 1 \\
                > "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
            printf 'connection refused\n\x1b[2J\x1b[Hforged launcher line\n' \\
                > "\$dir/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
            chown 0:0 "\$dir/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
            chmod 0644 "\$dir/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
            ;;
    esac
    # The controller-owned documents must look controller-owned.
    if [[ -e "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME" ]]; then
        chown 0:0 "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
        chmod 0600 "\$dir/$CONTROLLER_SUBDIR/$RESULT_NAME"
    fi
}

case "\$1" in
    is-active)
        # \$2 = --quiet, \$3 = unit
        if [[ -f \$active ]]; then
            n=0
            [[ -f \$calls ]] && n="\$(cat "\$calls")"
            n=\$((n + 1)); printf '%s' "\$n" > "\$calls"
            # Scenarios that never produce a controller result must not make the
            # launcher wait for its full deadline: the "VM" goes away, exactly
            # like a guest that died.
            if [[ \$STUB_MODE == worker-only || \$STUB_MODE == none ]] && (( n > 1 )); then
                rm -f "\$active"
                exit 3
            fi
            exit 0
        fi
        exit 3
        ;;
    start | restart)
        for a in "\$@"; do
            case "\$a" in
                microvm@*.service)
                    slot="\${a#microvm@}"; slot="\${slot%.service}"
                    : > "\$active"
                    rm -f "\$calls"
                    plant "\$slot"
                    ;;
                agent-microvm-hostkeys.service)
                    # PLAY the provisioning unit with the REAL provisioner
                    # (hostkeys.nix), not a stub: the launcher validates the
                    # slot's actual key files + known_hosts entry before it
                    # boots anything, so the only way this harness can reach
                    # 'systemctl start microvm@SLOT' is if the genuine
                    # provisioner produced a genuine, consistent identity.
                    "$PROVISION_HOSTKEYS" || exit 1
                    ;;
            esac
        done
        exit 0
        ;;
    stop) rm -f "\$active"; exit 0 ;;
esac
exit 0
EOF

cat >"$STUBS/mount" <<EOF
#!$BASH_BIN
set -u
printf 'mount %s\n' "\$*" >> "\$STUB_DIR/mount.log"
target="\${@: -1}"
mkdir -p "\$STUB_DIR/mounts"
printf '%s' "\${@: -2:1}" > "\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
exit 0
EOF
cat >"$STUBS/umount" <<EOF
#!$BASH_BIN
set -u
printf 'umount %s\n' "\$*" >> "\$STUB_DIR/mount.log"
target="\${@: -1}"
rm -f "\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
exit 0
EOF
cat >"$STUBS/findmnt" <<EOF
#!$BASH_BIN
set -u
target="\${@: -1}"
f="\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
[[ -f \$f ]] || exit 1
for a in "\$@"; do
    if [[ \$a == -no ]] || [[ \$a == SOURCE ]]; then
        printf 'none[%s]\n' "\$(cat "\$f")"
        exit 0
    fi
done
printf '%s\n' "\$target"
exit 0
EOF
# --- an ARGV RECORDER over the exact `jq` the launcher resolves -------------
# The allocation token must never appear in a process's ARGUMENT VECTOR:
# /proc/<pid>/cmdline is world-readable (0444) for every local user, while
# /proc/<pid>/environ is 0400. This records the argv of every jq run inside the
# sandbox; the assertions below require that the ACTIVE token never shows up in
# it (and that the recorder actually fired, so the check cannot pass vacuously).
JQ_ARGV_LOG="$WORK/jq-argv.log"
: >"$JQ_ARGV_LOG"
cp -L "$JQ_TARGET" "$WORK/jq-real"
chmod +x "$WORK/jq-real"
cat >"$STUBS/jq" <<EOF
#!$BASH_BIN
printf '%s\n' "\$*" >> "\$JQ_ARGV_LOG"
exec "$WORK/jq-real" "\$@"
EOF
chmod +x "$STUBS"/*
if ! "$WORK/jq-real" -n '1' >/dev/null 2>&1; then
    skip_all "the copied jq does not run here (cannot record argv)"
fi

# --- a curl stub for the endpoint-preflight test ---------------------------
# The preflight test does NOT skip the preflight (AGENT_MICROVM_SKIP_PREFLIGHT
# is left at its default 0), so the launcher really calls curl. Bind a stub
# that always exits 1 over the exact curl the launcher resolves, to PROVE the
# preflight aborts before any VM is booted. The stub ignores all arguments.
cat >"$STUBS/curl-fail" <<EOF
#!$BASH_BIN
exit 1
EOF
chmod +x "$STUBS/curl-fail"

# --- running the launcher ---------------------------------------------------
# Every scenario runs in its OWN `fakeroot` invocation, and fakeroot's faked
# ownership lives only inside one invocation. The launcher's session tree is
# created with an `agent`-owned `workspace/` and `state/` (session.nix's layout
# table), and its PRE-LAUNCH verifier re-checks exactly that — so a tree left
# behind by a previous scenario would appear root-owned to the next one and
# refuse the launch. Reset the two trees before each scenario; everything the
# assertions read afterwards (workspaces/, results/, slots/) is untouched.
reset_session_trees() {
    rm -rf "$WORK/runtime/${JOBS_ROOT##*/}" "$WORK/runtime/${JOBS_ROOT##*/}-ro"
}

# run_submit <mode> <task> [extra submit args...]
run_submit() {
    local mode="$1" task="$2"
    shift 2
    local stub_dir="$WORK/stub-$task"
    rm -rf "$stub_dir"
    mkdir -p "$stub_dir" "$WORK/runtime" "$WORK/state"
    reset_session_trees
    local rc=0
    "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname launcher-host \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$WORK/runtime" "$RUNTIME_ROOT" \
        --bind "$WORK/state" "$STATE_ROOT" \
        --bind "$STUBS/systemctl" "$SYSTEMCTL_TARGET" \
        --bind "$STUBS/mount" "$MOUNT_TARGET" \
        --bind "$STUBS/umount" "$UMOUNT_TARGET" \
        --bind "$STUBS/findmnt" "$FINDMNT_TARGET" \
        --bind "$STUBS/jq" "$JQ_TARGET" \
        --setenv JQ_ARGV_LOG "$JQ_ARGV_LOG" \
        --setenv STUB_DIR "$stub_dir" \
        --setenv STUB_MODE "$mode" \
        --setenv AGENT_MICROVM_SKIP_PREFLIGHT 1 \
        --setenv HOME "$WORK" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "
            set -e
            # The launcher stages an ALLOWLISTED copy of the host agent
            # configuration on every launch (config-seed.nix). The baked host
            # home must therefore exist inside this fresh tmpfs root; it stays
            # EMPTY, so the stager stages nothing and writes an empty manifest.
            mkdir -p '$HOST_HOME'
            printf 'prompt for %s\n' '$task' > '$WORK/prompt-$task.md'
            exec '$LAUNCHER' submit --name '$task' --repository '$REPO' \
                --agent '$AGENT' --prompt-file '$WORK/prompt-$task.md' $*
        " >"$WORK/submit-$task.log" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# Run submit WITHOUT skipping the endpoint preflight, with a stubbed curl that
# always fails — to PROVE the preflight aborts before any VM is booted. This is
# a NEGATIVE CONTROL for the preflight: if the preflight_model_endpoint call
# were removed from cmd_submit, the launcher would proceed past it, hit the
# stubbed `systemctl start` (STUB_MODE=valid → a completed result), and exit 0
# — which this check rejects.
run_preflight_fail() {
    local stub_dir="$WORK/stub-preflight"
    rm -rf "$stub_dir"
    mkdir -p "$stub_dir" "$WORK/runtime" "$WORK/state"
    reset_session_trees
    local rc=0
    "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname launcher-host \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$WORK/runtime" "$RUNTIME_ROOT" \
        --bind "$WORK/state" "$STATE_ROOT" \
        --bind "$STUBS/systemctl" "$SYSTEMCTL_TARGET" \
        --bind "$STUBS/mount" "$MOUNT_TARGET" \
        --bind "$STUBS/umount" "$UMOUNT_TARGET" \
        --bind "$STUBS/findmnt" "$FINDMNT_TARGET" \
        --bind "$STUBS/jq" "$JQ_TARGET" \
        --bind "$STUBS/curl-fail" "$CURL_TARGET" \
        --setenv JQ_ARGV_LOG "$JQ_ARGV_LOG" \
        --setenv STUB_DIR "$stub_dir" \
        --setenv STUB_MODE valid \
        --setenv AGENT_MICROVM_SKIP_PREFLIGHT 0 \
        --setenv HOME "$WORK" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "
            set -e
            # The launcher stages an ALLOWLISTED copy of the host agent
            # configuration on every launch (config-seed.nix). The baked host
            # home must therefore exist inside this fresh tmpfs root; it stays
            # EMPTY, so the stager stages nothing and writes an empty manifest.
            mkdir -p '$HOST_HOME'
            printf 'prompt for preflight\n' > '$WORK/prompt-preflight.md'
            exec '$LAUNCHER' submit --name preflight --repository '$REPO' \
                --agent '$AGENT' --prompt-file '$WORK/prompt-preflight.md' --timeout 30
        " >"$WORK/submit-preflight.log" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# run_cancel <state> <exitCode> <task> — allocate a slot with a detached `run`,
# make its marker look like a BATCH allocation, plant an authoritative
# CONTROLLER result carrying THAT allocation's own identity (task, token, slot,
# agent — read back from the session marker, so `verify_job_result` accepts it),
# then run `cancel`.
#
# Deterministic on purpose: the terminal result exists before cancel's first
# poll, so there is no race to lose. What this pins down is cmd_cancel's
# DECISION — which document it treats as "the cancellation".
run_cancel() {
    local state="$1" code="$2" task="$3"
    local stub_dir="$WORK/stub-$task"
    rm -rf "$stub_dir"
    mkdir -p "$stub_dir" "$WORK/runtime" "$WORK/state"
    reset_session_trees
    local rc=0
    "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname launcher-host \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$WORK/runtime" "$RUNTIME_ROOT" \
        --bind "$WORK/state" "$STATE_ROOT" \
        --bind "$STUBS/systemctl" "$SYSTEMCTL_TARGET" \
        --bind "$STUBS/mount" "$MOUNT_TARGET" \
        --bind "$STUBS/umount" "$UMOUNT_TARGET" \
        --bind "$STUBS/findmnt" "$FINDMNT_TARGET" \
        --bind "$STUBS/jq" "$JQ_TARGET" \
        --setenv JQ_ARGV_LOG "$JQ_ARGV_LOG" \
        --setenv STUB_DIR "$stub_dir" \
        --setenv STUB_MODE stay-active \
        --setenv AGENT_MICROVM_SKIP_PREFLIGHT 1 \
        --setenv HOME "$WORK" \
        --setenv PLANT_STATE "$state" \
        --setenv PLANT_CODE "$code" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "
            set -e
            # The launcher stages an ALLOWLISTED copy of the host agent
            # configuration on every launch (config-seed.nix). The baked host
            # home must therefore exist inside this fresh tmpfs root; it stays
            # EMPTY, so the stager stages nothing and writes an empty manifest.
            mkdir -p '$HOST_HOME'
            '$LAUNCHER' run --name '$task' --repository '$REPO' \
                --agent '$AGENT' >/dev/null 2>&1
            slot=\"\$('$LAUNCHER' list | awk '\$5 == \"$task\" { print \$1 }')\"
            [[ -n \$slot ]] || { echo 'NO SLOT ALLOCATED'; exit 64; }
            marker='$RUNTIME_ROOT'/slots/\$slot/session.json
            tmp=\$(mktemp)
            jq '.mode = \"batch\"' \"\$marker\" > \"\$tmp\" && mv -f \"\$tmp\" \"\$marker\"
            cdir='$JOBS_ROOT'/\$slot/'$CONTROLLER_SUBDIR'
            mkdir -p \"\$cdir\"
            ALLOC_TOKEN=\"\$(jq -r .token \"\$marker\")\" \
            jq -nc --argjson version $SPEC_VERSION \
                --argjson controllerVersion $CONTROLLER_VERSION \
                --arg taskId '$task' --arg slot \"\$slot\" \
                --arg agent '$AGENT' --arg state \"\$PLANT_STATE\" \
                --argjson exitCode \"\$PLANT_CODE\" \
                --argjson timedOut \"\$([[ \$PLANT_STATE == timed-out ]] && echo true || echo false)\" \
                '{version:\$version, controllerVersion:\$controllerVersion,
                  taskId:\$taskId, allocationToken:\$ENV.ALLOC_TOKEN, slot:\$slot,
                  agent:\$agent, state:\$state, exitCode:\$exitCode,
                  startedAt:\"2025-01-01T00:00:00Z\",
                  finishedAt:\"2025-01-01T00:00:05Z\",
                  timedOut:\$timedOut, message:\"\"}' > \"\$cdir/$RESULT_NAME\"
            chown 0:0 \"\$cdir/$RESULT_NAME\"
            chmod 0600 \"\$cdir/$RESULT_NAME\"
            exec '$LAUNCHER' cancel '$task'
        " >"$WORK/cancel-$task.log" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# The launcher's runtime root, as seen from OUTSIDE the sandbox (it is bound
# over $RUNTIME_ROOT inside it, so the archived results survive).
OUT_RUNTIME="$WORK/runtime"

archived() {
    local task="$1" field="$2"
    jq -r "$field" "$OUT_RUNTIME/results/$task.json" 2>/dev/null || printf ''
}

printf '=== 1. a genuine controller result is accepted ===\n'
rc="$(run_submit valid ok-task --timeout 30)"
if [[ $rc != 0 ]]; then sed 's/^/      /' "$WORK/submit-ok-task.log"; fi
expect "submit exits 0 for a completed job" 0 "$rc"
expect "the archived state is completed" completed "$(archived ok-task .state)"
expect "the archived result came from the controller" controller "$(archived ok-task .source)"
expect "the archived result carries the allocation token" \
    "$(cat "$WORK/stub-ok-task/token")" "$(archived ok-task .allocationToken)"

printf '\n=== 2. the launcher lays out the trust boundary ===\n'
layout="$WORK/stub-ok-task/layout"
check_layout() {
    local suffix="$1" want="$2" desc="$3" line
    line="$(grep -E "${suffix}( |$)" "$layout" | tail -1 || true)"
    if [[ -z $line ]]; then
        fail "$desc (no layout record for $suffix)"
        return
    fi
    # "<path> <uid> <gid> <mode>"
    local got
    got="$(printf '%s' "$line" | awk '{print $2" "$4}')"
    expect "$desc" "$want" "$got"
}
check_layout "/$INPUT_SUBDIR" "0 755" "input/ is root-owned 0755"
check_layout "/$CONTROLLER_SUBDIR" "0 700" "controller/ is root-owned 0700 (worker cannot read it)"
check_layout "/$WORKER_SUBDIR" "$WORKER_UID 755" "worker/ belongs to the guest agent"
# The worker's stdout/stderr are opened by the guest's systemd AS ROOT (and it
# follows symlinks), so their directory must be root-owned and must NOT sit
# inside the agent-owned worker/ dir, where the agent could rename it and plant
# a symlink.
check_layout "/$WORKER_LOGS_SUBDIR" "0 755" "worker-logs/ is ROOT-owned (systemd opens the logs as root)"
check_layout "/$INPUT_SUBDIR/$SPEC_NAME" "0 400" "the spec is root-only 0400 (it carries the token)"
check_layout "/$INPUT_SUBDIR/$PROMPT_NAME" "0 444" "the prompt is world-readable, writable by nobody"
if jq -e '.allocationToken | test("^[0-9a-f]{64}$")' "$WORK/stub-ok-task/spec.json" >/dev/null; then
    pass "the spec carries a 256-bit allocation token"
else
    fail "the spec's allocation token is not 64 hex chars"
fi
if jq -e 'has("command") or has("exec") or has("executable")' \
    "$WORK/stub-ok-task/spec.json" >/dev/null; then
    fail "the spec names an executable"
else
    pass "the spec names no executable"
fi

printf '\n=== 2b. the allocation token stays out of every process ARGV ===\n'
# EXECUTED property: every jq run inside the sandbox was recorded argv-for-argv
# (the recorder is bound over the exact store path the launcher resolves). The
# ACTIVE allocation token must not appear in any of them, because
# /proc/<pid>/cmdline is world-readable while the launcher runs.
token="$(cat "$WORK/stub-ok-task/token")"
if [[ -s $JQ_ARGV_LOG ]]; then
    pass "the argv recorder actually observed jq invocations ($(wc -l <"$JQ_ARGV_LOG") of them)"
else
    fail "the argv recorder saw nothing: the wrapper was not the jq the launcher ran"
fi
if [[ -n $token ]] && grep -qF -- "$token" "$JQ_ARGV_LOG"; then
    fail "the allocation token appeared in a process argv: $(grep -F -- "$token" "$JQ_ARGV_LOG" | head -1)"
else
    pass "no process the launcher ran received the allocation token in its argv"
fi
if [[ "$(archived ok-task .allocationToken)" == "$token" ]]; then
    pass "the token IS recorded in the archived result (so the check is not vacuous)"
else
    fail "the archived result does not carry the allocation token"
fi

printf '\n=== 2c. the archived result is root-only (it carries the token) ===\n'
# Real modes, not fakeroot metadata: mktemp+chmod / install -d set them.
expect "the result archive directory is 0700" 700 "$(stat -c %a "$OUT_RUNTIME/results")"
expect "the archived result file is 0600" 600 "$(stat -c %a "$OUT_RUNTIME/results/ok-task.json")"

printf '\n=== 3. a result from another allocation is rejected ===\n'
rc="$(run_submit wrong-token stale-task --timeout 30)"
expect "submit reports an infrastructure error (70)" 70 "$rc"
expect "the archived state is infrastructure-error" infrastructure-error \
    "$(archived stale-task .state)"
expect "the archived record is marked as host-generated" host "$(archived stale-task .source)"
if grep -q "REJECTED the guest result" "$WORK/submit-stale-task.log" &&
    grep -q "allocation token does not belong" "$WORK/submit-stale-task.log"; then
    pass "the launcher says WHY it rejected the result"
else
    fail "no rejection reason in the log: $(cat "$WORK/submit-stale-task.log")"
fi

printf '\n=== 4. a result for another slot is rejected ===\n'
rc="$(run_submit wrong-slot slot-task --timeout 30)"
expect "submit reports an infrastructure error (70)" 70 "$rc"
if grep -q "slot does not belong" "$WORK/submit-slot-task.log"; then
    pass "the slot mismatch is named"
else
    fail "no slot-mismatch reason in the log"
fi

printf '\n=== 5. a legacy v1 result is rejected (fail closed) ===\n'
rc="$(run_submit legacy-v1 v1-task --timeout 30)"
expect "submit reports an infrastructure error (70)" 70 "$rc"
if grep -q "schema version mismatch" "$WORK/submit-v1-task.log"; then
    pass "the version mismatch is named"
else
    fail "no version-mismatch reason in the log"
fi

printf '\n=== 6. a malformed result never becomes a success ===\n'
rc="$(run_submit malformed bad-task --timeout 30)"
expect "submit reports an infrastructure error (70)" 70 "$rc"
expect "the archived state is infrastructure-error" infrastructure-error \
    "$(archived bad-task .state)"

printf '\n=== 7. worker-written results are ignored ===\n'
rc="$(run_submit worker-only forge-task --timeout 30)"
expect "submit does NOT report success" 70 "$rc"
if [[ "$(archived forge-task .state)" != completed ]]; then
    pass "a worker-written result did not become the outcome"
else
    fail "a worker-written result was accepted"
fi
expect "the archived record is host-generated" host "$(archived forge-task .source)"

printf '\n=== 8. no result at all is an infrastructure error ===\n'
rc="$(run_submit none silent-task --timeout 30)"
expect "submit reports an infrastructure error (70)" 70 "$rc"

printf '\n=== 9. the workspace clone survives every one of those ===\n'
for t in ok-task stale-task slot-task v1-task bad-task forge-task silent-task; do
    if [[ -d "$OUT_RUNTIME/workspaces/$t/.git" ]]; then
        pass "the clone of $t was kept"
    else
        fail "the clone of $t was lost"
    fi
done
# ... and no slot stays allocated.
leftover=0
for f in "$OUT_RUNTIME"/slots/*/session.json; do
    [[ -e $f ]] || continue
    leftover=1
    printf '      leftover marker: %s\n' "$f"
done
if ((leftover)); then
    fail "a slot stayed allocated"
else
    pass "no slot stayed allocated"
fi

printf '\n=== 10. a failed job surfaces BOUNDED, SANITISED worker stderr ===\n'
# A `failed` controller result triggers surface_worker_stderr, which prints a
# BOUNDED tail of the worker's UNTRUSTED stderr to the operator. The stub
# planted an ESC byte (0x1b) in that stderr: `cat -v` MUST neutralise it to
# ^[ so a hostile worker cannot inject terminal-control escapes into the
# operator's TTY. This is a NEGATIVE CONTROL for the cat -v sanitiser — if it
# is reverted to a raw `tail -c ... >&2`, the raw ESC byte reaches stderr and
# the "no raw control byte" assertion FAILS.
rc="$(run_submit failed die-task --timeout 30)"
expect "submit exits 1 for a failed agent" 1 "$rc"
die_log="$WORK/submit-die-task.log"
if grep -q "UNTRUSTED worker stderr" "$die_log"; then
    pass "the failed worker stderr was surfaced (labelled UNTRUSTED)"
else
    fail "no UNTRUSTED worker stderr was surfaced for a failed job"
fi
if grep -q "connection refused" "$die_log"; then
    pass "the worker stderr content was surfaced (not suppressed)"
else
    fail "the worker stderr content was NOT surfaced"
fi
# NEGATIVE CONTROL: the raw ESC byte (0x1b) must NOT reach the operator's
# stderr. This FAILS if `cat -v` is removed from surface_worker_stderr.
if grep -qF "$(printf '\x1b')" "$die_log"; then
    fail "a raw ESC byte reached the operator's stderr (cat -v was removed?)"
else
    pass "no raw control byte reached the operator's stderr (cat -v neutralised it)"
fi
# POSITIVE CONTROL: cat -v renders ESC as ^[, which MUST appear — proving the
# content was surfaced AND sanitised (not silently dropped). This distinguishes
# "sanitised" from "suppressed".
if grep -qF '^[[' "$die_log"; then
    pass "the ESC byte was rendered as ^[ (cat -v sanitisation is active)"
else
    fail "no ^[ found — the sanitised form did not appear (content may have been suppressed)"
fi
# The workspace clone of a FAILED job must survive too (§35: the clone is
# ALWAYS kept, regardless of outcome).
if [[ -d "$OUT_RUNTIME/workspaces/die-task/.git" ]]; then
    pass "the clone of die-task was kept"
else
    fail "the clone of die-task was lost"
fi

printf '\n=== 11. the endpoint preflight aborts before booting a VM ===\n'
# The preflight is NOT skipped here (AGENT_MICROVM_SKIP_PREFLIGHT=0), and curl
# is stubbed to fail. The launcher MUST abort with the PREFLIGHT FAILED message
# before it ever calls `systemctl start` — so it exits non-zero and never
# reaches the result channel. This is a NEGATIVE CONTROL for the preflight: if
# the preflight_model_endpoint call were removed from cmd_submit, the launcher
# would proceed to `systemctl start` (stubbed: STUB_MODE=valid → exit 0),
# which this check rejects.
rc="$(run_preflight_fail)"
if [[ $rc -ne 0 ]]; then
    pass "submit aborted (exit $rc) when the endpoint is unreachable"
else
    fail "submit did not abort the preflight (exit 0) — the preflight call may have been removed"
fi
if grep -q "PREFLIGHT FAILED" "$WORK/submit-preflight.log"; then
    pass "the preflight failure message names the problem"
else
    fail "no PREFLIGHT FAILED message: $(head -5 "$WORK/submit-preflight.log")"
fi
if grep -q "doctor" "$WORK/submit-preflight.log"; then
    pass "the preflight message points to doctor for diagnosis"
else
    fail "the preflight message does not mention doctor"
fi

# --- cancellation: exit code and verdict fidelity --------------------------
# Observed on real KVM: a cancelled batch task was archived as `timed-out`, and
# a cancellation exited 70 (the infrastructure-error bucket). Both are host-side
# defects; the guest controller's own state machine is covered by
# `microvm-batch-controller-smoke` §4 (token-bound cancel → `cancelled`, foreign
# token ignored), which is what proves the controller was innocent.
printf '\n=== 12. a controller CANCELLATION is reported as such ===\n'
rc="$(run_submit cancelled cancel-state --timeout 30)"
# 130 = 128+SIGINT, the code the controller itself records. Exit 70 here means
# `cancelled` fell into the `*)` infrastructure-error bucket of cmd_submit.
if [[ $rc -eq 130 ]]; then
    pass "submit exits 130 on a cancelled job (not the 70 infra bucket)"
else
    fail "submit exited $rc on a cancelled job, expected 130"
fi
if [[ "$(archived cancel-state .state)" == cancelled ]]; then
    pass "the archived state is 'cancelled'"
else
    fail "archived state is '$(archived cancel-state .state)', expected 'cancelled'"
fi
if [[ "$(archived cancel-state .source)" == controller ]]; then
    pass "the cancellation verdict came from the controller"
else
    fail "cancellation source is '$(archived cancel-state .source)', expected 'controller'"
fi

printf '\n=== 13. cancel CONFIRMS only a real cancellation ===\n'
rc="$(run_cancel cancelled 130 cancel-ok)"
if [[ "$(archived cancel-ok .state)" == cancelled ]]; then
    pass "cancel archives the controller's 'cancelled' verdict (exit $rc)"
else
    fail "cancel archived '$(archived cancel-ok .state)', expected 'cancelled'"
fi

# THE REGRESSION: the job had already hit its deadline when cancel ran. Cancel
# must NOT adopt that result as "the cancellation" — the archive has to keep
# saying `timed-out`, because that is what actually happened.
rc="$(run_cancel timed-out 124 cancel-late)"
case "$(archived cancel-late .state)" in
    timed-out)
        pass "cancel preserved the pre-existing 'timed-out' verdict"
        ;;
    cancelled)
        fail "cancel RELABELLED a timed-out job as 'cancelled' (a verdict that never happened)"
        ;;
    *)
        fail "cancel archived '$(archived cancel-late .state)', expected 'timed-out'"
        ;;
esac
if grep -q "already terminated as 'timed-out'" "$WORK/cancel-cancel-late.log"; then
    pass "cancel told the operator the task had already terminated"
else
    fail "cancel did not report the pre-existing terminal state: $(tail -3 "$WORK/cancel-cancel-late.log")"
fi

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
