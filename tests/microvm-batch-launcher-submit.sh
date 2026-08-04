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
    UMOUNT_TARGET FINDMNT_TARGET RUNTIME_ROOT STATE_ROOT INPUT_SUBDIR \
    CONTROLLER_SUBDIR WORKER_SUBDIR SPEC_NAME PROMPT_NAME RESULT_NAME \
    SPEC_VERSION CONTROLLER_VERSION AGENT WORKER_UID; do
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
jobs_root="$RUNTIME_ROOT/jobs"

plant() {
    local slot="\$1" dir="\$jobs_root/\$slot" token
    # Record what the launcher actually created (this is the trust boundary).
    for p in "\$dir" "\$dir/$INPUT_SUBDIR" "\$dir/$CONTROLLER_SUBDIR" \\
             "\$dir/$WORKER_SUBDIR" "\$dir/$INPUT_SUBDIR/$SPEC_NAME" \\
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
    result() {
        jq -nc --argjson version "\$1" --argjson controllerVersion "$CONTROLLER_VERSION" \\
            --arg taskId "\$2" --arg allocationToken "\$3" --arg slot "\$4" \\
            --arg agent "$AGENT" --arg state "\$5" --argjson exitCode "\$6" \\
            '{version:\$version, controllerVersion:\$controllerVersion,
              taskId:\$taskId, allocationToken:\$allocationToken, slot:\$slot,
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
                > "$STATE_ROOT/\$slot/workspace/$RESULT_NAME" 2>/dev/null || true
            ;;
        none) ;;
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
    start)
        for a in "\$@"; do
            case "\$a" in
                microvm@*.service)
                    slot="\${a#microvm@}"; slot="\${slot%.service}"
                    : > "\$active"
                    rm -f "\$calls"
                    plant "\$slot"
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
chmod +x "$STUBS"/*

# --- running the launcher ---------------------------------------------------
# run_submit <mode> <task> [extra submit args...]
run_submit() {
    local mode="$1" task="$2"
    shift 2
    local stub_dir="$WORK/stub-$task"
    rm -rf "$stub_dir"
    mkdir -p "$stub_dir" "$WORK/runtime" "$WORK/state"
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
        --setenv STUB_DIR "$stub_dir" \
        --setenv STUB_MODE "$mode" \
        --setenv HOME "$WORK" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "
            set -e
            printf 'prompt for %s\n' '$task' > '$WORK/prompt-$task.md'
            exec '$LAUNCHER' submit --name '$task' --repository '$REPO' \
                --agent '$AGENT' --prompt-file '$WORK/prompt-$task.md' $*
        " >"$WORK/submit-$task.log" 2>&1 || rc=$?
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

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
