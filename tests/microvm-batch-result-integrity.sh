#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED regression harness for the myconfig.ai.microvm batch RESULT CHANNEL
# (improvement ticket 7). Run from the `microvm-batch-result-integrity` check in
# tests/microvm.nix, inside `fakeroot` so that fixtures can carry the ownership
# the real job share has (root-owned input/controller, agent-owned worker).
#
# This is NOT a KVM test: it does not boot a guest. What it DOES do is run the
# two real programs that decide whether a result is authentic —
#   * `agent-job-verify-result` (host side: is this document from the guest
#     controller, and does it belong to the ACTIVE allocation?)
#   * `agent-job-assert-paths`  (guest side: is the trust boundary intact
#     before anything runs?)
# — against deliberately hostile fixtures: worker-owned results, stale
# allocation tokens, wrong task/slot/agent, malformed JSON, symlinked result
# paths, world-writable controller directories and replaceable parents.
#
# fakeroot fakes METADATA, not enforcement: it proves the validators reject the
# hostile layouts, not that the kernel denies the write. The kernel-level
# denial (a real uid-1000 worker writing into a root-owned 0700 directory) is
# only observable in a booted guest and is covered by the `forgery` section of
# runtime-validation.sh.
set -euo pipefail

for v in VERIFIER ASSERT_PATHS SPEC_VERSION CONTROLLER_VERSION INPUT_SUBDIR \
    CONTROLLER_SUBDIR WORKER_SUBDIR WORKER_LOGS_SUBDIR SPEC_NAME PROMPT_NAME \
    CANCEL_NAME RESULT_NAME STATE_NAME WORKER_STDOUT_NAME WORKER_STDERR_NAME \
    WORKER_UID SLOT TASK AGENT SPEC_MODE PROMPT_MODE \
    INPUT_DIR_MODE CONTROLLER_DIR_MODE WORKER_DIR_MODE WORKER_LOGS_DIR_MODE; do
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

# The active allocation's token, and a DIFFERENT one for the stale-result tests.
TOKEN="$(printf 'a1b2c3d4%.0s' 1 2 3 4 5 6 7 8)"
STALE_TOKEN="$(printf 'f00df00d%.0s' 1 2 3 4 5 6 7 8)"

ROOT="$PWD/fixture"

# ---------------------------------------------------------------------------
# fixture construction
# ---------------------------------------------------------------------------
# A CORRECT job share, exactly as the host launcher / tmpfiles create it.
build_share() {
    local base="$1"
    rm -rf "$base"
    mkdir -p "$base/jobs/$SLOT/$INPUT_SUBDIR" \
        "$base/jobs/$SLOT/$CONTROLLER_SUBDIR" \
        "$base/jobs/$SLOT/$WORKER_SUBDIR/artifacts" \
        "$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
    : >"$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDOUT_NAME"
    : >"$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
    printf '{}\n' >"$base/jobs/$SLOT/$INPUT_SUBDIR/$SPEC_NAME"
    printf 'do the thing\n' >"$base/jobs/$SLOT/$INPUT_SUBDIR/$PROMPT_NAME"
    chown -R 0:0 "$base"
    chmod 0755 "$base" "$base/jobs" "$base/jobs/$SLOT"
    chmod "$INPUT_DIR_MODE" "$base/jobs/$SLOT/$INPUT_SUBDIR"
    chmod "$CONTROLLER_DIR_MODE" "$base/jobs/$SLOT/$CONTROLLER_SUBDIR"
    # The worker's log files stay ROOT-owned in a ROOT-owned directory: the
    # guest's systemd opens them as root (following symlinks), so nothing
    # running as the worker uid may be able to replace them or their directory.
    chmod "$WORKER_LOGS_DIR_MODE" "$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
    chmod 0644 "$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDOUT_NAME" \
        "$base/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
    chmod "$SPEC_MODE" "$base/jobs/$SLOT/$INPUT_SUBDIR/$SPEC_NAME"
    chmod "$PROMPT_MODE" "$base/jobs/$SLOT/$INPUT_SUBDIR/$PROMPT_NAME"
    # The ONLY worker-writable part of the share.
    chmod "$WORKER_DIR_MODE" "$base/jobs/$SLOT/$WORKER_SUBDIR"
    chown -R "$WORKER_UID:$WORKER_UID" "$base/jobs/$SLOT/$WORKER_SUBDIR"
}

result_json() {
    jq -nc \
        --argjson version "$SPEC_VERSION" \
        --argjson controllerVersion "$CONTROLLER_VERSION" \
        --arg taskId "$TASK" --arg allocationToken "$TOKEN" \
        --arg slot "$SLOT" --arg agent "$AGENT" \
        '{version:$version, controllerVersion:$controllerVersion,
          taskId:$taskId, allocationToken:$allocationToken, slot:$slot,
          agent:$agent, state:"completed", exitCode:0,
          startedAt:"2025-01-01T00:00:00Z", finishedAt:"2025-01-01T00:01:00Z",
          timedOut:false, message:""}'
}

state_json() {
    jq -nc \
        --argjson version "$SPEC_VERSION" \
        --argjson controllerVersion "$CONTROLLER_VERSION" \
        --arg taskId "$TASK" --arg allocationToken "$TOKEN" \
        --arg slot "$SLOT" --arg agent "$AGENT" \
        '{version:$version, controllerVersion:$controllerVersion,
          taskId:$taskId, allocationToken:$allocationToken, slot:$slot,
          agent:$agent, phase:"running", startedAt:"2025-01-01T00:00:00Z",
          updatedAt:"2025-01-01T00:00:02Z",
          workerUnit:"agent-job-worker@pi.service", message:""}'
}

CTRL_DIR="$ROOT/jobs/$SLOT/$CONTROLLER_SUBDIR"
RESULT="$CTRL_DIR/$RESULT_NAME"
STATE="$CTRL_DIR/$STATE_NAME"

# Install a document as the CONTROLLER would: root-owned, 0600.
put_controller_result() {
    rm -f "$RESULT"
    cat >"$RESULT"
    chown 0:0 "$RESULT"
    chmod 0600 "$RESULT"
}

# ---------------------------------------------------------------------------
# assertion helpers
# ---------------------------------------------------------------------------
# The EXPECTED allocation token the verifier is run with. It is passed in the
# ENVIRONMENT (AGENT_JOB_EXPECTED_TOKEN), never in argv, because
# /proc/<pid>/cmdline is world-readable while /proc/<pid>/environ is 0400 — the
# verifier rejects a `--token` argument outright.
EXPECT_TOKEN="$TOKEN"

# verify <want-rc> <needle-in-output> <description>
verify() {
    local want="$1" needle="$2" desc="$3"
    shift 3
    local out rc=0
    out="$(AGENT_JOB_EXPECTED_TOKEN="$EXPECT_TOKEN" "$VERIFIER" "$@" 2>&1)" || rc=$?
    if [[ $rc -ne $want ]]; then
        fail "$desc (expected rc=$want, got rc=$rc: $out)"
        return
    fi
    if [[ -n $needle && $out != *"$needle"* ]]; then
        fail "$desc (rc=$want as expected, but the reason did not mention '$needle': $out)"
        return
    fi
    pass "$desc"
}

verify_result() {
    local want="$1" needle="$2" desc="$3" path="${4:-$RESULT}"
    verify "$want" "$needle" "$desc" \
        --result "$path" --task "$TASK" \
        --slot "$SLOT" --agent "$AGENT"
}

# assert_paths <want-rc> <needle> <description> [extra args...]
assert_paths() {
    local want="$1" needle="$2" desc="$3"
    shift 3
    local out rc=0
    out="$("$ASSERT_PATHS" --root "$ROOT/jobs/$SLOT" --boundary "$ROOT" \
        --worker-uid "$WORKER_UID" "$@" 2>&1)" || rc=$?
    if [[ $rc -ne $want ]]; then
        fail "$desc (expected rc=$want, got rc=$rc: $out)"
        return
    fi
    if [[ -n $needle && $out != *"$needle"* ]]; then
        fail "$desc (rc=$want as expected, but the reason did not mention '$needle': $out)"
        return
    fi
    pass "$desc"
}

printf '=== A. the host verifier accepts ONLY a controller-authenticated result ===\n'
build_share "$ROOT"

verify_result 1 "" "no result yet is 'absent', not a result"
result_json | put_controller_result
verify_result 0 "" "a well-formed controller result for the active allocation is accepted"
result_json | jq '.state="failed"|.exitCode=1' | put_controller_result
verify_result 0 "" "a reported agent failure is accepted (exit 1)"
result_json | jq '.state="timed-out"|.exitCode=124|.timedOut=true' | put_controller_result
verify_result 0 "" "a controller-reported timeout is accepted"
result_json | jq '.state="cancelled"|.exitCode=130' | put_controller_result
verify_result 0 "" "a controller-reported cancellation is accepted"

printf '\n=== B. forged results (test A/C: worker-written documents) ===\n'
# The worker cannot write into controller/ in a real guest; if it EVER could,
# the host must still refuse the document because it is not controller-owned.
result_json | put_controller_result
chown "$WORKER_UID:$WORKER_UID" "$RESULT"
verify_result 2 "not by the guest controller" \
    "a result owned by the worker uid is rejected"
result_json | put_controller_result
chmod 0666 "$RESULT"
verify_result 2 "group/other-writable" \
    "a group/other-writable result is rejected"
# A worker-written result in the worker's OWN area is never even a candidate:
# the launcher only ever opens the controller path. Prove the verifier rejects
# it too, so a future caller cannot be tricked into accepting one.
WORKER_FAKE="$ROOT/jobs/$SLOT/$WORKER_SUBDIR/$RESULT_NAME"
result_json >"$WORKER_FAKE"
chown "$WORKER_UID:$WORKER_UID" "$WORKER_FAKE"
verify_result 2 "" "a result placed in the worker's own directory is rejected" "$WORKER_FAKE"
# Same for a "result" inside the workspace clone.
mkdir -p "$ROOT/workspace"
chown "$WORKER_UID:$WORKER_UID" "$ROOT/workspace"
result_json >"$ROOT/workspace/$RESULT_NAME"
chown "$WORKER_UID:$WORKER_UID" "$ROOT/workspace/$RESULT_NAME"
verify_result 2 "" "a result inside /workspace is rejected" "$ROOT/workspace/$RESULT_NAME"

printf '\n=== C. path-level forgery (test B: symlinks and replaced parents) ===\n'
build_share "$ROOT"
result_json >"$ROOT/elsewhere.json"
chown 0:0 "$ROOT/elsewhere.json"
chmod 0600 "$ROOT/elsewhere.json"
ln -s "$ROOT/elsewhere.json" "$RESULT"
verify_result 2 "symlink" "a symlinked result path is rejected"
rm -f "$RESULT"
build_share "$ROOT"
result_json | put_controller_result
chmod 0777 "$CTRL_DIR"
verify_result 2 "controller directory is group/other-writable" \
    "a group/other-writable controller directory is rejected"
build_share "$ROOT"
result_json | put_controller_result
chown "$WORKER_UID:$WORKER_UID" "$CTRL_DIR"
verify_result 2 "controller directory is owned by uid" \
    "a worker-owned controller directory is rejected"

printf '\n=== D. stale allocation results (test D) ===\n'
build_share "$ROOT"
result_json | jq --arg t "$STALE_TOKEN" '.allocationToken=$t' | put_controller_result
verify_result 2 "allocation token does not belong" \
    "a valid result from an EARLIER allocation is rejected (token mismatch)"

printf '\n=== E. wrong identity (test E) ===\n'
result_json | jq '.taskId="some-other-task"' | put_controller_result
verify_result 2 "task id does not belong" "a result for another task is rejected"
result_json | jq '.slot="agent-normal-99"' | put_controller_result
verify_result 2 "slot does not belong" "a result for another slot is rejected"
result_json | jq '.agent="nosuchagent"' | put_controller_result
verify_result 2 "agent does not belong" "a result naming another agent is rejected"
result_json | jq '.version=1' | put_controller_result
verify_result 2 "schema version mismatch" "a legacy v1 result is rejected (fail closed)"
result_json | jq '.controllerVersion=99' | put_controller_result
verify_result 2 "controller version mismatch" "an unknown controller version is rejected"

printf '\n=== F. malformed results are NEVER success (test F) ===\n'
printf '' | put_controller_result
verify_result 2 "empty" "an empty result file is rejected"
printf '{' | put_controller_result
verify_result 2 "not valid JSON" "a truncated result is rejected"
printf 'not json at all\n' | put_controller_result
verify_result 2 "not valid JSON" "a non-JSON result is rejected"
printf '[]' | put_controller_result
verify_result 2 "not a JSON object" "a JSON array result is rejected"
result_json | jq '.exitCode="0"' | put_controller_result
verify_result 2 "exitCode is not an integer" "a non-integer exitCode is rejected"
result_json | jq '.exitCode=9999' | put_controller_result
verify_result 2 "exitCode out of range" "an out-of-range exitCode is rejected"
result_json | jq 'del(.timedOut)' | put_controller_result
verify_result 2 "missing required field" "a result missing a required field is rejected"
result_json | jq '.smuggled="x"' | put_controller_result
verify_result 2 "unknown field" "a result with an unknown field is rejected"
result_json | jq '.state="running"' | put_controller_result
verify_result 2 "not a terminal state" "a NON-TERMINAL state in result.json is rejected"
result_json | jq '.state="pwned"' | put_controller_result
verify_result 2 "not a terminal state" "an invented terminal state is rejected"
result_json | jq '.state="failed"' | put_controller_result
verify_result 2 "must not report exitCode 0" "a failed result claiming exit 0 is rejected"
result_json | jq '.exitCode=1' | put_controller_result
verify_result 2 "must report exitCode 0" "a completed result claiming exit 1 is rejected"
result_json | jq '.timedOut=true' | put_controller_result
verify_result 2 "state/timedOut disagree" "state/timedOut disagreement is rejected"
result_json | jq '.startedAt="whenever"' | put_controller_result
verify_result 2 "ISO-8601" "a malformed timestamp is rejected"

printf '\n=== G. the progress channel is not a result channel ===\n'
build_share "$ROOT"
state_json >"$STATE"
chown 0:0 "$STATE"
chmod 0600 "$STATE"
verify 0 "" "a controller state document validates as --kind state" \
    --result "$STATE" --kind state --task "$TASK" \
    --slot "$SLOT" --agent "$AGENT"
verify 2 "unknown field" "a state document is NOT accepted as a result" \
    --result "$STATE" --task "$TASK" \
    --slot "$SLOT" --agent "$AGENT"
state_json | jq '.phase="totally-done"' >"$STATE"
chmod 0600 "$STATE"
verify 2 "not a known controller phase" "an invented phase is rejected" \
    --result "$STATE" --kind state --task "$TASK" \
    --slot "$SLOT" --agent "$AGENT"

printf '\n=== H. the verifier refuses ambiguous callers ===\n'
verify 64 "absolute path" "a relative result path is a usage error" \
    --result relative/result.json --task "$TASK" \
    --slot "$SLOT" --agent "$AGENT"
EXPECT_TOKEN=nope
verify 64 "AGENT_JOB_EXPECTED_TOKEN" "a malformed expected token is a usage error" \
    --result "$RESULT" --task "$TASK" --slot "$SLOT" --agent "$AGENT"
EXPECT_TOKEN=""
verify 64 "AGENT_JOB_EXPECTED_TOKEN" "a MISSING expected token is a usage error, never a pass" \
    --result "$RESULT" --task "$TASK" --slot "$SLOT" --agent "$AGENT"
EXPECT_TOKEN="$TOKEN"
# The token must not be acceptable on the command line at all: an argv would
# publish the ACTIVE allocation token via /proc/<pid>/cmdline (0444).
verify 64 "AGENT_JOB_EXPECTED_TOKEN" \
    "passing the token as --token is refused (it would leak via /proc/<pid>/cmdline)" \
    --result "$RESULT" --task "$TASK" --token "$TOKEN" --slot "$SLOT" --agent "$AGENT"
verify 64 "--task" "a malformed expected task is a usage error" \
    --result "$RESULT" --task 'evil task/../..' \
    --slot "$SLOT" --agent "$AGENT"
verify 64 "--kind" "an unknown --kind is a usage error" \
    --result "$RESULT" --kind whatever --task "$TASK" \
    --slot "$SLOT" --agent "$AGENT"

printf '\n=== I. the guest-side trust-boundary assertions ===\n'
build_share "$ROOT"
assert_paths 0 "" "the correct job share layout passes"

build_share "$ROOT"
chmod 0777 "$CTRL_DIR"
# 0777 trips the "no group/other WRITE anywhere in the trust boundary" rule
# before the stricter "no group/other access at all" rule for controller/.
assert_paths 1 "group/other-writable" \
    "a controller dir the worker could write is refused"

build_share "$ROOT"
chmod 0750 "$CTRL_DIR"
assert_paths 1 "grants group/other access" \
    "a controller dir the worker's GROUP could read is refused"

build_share "$ROOT"
chown "$WORKER_UID:$WORKER_UID" "$CTRL_DIR"
assert_paths 1 "must be owned by uid 0" "a worker-owned controller dir is refused"

build_share "$ROOT"
rm -rf "$CTRL_DIR"
assert_paths 1 "missing" "a missing controller dir is refused"

build_share "$ROOT"
rm -rf "$CTRL_DIR"
ln -s "$ROOT/jobs/$SLOT/$WORKER_SUBDIR" "$CTRL_DIR"
assert_paths 1 "symlink" "a controller dir replaced by a symlink is refused"

build_share "$ROOT"
ln -s /etc/passwd "$RESULT"
assert_paths 1 "symlink" "a symlinked result path is refused"

build_share "$ROOT"
# The worker could rename controller/ if it could write the SHARE ROOT.
chmod 0777 "$ROOT/jobs/$SLOT"
assert_paths 1 "group/other-writable" \
    "a share root the worker could write (and thus rename controller/) is refused"

build_share "$ROOT"
# ... or any PARENT of it (directory-replacement attack one level up).
chmod 0777 "$ROOT/jobs"
assert_paths 1 "group/other-writable" \
    "a group/other-writable PARENT of the share is refused"

build_share "$ROOT"
chmod 0444 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$SPEC_NAME"
assert_paths 1 "too permissive" \
    "a world-readable spec (which would leak the allocation token) is refused"

build_share "$ROOT"
chmod 0666 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$PROMPT_NAME"
assert_paths 1 "too permissive" "a writable prompt file is refused"

build_share "$ROOT"
chmod 0777 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR"
assert_paths 1 "group/other-writable" "a writable input dir is refused"

build_share "$ROOT"
chown 0:0 "$ROOT/jobs/$SLOT/$WORKER_SUBDIR"
assert_paths 1 "worker directory must be owned by uid" \
    "a worker dir the worker cannot write is refused"

# --- the worker's LOG directory is root-owned on purpose -------------------
# systemd opens stdout.log/stderr.log as ROOT and FOLLOWS symlinks, so a
# directory (or file) the worker uid could replace would redirect a
# root-opened append fd. All three of these must be refused BEFORE the worker
# is started.
build_share "$ROOT"
chown -R "$WORKER_UID:$WORKER_UID" "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
assert_paths 1 "the worker log directory must be owned by uid 0" \
    "a worker-owned log directory is refused"

build_share "$ROOT"
chmod 0777 "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
assert_paths 1 "group/other-writable" \
    "a group/other-writable worker log directory is refused"

build_share "$ROOT"
ln -sfn /etc/passwd "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDOUT_NAME"
assert_paths 1 "worker log file is a symlink" \
    "a symlinked worker log file is refused"

build_share "$ROOT"
chmod 0666 "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR/$WORKER_STDERR_NAME"
assert_paths 1 "too permissive" \
    "a worker-writable worker log file is refused"

build_share "$ROOT"
rm -rf "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
ln -s "$ROOT/jobs/$SLOT/$WORKER_SUBDIR" "$ROOT/jobs/$SLOT/$WORKER_LOGS_SUBDIR"
assert_paths 1 "symlink" "a log directory replaced by a symlink is refused"

build_share "$ROOT"
rm -f "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$SPEC_NAME"
assert_paths 1 "the job spec is missing" "a missing spec is refused"

build_share "$ROOT"
printf '{}' >"$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$CANCEL_NAME"
chown 0:0 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$CANCEL_NAME"
chmod 0400 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$CANCEL_NAME"
assert_paths 0 "" "a root-only cancellation request passes"
chmod 0666 "$ROOT/jobs/$SLOT/$INPUT_SUBDIR/$CANCEL_NAME"
assert_paths 1 "too permissive" "a worker-writable cancellation request is refused"

build_share "$ROOT"
assert_paths 1 "unprivileged" "a worker uid of 0 is refused" --worker-uid 0

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
