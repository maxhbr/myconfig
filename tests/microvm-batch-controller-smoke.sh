#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED smoke test for the TRUSTED guest-side batch job controller
# (`agent-job-controller`, see modules/myconfig.ai/myconfig.ai.microvm/job.nix).
# Run from the `microvm-batch-controller-smoke` check in tests/microvm.nix.
#
# It runs the REAL, unmodified controller script:
#
#   * inside `bwrap`, which supplies the paths it hard-codes
#     (a fixture bound at the guest job mount point, another at /workspace) and
#     the guest
#     hostname (= the slot name it validates the spec against);
#   * with `systemctl` STUBBED by bind-mounting a small script over the exact
#     store path the controller resolves, so the worker lifecycle can be driven
#     deterministically without systemd, KVM or a coding agent;
#   * under `fakeroot` where the fixture must show the real ownership split
#     (root-owned input/controller, agent-owned worker/workspace), because a
#     single-uid user namespace cannot express two different owners.
#
# What this DOES establish: the controller's spec validation, trust-boundary
# refusal, worker start/stop sequence, deadline handling, token-bound
# cancellation and result writing all work, and the documents it produces are
# accepted by the HOST verifier for the right allocation (and rejected for the
# wrong one) — i.e. host and guest really do agree on the protocol.
#
# What it does NOT establish: that the guest kernel denies a uid-1000 worker
# write access to the controller directory, or that systemd really kills the
# whole worker cgroup. Those need a booted guest — see
# runtime-validation.sh --section forgery.
set -euo pipefail

for v in BWRAP FAKEROOT CONTROLLER VERIFIER SYSTEMCTL_TARGET BASH_BIN JQ_TARGET \
    SPEC_VERSION GUEST_JOB_DIR INPUT_SUBDIR CONTROLLER_SUBDIR WORKER_SUBDIR WORKER_LOGS_SUBDIR \
    SPEC_NAME PROMPT_NAME CANCEL_NAME RESULT_NAME STATE_NAME WORKER_STDOUT_NAME \
    WORKER_UID SLOT TASK AGENT; do
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
# expect <description> <expected> <actual>
expect() {
    if [[ $2 == "$3" ]]; then
        pass "$1"
    else
        fail "$1 (expected '$2', got '$3')"
    fi
}

skip_all() {
    printf 'SKIP  %s\n' "$*"
    printf '\nSKIPPED: this sandbox cannot run the controller (see the reason above).\n'
    printf 'The controller is still covered by microvm-batch-result-integrity\n'
    printf '(validators + layout) and by runtime-validation.sh --section forgery.\n'
    exit 0
}

WORK="$PWD/ctlsmoke"
mkdir -p "$WORK"
TOKEN="$(printf 'c0ffee12%.0s' 1 2 3 4 5 6 7 8)"
OTHER_TOKEN="$(printf 'baadf00d%.0s' 1 2 3 4 5 6 7 8)"

# --- can we sandbox at all? -------------------------------------------------
if ! "$BWRAP" --unshare-user --uid 0 --gid 0 --tmpfs / --ro-bind /nix /nix \
    --proc /proc --dev /dev -- "$BASH_BIN" -c true 2>"$WORK/bwrap.err"; then
    skip_all "bwrap cannot create a user namespace here: $(cat "$WORK/bwrap.err")"
fi
if ! "$FAKEROOT" -- true 2>"$WORK/fakeroot.err"; then
    skip_all "fakeroot does not work here: $(cat "$WORK/fakeroot.err")"
fi

# --- the stubbed systemctl --------------------------------------------------
# Modes: ok (worker exits 0), fail (worker exits 3), hang (worker never
# finishes). A `stop` request makes the unit inactive, like the real thing.
STUB="$WORK/systemctl-stub"
cat >"$STUB" <<EOF
#!$BASH_BIN
log="\$STUB_LOG"
counter="\$STUB_STATE.n"
stopped="\$STUB_STATE.stopped"
printf 'systemctl %s\n' "\$*" >> "\$log"
n=0
[[ -f \$counter ]] && n="\$(cat "\$counter")"
case "\$1" in
    stop) : > "\$stopped" ;;
    show)
        prop="\$3"
        n=\$((n + 1)); printf '%s' "\$n" > "\$counter"
        if [[ -f \$stopped ]]; then
            case "\$prop" in
                ActiveState) echo inactive ;;
                SubState) echo dead ;;
                Result) echo success ;;
                ExecMainCode) echo 1 ;;
                ExecMainStatus) echo 0 ;;
            esac
            exit 0
        fi
        case "\$STUB_MODE:\$prop" in
            hang:ActiveState) echo activating ;;
            hang:SubState) echo start ;;
            hang:Result) echo "" ;;
            hang:ExecMainCode) echo 0 ;;
            hang:ExecMainStatus) echo "" ;;
            ok:ActiveState) if (( n < 2 )); then echo activating; else echo active; fi ;;
            ok:SubState) echo exited ;;
            ok:Result) echo success ;;
            ok:ExecMainCode) echo 1 ;;
            ok:ExecMainStatus) echo 0 ;;
            fail:ActiveState) if (( n < 2 )); then echo activating; else echo failed; fi ;;
            fail:SubState) echo failed ;;
            fail:Result) echo exit-code ;;
            fail:ExecMainCode) echo 1 ;;
            fail:ExecMainStatus) echo 3 ;;
            *) echo "" ;;
        esac
        ;;
esac
exit 0
EOF
chmod +x "$STUB"

# --- an ARGV RECORDER over the exact `jq` the controller resolves -----------
# The allocation token must never appear in a process's ARGUMENT VECTOR:
# /proc/<pid>/cmdline is world-readable (0444) and the untrusted worker shares
# the guest PID namespace, so an argument would hand it the one value it needs
# to mint a result the host would accept. /proc/<pid>/environ is 0400, so the
# token travels in the environment instead.
#
# This records the argv of EVERY jq the controller runs (the wrapper is
# bind-mounted over the store path the controller's own PATH resolves) and the
# assertions below require that none of them ever contained the token. The
# recorder must also actually fire — an empty log fails the check rather than
# passing vacuously.
JQ_ARGV_LOG="$WORK/jq-argv.log"
cp -L "$JQ_TARGET" "$WORK/jq-real"
chmod +x "$WORK/jq-real"
cat >"$WORK/jq-wrapper" <<EOF
#!$BASH_BIN
printf '%s\n' "\$*" >> "\$JQ_ARGV_LOG"
exec "$WORK/jq-real" "\$@"
EOF
chmod +x "$WORK/jq-wrapper"
if ! "$WORK/jq-real" -n '1' >/dev/null 2>&1; then
    skip_all "the copied jq does not run here (cannot record argv)"
fi

# --- fixture ---------------------------------------------------------------
FIXTURE="$WORK/share"
WSFIXTURE="$WORK/workspace"

# build_fixture [jq filter applied to the spec]
build_fixture() {
    local filter="${1:-.}"
    rm -rf "$FIXTURE" "$WSFIXTURE"
    mkdir -p "$FIXTURE/$INPUT_SUBDIR" "$FIXTURE/$CONTROLLER_SUBDIR" \
        "$FIXTURE/$WORKER_SUBDIR/artifacts" "$WSFIXTURE"
    jq -c \
        --argjson version "$SPEC_VERSION" \
        --arg taskId "$TASK" --arg allocationToken "$TOKEN" \
        --arg slot "$SLOT" --arg agent "$AGENT" \
        --arg promptFile "$GUEST_JOB_DIR/$INPUT_SUBDIR/$PROMPT_NAME" \
        "{version:\$version, taskId:\$taskId, allocationToken:\$allocationToken,
          slot:\$slot, agent:\$agent, workspace:\"/workspace\",
          promptFile:\$promptFile, timeoutSeconds:120,
          resourceClass:\"normal\", persistAgentState:false} | $filter" \
        -n >"$FIXTURE/$INPUT_SUBDIR/$SPEC_NAME"
    printf 'Print the word ok and exit.\n' >"$FIXTURE/$INPUT_SUBDIR/$PROMPT_NAME"
    chmod 0400 "$FIXTURE/$INPUT_SUBDIR/$SPEC_NAME"
    chmod 0444 "$FIXTURE/$INPUT_SUBDIR/$PROMPT_NAME"
    chmod 0755 "$FIXTURE" "$FIXTURE/$INPUT_SUBDIR" "$FIXTURE/$WORKER_SUBDIR"
    chmod 0700 "$FIXTURE/$CONTROLLER_SUBDIR"
}

put_cancel() {
    local token="$1"
    jq -nc --argjson version "$SPEC_VERSION" --arg taskId "$TASK" \
        --arg allocationToken "$token" --arg requestedAt "2025-01-01T00:00:00Z" \
        '{version:$version, taskId:$taskId, allocationToken:$allocationToken,
          requestedAt:$requestedAt}' >"$FIXTURE/$INPUT_SUBDIR/$CANCEL_NAME"
    chmod 0400 "$FIXTURE/$INPUT_SUBDIR/$CANCEL_NAME"
}

# run_controller <stub-mode> [--no-fakeroot]
# Echoes the controller's exit code; its output goes to $WORK/controller.log.
run_controller() {
    local mode="$1" fake="${2:-fakeroot}" rc=0
    export STUB_MODE="$mode"
    export STUB_LOG="$WORK/systemctl.log"
    export STUB_STATE="$WORK/stub"
    : >"$STUB_LOG"
    : >"$JQ_ARGV_LOG"
    rm -f "$STUB_STATE.n" "$STUB_STATE.stopped" "$WORK/logs.stat"
    # A fresh tmpfs root, so bwrap may create the mount points the controller
    # hard-codes ($GUEST_JOB_DIR, `/workspace`) even where `/` is not
    # writable — which is the case inside the Nix build sandbox. Only the store,
    # /etc, a minimal /dev, /proc, /tmp and the work directory are brought in.
    local -a cmd=(
        "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname "$SLOT"
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc
        --dev /dev --proc /proc --tmpfs /tmp
        --bind "$WORK" "$WORK"
        --bind "$FIXTURE" "$GUEST_JOB_DIR"
        --bind "$WSFIXTURE" /workspace
        --setenv STUB_MODE "$mode"
        --setenv STUB_LOG "$STUB_LOG"
        --setenv STUB_STATE "$STUB_STATE"
        --setenv JQ_ARGV_LOG "$JQ_ARGV_LOG"
        --bind "$STUB" "$SYSTEMCTL_TARGET"
        --bind "$WORK/jq-wrapper" "$JQ_TARGET"
        --
    )
    if [[ $fake == fakeroot ]]; then
        # The ownership split the controller insists on: root-owned input and
        # controller directories, agent-owned worker dir and workspace.
        cmd+=(
            "$FAKEROOT" -- "$BASH_BIN" -c "
                chown -R 0:0 $GUEST_JOB_DIR
                chown $WORKER_UID:$WORKER_UID $GUEST_JOB_DIR/$WORKER_SUBDIR
                chown -R $WORKER_UID:$WORKER_UID /workspace
                crc=0
                $CONTROLLER || crc=\$?
                # Ownership/mode of the worker LOG area as seen INSIDE the
                # namespace (the only place fakeroot's ownership is visible).
                stat -c '%n %u %a' $GUEST_JOB_DIR/$WORKER_LOGS_SUBDIR \
                    $GUEST_JOB_DIR/$WORKER_LOGS_SUBDIR/* > $WORK/logs.stat 2>&1 || true
                exit \$crc"
        )
    else
        cmd+=("$CONTROLLER")
    fi
    "${cmd[@]}" >"$WORK/controller.log" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# The host verifier, run in a namespace where the fixture appears root-owned
# (exactly as it does inside the guest over virtiofs). Prints its output (the
# validated JSON, or the rejection reason) and never aborts the harness; the
# verifier's exit code lands in VERIFY_RC.
# Results land in the globals VERIFY_RC / VERIFY_OUT rather than on stdout: a
# command substitution would run this in a subshell and lose the exit code.
VERIFY_RC=0
VERIFY_OUT=""
verify() {
    local kind="$1" token="$2"
    local name="$RESULT_NAME"
    [[ $kind == state ]] && name="$STATE_NAME"
    VERIFY_RC=0
    # The expected token goes in the ENVIRONMENT: the verifier refuses a
    # `--token` argument, because argv is world-readable via /proc.
    AGENT_JOB_EXPECTED_TOKEN="$token" \
        "$BWRAP" --unshare-user --uid 0 --gid 0 --bind / / --dev-bind /dev /dev --proc /proc \
        --setenv AGENT_JOB_EXPECTED_TOKEN "$token" \
        -- "$VERIFIER" --result "$FIXTURE/$CONTROLLER_SUBDIR/$name" --kind "$kind" \
        --task "$TASK" --slot "$SLOT" --agent "$AGENT" \
        >"$WORK/verify.out" 2>&1 || VERIFY_RC=$?
    VERIFY_OUT="$(cat "$WORK/verify.out")"
}

# One field of the VALIDATED result, or the empty string when the document was
# rejected (so a rejection can never be mistaken for a field value).
result_field() {
    verify result "$TOKEN"
    ((VERIFY_RC == 0)) || return 0
    jq -r "$1" <<<"$VERIFY_OUT"
}

printf '=== 1. a healthy job: the controller reports what the worker did ===\n'
build_fixture
rc="$(run_controller ok)"
if [[ $rc != 0 ]]; then cat "$WORK/controller.log"; fi
expect "the controller exits 0 for a completed job" 0 "$rc"
verify result "$TOKEN"
if ((VERIFY_RC == 0)) && [[ $VERIFY_OUT == \{* ]]; then
    pass "the HOST verifier accepts the controller's result for this allocation"
else
    fail "the host verifier rejected the controller's own result: $VERIFY_OUT"
fi
expect "state=completed" completed "$(result_field .state)"
expect "exitCode=0" 0 "$(result_field .exitCode)"
expect "the result carries this allocation's token" "$TOKEN" "$(result_field .allocationToken)"
expect "the result carries the slot" "$SLOT" "$(result_field .slot)"
verify result "$OTHER_TOKEN"
if ((VERIFY_RC == 2)) && [[ $VERIFY_OUT == *"allocation token does not belong"* ]]; then
    pass "the same result is REJECTED for a different allocation"
else
    fail "a foreign allocation accepted the result (rc=$VERIFY_RC, $VERIFY_OUT)"
fi
verify state "$TOKEN"
if ((VERIFY_RC == 0)) && [[ "$(jq -r .phase <<<"$VERIFY_OUT")" == finished ]]; then
    pass "the controller's progress channel ends at phase=finished"
else
    fail "unexpected controller phase: $VERIFY_OUT"
fi
if grep -q "start --no-block agent-job-worker@$AGENT.service" "$WORK/systemctl.log"; then
    pass "the worker was started as agent-job-worker@$AGENT.service"
else
    fail "the worker unit was not started as expected: $(cat "$WORK/systemctl.log")"
fi

printf '\n=== 1b. the allocation token never appears in a process ARGV ===\n'
# EXECUTED property (not a grep over the source): every jq the controller ran
# was recorded argv-for-argv by the wrapper bound over the store path the
# controller resolves. If the token had been passed with `--arg`, it would be
# in this log — and, at runtime, in /proc/<pid>/cmdline, which is 0444 and
# readable by the untrusted worker.
if [[ -s $JQ_ARGV_LOG ]]; then
    pass "the argv recorder actually observed jq invocations ($(wc -l <"$JQ_ARGV_LOG") of them)"
else
    fail "the argv recorder saw nothing: the wrapper was not the jq the controller ran"
fi
if grep -qF -- "$TOKEN" "$JQ_ARGV_LOG"; then
    fail "the allocation token appeared in a process argv: $(grep -F -- "$TOKEN" "$JQ_ARGV_LOG" | head -1)"
else
    pass "no jq the controller ran received the allocation token in its argv"
fi
# ... but the token IS in the documents it wrote, i.e. it really did have it.
if [[ "$(result_field .allocationToken)" == "$TOKEN" ]]; then
    pass "the controller nevertheless recorded the token in its result (so the check is not vacuous)"
else
    fail "the controller did not record the allocation token at all"
fi

printf '\n=== 1c. the worker log files are ROOT-owned, next to the worker dir ===\n'
# systemd (root) opens stdout.log/stderr.log with `append:` and follows
# symlinks, so neither the files nor their directory may be replaceable by the
# worker uid. Observed INSIDE the namespace, where fakeroot's ownership applies.
if [[ -s "$WORK/logs.stat" ]]; then
    if awk '{ if ($2 != 0) exit 1 }' "$WORK/logs.stat"; then
        pass "the worker log directory and its files are owned by uid 0"
    else
        fail "a worker log path is not root-owned: $(cat "$WORK/logs.stat")"
    fi
    if grep -q "$WORKER_LOGS_SUBDIR/$WORKER_STDOUT_NAME 0 644" "$WORK/logs.stat"; then
        pass "the worker's stdout log is root:root 0644 (the worker may read, never write it)"
    else
        fail "unexpected worker log layout: $(cat "$WORK/logs.stat")"
    fi
else
    fail "the controller created no worker log area"
fi
if [[ -d "$FIXTURE/$WORKER_LOGS_SUBDIR" && ! -e "$FIXTURE/$WORKER_SUBDIR/$WORKER_STDOUT_NAME" ]]; then
    pass "the logs live in $WORKER_LOGS_SUBDIR/, not inside the worker-writable dir"
else
    fail "the worker log files are inside the worker-writable directory"
fi

printf '\n=== 2. a failing agent is reported as failed, not as an error ===\n'
build_fixture
rc="$(run_controller fail)"
expect "the controller still exits 0 (the result is the outcome)" 0 "$rc"
expect "state=failed" failed "$(result_field .state)"
expect "the worker's real exit code (3) is reported" 3 "$(result_field .exitCode)"

printf '\n=== 3. the deadline belongs to the controller ===\n'
build_fixture '.timeoutSeconds = 1'
rc="$(run_controller hang)"
expect "the controller exits 0 after a timeout" 0 "$rc"
expect "state=timed-out" timed-out "$(result_field .state)"
expect "exitCode=124" 124 "$(result_field .exitCode)"
expect "timedOut=true" true "$(result_field .timedOut)"
if grep -q "^systemctl stop" "$WORK/systemctl.log"; then
    pass "the controller stopped the worker unit (i.e. its whole cgroup)"
else
    fail "the controller did not stop the worker: $(cat "$WORK/systemctl.log")"
fi

printf '\n=== 4. cancellation is bound to the allocation token ===\n'
build_fixture
put_cancel "$TOKEN"
rc="$(run_controller hang)"
expect "the controller exits 0 after a cancellation" 0 "$rc"
expect "state=cancelled" cancelled "$(result_field .state)"
expect "exitCode=130" 130 "$(result_field .exitCode)"

build_fixture
put_cancel "$OTHER_TOKEN"
rc="$(run_controller ok)"
expect "a cancellation for ANOTHER allocation is ignored" completed "$(result_field .state)"

printf '\n=== 5. a rejected spec becomes an infrastructure error ===\n'
build_fixture '. + {smuggled: "payload"}'
rc="$(run_controller ok)"
expect "an unknown spec field makes the controller exit 70" 70 "$rc"
expect "state=infrastructure-error" infrastructure-error "$(result_field .state)"
if ! grep -q "start --no-block" "$WORK/systemctl.log"; then
    pass "no worker was started for an invalid spec"
else
    fail "a worker was started despite an invalid spec"
fi

build_fixture '.version = 1'
rc="$(run_controller ok)"
expect "a v1 spec is refused (fail closed)" 70 "$rc"

build_fixture '.slot = "agent-normal-99"'
rc="$(run_controller ok)"
expect "a spec naming another slot is refused" 70 "$rc"

build_fixture '.agent = "nosuchagent"'
rc="$(run_controller ok)"
expect "a spec naming an unsupported agent is refused" 70 "$rc"

build_fixture '.promptFile = "/etc/shadow"'
rc="$(run_controller ok)"
expect "a spec pointing the prompt outside the input dir is refused" 70 "$rc"

build_fixture '.timeoutSeconds = 999999999'
rc="$(run_controller ok)"
expect "an out-of-range timeout is refused" 70 "$rc"

printf '\n=== 6. a broken trust boundary stops the job BEFORE it starts ===\n'
build_fixture
# No fakeroot: the worker directory then appears root-owned, i.e. the worker
# could not write its own logs — the layout is not the one we require.
rc="$(run_controller ok no-fakeroot)"
expect "the controller refuses a share with the wrong ownership" 70 "$rc"
if grep -q "REFUSING TO RUN" "$WORK/controller.log"; then
    pass "it says so loudly"
else
    fail "no refusal message: $(cat "$WORK/controller.log")"
fi
if [[ ! -e "$FIXTURE/$CONTROLLER_SUBDIR/$RESULT_NAME" ]]; then
    pass "and writes NO result into a channel it cannot trust"
else
    fail "it wrote a result into an untrusted channel"
fi
if ! grep -q "start --no-block" "$WORK/systemctl.log"; then
    pass "and starts no worker"
else
    fail "it started a worker anyway"
fi

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
