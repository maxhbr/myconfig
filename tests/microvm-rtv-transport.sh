#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED harness for the GUEST COMMAND TRANSPORT of
# modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh, run from the
# `microvm-rtv-transport` check in tests/microvm.nix.
#
# WHY THIS EXISTS
# ---------------
# The real-KVM suite needs /dev/kvm and root, so CI can never run it. What CI
# CAN run is the part that decided whether any of its 46 guest-side denials mean
# anything at all: the way a command gets from the suite into the guest.
#
# `agent-microvm ssh <slot> -- <argv...>` cannot preserve argument boundaries —
# OpenSSH joins the remaining argv with single spaces and the guest's LOGIN
# SHELL re-parses the result. That shell is the one guest.nix gives the agent
# user (`$GUEST_SHELL` here, plain bash since the lightweight guest dropped
# fish). So a payload like
#
#     ssh <slot> -- sh -c "timeout 5 sh -c '</dev/tcp/GW/22'"
#
# used to reach the guest as `sh -c timeout 5 sh -c '...'` — `sh -c` got only
# the word `timeout`, the command failed for a QUOTING reason, and the
# corresponding "the guest must not be able to ..." check passed VACUOUSLY.
# Payloads containing `${VAR:-}` were worse still: they were expanded (or, under
# the guest's previous fish login shell, a syntax error) before the inner shell
# ever saw them, so the whole credential-environment block could only fail or
# skip.
#
# This harness stubs `agent-microvm ssh` with something that behaves exactly
# like OpenSSH + the guest login shell (join argv with single spaces, hand the
# string to `$GUEST_SHELL -c`), sources the suite's transport block unmodified,
# and asserts:
#
#   1. every payload class from that bug arrives in the guest AS WRITTEN,
#   2. exit codes propagate verbatim,
#   3. NEGATIVE CONTROL: the previous, unquoted transport really does mangle
#      those payloads here — so this harness fails if the fix is reverted,
#   4. the suite's own transport PROBE rejects the unquoted transport, i.e. a
#      future regression turns the real suite's checks into SKIPs and a loud
#      FAIL instead of silent passes.
#
# SC2016: every `${VAR}` in a payload below must expand IN THE GUEST, never here
# — that is the whole point of the transport. SC2034: GATEWAY/LITELLM_PORT are
# consumed by the transport block this harness sources.
# shellcheck disable=SC2016,SC2034
set -euo pipefail

for v in SUITE GUEST_SHELL; do
    [[ -n ${!v:-} ]] || {
        printf 'harness: required environment variable %s is unset\n' "$v" >&2
        exit 2
    }
done

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# --- the stub launcher: OpenSSH argv flattening + the guest login shell ------
# An ABSOLUTE interpreter path: the nix build sandbox has no /usr/bin/env, and a
# stub that cannot start would make BOTH the fixed and the reverted transport
# "fail", i.e. the negative control would pass for the wrong reason.
STUB_BASH="${BASH:-$(command -v bash)}"
[[ -x $STUB_BASH ]] || {
    printf 'harness: no usable bash interpreter for the stub\n' >&2
    exit 2
}
cat >"$WORK/agent-microvm" <<EOF
#!$STUB_BASH
# usage: agent-microvm ssh <slot> -- <argv...>   (the only subcommand needed)
[[ \$1 == ssh ]] || exit 64
shift 2
[[ \${1-} == -- ]] && shift
joined="\$*"          # <- exactly what OpenSSH sends: argv joined with spaces
exec "$GUEST_SHELL" -c "\$joined"
EOF
chmod +x "$WORK/agent-microvm"
LAUNCHER="$WORK/agent-microvm"
export LAUNCHER
# Referenced by the extracted block (tcp_probe_works); never contacted here.
GATEWAY="127.0.0.1"
LITELLM_PORT="1"

# --- the block under test, taken verbatim from the suite --------------------
awk '/^# --- the guest command TRANSPORT/{f=1} f{print} /^# THE readiness gate/{f=0}' \
    "$SUITE" >"$WORK/transport.sh"
for fn in guest_transport_mode guest_sh guest assert_transport TRANSPORT_PROBE; do
    grep -q "$fn" "$WORK/transport.sh" || {
        printf 'harness: could not extract %s from %s (has the suite been restructured?)\n' \
            "$fn" "$SUITE" >&2
        exit 2
    }
done

PASSED=0
FAILED=0
pass() {
    printf 'PASS  %s\n' "$*"
    PASSED=$((PASSED + 1))
}
fail() {
    printf 'FAIL  %s\n' "$*"
    FAILED=$((FAILED + 1))
}
skip() { printf 'SKIP  %s\n' "$*"; }
# shellcheck source=/dev/null
source "$WORK/transport.sh"

slot=stub-slot

printf '=== 1. the transport the suite uses ===\n'
assert_transport "$slot" "stub guest"

# Payload class A: `sh -c "timeout N sh -c '<redirect>'"` (all seven /dev/tcp
# denials, the getent and the ip -6 route check).
if out="$(guest "$slot" sh -c "timeout 5 sh -c 'echo inner-ran'" 2>&1)" &&
    [[ ${out//$'\r'/} == "inner-ran" ]]; then
    pass "a nested sh -c payload runs in the guest as written"
else
    fail "the nested payload was mangled: '$out'"
fi

# Payload class B: ${VAR:-} in a login shell (the eleven token checks).
if HOME=/nonexistent guest "$slot" sh -c 'test -n "${HOME:-}"' >/dev/null 2>&1; then
    pass 'a ${VAR:-} expansion survives the transport'
else
    fail 'a ${VAR:-} expansion did not survive the transport'
fi

# Payload class C: the credential-environment POSITIVE CONTROL, which decides
# whether the whole block is evaluated or permanently skipped.
if out="$(OPENAI_BASE_URL=http://x/v1 guest_sh "$slot" 'printf %s "${OPENAI_BASE_URL-}"')" &&
    [[ ${out//$'\r'/} == "http://x/v1" ]]; then
    pass "the credential-environment probe returns the value, not the empty string"
else
    fail "the credential-environment probe returned '$out'"
fi

# Payload class D: an argument that must NOT be dropped (`ls <dir>` becoming a
# bare `ls` turned an expected denial into a reported SUCCESS).
if guest "$slot" sh -c "ls /definitely/not/here" >/dev/null 2>&1; then
    fail "'ls <missing dir>' succeeded — the argument was dropped on the way"
else
    pass "'ls <missing dir>' really fails (the argument reached the guest)"
fi

rc=0
guest_sh "$slot" 'exit 7' || rc=$?
if ((rc == 7)); then
    pass "exit codes propagate verbatim (7)"
else
    fail "exit code was $rc, not 7"
fi

printf '\n=== 2. NEGATIVE CONTROL: the previous, unquoted transport ===\n'
# Exactly what the suite used to do.
guest_flattened() {
    local s="$1"
    shift
    "$LAUNCHER" ssh "$s" -- "$@"
}
if out="$(guest_flattened "$slot" sh -c "timeout 5 sh -c 'echo inner-ran'" 2>&1)" &&
    [[ ${out//$'\r'/} == "inner-ran" ]]; then
    fail "the unquoted transport ALSO delivered the payload — this harness proves nothing"
else
    pass "the unquoted transport mangles the nested payload (got: '${out//$'\n'/ }')"
fi
if guest_flattened "$slot" sh -c 'test -n "${HOME:-}"' >/dev/null 2>&1; then
    fail 'the unquoted transport ALSO handled ${VAR:-}'
else
    pass 'the unquoted transport fails on ${VAR:-} (it is expanded/split before the inner shell) — the vacuity source'
fi

# ... and the suite's own probe must REJECT that transport, otherwise a
# regression would once again be invisible.
rc=0
out="$("$LAUNCHER" ssh "$slot" -- /bin/sh -c "$TRANSPORT_PROBE" 2>/dev/null)" || rc=$?
if [[ ${out//$'\r'/} == "a b" ]] && ((rc == 7)); then
    fail "the transport probe ACCEPTS an unquoted transport — it cannot detect the bug"
else
    pass "the transport probe rejects an unquoted transport (out='$out', rc=$rc)"
fi

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
