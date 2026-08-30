#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Harness for the in-container entrypoint wrapper (../nix/agent-gvisor-init.sh),
# run by nix/checks.nix (check `agent-gvisor-init`). It covers the `--nix`
# preflight (docs/nix-in-sandbox.md §1/§7): the wrapper must FAIL CLOSED when
# the writable store or the Nix state directories are not usable, instead of
# starting a session in which every later `nix` call breaks.
#
# The wrapper is plain bash and reads its inputs from the environment
# (AGENT_GVISOR_NIX, TMPDIR, NIX_LOG_DIR, NIX_STORE_DIR), so the whole
# preflight is testable without a container: point those at directories in a
# temp tree and make individual ones unwritable.
set -euo pipefail

INIT="${INIT:?INIT must point at agent-gvisor-init.sh}"

WORK="$(mktemp -d)"
# The failure cases leave read-only directories behind; chmod them back so the
# cleanup cannot fail.
trap 'chmod -R u+rwX "$WORK" 2>/dev/null; rm -rf "$WORK"' EXIT

passed=0
failed=0

fail() {
    printf 'FAIL %s\n' "$*" >&2
    failed=$((failed + 1))
}

pass() {
    printf 'ok   %s\n' "$*"
    passed=$((passed + 1))
}

# scenario NAME — a fresh tree with a writable store and state dirs; prints
# its root. Callers break individual directories afterwards.
scenario() {
    local root="$WORK/$1"
    mkdir -p "$root/store" "$root/tmp" "$root/log"
    printf '%s' "$root"
}

# run_init ROOT [LOGDIR] — runs the wrapper with a payload that records that
# it ran, capturing stdout+stderr in $OUT and the exit code in $STATUS. The
# output file and the payload marker live OUTSIDE the scenario tree, so they
# also work when the scenario made a directory read-only.
OUT="$WORK/output"
MARKER="$WORK/payload-ran"
STATUS=0
run_init() {
    local root=$1 logdir=${2:-$1/log}
    rm -f "$MARKER"
    set +e
    env -i \
        PATH="$PATH" \
        AGENT_GVISOR_NIX=1 \
        NIX_STORE_DIR="$root/store" \
        TMPDIR="$root/tmp" \
        NIX_LOG_DIR="$logdir" \
        bash "$INIT" touch "$MARKER" >"$OUT" 2>&1
    STATUS=$?
    set -e
}

expect_status() {
    local desc=$1 expected=$2
    if [ "$STATUS" = "$expected" ]; then
        pass "$desc"
    else
        fail "$desc (exit $STATUS, expected $expected)"
        sed 's/^/     /' "$OUT" >&2
    fi
}

expect_output() {
    local desc=$1 needle=$2
    if grep -q -- "$needle" "$OUT"; then
        pass "$desc"
    else
        fail "$desc (no match for: $needle)"
        sed 's/^/     /' "$OUT" >&2
    fi
}

expect_payload() {
    local desc=$1 want=$2 # want: ran | not-ran
    local got=not-ran
    [ -e "$MARKER" ] && got=ran
    if [ "$got" = "$want" ]; then
        pass "$desc"
    else
        fail "$desc (payload $got, expected $want)"
    fi
}

# 1. Everything writable: the wrapper prepares the state dirs and execs the
#    payload.
root="$(scenario ok)"
rmdir "$root/tmp" "$root/log" # the wrapper must create them itself
run_init "$root"
expect_status "nix session with a writable store starts" 0
expect_payload "payload runs" ran
if [ -d "$root/tmp" ] && [ -d "$root/log" ]; then
    pass "state dirs created"
else
    fail "state dirs created"
fi

# 2. Store not writable (the copy-up/ownership failure mode, §7 V1): refuse.
root="$(scenario ro-store)"
chmod a-w "$root/store"
run_init "$root"
expect_status "unwritable store aborts the session" 1
expect_output "unwritable store is reported" "is not writable in this sandbox"
expect_payload "payload does not run on an unusable store" not-ran

# 3. Store writable, but the Nix state dir cannot be created (read-only
#    parent, i.e. a session home that did not mount): refuse as well.
root="$(scenario ro-parent)"
mkdir -p "$root/state"
chmod a-w "$root/state"
run_init "$root" "$root/state/nix/log"
expect_status "uncreatable state dir aborts the session" 1
expect_output "uncreatable state dir is reported" "could not create the Nix state directory"
expect_payload "payload does not run without Nix state" not-ran

# 4. Existing but unwritable state dir: same refusal, different message.
root="$(scenario ro-log)"
chmod a-w "$root/log"
run_init "$root"
expect_status "unwritable state dir aborts the session" 1
expect_output "unwritable state dir is reported" "is not writable"
expect_payload "payload does not run with an unwritable state dir" not-ran

# 5. Without --nix nothing is probed: a session with an unwritable store is a
#    perfectly normal (non-nix) session.
root="$(scenario no-nix)"
chmod a-w "$root/store"
rm -f "$MARKER"
set +e
env -i PATH="$PATH" NIX_STORE_DIR="$root/store" \
    bash "$INIT" touch "$MARKER" >"$OUT" 2>&1
STATUS=$?
set -e
expect_status "non-nix session ignores the store" 0
expect_payload "payload runs in a non-nix session" ran

printf '\n%s passed, %s failed\n' "$passed" "$failed"
[ "$failed" -eq 0 ]
