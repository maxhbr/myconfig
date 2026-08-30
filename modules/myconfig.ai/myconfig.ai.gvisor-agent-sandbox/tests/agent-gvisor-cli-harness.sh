#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# CLI-level smoke harness for the Rust `agent-gvisor`, run by
# nix/checks.nix (check `agent-gvisor-cli-harness`). It drives the UNWRAPPED
# binary against the same recording git/podman stubs the cargo integration
# tests embed (../rust/tests/stubs/), covering flows that only show up end
# to end: `doctor`'s probes, a full session cycle, and `list`.
#
# The wrapped package binary cannot be used here: its wrapper prepends the
# REAL git/podman to PATH, which would shadow the stubs.
set -euo pipefail

BIN="${BIN:?BIN must point at the unwrapped agent-gvisor binary}"
STUBS="${STUBS:?STUBS must point at the recording stub directory}"
IMAGE="localhost/agent-gvisor-test:latest"

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT
RECORD="$WORK/record"
STUB_BIN="$WORK/stub-bin"
STATE="$WORK/state"
export HOME="$WORK/home"
REPO="$WORK/repo"
mkdir -p "$RECORD" "$STUB_BIN" "$STATE/sessions" "$HOME" "$REPO"
install -m 0755 "$STUBS/git.sh" "$STUB_BIN/git"
install -m 0755 "$STUBS/podman.sh" "$STUB_BIN/podman"
printf '#!/bin/sh\nexit 0\n' >"$STUB_BIN/runsc"
chmod 0755 "$STUB_BIN/runsc"

export PATH="$STUB_BIN:$PATH"
export RECORD
export AGENT_GVISOR_STATE="$STATE"
export AGENT_GVISOR_DEFAULT_IMAGE="$IMAGE"
export AGENT_GVISOR_PODMAN_RUNTIME="$STUB_BIN/runsc"
export AGENT_GVISOR_HOME_SEED_PATHS=""

passed=0
failed=0

# expect_ok DESC ARGS...
expect_ok() {
    local desc=$1
    shift
    if "$BIN" "$@" >"$WORK/out" 2>"$WORK/err"; then
        printf 'ok   %s\n' "$desc"
        passed=$((passed + 1))
    else
        printf 'FAIL %s\n' "$desc" >&2
        sed 's/^/     /' "$WORK/err" >&2
        failed=$((failed + 1))
    fi
}

# expect_fail DESC EXPECTED-EXIT-CODE ARGS...
expect_fail() {
    local desc=$1 expected=$2
    shift 2
    set +e
    "$BIN" "$@" >"$WORK/out" 2>"$WORK/err"
    local code=$?
    set -e
    if [ "$code" -eq "$expected" ]; then
        printf 'ok   %s\n' "$desc"
        passed=$((passed + 1))
    else
        printf 'FAIL %s (exit %s, expected %s)\n' "$desc" "$code" "$expected" >&2
        sed 's/^/     /' "$WORK/err" >&2
        failed=$((failed + 1))
    fi
}

# expect_err DESC PATTERN — the stderr of the last invocation must match.
expect_err() {
    local desc=$1 pattern=$2
    if grep -q -- "$pattern" "$WORK/err"; then
        printf 'ok   %s\n' "$desc"
        passed=$((passed + 1))
    else
        printf 'FAIL %s (no %q in stderr)\n' "$desc" "$pattern" >&2
        sed 's/^/     /' "$WORK/err" >&2
        failed=$((failed + 1))
    fi
}

# expect_out DESC PATTERN — the stdout of the last invocation must match.
expect_out() {
    local desc=$1 pattern=$2
    if grep -q -- "$pattern" "$WORK/out"; then
        printf 'ok   %s\n' "$desc"
        passed=$((passed + 1))
    else
        printf 'FAIL %s (no %q in stdout)\n' "$desc" "$pattern" >&2
        sed 's/^/     /' "$WORK/out" >&2
        failed=$((failed + 1))
    fi
}

echo "== usage =="
expect_ok "help exits 0" --help

echo "== doctor =="
expect_ok "doctor against a working sandbox" doctor
expect_err "doctor reports a working sandbox" "sandbox works"
touch "$RECORD/run-fail"
expect_fail "doctor against a broken sandbox" 1 doctor
expect_err "doctor explains the broken sandbox" "sandbox startup failed"
rm "$RECORD/run-fail"

echo "== session cycle =="
expect_ok "start creates a detached session" start s1 --repo "$REPO" --detach
expect_ok "status reports the session" status s1
expect_out "status shows the running container" 'status:    running'
expect_fail "run refuses a running container" 1 run s1
expect_err "run names the session" "container is already running: s1"
expect_ok "stop stops the container" stop s1
expect_ok "run recreates the container" run s1 --detach
rm -rf "$RECORD/containers"
expect_fail "logs on an absent container" 1 logs s1
expect_err "logs reports the absent container" "container is absent: s1"
expect_ok "destroy removes the session" destroy s1
expect_fail "destroy of a gone session" 1 destroy s1
expect_err "destroy says unknown session" "unknown session: s1"

echo "== podman argv sanity =="
expect_ok "recreate the session" start s1 --repo "$REPO" --detach
# argv_has PATTERN — matched against every recorded podman invocation
# (one argument per line, NULs replaced by newlines).
argv_has() {
    local f
    for f in "$RECORD"/podman-*.argv; do
        tr '\0' '\n' <"$f"
    done | grep -q -- "$1"
}
for flag in \
    "--userns=keep-id" \
    "--read-only" \
    "--read-only-tmpfs=true" \
    "--cap-drop=ALL" \
    "--security-opt=no-new-privileges" \
    "--cgroup-manager=cgroupfs" \
    "--runtime-flag=ignore-cgroups"; do
    if argv_has "$flag"; then
        printf 'ok   podman argv contains %s\n' "$flag"
        passed=$((passed + 1))
    else
        printf 'FAIL podman argv lacks %s\n' "$flag" >&2
        failed=$((failed + 1))
    fi
done

# A `--nix` session: the store volume mount in the argv, and its cleanup.
echo "== nix sessions =="
expect_ok "start a --nix session" start n1 --repo "$REPO" --nix --detach
if argv_has 'type=volume,src=.*-n1-nix,dst=/nix/store'; then
    printf 'ok   podman argv mounts the nix store volume\n'
    passed=$((passed + 1))
else
    printf 'FAIL podman argv lacks the nix store volume mount\n' >&2
    failed=$((failed + 1))
fi
expect_ok "destroy removes the nix session" destroy n1 --force --delete-branch
# Only destroy dispatches a `volume` subcommand (exists/rm) — proof it
# cleaned up the store volume (the stub accepts any volume command).
if argv_has '^volume$'; then
    printf 'ok   destroy removes the nix volume\n'
    passed=$((passed + 1))
else
    printf 'FAIL destroy does not touch the nix volume\n' >&2
    failed=$((failed + 1))
fi

echo "== list =="
expect_ok "list exits 0" list
expect_out "list shows the session" "s1 .*running .*agent/gvisor/s1"
mkdir -p "$WORK/inc-target"
ln -s "$WORK/inc-target" "$STATE/sessions/inc"
expect_ok "list shows an incomplete session" list
expect_out "list flags the incomplete session" "inc .*incomplete"
mkdir -p "$STATE/sessions/old"
expect_ok "list shows a pre-rewrite entry" list
expect_out "list flags the pre-rewrite entry" "incompatible (pre-rewrite layout)"

echo "== summary =="
if [ "$failed" -eq 0 ]; then
    printf 'all %s harness checks passed\n' "$passed"
else
    printf '%s of %s harness checks FAILED\n' "$failed" "$((passed + failed))" >&2
    exit 1
fi
