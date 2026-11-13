#!/usr/bin/env bash
# egress-broker-live-validation — the scripted half of the phase-B egress
# runbook (bd myconfig-n8kv.1.6, docs/design/egress.md). Run ON f13, from
# the repo checkout, once the rebuilt switch is in place:
#
#   MYSBX=<path-to-mysbx> ./nix/egress-broker-live-validation.sh <repo-dir>
#
# Each probe prints PASS/FAIL and names the decision record it validates;
# the first FAIL exits non-zero. The script is deliberately dumb: plain
# bash plus the sandbox itself.
#
# The pins are NOT read from this shell. A Nix-wrapped mysbx sets them in
# the CHILD it starts (`--set`/`--set-default` on the wrapper), so this
# process sees none of them and must ask the wrapper instead — `mysbx
# doctor` reports the broker/relay it resolved, and `mysbx --dry-run`,
# `mysbx status` and `mysbx diagnose egress` are the same crossing.
#
# What it does NOT do: press keys in a multiplexer, start a nested VM, or
# judge whether an agent's answer was any good. The agent/model requests
# are the one human step, and it asks for them explicitly.
set -euo pipefail

repo=${1:?usage: egress-broker-live-validation.sh <repo-dir>}
mysbx=${MYSBX:-mysbx}
# cli.md D8: 70 is the refused-configuration code every refusal uses.
MYSBX_EXIT_REFUSED=70
runtime=${XDG_RUNTIME_DIR:-}
[ -n "$runtime" ] ||
    {
        echo "egress-broker-live-validation: XDG_RUNTIME_DIR is unset; the run
directory root comes from it (docs/design/egress.md E4)" >&2
        exit 70
    }

fail() {
    printf 'FAIL %s — %s\n' "$1" "$2" >&2
    printf '  see docs/egress-validation-log.md and docs/design/egress.md\n' >&2
    exit 1
}

passes=0
skips=0
pass() {
    passes=$((passes + 1))
    printf 'PASS %s\n' "$1"
}

# A probe that could not be exercised reports SKIP, never PASS: a check
# that did not run must not read as evidence (bd myconfig-b102).
skip() {
    skips=$((skips + 1))
    printf 'SKIP %s — %s\n' "$1" "$2"
}

note() {
    printf '     %s\n' "$1"
}

finish() {
    printf '\n%d probe(s) passed, %d skipped (skipped probes are NOT evidence)\n' \
        "$passes" "$skips"
    [ "$skips" = 0 ] ||
        printf 're-run the skipped probes by hand before recording results\n'
}

# How many per-run directories exist right now. A missing root is ZERO, not
# an error: before the first broker run the directory does not exist, and
# `find`'s non-zero exit would abort this script under `set -e`.
count_run_dirs() {
    if [ -d "$runtime/mysbx-egress" ]; then
        find "$runtime/mysbx-egress" -mindepth 1 -maxdepth 1 | wc -l
    else
        printf '0\n'
    fi
}

find "$repo" -maxdepth 0 >/dev/null 2>&1 || fail "repo" "no such directory: $repo"
cd "$repo"

# ---- effective pins, read THROUGH the wrapper ---------------------------
# doctor's exit code says whether ANY of its checks failed, which on a
# healthy host can still be true for reasons this runbook does not care
# about (a nested-virt probe, a missing bwrap). Only the egress line is
# this probe's business, so the output is read and the status ignored.
doctor=$("$mysbx" doctor bubblewrap 2>&1 || true)
if ! grep -q 'egress broker:' <<<"$doctor"; then
    # No egress pins at all: an unwrapped binary. Say so, rather than
    # leaving the operator with a missing-line mystery.
    grep -q 'network = "proxy-only" is not pinned' <<<"$doctor" &&
        fail "pins" "this mysbx pins no egress channel; run the WRAPPED binary (the Nix switch), not a cargo/debug build"
    fail "pins" "mysbx doctor reported no egress broker line: $doctor"
fi
broker_line=$(grep -E '^[A-Z]+ +egress broker:' <<<"$doctor" || true)
[ -n "$broker_line" ] ||
    fail "pins" "mysbx doctor reported no egress broker line; is this the rebuilt wrapper?"
grep -qi 'not pinned' <<<"$broker_line" &&
    fail "pins" "this build pins no per-run broker, so there is no broker path to validate"
grep -qi 'is not an executable file' <<<"$broker_line" &&
    fail "pins" "the pinned broker is not executable: $broker_line"
broker=$(sed -E 's/^[A-Z]+ +egress broker: *([^ ]+).*/\1/' <<<"$broker_line")
note "broker:  $broker"
pass "the wrapper pins a per-run broker (docs/design/egress.md E1)"

# ---- E5: a dry run creates nothing, and names the per-run channel --------
before=$(count_run_dirs)
dry=$("$mysbx" --dry-run --verbose 2>&1) || fail "dry run" "a --dry-run exited non-zero"
grep -q "proxy-only" <<<"$dry" || fail "dry run" "the report does not name the proxy-only profile"
grep -q "via the relay from" <<<"$dry" ||
    fail "dry run" "the report names no per-run channel source: $(grep '^network:' <<<"$dry")"
if grep -qE "^network:.*via the relay from /run/" <<<"$dry"; then
    fail "dry run" "the report still names the STATIC forwarder directory as the source"
fi
[ "$before" = "$(count_run_dirs)" ] ||
    fail "dry run creates nothing" "the run directory count changed ($before -> $(count_run_dirs)); docs/design/egress.md E5"
pass "a dry run creates nothing and names the per-run channel (docs/design/egress.md E5)"

# ---- E1/E4: a REAL run brings the broker up and logs to the sidecar -----
# `--result` makes this a WAITED run: mysbx stays the parent, so it reaps the
# broker and removes the run directory (E2). A plain `run` is exec mode and
# leaves the directory behind ON PURPOSE — the next run's sweep owns it —
# so it cannot prove the E2/E3 cleanup.
"$mysbx" run --result -- true >/dev/null 2>&1 ||
    fail "real run" "a waited 'run --result -- true' failed"
sidecar=$("$mysbx" status 2>/dev/null | sed -n 's/^## sidecar:  *\([^ ]*\).*/\1/p')
[ -n "$sidecar" ] || fail "sidecar" "cannot resolve the sidecar from 'mysbx status'"
latest=$(find "$sidecar/egress" -mindepth 2 -maxdepth 2 -name decisions.jsonl -printf '%h\n' 2>/dev/null |
    tail -1)
[ -n "$latest" ] || fail "real run" "no decisions.jsonl appeared under $sidecar/egress; E4"
grep -q '"event":"listen"' "$latest/decisions.jsonl" ||
    fail "real run" "the log carries no listen line: $(head -3 "$latest/decisions.jsonl")"
pass "a real run starts the broker and logs to the sidecar ($(basename "$latest"))"

# ---- E2/E3: the run directory does not outlive a waited run -------------
leftover=$(find "$runtime/mysbx-egress" -mindepth 1 -maxdepth 1 2>/dev/null || true)
if [ -n "$leftover" ]; then
    while read -r stale; do
        pid=$(basename "$stale")
        kill -0 "$pid" 2>/dev/null && continue
        fail "run directory cleanup" "$stale outlived its run; E2/E3"
    done <<<"$leftover"
fi
pass "a waited run removes its run directory (E2/E3)"

# ---- diagnose reads the log back ---------------------------------------
diag=$("$mysbx" diagnose egress 2>&1) || fail "diagnose" "'mysbx diagnose egress' exited non-zero"
grep -q "run " <<<"$diag" || fail "diagnose" "the verb reported no run: $diag"
diag_json=$("$mysbx" diagnose egress --json 2>&1) || fail "diagnose --json" "exited non-zero"
grep -q '"pid":' <<<"$diag_json" || fail "diagnose --json" "no pid field: $diag_json"
pass "mysbx diagnose egress reads the log back (cli.md D29)"

# ---- E5: fail closed ----------------------------------------------------
# Inject the failure WITHOUT touching the pinned broker: the wrapper pins
# `MYSBX_EGRESS_BROKER` with `--set` (immovable) but `MYSBX_EGRESS_SOCKET`
# and `_PORT` with `--set-default`, so a run can be given a socket path
# step 4b rejects. That is the same fail-closed path (exit 70, nothing
# created) reached without moving a shared executable that a concurrent
# mysbx run may be using.
before=$(count_run_dirs)
sidecar_before=$(find "$sidecar" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)
MYSBX_EGRESS_SOCKET='/run/not-the-egress-dir/litellm.sock' \
    "$mysbx" run -- true >/dev/null 2>&1 && refused=0 || refused=$?
[ "$refused" = "$MYSBX_EXIT_REFUSED" ] ||
    fail "fail closed" "a run with a broken egress pin exited $refused, expected $MYSBX_EXIT_REFUSED (E5)"
[ "$before" = "$(count_run_dirs)" ] ||
    fail "fail closed" "the refused run left a run directory behind (E5)"
[ "$sidecar_before" = "$(find "$sidecar" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)" ] ||
    fail "fail closed" "the refused run created sidecar state (E5)"
pass "a broken egress pin refuses the run (exit $MYSBX_EXIT_REFUSED) and creates nothing (E5)"

# ---- the profile's SUPPORTED backends still run ------------------------
# `--verbose` is REQUIRED: the `network:` line (and with it the per-run
# channel) is part of the report, which a bare --dry-run does not print —
# it prints the argv only.
for backend in bubblewrap nono; do
    out=$("$mysbx" --backend "$backend" --dry-run --verbose 2>&1) ||
        fail "supported backends" "$backend exited non-zero on --dry-run --verbose: $out"
    grep -qiE "refus|not supported|not enforced" <<<"$out" &&
        fail "supported backends" "$backend refuses the profile: $out"
    grep -q "via the relay from" <<<"$out" ||
        fail "supported backends" "$backend's report names no egress channel"
done
pass "the bwrap-built tiers (bubblewrap, nono) both accept the profile"

# ---- the other backends still refuse it --------------------------------
# A refusing backend EXITS 70 (the profile is refused, never weakened), so
# the exit status is expected — what must hold is that it refused, not that
# it succeeded.
for backend in podman-gvisor podman-krun krun microvm-qemu microvm-ch; do
    out=$("$mysbx" --backend "$backend" --dry-run 2>&1) && status=0 || status=$?
    grep -qiE "refus|not enforced|cannot express" <<<"$out" ||
        fail "refusals" "$backend does not refuse network = proxy-only: $out"
    [ "$status" = 0 ] &&
        fail "refusals" "$backend refused the profile but exited 0 (expected the refusal code)"
done
pass "every other backend still refuses the profile"

# ---- The human step: an agent completes a model request -----------------
cat <<'EOF'

  The remaining probes need a human at the terminal:
    * run one model request from `pi` and one from `opencode` inside
      `mysbx` (with backend = "nono" as well, if that host uses it)
    * then `mysbx diagnose egress` shows the admissions, and
      `mysbx diagnose egress --verbose` the peer pids
    * kill the broker (`pkill -f mysbx-egress-broker`) and confirm the
      sandbox's requests now fail (no fallback) and the log's next line is
      an `unreachable` rather than an `allow`
    * `mysbx --detach`, then `mysbx attach`: the broker must outlive the
      outer run and die with the inner one
  Record the results as a dated block in docs/egress-validation-log.md.

EOF
printf 'PASS scripted probes (the agent, kill-mid-run and detach probes are manual)\n'
passes=$((passes + 1))
finish
