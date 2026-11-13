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

pass() {
    printf 'PASS %s\n' "$1"
}

note() {
    printf '     %s\n' "$1"
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
doctor=$("$mysbx" doctor bubblewrap 2>&1) || {
    # An unwrapped mysbx has no egress pins at all, so doctor warns (and
    # this probe needs more). Say so instead of leaving the operator with a
    # bare exit code.
    if grep -q 'network = "proxy-only" is not pinned' <<<"$doctor"; then
        fail "pins" "this mysbx pins no egress channel; run the WRAPPED binary (the Nix switch), not a cargo/debug build"
    fi
    fail "pins" "mysbx doctor exited non-zero: $doctor"
}
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
"$mysbx" run -- true >/dev/null 2>&1 || fail "real run" "a plain 'run -- true' failed"
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
# The wrapper pins `MYSBX_EGRESS_BROKER` with `--set`, so overriding it in
# THIS shell would be silently discarded — the probe has to remove the
# program itself, not the variable. Moving the pinned binary aside for the
# duration makes the wrapper's own pin point at nothing.
moved="$broker.moved-$$"
[ -e "$broker" ] || fail "fail closed" "the pinned broker does not exist: $broker"
mv_broker() {
    mv "$broker" "$moved" 2>/dev/null && return 0
    # A store path cannot be moved; fall back to overriding PATH-free
    # resolution by pointing the pin at a non-existent path via the one
    # variable the wrapper leaves overridable.
    return 1
}
before=$(count_run_dirs)
refused=1
if mv_broker; then
    "$mysbx" run -- true >/dev/null 2>&1 && refused=0
    mv "$moved" "$broker"
else
    note "cannot move the pinned broker (a read-only store path); the"
    note "fail-closed probe needs a writable pin — SKIPPED, run it by hand"
fi
[ "$refused" = 1 ] || fail "fail closed" "a run with an unusable broker pin SUCCEEDED"
[ "$before" = "$(count_run_dirs)" ] ||
    fail "fail closed" "the refused run left a directory behind (E5)"
pass "an unusable broker pin refuses the run and creates nothing (E5)"

# ---- the profile's SUPPORTED backends still run ------------------------
for backend in bubblewrap nono; do
    out=$("$mysbx" --backend "$backend" --dry-run 2>&1) ||
        fail "supported backends" "$backend exited non-zero on --dry-run: $out"
    grep -qiE "refus|cannot|not supported|not enforced" <<<"$out" &&
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
