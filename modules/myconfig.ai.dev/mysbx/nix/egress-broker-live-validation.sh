#!/usr/bin/env bash
# egress-broker-live-validation — the scripted half of the phase-B egress
# runbook (bd myconfig-n8kv.1.6, docs/design/egress.md). Run ON f13, from
# the repo checkout, once the rebuilt switch is in place:
#
#   MYSBX=<path-to-mysbx> ./nix/egress-broker-live-validation.sh <repo-dir>
#
# Each probe prints PASS/FAIL and names the decision record it validates;
# the first FAIL exits non-zero. The script is deliberately dumb: plain
# bash plus the sandbox itself. It needs the host's LiteLLM proxy and the
# wrapper's pins (MYSBX_EGRESS_BROKER, MYSBX_EGRESS_RELAY, ...), which the
# Nix wrapper sets, so run it with the WRAPPED mysbx on PATH or via MYSBX=.
#
# What it does NOT do: press keys in a multiplexer, start a nested VM, or
# judge whether an agent's answer was any good. The agent/model requests
# (probe 3) are the one human step, and it asks for them explicitly.
set -euo pipefail

repo=${1:?usage: egress-broker-live-validation.sh <repo-dir>}
mysbx=${MYSBX:-mysbx}

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

cd "$repo"

# The pins must all be present, or every later probe is meaningless.
for pin in MYSBX_EGRESS_SOCKET MYSBX_EGRESS_PORT MYSBX_EGRESS_RELAY MYSBX_EGRESS_ETC MYSBX_EGRESS_BROKER; do
    [ -n "${!pin:-}" ] || fail "pins" "$pin is unset — is this the Nix wrapper, rebuilt with the broker?"
done
[ -n "${XDG_RUNTIME_DIR:-}" ] || fail "pins" "XDG_RUNTIME_DIR is unset; the run directory root comes from it (egress.md E4)"
pass "pins (broker, relay, socket, port, name lookup)"

# Resolve the sidecar's log root from the run itself: `mysbx status` names
# the sidecar, so the script never has to guess the path convention.
sidecar=$("$mysbx" status 2>/dev/null | sed -n 's/^## sidecar:  *\([^ ]*\).*/\1/p')
[ -n "$sidecar" ] || fail "sidecar" "cannot resolve the sidecar from mysbx status"
log_root="$sidecar/egress"
note "sidecar: $sidecar"

# ---- E5: a dry run creates nothing, and names the per-run channel --------
runtime_before=$(find "${XDG_RUNTIME_DIR}/mysbx-egress" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)
sidecar_before=$(find "$sidecar" -mindepth 1 -newer "$sidecar" 2>/dev/null | wc -l)
dry=$("$mysbx" --dry-run --verbose 2>&1) || fail "dry run" "a --dry-run exited non-zero"
grep -q "proxy-only" <<<"$dry" || fail "dry run" "the report does not name the proxy-only profile"
grep -q "mysbx-egress" <<<"$dry" || fail "dry run" "the report names no egress directory"
if grep -q "^network:.*${mysbx_egress_static:-/run/mysbx-egress}" <<<"$dry" &&
    ! grep -q "via the relay from" <<<"$dry"; then
    fail "dry run" "the report still names the static forwarder directory as the source"
fi
runtime_after=$(find "${XDG_RUNTIME_DIR}/mysbx-egress" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)
[ "$runtime_before" = "$runtime_after" ] ||
    fail "dry run creates nothing" "the run directory count changed ($runtime_before -> $runtime_after); egress.md E5"
[ "$sidecar_before" = "$(find "$sidecar" -mindepth 1 -newer "$sidecar" 2>/dev/null | wc -l)" ] ||
    fail "dry run creates nothing" "the sidecar gained entries under --dry-run"
pass "dry run creates nothing and names the per-run channel (egress.md E5)"

# ---- E1/E4: a REAL run brings the broker up and logs to the sidecar -----
"$mysbx" run -- true >/dev/null 2>&1 || fail "real run" "a plain run -- true failed"
latest=$(find "$log_root" -mindepth 2 -maxdepth 2 -name decisions.jsonl -printf '%h\n' 2>/dev/null |
    sort -t/ -k1 | tail -1)
[ -n "$latest" ] || fail "real run" "no decisions.jsonl appeared under $log_root; egress.md E4"
grep -q '"event":"listen"' "$latest/decisions.jsonl" ||
    fail "real run" "the log carries no listen line: $(head -3 "$latest/decisions.jsonl")"
pass "a real run starts the broker and logs to the sidecar ($(basename "$latest"))"

# ---- E1: the log is OUTSIDE what the sandbox binds ----------------------
run_dir=$(sed -n 's/.*"why":"\([^ ]*\) -> .*/\1/p' "$latest/decisions.jsonl" | head -1)
if [ -n "$run_dir" ] && [ -e "$run_dir" ]; then
    fail "log placement" "the run directory still exists after a waited run: $run_dir"
fi
grep -q "$log_root" <<<"$latest" ||
    fail "log placement" "the log is not under the sidecar"
pass "the log lives in the sidecar, not in the sandbox's bound directory (egress.md E4)"

# ---- E2: the run directory is gone after a waited run -------------------
find "${XDG_RUNTIME_DIR}/mysbx-egress" -mindepth 1 -maxdepth 1 -name '[0-9]*' 2>/dev/null |
    while read -r stale; do
        pid=$(basename "$stale")
        kill -0 "$pid" 2>/dev/null ||
            fail "run directory cleanup" "$stale outlived its run; egress.md E2/E3"
    done
pass "a waited run removes its run directory (egress.md E2/E3)"

# ---- diagnose reads the log back ---------------------------------------
diag=$("$mysbx" diagnose egress 2>&1) || fail "diagnose" "mysbx diagnose egress exited non-zero"
grep -q "run " <<<"$diag" || fail "diagnose" "the verb reported no run: $diag"
grep -qE "[1-9][0-9]* admissions" <<<"$diag" ||
    note "diagnose reports no admissions yet (fine before the agent probe)"
diag_json=$("$mysbx" diagnose egress --json 2>&1) || fail "diagnose --json" "exited non-zero"
grep -q '"pid":' <<<"$diag_json" || fail "diagnose --json" "no pid field: $diag_json"
pass "mysbx diagnose egress reads the log back (cli.md D29)"

# ---- E5: fail closed ----------------------------------------------------
before=$(find "${XDG_RUNTIME_DIR}/mysbx-egress" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)
if MYSBX_EGRESS_BROKER=/nonexistent/mysbx-egress-broker "$mysbx" run -- true 2>/dev/null; then
    fail "fail closed" "a run with a broken broker pin SUCCEEDED"
fi
after=$(find "${XDG_RUNTIME_DIR}/mysbx-egress" -mindepth 1 -maxdepth 1 2>/dev/null | wc -l)
[ "$before" = "$after" ] ||
    fail "fail closed" "the refused run left a directory behind (egress.md E5)"
pass "a broken broker pin refuses the run and creates nothing (egress.md E5)"

# ---- Other backends still refuse the profile ---------------------------
for backend in nono podman-gvisor podman-krun krun microvm-qemu microvm-ch; do
    if "$mysbx" --backend "$backend" --dry-run 2>&1 | grep -qi "cannot\|refus"; then
        :
    else
        out=$("$mysbx" --backend "$backend" --dry-run 2>&1) ||
            fail "refusals" "$backend exited non-zero on --dry-run"
        grep -qi "refus\|cannot\|does not\|not supported" <<<"$out" ||
            fail "refusals" "$backend does not refuse network = proxy-only: $out"
    fi
done
pass "every other backend still refuses the profile"

# ---- The human step: an agent completes a model request -----------------
cat <<'EOF'

  The remaining probes need a human at the terminal:
    * start `mysbx` and run one model request from `pi` and one from `opencode`
    * then, in another terminal, confirm `mysbx diagnose egress` shows the
      admissions, and `mysbx diagnose egress --verbose` the peer pids
    * kill the broker (`pkill -f mysbx-egress-broker`) and confirm the
      sandbox's requests now fail (no fallback), and that the log's last
      line is an `unreachable` rather than an `allow`
    * `mysbx --detach`, follow it with `mysbx attach`, confirm the broker
      outlives the outer and dies with the inner
  Record the results as a dated block in docs/egress-validation-log.md.

EOF
printf 'PASS scripted probes (the agent, kill-mid-run and detach probes are manual)\n'
