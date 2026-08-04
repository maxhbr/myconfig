#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED harness for the SECTION DISPATCH of
# modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh, run from the
# `microvm-rtv-dispatch` check in tests/microvm.nix.
#
# WHY THIS EXISTS
# ---------------
# The real-KVM suite needs /dev/kvm and root, so CI can never run its section
# bodies. What CI CAN run is the part that decides WHICH sections run and which
# are SKIPPED: the endpoint preflight + the section planning (Bug 2 / Gap 3).
#
# Bug 2: under `--section all`, an unreachable endpoint used to ABORT THE ENTIRE
# RUN (because `all` was in endpoint_sections()), so the operator's next run
# would validate NOTHING — including the security-critical forgery section. The
# fix: under `--section all`, RUN the five endpoint-independent sections and
# SKIP only `net`+`forgery` with a loud, counted reason; hard-abort ONLY when the
# operator asked for just `net`/`forgery`.
#
# Gap 3: the suite now prints the resolved section PLAN up front, a per-section
# tally as each completes, and a final summary listing the sections that RAN.
#
# This harness extracts the dispatch block verbatim from the suite (between the
# `# --- host-side model-endpoint preflight` marker and EOF), stubs the section
# BODIES (they need KVM) and `curl`/`sleep` (the preflight), and asserts the
# DISPATCH DECISION against stubbed scenarios — the same technique
# microvm-rtv-transport.sh uses for the guest command transport.
#
# What it does NOT establish: anything about a booted guest. It exercises the
# suite's own dispatch LOGIC against stubs.
# SC2034: SECTION/RTV_ENDPOINT_UP are consumed by the sourced dispatch block,
# which shellcheck cannot see across `source`.
# shellcheck disable=SC2034
set -euo pipefail

LAUNCHER="agent-microvm"
PROG="agent-microvm-runtime-validation"

[[ -n ${SUITE:-} ]] || {
    printf 'harness: required environment variable SUITE is unset\n' >&2
    exit 2
}

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
skip_all() {
    printf 'SKIP  %s\n' "$*"
    printf '\nSKIPPED: this sandbox cannot run the dispatch harness.\n'
    exit 0
}

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# --- extract the dispatch block verbatim from the suite --------------------
# From the preflight marker to EOF: preflight_endpoint, ALL_SECTIONS,
# is_endpoint_section, resolve_sections, the plan print, the preflight
# decision, the section loop and the final summary/exit logic.
awk '/^# --- host-side model-endpoint preflight ---/{f=1} f{print}' \
    "$SUITE" >"$WORK/dispatch.sh"
for tok in preflight_endpoint ALL_SECTIONS is_endpoint_section \
    resolve_sections 'mapfile -t PLAN' RAN_SECTIONS SKIPPED_SECTIONS; do
    grep -q -- "$tok" "$WORK/dispatch.sh" || {
        printf 'harness: could not extract "%s" from %s (has the suite been restructured?)\n' \
            "$tok" "$SUITE" >&2
        exit 2
    }
done

# --- the stub environment the dispatch block runs in -----------------------
# The suite defines pass/fail/skip/info/section/die and the PASS/FAIL/SKIP
# counters earlier in the file; reproduce them here verbatim so the dispatch
# block (which calls them) behaves identically. The section BODIES are stubbed:
# they need /dev/kvm + root, which CI does not have. Each stub prints the
# section header and one PASS, so the harness can prove the section RAN (its
# header appears) and that per-section tallies are non-zero.
STUB_ENV="$WORK/stub-env.sh"
cat >"$STUB_ENV" <<'EOF'
PROG="agent-microvm-runtime-validation"
LAUNCHER="agent-microvm"
PASS=0
FAIL=0
SKIP=0
pass() {
    printf 'PASS  %s\n' "$*"
    PASS=$((PASS + 1))
}
fail() {
    printf 'FAIL  %s\n' "$*"
    FAIL=$((FAIL + 1))
}
skip() {
    printf 'SKIP  %s\n' "$*"
    SKIP=$((SKIP + 1))
}
info() { printf '#     %s\n' "$*"; }
section() { printf '\n=== %s ===\n' "$*"; }
die() {
    printf '%s: error: %s\n' "$PROG" "$*" >&2
    exit 1
}
# The suite's config vars (never contacted: curl/sleep are stubbed below).
GATEWAY="192.168.83.1"
LITELLM_PORT="4000"
BRIDGE="agentbr0"
LAUNCHER="agent-microvm"
# Stub section bodies: print a header + one pass so "it ran" is observable.
section_boot() { section "boot + filesystem"; pass "boot-check"; }
section_net() { section "network: proxy-only allow/deny matrix"; pass "net-check"; }
section_l2() { section "layer 2 isolation"; pass "l2-check"; }
section_creds() { section "credential boundary"; pass "creds-check"; }
section_lifecycle() { section "lifecycle failure handling"; pass "lc-check"; }
section_malrepo() { section "hostile repository fixture"; pass "mal-check"; }
section_forgery() { section "batch result channel: forgery"; pass "forg-check"; }
# Stub curl: RTV_ENDPOINT_UP=1 -> endpoint answers (return 0); 0 -> dead (return 1).
# Uses `return`, not `exit`: a function `exit` would kill the whole shell,
# whereas the real curl is an external binary whose process exit is just a status.
curl() {
    if [[ "${RTV_ENDPOINT_UP:-0}" == 1 ]]; then
        return 0
    else
        return 1
    fi
}
# Stub sleep so a dead-endpoint preflight does not slow the test.
sleep() { :; }
EOF

# run_dispatch <SECTION> <endpoint_up 0|1>  -> prints rc on stdout, log on fd 3
run_dispatch() {
    local section="$1" up="$2"
    local out="$WORK/run-$section-$up.log" rc=0
    (
        # shellcheck source=/dev/null
        source "$STUB_ENV"
        SECTION="$section"
        export RTV_ENDPOINT_UP="$up"
        # shellcheck source=/dev/null
        source "$WORK/dispatch.sh"
    ) >"$out" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# A section's header line (printed by the stub section()) proves it RAN.
ran_header() {
    # $1 = log, $2 = section header keyword (e.g. "layer 2 isolation")
    grep -q -- "=== $2 ===" "$1"
}

printf '=== 1. --section all with the endpoint UP: every section runs, exit 0 ===\n'
rc="$(run_dispatch all 1)"
expect() {
    if [[ $2 == "$3" ]]; then
        pass "$1"
    else
        fail "$1 (expected '$2', got '$3')"
    fi
}
log="$WORK/run-all-1.log"
expect "all+up exits 0" 0 "$rc"
for h in \
    "boot + filesystem" \
    "network: proxy-only allow/deny matrix" \
    "layer 2 isolation" \
    "credential boundary" \
    "lifecycle failure handling" \
    "hostile repository fixture" \
    "batch result channel: forgery"; do
    if ran_header "$log" "$h"; then
        pass "section ran: $h"
    else
        fail "section did NOT run: $h"
    fi
done
if grep -q '^#     sections: boot net l2 creds lifecycle malrepo forgery$' "$log"; then
    pass "the plan is printed up front"
else
    fail "no plan line: $(grep '^#     sections:' "$log" || echo missing)"
fi
if grep -q '^#     sections ran: boot net l2 creds lifecycle malrepo forgery$' "$log"; then
    pass "the final summary lists every section that ran"
else
    fail "no/incorrect 'sections ran' summary: $(grep '^#     sections ran:' "$log" || echo missing)"
fi
if ! grep -q 'sections skipped' "$log"; then
    pass "no sections were skipped when the endpoint is up"
else
    fail "sections were skipped despite the endpoint being up"
fi

printf '\n=== 2. --section all with the endpoint DOWN: run 5, SKIP net+forgery, exit non-zero (Bug 2) ===\n'
rc="$(run_dispatch all 0)"
log="$WORK/run-all-0.log"
if ((rc != 0)); then
    pass "all+down exits non-zero ($rc) — the run did not silently pass"
else
    fail "all+down exited 0 while two sections were skipped"
fi
# The five endpoint-INDEPENDENT sections MUST run.
for h in \
    "boot + filesystem" \
    "layer 2 isolation" \
    "credential boundary" \
    "lifecycle failure handling" \
    "hostile repository fixture"; do
    if ran_header "$log" "$h"; then
        pass "endpoint-independent section ran: $h"
    else
        fail "endpoint-independent section did NOT run: $h"
    fi
done
# NEGATIVE CONTROL: the two endpoint-dependent sections MUST NOT run.
if ran_header "$log" "network: proxy-only allow/deny matrix"; then
    fail "net RAN despite the endpoint being down (Bug 2 present)"
else
    pass "net did NOT run (skipped, endpoint down)"
fi
if ran_header "$log" "batch result channel: forgery"; then
    fail "forgery RAN despite the endpoint being down (Bug 2 present)"
else
    pass "forgery did NOT run (skipped, endpoint down) — the security-critical section was not silently passed"
fi
if grep -q "^#     sections ran: boot l2 creds lifecycle malrepo$" "$log"; then
    pass "the summary lists exactly the five sections that ran"
else
    fail "summary 'sections ran' is wrong: $(grep '^#     sections ran:' "$log" || echo missing)"
fi
if grep -q "^#     sections skipped (endpoint down): net forgery$" "$log"; then
    pass "the summary lists the two skipped sections"
else
    fail "summary 'sections skipped' is wrong: $(grep '^#     sections skipped' "$log" || echo missing)"
fi
if grep -q "SKIP  section 'net' SKIPPED" "$log" && grep -q "SKIP  section 'forgery' SKIPPED" "$log"; then
    pass "net and forgery were SKIPPED with a precise, counted reason"
else
    fail "net/forgery skips are missing"
fi
if grep -q "WARNING: 2 section(s) were SKIPPED, not decided" "$log"; then
    pass "the non-zero exit is explained by a prominent skipped-sections WARNING"
else
    fail "no skipped-sections WARNING: $(grep 'WARNING:' "$log" || echo missing)"
fi
if grep -q "sudo $LAUNCHER doctor" "$log"; then
    pass "the skip reason points at 'doctor' for diagnosis"
else
    fail "the skip reason does not point at doctor"
fi
# The plan is still the FULL seven (the plan shows what was resolved, not what
# was skipped) — so the operator sees both "what I asked for" and "what ran".
if grep -q '^#     sections: boot net l2 creds lifecycle malrepo forgery$' "$log"; then
    pass "the plan still lists all seven (the skip is reported separately)"
else
    fail "the plan line changed: $(grep '^#     sections:' "$log" || echo missing)"
fi

printf '\n=== 3. --section net with the endpoint DOWN: hard-abort (running it is pointless) ===\n'
rc="$(run_dispatch net 0)"
log="$WORK/run-net-0.log"
if ((rc != 0)); then
    pass "net+down hard-aborts (exit $rc)"
else
    fail "net+down did not abort (exit 0)"
fi
if grep -q "ABORTING section" "$log"; then
    pass "the abort is loud"
else
    fail "no ABORT message: $(cat "$log")"
fi
if ! ran_header "$log" "network: proxy-only allow/deny matrix"; then
    pass "net did not run after the abort"
else
    fail "net ran despite the abort"
fi

printf '\n=== 4. --section forgery with the endpoint DOWN: hard-abort ===\n'
rc="$(run_dispatch forgery 0)"
log="$WORK/run-forgery-0.log"
if ((rc != 0)); then
    pass "forgery+down hard-aborts (exit $rc)"
else
    fail "forgery+down did not abort (exit 0)"
fi
if ! ran_header "$log" "batch result channel: forgery"; then
    pass "forgery did not run after the abort"
else
    fail "forgery ran despite the abort"
fi

printf '\n=== 5. --section boot with the endpoint DOWN: runs (endpoint not needed) ===\n'
# boot is NOT endpoint-dependent, so a down endpoint is irrelevant: no preflight
# abort, no skip. This proves the preflight only fires for endpoint-dependent
# contexts (net/forgery/all), not for every section.
rc="$(run_dispatch boot 0)"
log="$WORK/run-boot-0.log"
expect "boot+down exits 0" 0 "$rc"
if ran_header "$log" "boot + filesystem"; then
    pass "boot ran despite the endpoint being down"
else
    fail "boot did not run: $(cat "$log")"
fi
if grep -q '^#     sections ran: boot$' "$log"; then
    pass "the summary lists only boot"
else
    fail "summary 'sections ran' is wrong: $(grep '^#     sections ran:' "$log" || echo missing)"
fi
if ! grep -q 'sections skipped' "$log"; then
    pass "no sections were skipped (boot needs no endpoint)"
else
    fail "sections were skipped despite boot needing no endpoint"
fi

printf '\n=== 6. negative control: an unknown section is rejected, not silently dropped ===\n'
rc="$(run_dispatch bogus 1)"
log="$WORK/run-bogus-1.log"
if ((rc != 0)); then
    pass "an unknown section exits non-zero"
else
    fail "an unknown section exited 0"
fi
if grep -q "unknown --section" "$log"; then
    pass "the unknown section is named"
else
    fail "no 'unknown --section' message: $(cat "$log")"
fi

printf '\n=== 7. per-section tally: each section announces its own delta (Gap 3) ===\n'
log="$WORK/run-all-1.log"
for s in boot net l2 creds lifecycle malrepo forgery; do
    if grep -q "^#     section $s: [0-9]\+ passed, [0-9]\+ failed, [0-9]\+ skipped$" "$log"; then
        pass "section $s printed its own pass/fail/skip tally"
    else
        fail "section $s printed no tally: $(grep "^#     section $s:" "$log" || echo missing)"
    fi
done

printf '\n%s: %d passed, %d failed\n' "$0" "$PASSED" "$FAILED"
((FAILED == 0)) || exit 1
