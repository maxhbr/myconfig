#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED harness for `agent-microvm doctor`, run from the `microvm-doctor`
# check in tests/microvm.nix.
#
# It runs the REAL, unmodified launcher's `doctor` subcommand (same technique as
# microvm-batch-launcher-submit.sh: `bwrap` for a writable tmpfs root, `fakeroot`
# for uid 0, and stubs bind-mounted over the EXACT store paths the launcher
# resolves, so the script under test is byte-identical to the installed one).
#
# What this locks down as a PROPERTY (Bug 1):
#
#   `doctor` MUST report OK (exit 0, zero problems) against a stubbed HEALTHY
#   host, and MUST report non-OK (exit non-zero) against a stubbed BROKEN host.
#
#   The historical defect: `cmd_doctor` checked the LiteLLM ACCEPT rule by
#   grepping `iptables -S`'s PRINTED form for "-d <addr> <space> ...". `iptables
#   -S` canonicalises the destination address (-d 192.168.83.1 is printed back
#   as -d 192.168.83.1/32), so the pattern's required space never matched and
#   `doctor` ALWAYS reported at least one problem on a healthy host, exiting
#   non-zero and breaking "sudo agent-microvm doctor && ...".
#
#   The fix tests the rule with `iptables -C <chain> <spec>` (exit 0 = the rule
#   exists), building the spec from the SAME $SUBNET/$GATEWAY/$LITELLM_PORT
#   variables network.nix installs the rule with. This harness drives that
#   through a stub `iptables` that ACCEPTS -C (rule present) or REJECTS it
#   (rule absent), and additionally RECORDS the exact -C argv so the harness
#   can assert the spec the doctor builds matches the rule network.nix installs,
#   proving the two cannot drift.
#
#   Negative control: with the rule genuinely ABSENT (stub -C -> exit 1),
#   `doctor` MUST exit non-zero and name the broken INPUT-chain check. This
#   proves the check was not loosened into something that passes vacuously.
#
# What it does NOT establish: anything about a real host (no /dev/kvm, no real
# iptables). It exercises `cmd_doctor`'s LOGIC against stubs.
set -euo pipefail

for v in LAUNCHER BWRAP FAKEROOT BASH_BIN SYSTEMCTL_TARGET IP_TARGET \
    IPTABLES_TARGET CURL_TARGET RUNTIME_ROOT HOSTKEYS_ROOT GATEWAY SUBNET \
    LITELLM_PORT BRIDGE SLOT_NAMES; do
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

WORK="$PWD/doctor"
mkdir -p "$WORK"
STUBS="$WORK/stubs"
mkdir -p "$STUBS"

if ! "$BWRAP" --unshare-user --uid 0 --gid 0 --tmpfs / --ro-bind /nix /nix \
    --proc /proc --dev /dev -- "$BASH_BIN" -c true 2>"$WORK/bwrap.err"; then
    skip_all "bwrap cannot create a user namespace here: $(cat "$WORK/bwrap.err")"
fi

# The expected `iptables -C` rule spec network.nix installs (inputAllowLines),
# built from the SAME variables the launcher bakes in. The harness asserts the
# doctor's -C call carries EXACTLY this argv, so a drift between the check and
# the rule (the original defect) cannot pass silently.
expected_iptables_c=(
    -C AGENT_MICROVM_INPUT
    -s "$SUBNET" -d "$GATEWAY" -p tcp --dport "$LITELLM_PORT" -j ACCEPT
)

# --- a systemctl stub ------------------------------------------------------
# doctor asks: is-active litellm.service / agent-litellm-proxy.socket, and
# "show -p After --value agent-litellm-proxy.socket". The healthy host answers
# all of those affirmatively and reports the bridge netdev in the socket's
# After= list (so the ordering grep matches).
cat >"$STUBS/systemctl" <<EOF
#!$BASH_BIN
set -u
case "\$1" in
    is-active)
        # \$2 = --quiet, \$3 = unit. Healthy: both units are active.
        exit 0
        ;;
    show)
        # "show -p After --value agent-litellm-proxy.socket" -> unit list.
        # Print the bridge netdev so the ordering grep matches on the
        # healthy host.
        printf 'system.slice %s-netdev.service sockets.target\\n' "$BRIDGE"
        exit 0
        ;;
esac
exit 1
EOF

# --- a curl stub -----------------------------------------------------------
# Both the loopback and the bridge endpoint probes succeed on the healthy host.
cat >"$STUBS/curl" <<EOF
#!$BASH_BIN
exit 0
EOF

# --- an ip stub ------------------------------------------------------------
# "ip -br link show $BRIDGE" succeeds (bridge exists); "ip -br addr show $BRIDGE"
# prints the gateway address with its prefix ($GATEWAY/24), the realistic
# canonical form the dot-escaped grep must still match.
cat >"$STUBS/ip" <<EOF
#!$BASH_BIN
set -u
case "\$1 \$2" in
    "-br link")
        # bridge exists
        exit 0
        ;;
    "-br addr")
        # realistic output WITH the /24 mask
        printf '%s         UP             %s/24 fe80::1/64\\n' "$BRIDGE" "$GATEWAY"
        exit 0
        ;;
esac
exit 1
EOF

# --- an iptables stub ------------------------------------------------------
# doctor calls "iptables -L <chain> -n" (chain presence) and
# "iptables -C <chain> <spec>" (rule presence). The -C argv is RECORDED so the
# harness can assert the spec matches network.nix's rule exactly. Whether -C
# reports the rule as present is SCENARIO-controlled via $DOCTOR_RULE_PRESENT.
cat >"$STUBS/iptables" <<EOF
#!$BASH_BIN
set -u
case "\$1" in
    -L)
        # Chain exists (output is discarded by doctor). Healthy for both chains.
        exit 0
        ;;
    -C)
        printf '%s\\n' "\$*" >> "\$DOCTOR_IPTABLES_C_LOG"
        if [[ "\${DOCTOR_RULE_PRESENT:-1}" == "1" ]]; then
            exit 0
        else
            exit 1
        fi
        ;;
esac
exit 1
EOF
chmod +x "$STUBS"/*

# --- per-slot host-key directories ----------------------------------------
# doctor checks "[[ -e "$HOSTKEYS_ROOT/<slot>" ]]" for every slot. $HOSTKEYS_ROOT
# comes from the MODULE (hostkeys.nix: the read-only session tree), and it lives
# under $RUNTIME_ROOT, which is bind-mounted below — so map it into the fixture
# by stripping that prefix instead of hardcoding a second copy of the layout.
hostkeys_rel="${HOSTKEYS_ROOT#"$RUNTIME_ROOT"}"
hostkeys_rel="${hostkeys_rel#/}"
[[ -n $hostkeys_rel ]] || {
    printf 'harness: HOSTKEYS_ROOT (%s) is not under RUNTIME_ROOT (%s)\n' \
        "$HOSTKEYS_ROOT" "$RUNTIME_ROOT" >&2
    exit 2
}
mkdir -p "$WORK/runtime/$hostkeys_rel"
# shellcheck disable=SC2086  # SLOT_NAMES is a deliberate word-split list
for s in $SLOT_NAMES; do
    mkdir -p "$WORK/runtime/$hostkeys_rel/$s"
done

# run_doctor <scenario> <rule_present 0|1>
# Runs the REAL "agent-microvm doctor" under bwrap+fakeroot with the stubs
# bound over the exact store paths the launcher resolves. Prints the rc.
run_doctor() {
    local scenario="$1" rule_present="$2"
    local out="$WORK/doctor-$scenario.log"
    local rc=0
    "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname doctor-host \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$WORK/runtime" "$RUNTIME_ROOT" \
        --bind "$STUBS/systemctl" "$SYSTEMCTL_TARGET" \
        --bind "$STUBS/curl" "$CURL_TARGET" \
        --bind "$STUBS/ip" "$IP_TARGET" \
        --bind "$STUBS/iptables" "$IPTABLES_TARGET" \
        --setenv DOCTOR_RULE_PRESENT "$rule_present" \
        --setenv DOCTOR_IPTABLES_C_LOG "$WORK/iptables-c-$scenario.log" \
        --setenv HOME "$WORK" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "exec '$LAUNCHER' doctor" \
        >"$out" 2>&1 || rc=$?
    printf '%s' "$rc"
}

printf '=== 1. a HEALTHY host: doctor reports OK and exits 0 ===\n'
: >"$WORK/iptables-c-healthy.log"
rc="$(run_doctor healthy 1)"
expect "doctor exits 0 on a healthy host" 0 "$rc"
if grep -q '0 problem(s)' "$WORK/doctor-healthy.log"; then
    pass "doctor reported zero problems on a healthy host"
else
    fail "doctor did not report zero problems on a healthy host: $(cat "$WORK/doctor-healthy.log")"
fi
# The old bug: the iptables ACCEPT check ALWAYS failed even when healthy. Assert
# the healthy run has NO FAIL line for the INPUT-chain endpoint check.
if grep -q 'FAIL.*INPUT chain does NOT ACCEPT' "$WORK/doctor-healthy.log"; then
    fail "doctor false-failed the INPUT-chain check on a healthy host (Bug 1 present?): $(grep 'INPUT chain' "$WORK/doctor-healthy.log")"
else
    pass "doctor did NOT false-fail the INPUT-chain endpoint check on a healthy host"
fi
if grep -q 'OK.*INPUT chain ACCEPTs tcp dport' "$WORK/doctor-healthy.log"; then
    pass "doctor OK'd the INPUT-chain endpoint check on a healthy host"
else
    fail "doctor did not OK the INPUT-chain endpoint check on a healthy host"
fi

printf '\n=== 2. the iptables -C spec matches the rule network.nix installs ===\n'
# The doctor built its -C argv from $SUBNET/$GATEWAY/$LITELLM_PORT. Assert it
# carries EXACTLY the spec network.nix installs (inputAllowLines), so a drift
# between the check and the rule cannot pass silently.
recorded="$(cat "$WORK/iptables-c-healthy.log")"
expected="$(printf '%s' "${expected_iptables_c[*]}")"
if [[ $recorded == "$expected" ]]; then
    pass "the -C spec matches the rule network.nix installs"
else
    fail "the -C spec drifted from network.nix's rule"
    printf '      expected: %s\n' "$expected"
    printf '      recorded: %s\n' "$recorded"
fi

printf '\n=== 3. a BROKEN host (rule ABSENT): doctor reports non-OK and exits non-zero ===\n'
# NEGATIVE CONTROL: everything else is still healthy, but the iptables ACCEPT
# rule is genuinely absent (stub -C -> exit 1). doctor MUST exit non-zero and
# MUST name the broken INPUT-chain check, proving the check was not loosened
# into something that passes vacuously.
: >"$WORK/iptables-c-broken.log"
rc="$(run_doctor broken-rule 0)"
if ((rc != 0)); then
    pass "doctor exited non-zero ($rc) when the rule is genuinely absent"
else
    fail "doctor exited 0 when the rule is genuinely absent (the check passes vacuously)"
fi
if grep -q 'FAIL.*INPUT chain does NOT ACCEPT' "$WORK/doctor-broken-rule.log"; then
    pass "doctor named the absent INPUT-chain endpoint check"
else
    fail "doctor did not name the absent INPUT-chain check: $(cat "$WORK/doctor-broken-rule.log")"
fi
if grep -q '1 problem(s)' "$WORK/doctor-broken-rule.log"; then
    pass "doctor counted exactly one problem (only the absent rule)"
else
    fail "doctor did not count exactly one problem: $(grep 'problem(s)' "$WORK/doctor-broken-rule.log" || echo none)"
fi

printf '\n=== 4. the rest of doctor still works: a fully-broken host fails more ===\n'
# Smoke: if the bridge address is ALSO missing, doctor reports BOTH problems
# (so the per-check logic is not short-circuited into a single coarse verdict).
cat >"$STUBS/ip" <<EOF
#!$BASH_BIN
set -u
case "\$1 \$2" in
    "-br link") exit 0 ;;
    "-br addr")
        # Bridge exists but carries NO gateway address.
        printf '%s         UP             fe80::1/64\\n' "$BRIDGE"
        exit 0
        ;;
esac
exit 1
EOF
rc="$(run_doctor broken-multi 0)"
if ((rc != 0)); then
    pass "doctor exited non-zero ($rc) when the rule AND the gateway are both absent"
else
    fail "doctor exited 0 when two components are broken"
fi
problems="$(grep -oE '[0-9]+ problem\(s\)' "$WORK/doctor-broken-multi.log" | tail -1 || true)"
if [[ $problems == "2 problem(s)" ]]; then
    pass "doctor counted exactly two problems (gateway + absent rule)"
else
    fail "doctor did not count exactly two problems: ${problems:-none}"
fi

printf '\n%s: %d passed, %d failed\n' "$0" "$PASSED" "$FAILED"
((FAILED == 0)) || exit 1
