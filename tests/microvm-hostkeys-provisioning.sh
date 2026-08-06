#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED regression harness for the SELF-HEALING per-slot SSH host identity
# (myconfig.ai.microvm), run from the `microvm-host-identity-self-healing` check
# in tests/microvm.nix.
#
# It runs the REAL, unmodified provisioner
# (`agent-microvm-provision-hostkeys`, hostkeys.nix) inside `bwrap` (a fresh
# tmpfs root, so the absolute runtime root the script hard-codes can be created)
# plus `fakeroot` (the script chowns to root:root and reads back the ownership).
#
# What this establishes, BY EXECUTION rather than by grepping the script:
#
#   1. IDEMPOTENCY — a second run keeps every existing private key byte for
#      byte, and known_hosts is byte-stable.
#   2. TAP KEY DELETION — a deleted private key is regenerated, and the OTHER
#      slot's identity is untouched (no collateral re-keying).
#   3. PUBLIC-KEY LOSS — a deleted public key is DERIVED from the surviving
#      private key; the private key is NOT replaced.
#   4. MISMATCH — a public key that conflicts with the private key is rebuilt
#      from the private key (the private key is authoritative), and known_hosts
#      follows.
#   5. PERMISSION DRIFT — a group/world-readable private key is forced back to
#      root:root 0400.
#   6. KNOWN_HOSTS — deterministic, duplicate-free, atomically installed, and
#      resolvable with `ssh-keygen -F` (the matcher `ssh` itself uses) for every
#      slot alias.
#   7. CONCURRENCY — N simultaneous runs leave a single consistent database
#      (the `flock` critical section), never a half-written one.
#   8. THE VSOCK-ONLY SHAPE — a batch+vsock host (no TCP sshd at all) gets the
#      same identity pinned under its `vsock-mux/...` alias, and a deleted key
#      there heals exactly the same way.
#
# What it does NOT establish: anything that needs a booted guest or /dev/kvm
# (that the guest's sshd actually presents this key, and that the launcher's
# `ssh` therefore verifies) — see runtime-validation.sh.
set -euo pipefail

for v in BWRAP FAKEROOT BASH_BIN PROVISIONER BV_PROVISIONER RUNTIME_ROOT \
    KNOWN_HOSTS LOCK_FILE KEY_NAME SLOT_KEY_DIRS SLOT_ALIASES \
    BV_SLOT_KEY_DIR BV_ALIAS SANDBOX_PATH; do
    [[ -n ${!v:-} ]] || {
        printf 'harness: required environment variable %s is unset\n' "$v" >&2
        exit 2
    }
done

WORK="$PWD/hostkeys"
mkdir -p "$WORK"

if ! "$BWRAP" --unshare-user --uid 0 --gid 0 --tmpfs / --ro-bind /nix /nix \
    --proc /proc --dev /dev -- "$BASH_BIN" -c true 2>"$WORK/bwrap.err"; then
    printf 'SKIP  bwrap cannot create a user namespace here: %s\n' "$(cat "$WORK/bwrap.err")"
    printf '\nSKIPPED: this sandbox cannot run the provisioner (see the reason above).\n'
    exit 0
fi

# --- the scenario script ----------------------------------------------------
# Everything below runs INSIDE the sandbox, as (fake) root, with $RUNTIME_ROOT
# bind-mounted onto a fresh per-scenario directory. It is a separate file rather
# than a `-c` string so that shellcheck/shfmt can see it and so quoting stays
# readable.
cat >"$WORK/scenarios.sh" <<'INNER'
set -uo pipefail

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

# The reference host's two slots (space-separated lists, index-aligned).
read -r -a KEY_DIRS <<<"$SLOT_KEY_DIRS"
read -r -a ALIASES <<<"$SLOT_ALIASES"

d0="${KEY_DIRS[0]}"
d1="${KEY_DIRS[1]}"
k0="$d0/$KEY_NAME"
k1="$d1/$KEY_NAME"

WARN_LOG=/tmp/provisioner-warnings.log

digest() { sha256sum "$1" | cut -d' ' -f1; }
# The `<type> <body>` the guest's sshd would present, derived from the private
# key. Never prints private material.
pubof() { ssh-keygen -y -f "$1" | cut -d' ' -f1,2; }
# The `<type> <body>` recorded in known_hosts for an alias, via the matcher ssh
# itself uses. Empty when absent.
recorded() {
    ssh-keygen -F "$1" -f "$KNOWN_HOSTS" 2>/dev/null | grep -v '^#' | cut -d' ' -f2,3
}

echo "=== 1. first run + IDEMPOTENCY ==="
"$PROVISIONER"
if [[ -s $k0 && -s $k1 ]]; then
    pass "both slots have a non-empty private key"
else
    fail "a slot has no private key after the first run"
fi
a0="$(digest "$k0")"
a1="$(digest "$k1")"
kh_first="$(cat "$KNOWN_HOSTS")"
"$PROVISIONER"
expect "a second run keeps slot 0's private key byte for byte" "$a0" "$(digest "$k0")"
expect "a second run keeps slot 1's private key byte for byte" "$a1" "$(digest "$k1")"
expect "a second run leaves known_hosts byte-identical" "$kh_first" "$(cat "$KNOWN_HOSTS")"
if [[ -e $LOCK_FILE ]]; then
    pass "the provisioner created its lock file ($LOCK_FILE)"
else
    fail "the provisioner did not create its lock file"
fi

echo
echo "=== 2. every slot alias resolves, matches, and is unique ==="
for i in 0 1; do
    al="${ALIASES[$i]}"
    kd="${KEY_DIRS[$i]}"
    expect "known_hosts entry for $al matches the slot's own key" \
        "$(pubof "$kd/$KEY_NAME")" "$(recorded "$al")"
done
if [[ "$(pubof "$k0")" != "$(pubof "$k1")" ]]; then
    pass "the two slots have DIFFERENT keys (no shared private key)"
else
    fail "the two slots share a key"
fi
dups="$(grep -v '^#' "$KNOWN_HOSTS" | cut -d' ' -f1 | sort | uniq -d)"
expect "known_hosts has no duplicate alias" "" "$dups"

echo
echo "=== 3. TAP key DELETION heals, and only the affected slot ==="
rm -f "$k0" "$k0.pub"
"$PROVISIONER"
if [[ -s $k0 ]]; then
    pass "the deleted private key was regenerated"
else
    fail "the deleted private key was NOT regenerated"
fi
if [[ "$(digest "$k0")" != "$a0" ]]; then
    pass "the regenerated key is a NEW key"
else
    fail "the 'regenerated' key is the old one"
fi
expect "the OTHER slot's identity was not touched" "$a1" "$(digest "$k1")"
expect "known_hosts followed the new key" "$(pubof "$k0")" "$(recorded "${ALIASES[0]}")"
b0="$(digest "$k0")"

echo
echo "=== 4. PUBLIC-key loss is repaired by DERIVING it ==="
want="$(pubof "$k0")"
rm -f "$k0.pub"
"$PROVISIONER"
expect "the private key survived (it is authoritative)" "$b0" "$(digest "$k0")"
expect "the public key was derived from the private key" "$want" "$(cut -d' ' -f1,2 "$k0.pub")"

echo
echo "=== 5. a MISMATCHED public key is rebuilt from the private key ==="
# Plant slot 1's public key as slot 0's: a conflict that must NOT be resolved by
# re-keying slot 0 (that would invalidate an already-distributed known_hosts).
rm -f "$k0.pub"
cp -- "$k1.pub" "$k0.pub"
chmod 0444 "$k0.pub"
"$PROVISIONER"
expect "the private key survived the public-key conflict" "$b0" "$(digest "$k0")"
expect "the public key was rebuilt from the private key" "$want" "$(cut -d' ' -f1,2 "$k0.pub")"
expect "known_hosts records the private key's real public half" "$want" "$(recorded "${ALIASES[0]}")"

echo
echo "=== 6. PERMISSION drift is forced back, WITHOUT re-keying ==="
# A mode drift must not cost the slot its identity: `ssh-keygen` refuses to read
# an over-permissive private key, so a provisioner that judged the key before
# fixing the mode would mistake drift for corruption and silently re-key.
chmod 0644 "$k0"
chmod 0600 "$k0.pub"
"$PROVISIONER" 2>"$WARN_LOG"
expect "the private key is root:root 0400 again" "root:root 400" "$(stat -c '%U:%G %a' "$k0")"
expect "the public key is root:root 0444 again" "root:root 444" "$(stat -c '%U:%G %a' "$k0.pub")"
expect "known_hosts is root:root 0444" "root:root 444" "$(stat -c '%U:%G %a' "$KNOWN_HOSTS")"
expect "the private key survived the mode repair (no silent re-key)" "$b0" "$(digest "$k0")"
if grep -q 'normalised over-permissive mode 644' "$WARN_LOG"; then
    pass "the over-permissive mode was REPORTED, not silently swallowed"
else
    fail "the over-permissive mode was repaired without telling the operator"
fi

echo
echo "=== 7. CONCURRENCY: 8 simultaneous runs ==="
before="$(cat "$KNOWN_HOSTS")"
for _ in 1 2 3 4 5 6 7 8; do
    "$PROVISIONER" &
done
wait
expect "known_hosts is unchanged after 8 concurrent runs" "$before" "$(cat "$KNOWN_HOSTS")"
expect "no duplicate alias after 8 concurrent runs" "" \
    "$(grep -v '^#' "$KNOWN_HOSTS" | cut -d' ' -f1 | sort | uniq -d)"
expect "slot 0's key is unchanged after 8 concurrent runs" "$b0" "$(digest "$k0")"
expect "slot 1's key is unchanged after 8 concurrent runs" "$a1" "$(digest "$k1")"
# No temp file left behind: the atomic install renames, and the EXIT trap
# removes a temp file of a run that failed.
leftovers="$(find "$(dirname "$KNOWN_HOSTS")" -maxdepth 1 -name "$(basename "$KNOWN_HOSTS").*" -printf '%f\n' | sort | tr '\n' ' ')"
expect "no known_hosts temp file survived" "" "$leftovers"

printf '\n%s passed, %s failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
INNER

# --- the VSOCK-only scenario ------------------------------------------------
# A batch+vsock host has NO TCP sshd, so its known_hosts holds exactly one alias
# per slot: the VSOCK mux socket path. Run in its OWN sandbox, because the two
# hosts share the same absolute runtime root.
cat >"$WORK/scenarios-vsock.sh" <<'INNER'
set -uo pipefail

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

key="$BV_SLOT_KEY_DIR/$KEY_NAME"
digest() { sha256sum "$1" | cut -d' ' -f1; }
pubof() { ssh-keygen -y -f "$1" | cut -d' ' -f1,2; }
recorded() {
    ssh-keygen -F "$1" -f "$KNOWN_HOSTS" 2>/dev/null | grep -v '^#' | cut -d' ' -f2,3
}

echo "=== V1. a batch+vsock host provisions its VSOCK identity ==="
"$BV_PROVISIONER"
if [[ -s $key ]]; then
    pass "the vsock-only slot has a non-empty private key"
else
    fail "the vsock-only slot has no private key"
fi
expect "the vsock-mux alias resolves to the slot's own key" \
    "$(pubof "$key")" "$(recorded "$BV_ALIAS")"
# ONLY the vsock alias: there is no guest network interface, so an IPv4 entry
# would pin a key to an address nothing listens on.
entries="$(grep -cv '^#' "$KNOWN_HOSTS")"
expect "known_hosts holds exactly one entry (the vsock-mux alias)" "1" "$entries"

echo
echo "=== V2. VSOCK-only key DELETION heals ==="
old="$(digest "$key")"
rm -f "$key" "$key.pub"
"$BV_PROVISIONER"
if [[ -s $key ]]; then
    pass "the deleted VSOCK-only private key was regenerated"
else
    fail "the deleted VSOCK-only private key was NOT regenerated"
fi
if [[ "$(digest "$key")" != "$old" ]]; then
    pass "the regenerated VSOCK-only key is a NEW key"
else
    fail "the 'regenerated' VSOCK-only key is the old one"
fi
expect "the vsock-mux alias followed the new key" \
    "$(pubof "$key")" "$(recorded "$BV_ALIAS")"
expect "the regenerated key is root:root 0400" "root:root 400" "$(stat -c '%U:%G %a' "$key")"

printf '\n%s passed, %s failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
INNER

run_scenarios() {
    local name="$1" script="$2"
    local root="$WORK/$name"
    rm -rf "$root"
    mkdir -p "$root"
    "$BWRAP" --unshare-user --uid 0 --gid 0 \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$root" "$RUNTIME_ROOT" \
        --setenv PATH "$SANDBOX_PATH" \
        --setenv PROVISIONER "$PROVISIONER" \
        --setenv BV_PROVISIONER "$BV_PROVISIONER" \
        --setenv KNOWN_HOSTS "$KNOWN_HOSTS" \
        --setenv LOCK_FILE "$LOCK_FILE" \
        --setenv KEY_NAME "$KEY_NAME" \
        --setenv SLOT_KEY_DIRS "$SLOT_KEY_DIRS" \
        --setenv SLOT_ALIASES "$SLOT_ALIASES" \
        --setenv BV_SLOT_KEY_DIR "$BV_SLOT_KEY_DIR" \
        --setenv BV_ALIAS "$BV_ALIAS" \
        -- "$FAKEROOT" -- "$BASH_BIN" "$script"
}

rc=0
run_scenarios tap "$WORK/scenarios.sh" || rc=1
echo
run_scenarios vsock "$WORK/scenarios-vsock.sh" || rc=1
exit "$rc"
