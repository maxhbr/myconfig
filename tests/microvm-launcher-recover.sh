#!/usr/bin/env bash
# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# EXECUTED harness for `agent-microvm recover`, run from the
# `microvm-launcher-recover` check in tests/microvm.nix.
#
# It runs the REAL, unmodified launcher (same technique as
# microvm-batch-launcher-submit.sh: `bwrap` for a writable tmpfs root, `fakeroot`
# for uid 0, and stubs bind-mounted over the EXACT store paths the launcher
# resolves, so the script under test is byte-identical to the installed one).
#
# What it establishes, as BEHAVIOUR rather than as the presence of a string:
#
#   1. A bind mount that is still there after `umount` must never be reported as
#      recovered. The `umount` stub models the real post-crash situation: it
#      returns EBUSY while the slot's virtiofsd still holds the share, and only
#      succeeds once that unit has been stopped. The launcher must therefore stop
#      `microvm-virtiofsd@<slot>.service` and re-try, and must NOT reach for
#      `umount -l` (a lazy unmount leaves the mount in `findmnt`, which is
#      exactly how a stale bind used to survive a `recover` that claimed
#      success).
#   2. When the mount survives no matter what, `recover` must say so, emit the
#      `mount-leak` lifecycle event and exit NON-ZERO.
#   3. Per-slot state whose slot name is NOT in the current pool (e.g. `agent-0`
#      from before the `agent-<class>-<i>` rename) is REPORTED as its own
#      `foreign:` finding and left strictly alone — unless `--prune-foreign` is
#      given, and even then `--dry-run` still only prints.
set -euo pipefail

for v in LAUNCHER BWRAP FAKEROOT BASH_BIN SYSTEMCTL_TARGET MOUNT_TARGET \
    UMOUNT_TARGET FINDMNT_TARGET RUNTIME_ROOT STATE_ROOT SLOT FOREIGN_SLOT; do
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

WORK="$PWD/recover"
mkdir -p "$WORK"
STUBS="$WORK/stubs"
mkdir -p "$STUBS"

if ! "$BWRAP" --unshare-user --uid 0 --gid 0 --tmpfs / --ro-bind /nix /nix \
    --proc /proc --dev /dev -- "$BASH_BIN" -c true 2>"$WORK/bwrap.err"; then
    skip_all "bwrap cannot create a user namespace here: $(cat "$WORK/bwrap.err")"
fi

# --- the stubs --------------------------------------------------------------
# A "mount" is a file under $STUB_DIR/mounts named after its target.
cat >"$STUBS/mount" <<EOF
#!$BASH_BIN
set -u
printf 'mount %s\n' "\$*" >> "\$STUB_DIR/mount.log"
target="\${@: -1}"
mkdir -p "\$STUB_DIR/mounts"
printf '%s' "\${@: -2:1}" > "\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
exit 0
EOF

# The REAL post-crash behaviour: virtiofsd still has the share open, so a plain
# umount gets EBUSY. STUB_UMOUNT decides what it takes to succeed:
#   holder  — succeeds only after microvm-virtiofsd@<slot> has been stopped
#   wedged  — never succeeds (models a holder the launcher cannot stop)
#   free    — succeeds immediately
# A lazy unmount (-l) is REFUSED outright: if the launcher ever reaches for it
# again, the scenario fails instead of silently passing.
cat >"$STUBS/umount" <<EOF
#!$BASH_BIN
set -u
printf 'umount %s\n' "\$*" >> "\$STUB_DIR/mount.log"
for a in "\$@"; do
    if [[ \$a == -l || \$a == --lazy ]]; then
        printf 'LAZY-UNMOUNT-ATTEMPTED %s\n' "\$*" >> "\$STUB_DIR/mount.log"
        exit 1
    fi
done
target="\${@: -1}"
f="\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
[[ -f \$f ]] || exit 1
case "\${STUB_UMOUNT:-free}" in
    holder)
        if [[ -f \$STUB_DIR/virtiofsd-stopped ]]; then
            rm -f "\$f"; exit 0
        fi
        printf 'umount EBUSY %s\n' "\$target" >> "\$STUB_DIR/mount.log"
        exit 32
        ;;
    wedged)
        printf 'umount EBUSY %s\n' "\$target" >> "\$STUB_DIR/mount.log"
        exit 32
        ;;
    *) rm -f "\$f"; exit 0 ;;
esac
EOF

cat >"$STUBS/findmnt" <<EOF
#!$BASH_BIN
set -u
target="\${@: -1}"
f="\$STUB_DIR/mounts/\$(printf '%s' "\$target" | tr / _)"
[[ -f \$f ]] || exit 1
for a in "\$@"; do
    if [[ \$a == -no ]] || [[ \$a == SOURCE ]]; then
        printf 'none[%s]\n' "\$(cat "\$f")"
        exit 0
    fi
done
printf '%s\n' "\$target"
exit 0
EOF

# No VM is ever active here; the only interesting call is the virtiofsd stop.
cat >"$STUBS/systemctl" <<EOF
#!$BASH_BIN
set -u
printf 'systemctl %s\n' "\$*" >> "\$STUB_DIR/systemctl.log"
case "\$1" in
    is-active) exit 3 ;;
    stop)
        for a in "\$@"; do
            case "\$a" in
                microvm-virtiofsd@*.service) : > "\$STUB_DIR/virtiofsd-stopped" ;;
            esac
        done
        exit 0
        ;;
esac
exit 0
EOF
chmod +x "$STUBS"/*

# --- running `recover` ------------------------------------------------------
# run_recover <stub-dir-name> <umount-mode> [recover args...]
run_recover() {
    local name="$1" umount_mode="$2"
    shift 2
    local stub_dir="$WORK/stub-$name"
    local rc=0
    "$BWRAP" --unshare-user --uid 0 --gid 0 --unshare-uts --hostname launcher-host \
        --tmpfs / --ro-bind /nix /nix --ro-bind-try /etc /etc \
        --dev /dev --proc /proc --tmpfs /tmp \
        --bind "$WORK" "$WORK" \
        --bind "$WORK/runtime-$name" "$RUNTIME_ROOT" \
        --bind "$WORK/state-$name" "$STATE_ROOT" \
        --bind "$STUBS/systemctl" "$SYSTEMCTL_TARGET" \
        --bind "$STUBS/mount" "$MOUNT_TARGET" \
        --bind "$STUBS/umount" "$UMOUNT_TARGET" \
        --bind "$STUBS/findmnt" "$FINDMNT_TARGET" \
        --setenv STUB_DIR "$stub_dir" \
        --setenv STUB_UMOUNT "$umount_mode" \
        --setenv HOME "$WORK" \
        -- "$FAKEROOT" -- "$BASH_BIN" -c "
            exec '$LAUNCHER' recover $*
        " >"$WORK/recover-$name.log" 2>&1 || rc=$?
    printf '%s' "$rc"
}

# Lay out the per-scenario fixture OUTSIDE the sandbox: a stale workspace bind
# mount for $SLOT and no allocation marker — the situation `recover` classifies
# as "stale bind mount".
fixture() {
    local name="$1"
    local stub_dir="$WORK/stub-$name"
    rm -rf "$stub_dir" "$WORK/runtime-$name" "$WORK/state-$name"
    mkdir -p "$stub_dir/mounts" "$WORK/runtime-$name" "$WORK/state-$name"
    printf '%s' "$RUNTIME_ROOT/workspaces/some-task" \
        >"$stub_dir/mounts/$(printf '%s' "$STATE_ROOT/$SLOT/workspace" | tr / _)"
}

mounts_left() {
    local name="$1"
    find "$WORK/stub-$name/mounts" -type f 2>/dev/null | wc -l
}

# The five host directories that are keyed by SLOT NAME, as seen from outside
# the sandbox. `foreign_paths <scenario>` prints them for $FOREIGN_SLOT.
foreign_paths() {
    local name="$1"
    printf '%s\n' \
        "$WORK/runtime-$name/slots/$FOREIGN_SLOT" \
        "$WORK/runtime-$name/jobs/$FOREIGN_SLOT" \
        "$WORK/runtime-$name/hostkeys/$FOREIGN_SLOT" \
        "$WORK/runtime-$name/state/slots/$FOREIGN_SLOT" \
        "$WORK/state-$name/$FOREIGN_SLOT/workspace"
}

# A fixture with NO current-slot residue at all, but per-slot state under a slot
# name this generation does not define — plus, when $2 is given, a stale bind
# mount on the foreign workspace path.
fixture_foreign() {
    local name="$1" with_mount="''${2:-}"
    local stub_dir="$WORK/stub-$name" p
    rm -rf "$stub_dir" "$WORK/runtime-$name" "$WORK/state-$name"
    mkdir -p "$stub_dir/mounts" "$WORK/runtime-$name" "$WORK/state-$name"
    while read -r p; do mkdir -p "$p"; done < <(foreign_paths "$name")
    : >"$WORK/runtime-$name/slots/$FOREIGN_SLOT/session.json"
    # A CURRENT-pool per-slot directory that recover must never touch.
    mkdir -p "$WORK/runtime-$name/hostkeys/$SLOT"
    : >"$WORK/runtime-$name/hostkeys/$SLOT/ssh_host_ed25519_key"
    if [[ -n $with_mount ]]; then
        printf '%s' "$RUNTIME_ROOT/workspaces/old-task" \
            >"$stub_dir/mounts/$(printf '%s' "$STATE_ROOT/$FOREIGN_SLOT/workspace" | tr / _)"
    fi
}

foreign_paths_present() {
    local name="$1" p n=0
    while read -r p; do [[ -e $p ]] && n=$((n + 1)); done < <(foreign_paths "$name")
    printf '%s' "$n"
}

printf '=== 1. nothing to recover is a success ===\n'
fixture clean
rm -f "$WORK/stub-clean/mounts"/*
rc="$(run_recover clean free)"
expect "recover exits 0 when there is nothing to do" 0 "$rc"
if grep -q "nothing to recover" "$WORK/recover-clean.log"; then
    pass "recover says there was nothing to recover"
else
    fail "recover did not report an empty run: $(cat "$WORK/recover-clean.log")"
fi

printf '\n=== 2. a stale bind held by virtiofsd is really released ===\n'
# The umount stub refuses (EBUSY) until microvm-virtiofsd@<slot> is stopped, so
# this only passes if the launcher stops that unit and re-tries.
fixture holder
rc="$(run_recover holder holder)"
if [[ $rc != 0 ]]; then sed 's/^/      /' "$WORK/recover-holder.log"; fi
expect "recover exits 0 after releasing the holder" 0 "$rc"
expect "the mount is really gone afterwards" 0 "$(mounts_left holder)"
if grep -q "stop microvm-virtiofsd@$SLOT.service" "$WORK/stub-holder/systemctl.log"; then
    pass "recover stopped the slot's virtiofsd to release the share"
else
    fail "recover never stopped microvm-virtiofsd@$SLOT.service: $(cat "$WORK/stub-holder/systemctl.log")"
fi
if grep -q "LAZY-UNMOUNT-ATTEMPTED" "$WORK/stub-holder/mount.log"; then
    fail "recover fell back to a LAZY unmount (which leaves the mount in findmnt)"
else
    pass "recover never reached for a lazy unmount"
fi
if grep -q "unmounting $STATE_ROOT/$SLOT/workspace" "$WORK/recover-holder.log"; then
    pass "recover reported the unmount it performed"
else
    fail "recover did not report the unmount"
fi
if grep -q "FAILED to unmount" "$WORK/recover-holder.log"; then
    fail "recover reported a failure although the mount is gone"
else
    pass "recover reported no failure for a mount it really released"
fi

printf '\n=== 3. a mount that survives is a LOUD failure, not a success ===\n'
fixture wedged
rc="$(run_recover wedged wedged)"
if [[ $rc == 0 ]]; then
    fail "recover exited 0 although the bind mount is still there"
else
    pass "recover exited non-zero for a mount it could not release (rc $rc)"
fi
expect "the mount is (still) there, i.e. the scenario really wedged" 1 "$(mounts_left wedged)"
if grep -q "FAILED to unmount" "$WORK/recover-wedged.log"; then
    pass "recover names the mount it could not release"
else
    fail "recover did not name the surviving mount: $(cat "$WORK/recover-wedged.log")"
fi
if grep -q "STILL mounted" "$WORK/recover-wedged.log"; then
    pass "the launcher logs that the mount survived the unmount"
else
    fail "no 'STILL mounted' diagnostic in the log"
fi
# The lifecycle stream must carry it as a well-formed JSON record.
if grep -h '"event":"mount-leak"' "$WORK/recover-wedged.log" >"$WORK/leak-events.json"; then
    pass "a mount-leak lifecycle event was emitted"
    if command -v jq >/dev/null 2>&1; then
        if jq -e '.event == "mount-leak" and (.message | test("could not unmount"))' \
            <"$WORK/leak-events.json" >/dev/null; then
            pass "the mount-leak event is well-formed JSON naming the mount point"
        else
            fail "the mount-leak event is malformed: $(cat "$WORK/leak-events.json")"
        fi
    fi
else
    fail "no mount-leak event in the lifecycle stream: $(cat "$WORK/recover-wedged.log")"
fi
if grep -q "LAZY-UNMOUNT-ATTEMPTED" "$WORK/stub-wedged/mount.log"; then
    fail "recover fell back to a LAZY unmount instead of reporting the leak"
else
    pass "recover reported the leak instead of hiding it behind a lazy unmount"
fi

printf '\n=== 4. foreign per-slot state is REPORTED and left alone ===\n'
if [[ $FOREIGN_SLOT == "$SLOT" ]]; then
    fail "the harness fixture uses a slot name that IS in the pool ($FOREIGN_SLOT)"
fi
for mode in dry live; do
    scen="foreign-$mode"
    fixture_foreign "$scen" mounted
    if [[ $mode == dry ]]; then
        rc="$(run_recover "$scen" holder --dry-run)"
    else
        rc="$(run_recover "$scen" holder)"
    fi
    log="$WORK/recover-$scen.log"
    if [[ $rc != 0 ]]; then sed 's/^/      /' "$log"; fi
    expect "recover ($mode) exits 0 with only foreign state present" 0 "$rc"
    if grep -q "^foreign: " "$log"; then
        pass "recover ($mode) reports the foreign state as its own finding"
    else
        fail "recover ($mode) never mentioned the foreign state: $(cat "$log")"
    fi
    if grep -q "slot name $FOREIGN_SLOT" "$log"; then
        pass "recover ($mode) names the foreign slot ($FOREIGN_SLOT)"
    else
        fail "recover ($mode) does not name the foreign slot"
    fi
    missing=0
    for d in slots jobs hostkeys state/slots; do
        grep -q "$RUNTIME_ROOT/$d/$FOREIGN_SLOT" "$log" || {
            missing=1
            printf '      not reported: %s\n' "$RUNTIME_ROOT/$d/$FOREIGN_SLOT"
        }
    done
    grep -q "$STATE_ROOT/$FOREIGN_SLOT/workspace" "$log" || missing=1
    if ((missing)); then
        fail "recover ($mode) does not report every foreign per-slot path"
    else
        pass "recover ($mode) reports every foreign per-slot path"
    fi
    if grep -q "STILL MOUNTED" "$log"; then
        pass "recover ($mode) flags the foreign path that is still mounted"
    else
        fail "recover ($mode) does not flag the mounted foreign path"
    fi
    expect "recover ($mode) removed nothing without --prune-foreign" 5 \
        "$(foreign_paths_present "$scen")"
    expect "recover ($mode) left the foreign mount in place" 1 "$(mounts_left "$scen")"
    if grep -q "recover --prune-foreign" "$log"; then
        pass "recover ($mode) says how to remove it"
    else
        fail "recover ($mode) does not point at --prune-foreign"
    fi
done

printf '\n=== 5. --dry-run --prune-foreign still only prints ===\n'
fixture_foreign foreign-dryprune mounted
rc="$(run_recover foreign-dryprune holder --dry-run --prune-foreign)"
expect "recover --dry-run --prune-foreign exits 0" 0 "$rc"
if grep -q "would remove $RUNTIME_ROOT/slots/$FOREIGN_SLOT" "$WORK/recover-foreign-dryprune.log" &&
    grep -q "would unmount $STATE_ROOT/$FOREIGN_SLOT/workspace" "$WORK/recover-foreign-dryprune.log"; then
    pass "it says what it WOULD remove and unmount"
else
    fail "no would-remove/would-unmount lines: $(cat "$WORK/recover-foreign-dryprune.log")"
fi
expect "nothing was actually removed" 5 "$(foreign_paths_present foreign-dryprune)"
expect "nothing was actually unmounted" 1 "$(mounts_left foreign-dryprune)"

printf '\n=== 6. --prune-foreign removes it, through the VERIFIED unmount ===\n'
fixture_foreign foreign-prune mounted
rc="$(run_recover foreign-prune holder --prune-foreign)"
if [[ $rc != 0 ]]; then sed 's/^/      /' "$WORK/recover-foreign-prune.log"; fi
expect "recover --prune-foreign exits 0" 0 "$rc"
expect "every foreign per-slot path is gone" 0 "$(foreign_paths_present foreign-prune)"
expect "the foreign bind mount is gone" 0 "$(mounts_left foreign-prune)"
if grep -q "stop microvm-virtiofsd@$FOREIGN_SLOT.service" "$WORK/stub-foreign-prune/systemctl.log"; then
    pass "the foreign mount was released through the same verified path (virtiofsd stopped)"
else
    fail "the foreign unmount did not go through the verified path"
fi
if grep -q "LAZY-UNMOUNT-ATTEMPTED" "$WORK/stub-foreign-prune/mount.log"; then
    fail "the foreign unmount fell back to a lazy unmount"
else
    pass "the foreign unmount never used a lazy unmount"
fi
if [[ -f "$WORK/runtime-foreign-prune/hostkeys/$SLOT/ssh_host_ed25519_key" ]]; then
    pass "per-slot state of the CURRENT pool was left untouched"
else
    fail "--prune-foreign deleted state belonging to a current slot"
fi
if grep -h '"event":"recovery-action"' "$WORK/recover-foreign-prune.log" \
    >"$WORK/foreign-events.json"; then
    if jq -e 'select(.message | test("foreign per-slot state")) | .slot == "'"$FOREIGN_SLOT"'"' \
        <"$WORK/foreign-events.json" >/dev/null; then
        pass "a well-formed recovery-action event names the foreign slot"
    else
        fail "no well-formed foreign recovery-action event: $(cat "$WORK/foreign-events.json")"
    fi
else
    fail "no recovery-action event was emitted for the foreign state"
fi

printf '\n=== 7. a foreign mount that cannot be released is not deleted ===\n'
fixture_foreign foreign-wedged mounted
rc="$(run_recover foreign-wedged wedged --prune-foreign)"
if [[ $rc == 0 ]]; then
    fail "recover --prune-foreign exited 0 although the foreign mount survived"
else
    pass "recover --prune-foreign exited non-zero for a wedged foreign mount (rc $rc)"
fi
if grep -q "FAILED to unmount $STATE_ROOT/$FOREIGN_SLOT/workspace" "$WORK/recover-foreign-wedged.log"; then
    pass "it names the foreign mount it could not release"
else
    fail "no FAILED-to-unmount line for the foreign mount"
fi
if [[ -d "$WORK/state-foreign-wedged/$FOREIGN_SLOT/workspace" ]]; then
    pass "the still-mounted foreign path was NOT removed"
else
    fail "a still-mounted foreign path was removed anyway"
fi

printf '\n=== 8. --prune-foreign with nothing foreign is a quiet success ===\n'
fixture clean
rm -f "$WORK/stub-clean/mounts"/*
rc="$(run_recover clean free --prune-foreign)"
expect "recover --prune-foreign exits 0 with nothing to prune" 0 "$rc"
if grep -q "no per-slot state outside the current pool" "$WORK/recover-clean.log"; then
    pass "it says there was nothing foreign to prune"
else
    fail "no 'nothing foreign' line: $(cat "$WORK/recover-clean.log")"
fi

printf '\n%d passed, %d failed\n' "$PASSED" "$FAILED"
[[ $FAILED -eq 0 ]]
