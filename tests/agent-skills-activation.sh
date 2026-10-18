#!/usr/bin/env bash
# Exercise the real home-manager activation of the agent-skills integration
# without deploying: build two home-manager generations (framework off, then
# on), activate them in sequence against a throwaway HOME, and assert that the
# second activation takes over the directories the first one populated.
#
# This is the upgrade path that failed on f13 with
#   agent-skills: ... exists and is non-empty but has no
#   .agent-skills-managed.json marker
#
# Usage: tests/agent-skills-activation.sh [host] [user]
#   host defaults to test-f13, user to mhuber.
#
# Requires: nix (flakes), bubblewrap. Runs in ~1 minute after the closures
# are built; no VM and no deployment needed.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HOST="${1:-test-f13}"
USER_NAME="${2:-mhuber}"

BWRAP="$(command -v bwrap || true)"
if [ -z "$BWRAP" ]; then
    # Not on PATH (e.g. inside an agent sandbox): use the flake's nixpkgs.
    BWRAP="$(nix build --impure --no-link --print-out-paths --expr "
      let flake = builtins.getFlake \"$REPO_ROOT\";
      in flake.inputs.nixpkgs.legacyPackages.\"$(nix eval --impure --raw --expr 'builtins.currentSystem')\".bubblewrap" 2>/dev/null)/bin/bwrap"
fi
if [ ! -x "$BWRAP" ]; then
    echo "error: bubblewrap (bwrap) is required to bind the throwaway HOME" >&2
    exit 1
fi

WORK="$(mktemp -d)"
# rsync mirrors the store's read-only permissions, so relax them to clean up.
cleanup() {
    if [ -n "${AGENT_SKILLS_TEST_KEEP:-}" ]; then
        echo "kept test workdir: $WORK" >&2
        return
    fi
    chmod -R u+w "$WORK" 2>/dev/null || true
    rm -rf "$WORK"
}
trap cleanup EXIT
FAKE_HOME="$WORK/home"
mkdir -p "$FAKE_HOME"

# The configured home directory, which the activation script insists HOME
# equals. It is bound onto $FAKE_HOME for the duration of each run.
HOME_DIR="$(nix eval --impure --raw --expr "
  let flake = builtins.getFlake \"$REPO_ROOT\";
  in flake.nixosConfigurations.\"$HOST\".config.home-manager.users.\"$USER_NAME\".home.homeDirectory")"

gen_for() {
    local enable="$1"
    nix build --impure --no-link --print-out-paths --expr "
      let
        flake = builtins.getFlake \"$REPO_ROOT\";
        lib = flake.inputs.nixpkgs.lib;
      in
      (flake.nixosConfigurations.\"$HOST\".extendModules {
        modules = [{
          myconfig.ai.dev.skills.enable = lib.mkForce $enable;
        }];
      }).config.home-manager.users.\"$USER_NAME\".home.activationPackage" 2>/dev/null
}

RSYNC_SHIM=""
RSYNC_REAL=""
# Some sandboxes block chgrp outright (EINVAL), which makes the sync's
# `rsync -a` exit 23. On a real host rsync skips groups the user is not a
# member of, so bind a shim that disables group preservation. Attribute
# fidelity is not what this test is about.
prepare_rsync() {
    local probe="$WORK/probe"
    : >"$probe"
    # A normal host either preserves the store's group (root) or skips it.
    # A sandbox that blocks the syscall reports EINVAL ("Invalid argument")
    # instead, which rsync treats as a failure (exit 23).
    if ! chgrp 0 "$probe" 2>"$WORK/chgrp.err"; then
        grep -q "Invalid argument" "$WORK/chgrp.err" || return
    fi
    echo "note: chgrp is blocked here, disabling rsync group preservation" >&2
    [ -n "$STORE_RSYNC_BIN" ] || return
    cp -L "$STORE_RSYNC_BIN" "$WORK/real-rsync"
    RSYNC_REAL="$WORK/real-rsync"
    mkdir -p "$WORK/bin"
    local bash_path
    bash_path="$(command -v bash)"
    cat >"$WORK/bin/rsync" <<EOF
#!$bash_path
args=()
for a in "\$@"; do
    if [ "\$a" = "--" ]; then args+=("--no-group"); fi
    args+=("\$a")
done
exec $RSYNC_REAL "\${args[@]}"
EOF
    chmod +x "$WORK/bin/rsync"
    RSYNC_SHIM="$WORK/bin/rsync"
}

run_activate() {
    local generation="$1"
    shift
    local -a bind_args=(
        --dev-bind / /
        --bind "$FAKE_HOME" "$HOME_DIR"
        --chdir "$WORK"
        --setenv USER "$USER_NAME"
        --setenv LOGNAME "$USER_NAME"
        --setenv HOME "$HOME_DIR"
    )
    if [ -n "$RSYNC_SHIM" ] && [ -n "$STORE_RSYNC_BIN" ]; then
        bind_args+=(--ro-bind "$RSYNC_SHIM" "$STORE_RSYNC_BIN")
    fi
    "$BWRAP" "${bind_args[@]}" "$generation/activate" "$@"
}

# The sync program pins rsync in its own PATH, so a shim needs to be bound
# over that exact store path.
find_store_rsync() {
    local activate="$1/activate" skills_install rsync_dir
    skills_install="$(grep -oE '/nix/store/[a-z0-9]+-skills-install/bin/skills-install' "$activate" | head -1)"
    [ -n "$skills_install" ] || return 0
    rsync_dir="$(grep -oE '/nix/store/[a-z0-9]+-rsync-[0-9.]+/bin' "$skills_install" | head -1)"
    [ -n "$rsync_dir" ] || return 0
    printf '%s/rsync\n' "$rsync_dir"
}

fail() {
    echo "FAIL: $*" >&2
    exit 1
}

SYNC_DESTS=(
    ".claude/skills"
    ".codex/skills"
    ".config/opencode/skills"
    ".agents/skills"
)

echo "== building generations for $HOST/$USER_NAME"
OLD_GEN="$(gen_for false)"
NEW_GEN="$(gen_for true)"
[ -n "$OLD_GEN" ] && [ -n "$NEW_GEN" ] || fail "could not build the home-manager generations"
STORE_RSYNC_BIN="$(find_store_rsync "$NEW_GEN")"
prepare_rsync

echo "== step 1: activate the generation with the framework disabled"
run_activate "$OLD_GEN" >"$WORK/step1.log" 2>&1 || true
for dest in "${SYNC_DESTS[@]}"; do
    [ -d "$FAKE_HOME/$dest" ] || fail "old generation did not populate $dest"
    [ -e "$FAKE_HOME/$dest/.agent-skills-managed.json" ] &&
        fail "$dest unexpectedly has a marker before the framework is enabled"
done

echo "== step 2: activate the generation with the framework enabled"
run_activate "$NEW_GEN" >"$WORK/step2.log" 2>&1 || true
# Entries after the skills sync touch the real system (dconf/D-Bus, systemd
# units) and cannot run against a throwaway HOME; only require that the sync
# itself succeeded.
for dest in "claude" "codex" "opencode" "agents"; do
    grep -q "agent-skills: installed $dest to " "$WORK/step2.log" ||
        fail "the sync did not install the $dest target (see $WORK/step2.log)"
done
if grep -qE "agent-skills.*(error|failed|marker)" "$WORK/step2.log"; then
    tail -20 "$WORK/step2.log" >&2
    fail "the agent-skills activation reported an error"
fi

for dest in "${SYNC_DESTS[@]}"; do
    grep -q "agent-skills: pre-marking .*$dest" "$WORK/step2.log" ||
        fail "$dest was not taken over (marker missing?)"
    [ -f "$FAKE_HOME/$dest/.agent-skills-managed.json" ] ||
        fail "$dest has no marker after activation"
done

# Handcrafted skills must be present as real content, not as dangling links.
for skill in commit beads rtk; do
    [ -f "$FAKE_HOME/.claude/skills/$skill/SKILL.md" ] ||
        fail "handcrafted skill $skill is missing from .claude/skills"
done
# mattpocock sources must be synced too (the framework's own content).
[ -d "$FAKE_HOME/.agents/skills/engineering" ] || fail "mattpocock skills missing"
# A home.file entry that shares the directory with the sync must survive.
[ -L "$FAKE_HOME/.claude/skills/claude-code-home-manager" ] ||
    fail "the home-manager claude plugin was deleted by the sync"

# Negative case: user content in a synced destination must NOT be claimed.
# The sync has to fail loudly instead of deleting it.
USER_HOME="$WORK/userhome"
rm -rf "$USER_HOME"
mkdir -p "$USER_HOME"
FAKE_HOME_SAVED="$FAKE_HOME"
FAKE_HOME="$USER_HOME"
mkdir -p "$FAKE_HOME/.agents/skills"
echo "handwritten notes" >"$FAKE_HOME/.agents/skills/keep.md"
if run_activate "$NEW_GEN" >"$WORK/step3.log" 2>&1; then
    fail "activation succeeded on a destination holding user data"
fi
grep -q "agent-skills: installed agents to " "$WORK/step3.log" &&
    fail "the sync claimed a destination holding user data"
[ -f "$FAKE_HOME/.agents/skills/keep.md" ] || fail "user data was destroyed"
grep -q "AGENT_SKILLS_FORCE" "$WORK/step3.log" ||
    fail "the refusal did not point at AGENT_SKILLS_FORCE"
FAKE_HOME="$FAKE_HOME_SAVED"

echo "PASS: takeover, sync, exclusions and the user-data refusal all behave"
