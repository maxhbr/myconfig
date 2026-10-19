#!/usr/bin/env bash
# Integration check for the agent-skills home-manager wiring.
#
# Reproduces the upgrade path that failed on f13: a generation with the
# framework disabled populates the skills directories via home.file /
# programs.<harness>.skills, then the framework-enabled generation has to take
# those directories over instead of refusing with
#   agent-skills: ... exists and is non-empty but has no
#   .agent-skills-managed.json marker
#
# The activation itself runs through tests/test_home-manager_activation.sh,
# which owns the throwaway HOME.
#
# Usage: tests/agent-skills-activation.sh [host] [user]
#
# The --check/--pre-populate snippets are single-quoted on purpose: they are
# evaluated by the harness with HOME set to the throwaway tree.
# shellcheck disable=SC2016
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HOST="${1:-test-f13}"
USER_NAME="${2:-mhuber}"
HARNESS="$REPO_ROOT/tests/test_home-manager_activation.sh"

failures=0
report() {
    local ok="$1" description="$2"
    if [ "$ok" -eq 0 ]; then
        echo "ok: $description"
    else
        echo "FAILED: $description"
        failures=$((failures + 1))
    fi
}

SYNC_DESTS=(.claude/skills .codex/skills .config/opencode/skills .agents/skills)

echo "== upgrade path: framework disabled -> enabled"
# The preparatory generation deploys the handcrafted skills the old way
# (programs.<harness>.skills and home.file), which populates the very
# directories the framework then has to take over.
MARKER_CHECKS=()
for dest in "${SYNC_DESTS[@]}"; do
    MARKER_CHECKS+=(--check "test -f \"\$HOME/$dest/.agent-skills-managed.json\"")
done
if UPGRADE_LOG="$("$HARNESS" \
    --first-modules '{ myconfig.ai.dev.skills.enable = lib.mkForce false; }' \
    --check 'test -f "$HOME/.claude/skills/commit/SKILL.md"' \
    --check 'test -f "$HOME/.claude/skills/rtk/SKILL.md"' \
    --check 'test -d "$HOME/.agents/skills/engineering"' \
    --check 'test -L "$HOME/.claude/skills/claude-code-home-manager"' \
    "${MARKER_CHECKS[@]}" \
    --no-wait "$HOST" "$USER_NAME" 2>&1)"; then
    report 0 "upgrade path activates"
else
    report 1 "upgrade path activates"
fi

if [ "$failures" -ne 0 ]; then
    echo "$UPGRADE_LOG" >&2
    echo "FAIL: see above" >&2
    exit 1
fi

echo "== safety: a destination holding user data must not be claimed"
USER_HOME="$(mktemp -d)"
cleanup() {
    chmod -R u+w "$USER_HOME" 2>/dev/null || true
    rm -rf "$USER_HOME"
}
trap cleanup EXIT
mkdir -p "$USER_HOME/.agents/skills"
printf 'handwritten notes\n' >"$USER_HOME/.agents/skills/keep.md"

# --pre-populate cannot reach a pre-existing tree, so seed the run's HOME
# through the harness' own pre-populate step instead, using a file outside
# the throwaway tree.
KEEP_FILE="$USER_HOME/.agents/skills/keep.md"
if REFUSE_LOG="$("$HARNESS" \
    --pre-populate "mkdir -p \"\$HOME/.agents/skills\" && cp '$KEEP_FILE' \"\$HOME/.agents/skills/keep.md\"" \
    --expect-failure \
    --check 'test -f "$HOME/.agents/skills/keep.md"' \
    --check 'grep -q AGENT_SKILLS_FORCE "$ACTIVATION_LOG"' \
    --no-wait "$HOST" "$USER_NAME" 2>&1)"; then
    report 0 "user data is refused, not claimed or deleted"
else
    report 1 "user data is refused, not claimed or deleted"
fi

if [ "$failures" -ne 0 ]; then
    echo "$REFUSE_LOG" >&2
    echo "FAIL: see above" >&2
    exit 1
fi

echo "PASS: takeover, sync, exclusions and the user-data refusal all behave"
