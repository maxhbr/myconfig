#!/usr/bin/env nix-shell
# shellcheck shell=bash
#! nix-shell -i bash -p bubblewrap rsync coreutils jq
#! nix-shell -I nixpkgs=https://channels.nixos.org/nixpkgs-unstable/nixexprs.tar.zst
#
# Run a host's home-manager activation against a throwaway HOME, without
# touching the real home directory, systemd or the running generation.
#
# The activation script refuses to run when HOME differs from the configured
# home directory, so the throwaway tree is bind-mounted onto that path with
# bubblewrap for the duration of the run. Everything the activation does to
# HOME therefore lands in the throwaway tree and stays inspectable.
#
# On success the tree's path is printed on stdout, the script waits for a
# keypress so it can be inspected from another shell, and then removes it.
#
# Usage
#   tests/test_home-manager_activation.sh [options] [host] [user]
#
#   host                nixosConfiguration to use (default: test-$(hostname -s))
#   user                home-manager user (default: $USER)
#
# Options
#   --first-modules <nix>
#        Extra module config for a preparatory generation that is activated
#        BEFORE the real one, to reproduce upgrade paths, e.g.
#          --first-modules \
#            '{ myconfig.ai.dev.skills.enable = lib.mkForce false; }'
#   --pre-populate <shell>
#        Run <shell> with HOME set to the throwaway tree before activating.
#        Repeatable. Creates the starting state of an "existing home".
#   --check <shell>
#        Run <shell> with HOME set to the throwaway tree after activating.
#        Repeatable; a non-zero exit marks the run as failed. $ACTIVATION_LOG
#        points at the activation output.
#   --expect-failure
#        Invert the activation exit-code expectation: the run only succeeds
#        when the activation fails.
#   --no-wait             Do not wait for a keypress (non-interactive/CI).
#   --keep                Keep the throwaway tree instead of deleting it.
#   --strict              Fail on a non-zero activation exit code. By default
#                         entries that need a real system (dconf/D-Bus,
#                         systemd units) may fail without failing the run.
#   -h, --help            Show this help.
#
# Note: the flake is evaluated from the git tree, so `git add` new or changed
# files before running this, otherwise the evaluation does not see them.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

FIRST_MODULES=""
PRE_POPULATE=()
CHECKS=()
WAIT=1
KEEP=0
STRICT=0
EXPECT_FAILURE=0
POSITIONAL=()

usage() {
    # The header comment block above `set -euo pipefail`, minus the shebang
    # and the nix-shell directives.
    sed -n '1,/^set -euo pipefail$/p' "${BASH_SOURCE[0]}" |
        sed -e '/^#!\/usr\/bin\/env nix-shell$/d' \
            -e '/^# shellcheck shell=bash$/d' \
            -e '/^#! nix-shell/d' \
            -e '/^set -euo pipefail$/d' \
            -e 's/^# \{0,1\}//'
}

while [ $# -gt 0 ]; do
    case "$1" in
        --first-modules)
            FIRST_MODULES="${2:?--first-modules needs a value}"
            shift 2
            ;;
        --pre-populate)
            PRE_POPULATE+=("${2:?--pre-populate needs a value}")
            shift 2
            ;;
        --check)
            CHECKS+=("${2:?--check needs a value}")
            shift 2
            ;;
        --expect-failure)
            EXPECT_FAILURE=1
            shift
            ;;
        --no-wait)
            WAIT=0
            shift
            ;;
        --keep)
            KEEP=1
            shift
            ;;
        --strict)
            STRICT=1
            shift
            ;;
        -h | --help)
            usage
            exit 0
            ;;
        --)
            shift
            POSITIONAL+=("$@")
            break
            ;;
        -*)
            echo "error: unknown option $1" >&2
            exit 2
            ;;
        *)
            POSITIONAL+=("$1")
            shift
            ;;
    esac
done

HOST="${POSITIONAL[0]:-test-$(hostname -s)}"
USER_NAME="${POSITIONAL[1]:-${USER:-$(id -un)}}"

WORK="$(mktemp -d)"
FAKE_HOME="$WORK/home"
ACTIVATION_LOG="$WORK/activation.log"
mkdir -p "$FAKE_HOME"

cleanup() {
    if [ "$KEEP" -eq 1 ]; then
        echo "kept: $WORK" >&2
        return
    fi
    # The activation mirrors the store's read-only permissions.
    chmod -R u+w "$WORK" 2>/dev/null || true
    rm -rf "$WORK"
}
trap cleanup EXIT

fail() {
    echo "error: $*" >&2
    exit 1
}

configurations="$(nix eval --impure --raw --expr "
  let flake = builtins.getFlake \"$REPO_ROOT\";
  in builtins.concatStringsSep \" \" (builtins.attrNames flake.nixosConfigurations)" 2>/dev/null)" ||
    fail "could not evaluate the flake at $REPO_ROOT"
case " $configurations " in
    *" $HOST "*) ;;
    *) fail "no nixosConfiguration '$HOST' (available: $configurations)" ;;
esac

# Both values come from the evaluated configuration, so the throwaway tree is
# the one the activation script expects.
home_dir="$(nix eval --impure --raw --expr "
  let flake = builtins.getFlake \"$REPO_ROOT\";
  in flake.nixosConfigurations.\"$HOST\".config.home-manager.users.\"$USER_NAME\".home.homeDirectory" |
    tr -d '"')" || fail "cannot read homeDirectory of $HOST/$USER_NAME"

gen_for() {
    local modules="$1" overrides
    if [ -n "$modules" ]; then
        # The snippet is a module body; `lib` is in scope so callers can set
        # priorities (lib.mkForce) against definitions that already exist.
        overrides="let lib = flake.inputs.nixpkgs.lib; in ($modules)"
    else
        overrides="{ }"
    fi
    nix build --impure --no-link --print-out-paths --expr "
      let
        flake = builtins.getFlake \"$REPO_ROOT\";
      in
      (flake.nixosConfigurations.\"$HOST\".extendModules {
        modules = [ ($overrides) ];
      }).config.home-manager.users.\"$USER_NAME\".home.activationPackage"
}

# Some sandboxes block the chgrp syscall outright (EINVAL), which makes the
# activation's `rsync -a` exit non-zero. A normal host either preserves the
# store's group or skips it. Detect that and disable group preservation.
RSYNC_SHIM=""
STORE_RSYNC_BIN=""
prepare_rsync() {
    local probe="$WORK/probe"
    : >"$probe"
    if ! chgrp 0 "$probe" 2>"$WORK/chgrp.err"; then
        grep -q "Invalid argument" "$WORK/chgrp.err" || return 0
    fi
    [ -n "$STORE_RSYNC_BIN" ] || return 0
    echo "note: chgrp is blocked here, disabling rsync group preservation" >&2
    cp -L "$STORE_RSYNC_BIN" "$WORK/real-rsync"
    mkdir -p "$WORK/bin"
    cat >"$WORK/bin/rsync" <<EOF
#!$(command -v bash)
args=()
for a in "\$@"; do
    if [ "\$a" = "--" ]; then args+=("--no-group"); fi
    args+=("\$a")
done
exec $WORK/real-rsync "\${args[@]}"
EOF
    chmod +x "$WORK/bin/rsync"
    RSYNC_SHIM="$WORK/bin/rsync"
}

# Sync programs pin their tools in their own PATH, so the shim is bound over
# the exact store path instead of being prepended to PATH.
find_store_rsync() {
    local activate="$1/activate" path
    while IFS= read -r path; do
        [ -f "$path" ] || continue
        # The wrapper lists the package's bin directory, not the binary.
        grep -oE '/nix/store/[a-z0-9]+-rsync-[0-9.]+/bin' "$path" 2>/dev/null | head -1 && return 0
    done < <(grep -oE '/nix/store/[a-z0-9]+-[^/]*/bin/[^/ ]+' "$activate" | sort -u)
    return 0
}

run_activate() {
    local generation="$1"
    local -a bind_args=(
        --dev-bind / /
        --bind "$FAKE_HOME" "$home_dir"
        --chdir "$WORK"
        --setenv USER "$USER_NAME"
        --setenv LOGNAME "$USER_NAME"
        --setenv HOME "$home_dir"
    )
    if [ -n "$RSYNC_SHIM" ] && [ -n "$STORE_RSYNC_BIN" ]; then
        bind_args+=(--ro-bind "$RSYNC_SHIM" "$STORE_RSYNC_BIN")
    fi
    bwrap "${bind_args[@]}" "$generation/activate"
}

echo "== $HOST/$USER_NAME: building the activation" >&2
NEW_GEN="$(gen_for "")" || fail "could not build the activation"
STORE_RSYNC_BIN="$(find_store_rsync "$NEW_GEN")"
[ -n "$STORE_RSYNC_BIN" ] && STORE_RSYNC_BIN="$STORE_RSYNC_BIN/rsync"
prepare_rsync

if [ -n "$FIRST_MODULES" ]; then
    echo "== building a first generation with: $FIRST_MODULES" >&2
    FIRST_GEN="$(gen_for "$FIRST_MODULES")" || fail "could not build the preparatory generation"
    echo "== activating the preparatory generation" >&2
    run_activate "$FIRST_GEN" >"$WORK/first.log" 2>&1 || true
fi

for snippet in "${PRE_POPULATE[@]+"${PRE_POPULATE[@]}"}"; do
    echo "== pre-populating: $snippet" >&2
    HOME="$FAKE_HOME" bash -c "$snippet" || fail "pre-populate step failed: $snippet"
done

echo "== activating" >&2
STATUS=0
run_activate "$NEW_GEN" >"$ACTIVATION_LOG" 2>&1 || STATUS=$?

failed=0
if [ "$EXPECT_FAILURE" -eq 1 ]; then
    if [ "$STATUS" -eq 0 ]; then
        echo "FAILED: activation succeeded, a failure was expected" >&2
        failed=1
    else
        echo "ok: activation failed as expected (exit $STATUS)" >&2
    fi
elif [ "$STATUS" -ne 0 ]; then
    echo "warning: activation exited with $STATUS; last lines:" >&2
    tail -10 "$ACTIVATION_LOG" >&2
    if [ "$STRICT" -eq 1 ]; then
        fail "activation failed (--strict)"
    fi
    echo "warning: continuing without --strict (entries needing a real system may fail here)" >&2
fi

for snippet in "${CHECKS[@]+"${CHECKS[@]}"}"; do
    if HOME="$FAKE_HOME" ACTIVATION_LOG="$ACTIVATION_LOG" bash -c "$snippet"; then
        echo "ok: $snippet" >&2
    else
        echo "FAILED: $snippet" >&2
        failed=1
    fi
done

echo "$FAKE_HOME"

if [ "$failed" -ne 0 ]; then
    fail "the run did not behave as expected (tree at $FAKE_HOME)"
fi

if [ "$WAIT" -eq 1 ]; then
    if [ -t 0 ]; then
        echo "inspect the tree above, then press any key to remove it..." >&2
        read -rsn1
    else
        echo "note: stdin is not a terminal, not waiting (pass --no-wait to silence)" >&2
    fi
fi
