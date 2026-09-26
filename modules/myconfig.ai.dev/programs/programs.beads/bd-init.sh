#!/usr/bin/env bash
# shellcheck shell=bash
#
# bd-init: bring up beads for the current repository and wire it to the
# matching gitolite repo on vserver.
#
# Usage: bd-init [--force] [--host <host>] [--user <user>] [--repo <name>]
#                [--remote-name <git-remote-name>]
#
# Performs the full beads bring-up for the repository containing the
# working directory:
#
#   1. `bd init --non-interactive` (skipped when .beads/ already exists)
#   2. Dolt sync wiring: sync.remote and the Dolt remote "origin" both
#      point at git+ssh://<user>@<host>/<repo>, so `bd sync` publishes
#      refs/dolt/data into the source repo itself — no separate beads
#      repo on the gitolite server (the setup this myconfig repo uses,
#      see its .beads/config.yaml)
#   3. git remote <remote-name> → <user>@<host>:<repo> for plain git
#      operations against the gitolite repo
#   4. one `bd sync` to seed refs/dolt/data on the server
#
# Derivation and defaults: <repo> is the basename of the repository's
# top-level directory (--repo overrides), <host> is
# vserver.wg0.maxhbr.local (--host overrides), <user> is gitolite and
# <remote-name> is gitolite.
#
# The matching repo on the gitolite server is created by hand (in the
# gitolite-admin repo); bd-init must NOT assume it exists. It probes with
# `git ls-remote` before touching anything and aborts with instructions
# when the probe fails. --force (or BD_INIT_FORCE=1) skips the probe and
# wires everything anyway, for use before the repo is created on the
# server or while offline; the final `bd sync` is always left to the user
# in that case (`bd sync`).
#
# A reachable but branchless repo (freshly created on the server, nothing
# pushed yet) cannot be synced: bd needs at least one branch. bd-init
# wires it up and prints the missing `git push` step instead of failing.
#
# Runtime deps: bd and git (both resolved from PATH at runtime — bd ships
# right next to this wrapper in home.packages).

set -euo pipefail

usage() {
    echo "Usage: bd-init [--force] [--host <host>] [--user <user>]" >&2
    echo "               [--repo <name>] [--remote-name <git-remote-name>]" >&2
    echo "" >&2
    echo "Initialize beads for the current repository and wire it to the" >&2
    echo "matching gitolite repo: bd init, Dolt remote origin +" >&2
    echo "sync.remote → git+ssh://<user>@<host>/<repo>, and a git remote" >&2
    echo "<remote-name> → <user>@<host>:<repo>. The remote repo must exist" >&2
    echo "already; --force wires everything without checking." >&2
}

force=0
host="vserver.wg0.maxhbr.local"
user="gitolite"
repo=""
remote_name="gitolite"

while [ "$#" -gt 0 ]; do
    case "$1" in
    --force) force=1 ;;
    --host)
        [ "$#" -ge 2 ] || {
            echo "bd-init: --host requires a value" >&2
            exit 2
        }
        host="$2"
        shift
        ;;
    --user)
        [ "$#" -ge 2 ] || {
            echo "bd-init: --user requires a value" >&2
            exit 2
        }
        user="$2"
        shift
        ;;
    --repo)
        [ "$#" -ge 2 ] || {
            echo "bd-init: --repo requires a value" >&2
            exit 2
        }
        repo="$2"
        shift
        ;;
    --remote-name)
        [ "$#" -ge 2 ] || {
            echo "bd-init: --remote-name requires a value" >&2
            exit 2
        }
        remote_name="$2"
        shift
        ;;
    -h | --help)
        usage
        exit 0
        ;;
    --)
        shift
        break
        ;;
    *)
        echo "bd-init: unknown option: $1" >&2
        usage
        exit 2
        ;;
    esac
    shift
done
if [ "$#" -gt 0 ]; then
    echo "bd-init: unexpected argument: $1 (no positional arguments)" >&2
    usage
    exit 2
fi
if [ "${BD_INIT_FORCE:-0}" = "1" ]; then
    force=1
fi

command -v bd >/dev/null 2>&1 || {
    echo "bd-init: bd not found on PATH" >&2
    exit 1
}

root="$(git rev-parse --show-toplevel 2>/dev/null)" || {
    echo "bd-init: not inside a git repository" >&2
    exit 1
}
cd "$root"
if [ -z "$repo" ]; then
    repo="$(basename "$root")"
fi

dolt_url="git+ssh://$user@$host/$repo"
git_url="$user@$host:$repo"

# --- reachability probe (skipped by --force) ------------------------------
#
# git ls-remote is the direct test for "repo exists and is readable":
# gitolite answers DENIED/not-found for missing repos. A zero-exit with
# empty output means the repo exists but has no branches yet.

probe_status=0
probe=""
if [ "$force" = 0 ]; then
    probe_ssh="${GIT_SSH_COMMAND:-ssh -o BatchMode=yes -o ConnectTimeout=10}"
    probe="$(GIT_SSH_COMMAND="$probe_ssh" git ls-remote "$dolt_url" 2>&1)" ||
        probe_status=$?
    if [ "$probe_status" -ne 0 ]; then
        echo "bd-init: gitolite repo not reachable: $dolt_url" >&2
        echo "$probe" >&2
        echo "" >&2
        echo "Create the repo '$repo' for user '$user' on $host" >&2
        echo "(gitolite-admin conf + keydir push) and make sure ssh access" >&2
        echo "works, then re-run bd-init. Or pass --force to wire everything" >&2
        echo "up anyway and run 'bd sync' once the repo exists." >&2
        exit 1
    fi
fi

# --- beads initialization --------------------------------------------------

if [ -d .beads ]; then
    echo "bd-init: .beads/ already present, skipping bd init"
else
    bd init --non-interactive
fi

# --- Dolt sync wiring ------------------------------------------------------
#
# Mirror the setup of the myconfig repo: sync.remote (the remote `bd sync`
# targets) and the Dolt remote "origin" both carry the same URL, and dolt
# keeps its state in refs/dolt/data of the source repo itself.

echo "bd-init: configuring Dolt sync remote: $dolt_url"
bd config set sync.remote "$dolt_url"

current_dolt_url="$(bd dolt remote list | awk -v n='origin' '$1 == n { print $2 }')"
if [ -n "$current_dolt_url" ] && [ "$current_dolt_url" != "$dolt_url" ]; then
    echo "bd-init: replacing Dolt remote origin ($current_dolt_url)"
    bd dolt remote remove origin
fi
if [ -z "$current_dolt_url" ] || [ "$current_dolt_url" != "$dolt_url" ]; then
    # --allow-git-origin: when the repo was cloned from this very gitolite
    # repo, its git origin already is that URL and bd would abort without it.
    bd dolt remote add --allow-git-origin origin "$dolt_url"
fi

# --- git remote ------------------------------------------------------------

if existing="$(git remote get-url "$remote_name" 2>/dev/null)"; then
    if [ "$existing" != "$git_url" ]; then
        echo "bd-init: WARNING: git remote '$remote_name' already exists" >&2
        echo "with URL '$existing', leaving it unchanged" >&2
        echo "(bd-init would use '$git_url')" >&2
    fi
else
    echo "bd-init: adding git remote '$remote_name' → $git_url"
    git remote add "$remote_name" "$git_url"
fi

# --- seed refs/dolt/data ---------------------------------------------------

if [ "$force" = 1 ]; then
    echo "bd-init: skipping bd sync (--force: remote reachability unverified)"
    echo "bd-init: run 'bd sync' once the repo exists on the server"
elif [ -z "$probe" ]; then
    echo "bd-init: gitolite repo '$repo' exists but is empty (no branches)" >&2
    echo "bd-init: bd cannot sync against a branchless remote; push a" >&2
    echo "branch first, then sync:" >&2
    echo "  git push '$remote_name' HEAD" >&2
    echo "  bd sync" >&2
else
    sync_status=0
    bd sync || sync_status=$?
    if [ "$sync_status" -ne 0 ]; then
        echo "bd-init: WARNING: bd sync exited with $sync_status" >&2
        echo "bd-init: the wiring is complete; re-run 'bd sync' later" >&2
        echo "(bd sync exit codes: 2 merge conflict, 3 retries exhausted," >&2
        echo "4 stuck dirty working set)" >&2
    fi
fi

echo "bd-init: done for repo '$repo'"
