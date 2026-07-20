#!/usr/bin/env bash
#
# tmp.2026-07-20.agent-uid-migration.sh
#
# ONE-TIME migration: move agent users off their old dynamically-allocated
# UID/GID (e.g. 1001) onto the new static block based at 31000 introduced in
# modules/myconfig.agentUsers.nix.
#
# NixOS never rewrites the UID/GID of an already-existing user, so on any host
# that already had an agent user this must be done by hand *before* the next
# `switch`, otherwise:
#   * home-manager-<agent>.service fails with: UID is "1001", expected "31000"
#   * user activation fails with: Failed to create /run/user/1001/nixos
#     (stale logind session/runtime dir pinned to the old UID)
#
# Run this AS ROOT on each affected machine (workstation, ...), then re-run the
# deployment (`upg --local <host>`). A reboot afterwards is recommended and is
# safe because agent homes are ephemeral (impermanence).
#
# Usage:
#   sudo ./tmp.2026-07-20.agent-uid-migration.sh                 # migrates: agent -> 31000
#   sudo ./tmp.2026-07-20.agent-uid-migration.sh agent=31000 offline=31001
#
# Each argument is <username>=<new-id>; the new id is used for BOTH uid and gid
# (agentUsers assigns matching uid/gid = agentIdBase + index). Order must match
# the host's myconfig.agentUsers.names list.
#
# Delete this script once every machine has been migrated.

set -euo pipefail

if [[ $EUID -ne 0 ]]; then
    echo "ERROR: must run as root" >&2
    exit 1
fi

# --- parse mappings ----------------------------------------------------------
declare -a MAPPINGS=("$@")
if [[ ${#MAPPINGS[@]} -eq 0 ]]; then
    MAPPINGS=("agent=31000")
fi

log() { printf '\n=== %s ===\n' "$*"; }

# Extra filesystem roots that may hold agent-owned state and live on their own
# mount (find -xdev will not cross into them from /). Adjust per host if needed.
EXTRA_ROOTS=(/home/agent)

migrate_user() {
    local name="$1" new_id="$2"

    if ! id "$name" &>/dev/null; then
        log "user '$name' does not exist on this host — skipping"
        return 0
    fi

    local old_uid old_gid
    old_uid="$(id -u "$name")"
    old_gid="$(id -g "$name")"

    if [[ $old_uid == "$new_id" && $old_gid == "$new_id" ]]; then
        log "user '$name' already at uid/gid $new_id — nothing to do"
        return 0
    fi

    log "migrating '$name': uid $old_uid -> $new_id, gid $old_gid -> $new_id"

    # 1. Stop everything owned by the agent (linger keeps a user manager alive).
    log "stopping sessions/processes for '$name' (old uid $old_uid)"
    loginctl disable-linger "$name" 2>/dev/null || true
    loginctl terminate-user "$old_uid" 2>/dev/null || true
    systemctl stop "user@${old_uid}.service" "user-runtime-dir@${old_uid}.service" 2>/dev/null || true
    systemctl reset-failed "user@${old_uid}.service" "user-runtime-dir@${old_uid}.service" 2>/dev/null || true
    pkill -9 -u "$name" 2>/dev/null || true
    sleep 1

    if pgrep -u "$name" >/dev/null 2>&1; then
        echo "ERROR: processes still running as '$name'; aborting to avoid corruption" >&2
        pgrep -au "$name" >&2 || true
        exit 1
    fi

    # 2. Reassign gid then uid. groupmod first so the group exists at new id.
    log "groupmod/usermod '$name' -> $new_id"
    groupmod -g "$new_id" "$name"
    usermod -u "$new_id" "$name"

    # 3. Rechown any remaining files that still reference the OLD ids.
    #    usermod already fixed the home dir contents; catch everything else.
    log "rechowning files owned by old uid $old_uid / gid $old_gid -> $new_id:$new_id"
    find / -xdev \( -uid "$old_uid" -o -gid "$old_gid" \) -exec chown -h "$new_id:$new_id" {} + 2>/dev/null || true
    for root in "${EXTRA_ROOTS[@]}"; do
        [[ -d $root ]] || continue
        find "$root" -xdev \( -uid "$old_uid" -o -gid "$old_gid" \) -exec chown -h "$new_id:$new_id" {} + 2>/dev/null || true
    done

    # 4. Drop the stale runtime dir pinned to the old uid.
    log "removing stale /run/user/$old_uid"
    rm -rf "/run/user/$old_uid"

    # 5. Re-enable lingering so the manager comes back up at the new uid.
    loginctl enable-linger "$name" 2>/dev/null || true

    log "'$name' migrated: $(id "$name")"
}

for m in "${MAPPINGS[@]}"; do
    name="${m%%=*}"
    new_id="${m#*=}"
    if [[ -z $name || -z $new_id || $name == "$m" ]]; then
        echo "ERROR: bad mapping '$m' (expected <username>=<new-id>)" >&2
        exit 1
    fi
    migrate_user "$name" "$new_id"
done

cat <<'EOF'

=== DONE ===
Next steps:
  1. Re-run the deployment from your workstation, e.g.:  upg --local <host>
  2. A reboot of this host is recommended (agent homes are ephemeral):
        sudo reboot
EOF
