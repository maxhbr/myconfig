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
# It also handles the GID-only variant of this problem, i.e. the warnings:
#   warning: not applying GID change of group 'agent' (1001 -> 31000) in /etc/group
#   warning: not applying GID change of group 'assistant' (1002 -> 31001) in /etc/group
#   warning: not applying GID change of group 'offline' (1003 -> 31002) in /etc/group
# For each mapping the matching group is migrated even when no user of that name
# exists (agentUsers declares a group per agent with gid = agentIdBase + index).
#
# Usage:
#   sudo ./tmp.2026-07-20.agent-uid-migration.sh                 # migrates: agent -> 31000
#   sudo ./tmp.2026-07-20.agent-uid-migration.sh agent=31000 assistant=31001 offline=31002
#
# Each argument is <name>=<new-id>; the new id is used for BOTH uid and gid
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
    MAPPINGS=("agent=31000" "assistant=31001" "offline=31002")
fi

log() { printf '\n=== %s ===\n' "$*"; }

# Migrate a group's GID independently of any user of the same name. Handles the
# "not applying GID change of group" warnings for groups that have no matching
# user (or whose user is migrated separately below).
migrate_group() {
    local name="$1" new_id="$2"

    if ! getent group "$name" &>/dev/null; then
        return 0
    fi

    local old_gid
    old_gid="$(getent group "$name" | cut -d: -f3)"

    if [[ $old_gid == "$new_id" ]]; then
        log "group '$name' already at gid $new_id — nothing to do"
        return 0
    fi

    log "migrating group '$name': gid $old_gid -> $new_id"
    groupmod -g "$new_id" "$name"

    log "rechowning files under /home/$name with old gid $old_gid -> gid $new_id"
    [[ -d /home/$name ]] && find "/home/$name" -gid "$old_gid" -exec chgrp -h "$new_id" {} + 2>/dev/null || true

    log "group '$name' migrated: $(getent group "$name")"
}

migrate_user() {
    local name="$1" new_id="$2"

    if ! id "$name" &>/dev/null; then
        log "user '$name' does not exist on this host — migrating group only"
        migrate_group "$name" "$new_id"
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
    log "rechowning files under /home/$name owned by old uid $old_uid / gid $old_gid -> $new_id:$new_id"
    [[ -d /home/$name ]] && find "/home/$name" \( -uid "$old_uid" -o -gid "$old_gid" \) -exec chown -h "$new_id:$new_id" {} + 2>/dev/null || true

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
