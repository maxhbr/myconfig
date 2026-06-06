#!/usr/bin/env bash
# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Derive the activation timestamp of the currently running NixOS system
# from profile generation symlinks and write Prometheus textfile metrics.
#
# All site-specific values are injected by the Nix writeShellApplication
# wrapper via @PLACEHOLDER@ substitutions (pkgs.substituteAll):
#
#   @textfileDir@       directory to write system-age.prom into
#   @staticInfoSnippet@ pre-rendered nixos_system_info metric line(s)
#
# Strategy for finding the activation timestamp:
#   1. Resolve the store path of /run/current-system.
#   2. Walk /nix/var/nix/profiles/system-*-link, find the one whose
#      target equals that store path, and use its mtime.
#      This pinpoints the moment THIS specific closure was first
#      activated, even if the user has since switched away and rolled
#      back, and it persists across reboots.
#   3. If no matching profile link is found (closure activated
#      out-of-band, profile generations garbage-collected, ...),
#      fall back to mtime /nix/var/nix/profiles/system, which still
#      reflects "last successful switch" and is reboot-stable.
#   4. Last resort: mtime of /run/current-system (resets on reboot).
#
# shellcheck disable=SC2154  # variables are substituted by Nix at build time

set -euo pipefail

target="@textfileDir@/system-age.prom"
tmp="$(mktemp "@textfileDir@/.system-age.prom.XXXXXX")"
trap 'rm -f "$tmp"' EXIT

current_toplevel="$(readlink -f /run/current-system)"
activation_ts=""
activation_source="unknown"

# 1. Try profile generation symlinks (reboot-stable).
for link in /nix/var/nix/profiles/system-*-link; do
    [ -L "$link" ] || continue
    if [ "$(readlink -f "$link")" = "$current_toplevel" ]; then
        # %Y on a symlink (without -L) returns the symlink's own mtime,
        # which is what `nix-env --switch-generation` / nixos-rebuild set
        # when this generation was created.
        ts="$(stat -c %Y "$link")"
        if [ -z "$activation_ts" ] || [ "$ts" -lt "$activation_ts" ]; then
            activation_ts="$ts"
            activation_source="profile-link"
        fi
    fi
done

# 2. Fallback: mtime of the system profile symlink itself.
if [ -z "$activation_ts" ] && [ -L /nix/var/nix/profiles/system ]; then
    activation_ts="$(stat -c %Y /nix/var/nix/profiles/system)"
    activation_source="profile-symlink"
fi

# 3. Last-resort fallback: mtime of /run/current-system
# (resets on reboot, but better than no metric at all).
if [ -z "$activation_ts" ]; then
    activation_ts="$(stat -c %Y /run/current-system)"
    activation_source="run-current-system"
fi

now_ts="$(date +%s)"
age_seconds=$(( now_ts - activation_ts ))

{
    printf '# HELP nixos_system_activation_timestamp_seconds Unix time when the currently running NixOS generation was activated (reboot-stable).\n'
    printf '# TYPE nixos_system_activation_timestamp_seconds gauge\n'
    printf 'nixos_system_activation_timestamp_seconds{source="%s"} %s\n' "$activation_source" "$activation_ts"
    printf '# HELP nixos_system_age_seconds Age of the currently active NixOS generation in seconds (now - activation).\n'
    printf '# TYPE nixos_system_age_seconds gauge\n'
    printf 'nixos_system_age_seconds{source="%s"} %s\n' "$activation_source" "$age_seconds"
    cat <<'EOF'
@staticInfoSnippet@EOF
} > "$tmp"

chmod 0644 "$tmp"
mv -f "$tmp" "$target"
trap - EXIT
