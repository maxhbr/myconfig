# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Kanshi wiring for the wayland sessions. Per-host display layouts live in
# `hosts/host.<hostname>/kanshi/*.nix` (one profile per file, written by the
# `kanshi-add-layout` skill or by hand); every `.nix` file in that directory
# is imported here and only appends to `services.kanshi.settings`.
{
  config,
  lib,
  pkgs,
  super,
  ...
}:

let
  layoutsDir = toString ./../../hosts + "/host.${super.networking.hostName}/kanshi";

  profileNames = map (x: x.profile.name) (
    lib.filter (x: x ? profile && x.profile.name != "") config.services.kanshi.settings
  );

  kanshiPackage = config.services.kanshi.package;

  # kanshictl derives its varlink socket path from $WAYLAND_DISPLAY
  # ("$XDG_RUNTIME_DIR/fr.emersion.kanshi.$WAYLAND_DISPLAY"). Waybar's
  # custom-module children inherit waybar's environment, which can carry a
  # stale or missing WAYLAND_DISPLAY (session restarts, env import races).
  # The daemon's socket is named after its wayland display, so instead of
  # trusting the inherited env the probe locates the actual socket and
  # aligns WAYLAND_DISPLAY to it; only if no socket exists at all does it
  # fall back to the imported user-manager environment.
  ensureWaylandEnv = ''
    socket_glob="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/fr.emersion.kanshi.*"
    found_socket=""
    for s in $socket_glob; do
      if [ -S "$s" ]; then
        found_socket="$s"
        break
      fi
    done
    if [ -n "$found_socket" ]; then
      export WAYLAND_DISPLAY="''${found_socket##*.}"
    elif [ -z "''${WAYLAND_DISPLAY:-}" ] && command -v systemctl >/dev/null 2>&1; then
      wayland_display="$(systemctl --user show-environment 2>/dev/null \
        | ${pkgs.gnused}/bin/sed -n 's/^WAYLAND_DISPLAY=//p')" || wayland_display=""
      [ -n "$wayland_display" ] && export WAYLAND_DISPLAY="$wayland_display"
    fi
  '';

  kanshiStatus = pkgs.writeShellApplication {
    name = "kanshi-status";
    runtimeInputs = [
      pkgs.systemd
      pkgs.gnused
    ];
    text = ''
            ${ensureWaylandEnv}
            set -euo pipefail
            status=""
            if [ -n "''${WAYLAND_DISPLAY:-}" ]; then
              status="$(${kanshiPackage}/bin/kanshictl status 2>/dev/null)" || status=""
            fi
            current="$(printf '%s\n' "$status" \
              | ${pkgs.gnused}/bin/sed -n 's/^Current profile: //p')"
            pending="$(printf '%s\n' "$status" \
              | ${pkgs.gnused}/bin/sed -n 's/^Pending profile: //p')"
            # kanshi only records a current profile once the compositor confirms
            # the apply; while a re-apply is pending it reports "(none)" even
            # though the outputs already show the profile's layout. Prefer the
            # confirmed profile, fall back to the pending one.
            profile="$current"
            if [ -z "$profile" ] || [ "$profile" = "(none)" ]; then
              profile="$pending"
            fi
            if [ -z "''${profile:-}" ] || [ "$profile" = "(none)" ]; then
              if [ -z "$status" ]; then
                tooltip="kanshictl: cannot reach the kanshi daemon (socket \
      \$XDG_RUNTIME_DIR/fr.emersion.kanshi.\$WAYLAND_DISPLAY)"
              else
                tooltip="kanshi: no profile matched"
              fi
              ${pkgs.jq}/bin/jq -cn --arg t "$tooltip" \
                '{text:" no profile",class:"disconnected",tooltip:$t}'
              exit 0
            fi
            ${pkgs.jq}/bin/jq -cn --arg p "$profile" --arg c "$current" \
              '{text:(" " + $p),profile:$p,class:(if $c == "" or $c == "(none)" then "pending" else "connected" end),tooltip:("profile: " + $p)}'
    '';
  };

  kanshiSwitch = pkgs.writeShellApplication {
    name = "kanshi-switch";
    runtimeInputs = [
      pkgs.systemd
      pkgs.gnused
    ];
    text = ''
      ${ensureWaylandEnv}
      set -euo pipefail
      choice="$(printf '%s\n' ${lib.escapeShellArgs profileNames} \
        | ${pkgs.wofi}/bin/wofi --dmenu --prompt "kanshi:" --cache-file /dev/null)" || exit 0
      [ -n "$choice" ] || exit 0
      ${kanshiPackage}/bin/kanshictl switch "$choice"
      # waybar custom modules default to signal = 8 (SIGRTMIN+8)
      ${pkgs.psmisc}/bin/pkill -SIGRTMIN+8 waybar 2>/dev/null || true
    '';
  };

  # kanshi profile hook: switch an ALSA card's pipewire profile, e.g. to
  # silence the Radeon HDMI audio unless the TV is connected. Used from
  # `profile.exec` of per-host kanshi layouts; kanshi re-runs the hook on
  # every layout change, which covers the monitor hotplug case. The script
  # lands in home.packages, so `exec kanshi-audio-card ...` resolves by name:
  # kanshi's exec runs /bin/sh with the daemon's environment, and the kanshi
  # user service inherits the PATH that niri-session imports into the user
  # manager.
  # Usage: kanshi-audio-card <card> off|auto|<profile>
  #   off      - disable the card entirely
  #   auto     - select the highest-priority *available* profile; the
  #              hotplugged monitor marks exactly its own ports available,
  #              so this lands on the port the TV/monitor actually uses
  #   <profile> - an explicit profile name, e.g. output:hdmi-stereo-extra2
  # A missing card is not an error (the device may be hot-plugged away);
  # auto without any available profile also succeeds as a no-op.
  kanshiAudioCard = pkgs.writeShellApplication {
    name = "kanshi-audio-card";
    runtimeInputs = [
      pkgs.pulseaudio
      pkgs.gnugrep
      pkgs.gawk
    ];
    text = ''
      set -euo pipefail
      card="''${1:-}"
      action="''${2:-}"
      if [ -z "$card" ] || [ -z "$action" ]; then
        echo "usage: kanshi-audio-card <alsa_card.pci-...> off|auto|<profile>" >&2
        exit 2
      fi
      if ! pactl list cards short | awk -v c="$card" -F'\t' '$2 == c { found=1 } END { exit !found }'; then
        echo "kanshi-audio-card: card $card not present" >&2
        exit 0
      fi
      if [ "$action" != "auto" ]; then
        pactl set-card-profile "$card" "$action"
        exit 0
      fi
      # auto: pick the highest-priority profile that is currently available
      # (pactl marks ports of unplugged monitors "available: no"); "off" and
      # pro-audio are never candidates
      profile="$(
        pactl list cards \
          | awk -v c="$card" '
            /^\tName: / { in_card = ($0 == "\tName: " c) }
            in_card && /^\t\t/ && /available: yes/ && $0 !~ /^\t\t(off|pro-audio):/ {
              match($0, /priority: [0-9]+/)
              priority = substr($0, RSTART + 10, RLENGTH - 10) + 0
              name = $0
              sub(/^\t\t/, "", name)
              # the profile name itself contains colons (output:hdmi-...);
              # strip only the ": <description>" tail
              sub(/: .*/, "", name)
              print priority, name
            }
          ' \
          | sort -rn \
          | head -n1 \
          | cut -d' ' -f2-
      )"
      if [ -z "''${profile:-}" ]; then
        echo "kanshi-audio-card: no available profile for $card" >&2
        exit 0
      fi
      pactl set-card-profile "$card" "$profile"
    '';
  };

  waybarApplet = profileNames != [ ] && config.programs.waybar.enable;
in
{
  imports = lib.optionals (builtins.pathExists layoutsDir) (
    map (n: layoutsDir + "/${n}") (
      lib.filter (lib.hasSuffix ".nix") (builtins.attrNames (builtins.readDir layoutsDir))
    )
  );

  config = lib.mkIf config.services.kanshi.enable {
    home.packages =
      (lib.optionals waybarApplet [
        kanshiStatus
        kanshiSwitch
      ])
      ++ [ kanshiAudioCard ];

    programs.waybar = lib.mkIf waybarApplet {
      settings.mainBar = {
        modules-center = [ "custom/kanshi" ];
        "custom/kanshi" = {
          format = "{}";
          exec = "${kanshiStatus}/bin/kanshi-status";
          return-type = "json";
          interval = 5;
          tooltip = true;
          on-click = "${kanshiSwitch}/bin/kanshi-switch";
          on-click-middle = "${kanshiPackage}/bin/kanshictl reload";
          rotate = 90;
        };
      };
    };
  };
}
