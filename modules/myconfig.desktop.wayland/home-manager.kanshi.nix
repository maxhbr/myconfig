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
  # custom-module children inherit waybar's environment; when waybar was
  # started outside the wayland session scope (or the env was not imported),
  # kanshictl cannot find the socket and reports nothing. Recover the env
  # from the kanshi systemd user service if available.
  ensureWaylandEnv = ''
    if [ -z "''${WAYLAND_DISPLAY:-}" ] && command -v systemctl >/dev/null 2>&1; then
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
            status="$(${kanshiPackage}/bin/kanshictl status 2>/dev/null)" || status=""
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
    runtimeInputs = [ pkgs.systemd ];
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

  waybarApplet = profileNames != [ ] && config.programs.waybar.enable;
in
{
  imports = lib.optionals (builtins.pathExists layoutsDir) (
    map (n: layoutsDir + "/${n}") (
      lib.filter (lib.hasSuffix ".nix") (builtins.attrNames (builtins.readDir layoutsDir))
    )
  );

  config = lib.mkIf config.services.kanshi.enable {
    home.packages = lib.optionals waybarApplet [
      kanshiStatus
      kanshiSwitch
    ];

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
