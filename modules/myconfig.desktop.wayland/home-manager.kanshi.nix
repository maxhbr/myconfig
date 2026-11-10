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

  kanshiStatus = pkgs.writeShellApplication {
    name = "kanshi-status";
    text = ''
      set -euo pipefail
      profile="$(${kanshiPackage}/bin/kanshictl status 2>/dev/null \
        | ${pkgs.gnused}/bin/sed -n 's/^Current profile: //p')" || profile=""
      if [ -z "''${profile:-}" ] || [ "$profile" = "(none)" ]; then
        ${pkgs.jq}/bin/jq -cn '{text:" no profile",class:"disconnected",tooltip:"kanshi: no profile matched"}'
        exit 0
      fi
      ${pkgs.jq}/bin/jq -cn --arg p "$profile" \
        '{text:(" " + $p),profile:$p,class:"connected",tooltip:("profile: " + $p)}'
    '';
  };

  kanshiSwitch = pkgs.writeShellApplication {
    name = "kanshi-switch";
    text = ''
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
