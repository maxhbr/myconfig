# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }:
let
  cfg = config.myconfig;
  user = myconfig.user;
  niri = pkgs.niri;
in {
  # add option for additional config added to config.kdl
  options.myconfig = with lib; {
    desktop.wayland.niri.additionalConfigKdl = lib.mkOption {
      type = types.str;
      default = "";
      description = "Additional config added to config.kdl";
    };
  };
  config = (lib.mkIf (cfg.desktop.wayland.enable
    && (builtins.elem "niri" cfg.desktop.wayland.selectedSessions
      || builtins.elem "niri-plain" cfg.desktop.wayland.selectedSessions)) {
        home-manager.sharedModules = [
          ({ config, ... }: {
            home.packages = [ niri ];
            xdg.configFile = {
              "niri/config.kdl".source = let
                autostart = pkgs.writeShellScriptBin "autostart.sh" ''
                  set -x
                  exec &> >(tee -a /tmp/niri.''${XDG_VTNR}.''${USER}.autostart.log)
                  ${cfg.desktop.wayland.autostartCommands}
                '';
                drv = pkgs.runCommand "niri-config" {
                  nativeBuildInputs = [ niri ];
                  src = ./config.kdl;
                } ''
                  mkdir $out
                  cat <<EOF >$out/config.kdl
                  $(cat $src)

                  ${cfg.desktop.wayland.niri.additionalConfigKdl}

                  spawn-at-startup "${autostart}/bin/autostart.sh"
                  EOF
                  niri validate --config $out/config.kdl > $out/config.kdl.validate
                '';
              in "${drv}/config.kdl";
            };
            systemd.user.services.niri = {
              Unit = {
                Description = "A scrollable-tiling Wayland compositor";
                BindsTo = "graphical-session.target";
                Before =
                  "graphical-session.target xdg-desktop-autostart.target";
                Wants =
                  "graphical-session-pre.target xdg-desktop-autostart.target";
                After = "graphical-session-pre.target";
              };
              Service = {
                Slice = "session.slice";
                Type = "notify";
                ExecStart =
                  "${niri}/bin/niri --session";
              };
            };
            systemd.user.targets.niri-shutdown = {
              Unit = {
                Description = "Shutdown running niri session";
                DefaultDependencies = "no";
                StopWhenUnneeded = "true";

                Conflicts =
                  "graphical-session.target graphical-session-pre.target";
                After = "graphical-session.target graphical-session-pre.target";
              };
            };
          })
        ];

        myconfig.desktop.wayland.sessions = {
          niri = { command = "${niri}/bin/niri-session"; };
          niri-plain = { command = "${niri}/bin/niri"; };
        };
      });
}
