# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:
let
  waylandSlack = pkgs.slack.overrideAttrs (old: {
    installPhase = old.installPhase + ''
      rm $out/bin/slack

      makeWrapper $out/lib/slack/slack $out/bin/slack \
        --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
    '';
    # --prefix xdg_data_dirs : $gsettings_schemas_path \
    # --prefix path : ${lib.makebinpath [pkgs.xdg-utils]} \
    # --set NIXOS_OZONE_WL 1 \
  });
  slack-pkg = pkgs.slack;
in
{
  config = {
    home-manager.sharedModules = [
      {
        imports = [
          {
            programs.zsh.shellAliases = {
              unteams = ''while pkill teams; do echo "kill it with fire!"; done'';
            };
            programs.fish.functions = {
              unteams = ''
                while pkill teams
                  echo "kill it with fire!"
                end
                echo "now wo are happy again"
              '';
            };
          }
          {
            myconfig.persistence.work-directories = [
              ".config/Slack"
            ];
            home.packages = [
              slack-pkg
            ]
            myconfig.desktop.wayland.launcherCommands = [
              "slack"
            ];
          }
        ];
        myconfig.persistence.work-directories = [
          "TNG"
        ];
        home.packages = with pkgs; [
          exiftool
          libreoffice
          subversion
        ];
      }

      ./home-manager.dotnet.nix
      ./home-manager.teams-for-linux.nix
      ./home-manager.zoom-us
    ];
  };
}
