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
    installPhase =
      old.installPhase
      + ''
        rm $out/bin/slack

        makeWrapper $out/lib/slack/slack $out/bin/slack \
          --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
      '';
    # --prefix xdg_data_dirs : $gsettings_schemas_path \
    # --prefix path : ${lib.makebinpath [pkgs.xdg-utils]} \
    # --set NIXOS_OZONE_WL 1 \
  });
  slack-pkg = pkgs.slack;
  teams-for-linux-pkg = pkgs.nixos-2405.teams-for-linux;
in
{
  imports = [
    ./zoom-us
    # ./jdk.nix
    # ./node.nix
    # ./azure-cli.nix
    ./dotnet.nix
    # ({ pkgs, ... }:
    #   let wing-edit = pkgs.callPackage ../../../pkgs/wing-edit { };
    #   in {
    #     config = {
    #       home-manager.users.mhuber = { home.packages = [ wing-edit ]; };
    #     };
    #   })
  ];
  config = {
    nixpkgs.overlays = map (n: import n) [
      # ./idea-ultimate
    ];
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
        ];
        myconfig.persistence.work-directories = [
          ".config/teams-for-linux"
          ".config/Slack"
          "TNG"
        ];
        home.packages =
          [
            slack-pkg
            teams-for-linux-pkg
          ]
          ++ (with pkgs; [
            # idea.idea-ultimate # jetbrains.phpstorm
            dia
            # insync
            exiftool
            # misc-desktop-tools:
            libreoffice
            # element-desktop
            subversion
            google-cloud-sdk
          ]);
      }
    ];
    myconfig.desktop.wayland.launcherCommands = [
      "slack"
      "teams-for-linux"
    ];
  };
}
