# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:
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
in {
  imports = [
    ./zoom.nix
    # ./jdk.nix
    # ./node.nix
  ];
  config = {
    nixpkgs.overlays = map (n: import n) [
      # ./idea-ultimate
    ];
    home-manager.sharedModules = [{
      imports = [
        # {
        #   home.packages = with pkgs; [ teams ];
        #   xdg.mimeApps = {
        #     defaultApplications."x-scheme-handler/msteams" =
        #       [ "teams.desktop" ];
        #   };
        #   programs.zsh.shellAliases = {
        #     unteams = ''while pkill teams; do echo "kill it with fire!"; done'';
        #   };
        #   programs.fish.functions = {
        #     unteams = ''
        #       while pkill teams
        #         echo "kill it with fire!"
        #       end
        #       echo "now wo are happy again"
        #     '';
        #   };
        # }
        {
          home.packages = with pkgs; [
            openvpn
            networkmanager-openvpn
            openconnect
            networkmanager-openconnect
            # strongswan
            # networkmanager_strongswan
            networkmanagerapplet
          ];
        }
      ];
      home.packages = [slack-pkg] ++ (with pkgs; [
        # idea.idea-ultimate # jetbrains.phpstorm
        dia
        # insync
        exiftool
        # misc-desktop-tools:
        libreoffice
        slack
        # element-desktop
        subversion
        google-cloud-sdk
      ]);
      myconfig.desktop.wayland.wrappedElectronPackages = [
        {
          pkg = slack-pkg;
          executable = "slack";
        }
      ];
    }];
  };
}
