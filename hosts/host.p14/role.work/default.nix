# Copyright 2017-2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }: let
  waylandSlack = pkgs.slack.overrideAttrs (old: {
    installPhase = old.installPhase + ''
      rm $out/bin/slack

      makeWrapper $out/lib/slack/slack $out/bin/slack \
        --prefix XDG_DATA_DIRS : $GSETTINGS_SCHEMAS_PATH \
        --prefix PATH : ${lib.makeBinPath [pkgs.xdg-utils]} \
        --set NIXOS_OZONE_WL 1 \
        --add-flags "--ozone-platform=wayland --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer"
    '';
  });
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
    programs.evolution.enable = true;
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
      home.packages = with pkgs; [
        # idea.idea-ultimate # jetbrains.phpstorm
        dia
        # insync
        exiftool
        # misc-desktop-tools:
        # libreoffice
        waylandSlack
        # element-desktop
        # rambox
        subversion
        google-cloud-sdk
      ];
    }];
  };
}
