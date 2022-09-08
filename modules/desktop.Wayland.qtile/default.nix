# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  qtilePackage = pkgs.callPackage ./wrapper.nix {
    withBaseWrapper = true;
    extraPaths = with pkgs; [
      foot
      ## Output configuration
      wlopm
      wlr-randr
      kanshi
      way-displays
      ## Program Launchers
      bemenu
      fuzzel
      ## Other
      ristate
      wayshot
    ];
    extraSessionCommands = ''
      export XKB_DEFAULT_LAYOUT=de
      export XKB_DEFAULT_VARIANT=neo
    '';
    withGtkWrapper = true;
    extraOptions = [ ];
  };
in {
  options.myconfig = with lib; { qtile.enable = mkEnableOption "qtile"; };
  config = (lib.mkIf cfg.qtile.enable {
    services.xserver.windowManager.qtile = {
      enable = true;
      # package = qtilePackage;
    };
    services.xserver.displayManager.sessionPackages = [
      qtilePackage
      # (pkgs.symlinkJoin {
      #   name = "qtile-session-${pkgs.qtile.version}";

      #   paths = [ (pkgs.writeTextFile {
      #     name = "qtileSessionPackage";
      #     text = ''
      #       [Desktop Entry]
      #       Name=qtile
      #       Comment=${pkgs.qtile}/bin/qtile
      #       Exec=qtile start -b wayland
      #       Type=Application
      #     '';
      #     destination = "/share/wayland-sessions/qtile.desktop";
      #   }) ];

      #   strictDeps = false;

      #   # We want to run wrapProgram manually
      #   dontWrapGApps = true;

      #   passthru = {
      #     providedSessions = [ "qtile" ];
      #   };
      # })
    ];
    home-manager.sharedModules = [{
      home.file = { ".config/qtile/config.py".source = ./qtile/config.py; };
    }];
  });
}
