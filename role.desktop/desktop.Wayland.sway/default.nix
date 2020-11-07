# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  imports = [ ../desktop.common ];

  config = {
    home-manager.users.mhuber = {
      home.file = { ".config/sway/config".source = ./config/sway/config; };
      home.packages = with pkgs; [
        grim # for screenshots
        qt5.qtwayland
      ];
    };
    programs.sway = {
      enable = true;
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        # needs qt5.qtwayland in systemPackages
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        # Fix for some Java AWT applications (e.g. Android Studio),
        # use this if they aren't displayed properly:
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    };
    environment = {
      loginShellInit = ''
        [[ -z $DISPLAY && $XDG_VTNR -eq 6 ]] && exec sway --my-next-gpu-wont-be-nvidia
      '';
    };
    # nixpkgs.overlays = [
    #   (final: prev: {
    #     # with sway/wayland support
    #     redshift = prev.redshift.overrideAttrs (o: {
    #       src = prev.fetchFromGitHub {
    #         owner = "CameronNemo";
    #         repo = "redshift";
    #         rev = "39c162ca487a59857c2eac231318f4b28855798b";
    #         sha256 = "1in27draskwwi097wiam26bx2szcf58297am3gkyng1ms3rz6i58";
    #       };
    #     });
    #   })
    # ];
  };
}
