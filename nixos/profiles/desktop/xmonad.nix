{ config, pkgs, ... }:
let
  unstable = (import (fetchTarball http://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) {});
in {
  environment.systemPackages = (with pkgs; [
    unstable.dmenu unclutter
    xss-lock
    libnotify dzen2 # xfce.xfce4notifyd # notify-osd
  ]) ++ (with unstable.haskellPackages; [
    xmonad xmobar yeganesh
  ]);

  services.xserver = {
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      default = "xmonad";
    };

    desktopManager = {
      xterm.enable = false;
      default = "none";
    };
  };
}
