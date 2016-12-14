{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    slock dmenu unclutter
  ] ++ (with pkgs.haskellPackages; [
    xmonad xmobar yeganesh
  ]);

  services.xserver = {
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      # i3.enable = true;
      default = "xmonad";
    };

    desktopManager = {
      xterm.enable = false;
      default = "none";
    };
  };
}
