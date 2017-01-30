{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dmenu unclutter
    slock
  ] ++ (with haskellPackages; [
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

    displayManager.slim = {
      enable = true;
      defaultUser = "mhuber";
    };
  };

  security.setuidPrograms = [ "slock" ];
}
