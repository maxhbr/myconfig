{ config, pkgs, ... }:

{
  services.xserver = {
    # desktopManager.kdm.enable = true;
    desktopManager.kde5.enable = true;
  };
}
