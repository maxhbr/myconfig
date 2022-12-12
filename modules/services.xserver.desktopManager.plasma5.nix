{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.xserver.desktopManager.plasma5.enable {
    services.xserver.desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [
  elisa
  gwenview
  okular
  oxygen
  khelpcenter
  konsole
  plasma-browser-integration
  print-manager
];
  };
}
