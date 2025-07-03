{
  config,
  lib,
  pkgs,
  ...
}:

{
  home-manager.sharedModules = [ { home.packages = with pkgs; [ solaar ]; } ];
  hardware.logitech.wireless.enable = true;
}
