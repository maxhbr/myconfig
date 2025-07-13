{
  config,
  lib,
  pkgs,
  ...
}:

{
  config = lib.mkIf config.virtualisation.podman.enable {
    home-manager.sharedModules = [{ home.packages = with pkgs; [ podman-compose ]; }];
    environment.systemPackages = [ pkgs.netavark ];
  };
}
