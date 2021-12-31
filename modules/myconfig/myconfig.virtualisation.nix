{ config, lib, pkgs, ... }:

let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    virtualisation.enable = mkEnableOption "myconfig.virtualisation";
  };
  config = (lib.mkIf cfg.virtualisation.enable {
    virtualisation.podman.enable = true;
    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
  });
}
