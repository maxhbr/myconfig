{ config, lib, pkgs, ... }:

let
  cfg = config.myconfig;
  user = cfg.user;
in {
  options.myconfig = with lib; {
    virtualisation.enable = mkEnableOption "myconfig.desktop";
  };
  config = (lib.mkIf cfg.virtualisation.enable {
    virtualisation.docker.enable = true;
    virtualisation.libvirtd.enable = true;
    virtualisation.virtualbox.host.enable = true;
  });
}
