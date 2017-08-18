{ config, lib, pkgs, ... }:

let
  unstable = (import <unstable> {});
in {
  options = {
    myconfig.roles.work = {
      enable = lib.mkEnableOption "Work role";
    };
  };

  config = lib.mkIf config.myconfig.roles.work.enable {
    environment.systemPackages = with pkgs; [
      openvpn networkmanager_openvpn
      rdesktop
      # citrix_receiver
      openjdk unstable.maven thrift gradle
      libreoffice
    ];
  };
}
