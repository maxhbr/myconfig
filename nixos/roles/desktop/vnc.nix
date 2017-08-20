{ config, lib, pkgs, ... }:
let
  unstable = (import <unstable> {});
in {
  options = {
    myconfig.roles.vnc = {
      enable = lib.mkEnableOption "VNC desktop environment";
    };
  };

  config =lib.mkIf config.myconfig.roles.vnc.enable {
    environment.systemPackages = with pkgs; [
      x11vnc
    ];
    networking.firewall.allowedUDPPorts = [ 5900 ];
    networking.firewall.allowedTCPPorts = [ 5900 ];
  };
}
