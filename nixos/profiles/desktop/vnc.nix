{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    x11vnc
  ];
  networking.firewall.allowedUDPPorts = [ 5900 ];
  networking.firewall.allowedTCPPorts = [ 5900 ];
}
