{ config, lib, pkgs, ... }:

{
  services.unifi = {
    enable = true;

    networking.firewall = {
      allowedTCPPorts = [ 8443 ];
      allowedUDPPorts = [ 8443 ];
    };
  };
}
