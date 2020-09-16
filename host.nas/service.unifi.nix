{ config, lib, pkgs, ... }:

{
  config.services.unifi.enable = true;
  imports = [
    (lib.mkIf config.services.unifi.enable {
      networking.firewall = {
        allowedTCPPorts = [ 8443 ];
        allowedUDPPorts = [ 8443 ];
      };
    })
  ];
}
