{ config, lib, pkgs, ... }:

{
  config.services.unifi.enable = true;
  imports = [
    (lib.mkIf config.services.unifi.enable {
      services.unifi = {
        unifiPackage = pkgs.unifi7;
        openFirewall = true;
      };
      networking.firewall.allowedTCPPorts = [ 8443 ];
      users.users.unifi = {
        group = "unifi";
        isSystemUser = true;
      };
      users.groups.unifi = { };
      # networking.firewall = {
      #   allowedTCPPorts = [ 8443 ];
      #   allowedUDPPorts = [ 8443 ];
      # };
      # services.prometheus.exporters.unifi = {
      #   unifiAddress = "localhost:8443";
      #   unifiUsername = "ReadOnlyUser";
      # };
    })
  ];
}
