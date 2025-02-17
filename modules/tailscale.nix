{ config, pkgs, lib, ... }:
{
  config = lib.mkIf config.services.tailscale.enable {
    environment.systemPackages = [ pkgs.tailscale ];

    networking.firewall = {
      # always allow traffic from your Tailscale network
      trustedInterfaces = [ "tailscale0" ];

      # allow the Tailscale UDP port through the firewall
      allowedUDPPorts = [ config.services.tailscale.port ];
    };
  };
}
