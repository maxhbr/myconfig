{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
let
  # Both wireguard modules in this repo render a peer as `ips`, never as
  # `ip4`, so the former `ip4` test never matched and the port stayed
  # closed on every wg0 host — `et` was unreachable even though the server
  # listened. `ips` is the option the modules actually set.
  wg0Enabled =
    (lib.attrsets.hasAttrByPath [ "networking" "wireguard" "interfaces" "wg0" "ips" ] config)
    && config.networking.wireguard.interfaces.wg0.ips != [ ];
  port = 22022;
in
{
  config = lib.mkIf config.services.eternal-terminal.enable {
    services.eternal-terminal.port = port;
    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals wg0Enabled [ port ];
    networking.firewall.interfaces."wg0".allowedUDPPorts = lib.optionals wg0Enabled [ port ];
  };
}
