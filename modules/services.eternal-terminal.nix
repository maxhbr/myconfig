{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
let
  wg0Enabled =
    lib.attrsets.hasAttrByPath [ "networking" "wireguard" "interfaces" "wg0" "ip4" ] config
    && config.networking.wireguard.interfaces.wg0.ip4 != [ ];
  port = 22022;
in
{
  config = lib.mkIf config.services.eternal-terminal.enable {
    services.eternal-terminal.port = port;
    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals wg0Enabled [ port ];
    networking.firewall.interfaces."wg0".allowedUDPPorts = lib.optionals wg0Enabled [ port ];
  };
}
