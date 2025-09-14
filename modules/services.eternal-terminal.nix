{
  config,
  pkgs,
  lib,
  myconfig,
  ...
}:
{
  config = lib.mkIf config.services.eternal-terminal.enable {
    services.eternal-terminal = {
      port = 22022;
    };
    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.wireguard.enable [ 22022 ];
    networking.firewall.interfaces."wg0".allowedUDPPorts = lib.optionals config.services.wireguard.enable [ 22022 ];
  };
}