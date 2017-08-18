{ config, lib, pkgs, ... }:
{
  options = {
    myconfig.roles.games = {
      enable = lib.mkEnableOption "Games role";
    };
  };

  config = lib.mkIf config.myconfig.roles.games.enable {
    environment.systemPackages = with pkgs; [
      steam
    ];

    hardware.opengl.driSupport32Bit = true;
    networking.firewall.allowedUDPPorts = [ 27031 27036 ];
    networking.firewall.allowedTCPPorts = [ 27036 27037 ];
  };
}
