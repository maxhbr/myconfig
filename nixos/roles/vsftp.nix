{ config, lib, pkgs, ... }:

{
  options = {
    myconfig.roles.vsftp = {
      enable = lib.mkEnableOption "Vsftp role";
    };
  };

  config = lib.mkIf config.myconfig.roles.vsftp.enable {
    networking.firewall.allowedTCPPorts = [ 9136 ];
    networking.firewall.allowedUDPPorts = [ 9136 ];
    services.vsftpd = {
      enable = true;
      userlist = [ "mhuber" ];
      userlistEnable = true;
      localUsers = true;
      extraConfig = ''
        listen_port=9136
      '';
    };
  };
}
