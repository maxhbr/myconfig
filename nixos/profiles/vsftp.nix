{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [ 9136 ];
  # networking.firewall.allowedUDPPorts = [ 9136 ];
  services.vsftpd = {
    enable = true;
    userlist = [ "mhuber" ];
    userlistEnable = true;
    localUsers = true;
    extraConfig = ''
      listen_port=9136
    '';
  };
}
