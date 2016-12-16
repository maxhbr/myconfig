{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [ 9136 ];
  services.vsftpd = {
    enable = true;
    userlist = ["mhuber"];
    localUsers = true;
    extraConfig = ''
      listen_port=9136
    '';
  };
}
