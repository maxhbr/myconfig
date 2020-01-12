{  pkgs, ... }:
{
  imports = [
    ./wine
  ];

  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        steam powder
      ];
    };

    hardware= {
      opengl = {
        driSupport = true;
        driSupport32Bit = true;
      };
      pulseaudio.support32Bit = true;
    };
    networking.firewall = {
      allowedUDPPorts = [ 27031 27036 ];
      allowedTCPPorts = [ 27036 27037 ];
    };
  };
}
