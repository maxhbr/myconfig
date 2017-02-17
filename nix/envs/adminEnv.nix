{pkgs}:
with pkgs; let
  name = "adminEnv";
  paths = [
    htop iftop iptraf-ng iotop
    mkpasswd
    usbutils
  ];
in buildEnv { inherit name paths; }
