{pkgs,unstable}:
with pkgs; let
  name = "workEnv";
  paths  = with pkgs; [
    openvpn networkmanager_openvpn
    ruby
    rdesktop
    openjdk maven thrift gradle
  ];
in buildEnv { inherit name paths; }
