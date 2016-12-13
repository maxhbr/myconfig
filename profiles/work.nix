{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    openvpn networkmanager_openvpn maven thrift
    ruby
    rdesktop
    openjdk
    cloc
  ];
}
