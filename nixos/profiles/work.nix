{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    openvpn networkmanager_openvpn
    ruby
    rdesktop
    openjdk maven thrift
  ];
}
