{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [
    htop iftop iptraf-ng iotop
    mkpasswd
    usbutils
  ];
}
