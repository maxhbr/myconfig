{ config, pkgs, ... }:

{
  imports = [
    ./hardware/grub.nix
    ./hardware/quadroFX4800.nix
    ./hardware/steamcontroller.nix
  ];
  myconfig.active-roles = [
    "desktop" "xmonad" "xfce" "vnc"
    "mail" "irc"
    "work" "virtualization" "dev"
    "imagework"
    "games" "wine"
    "vsftp" "openssh"
  ];
}
