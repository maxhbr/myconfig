{ config, pkgs, ... }:

{
  imports = [
    ./hardware/T470p.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix
    ./hardware/pulseaudio.nix
    # ./hardware/sdr.nix
  ];

  myconfig.active-roles = [
   "desktop" "xmonad" "xfce" "vnc"
   "mail" "irc"
   "work" "virtualization" "dev"
   "imagework"
   ];

  boot.initrd.supportedFilesystems = [ "luks" ];
  boot.initrd.luks.devices = [ {
    device = "/dev/disk/by-uuid/fc9ecff5-e0c5-4cff-bb5c-08a745c76e3c";
    name = "crypted";
    preLVM = true;
    allowDiscards = true;
  } ];
}
