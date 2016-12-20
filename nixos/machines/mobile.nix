{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/T450s.nix

    ## software:
    ../profiles/efi.nix
    #../profiles/grub.nix
    ../profiles/desktop.nix
    ../profiles/xmonad.nix
    # ../profiles/xfce.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    ../profiles/dev.nix
    ../profiles/work.nix
    ../profiles/imagework.nix
    # ../profiles/games.nix
    ../profiles/printing.nix
  ];

  boot = {
    initrd = {
      supportedFilesystems = [ "luks" ];
      luks.devices = [ {
        device = "/dev/sda2";
        name = "crypted";
        preLVM = true;
        allowDiscards = true;
      } ];
    };
  };
}
