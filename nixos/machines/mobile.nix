{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/T450s.nix
    ./hardware/efi.nix

    ## software:
    ../profiles/desktop.nix
    ../profiles/xmonad.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    ../profiles/dev.nix
    ../profiles/work.nix
    ../profiles/imagework.nix
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
