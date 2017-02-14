{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/T450s.nix
    ./hardware/efi.nix

    ## software:
    ../profiles/desktop
    ../profiles/virtualization
    ../profiles/mail.nix
    ../profiles/dev.nix
    ../profiles/work.nix
    ../profiles/desktop/imagework.nix

    ../profiles/vgrep.nix

    ## others
    # ../profiles/desktop/games.nix
    # ../profiles/vsftp.nix
  ];

  boot.initrd.supportedFilesystems = [ "luks" ];
  boot.initrd.luks.devices = [ {
    device = "/dev/sda2";
    name = "crypted";
    preLVM = true;
    allowDiscards = true;
  } ];
}
