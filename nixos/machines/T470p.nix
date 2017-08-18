{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/T470p.nix
    ./hardware/efi.nix
    ./hardware/exfat.nix
    ./hardware/steamcontroller.nix

    # ./hardware/sdr.nix

    ## software:
    ../profiles/desktop
    ../profiles/desktop/xfce.nix
    # ../profiles/virtualization
    # ../profiles/mail.nix
    # ../profiles/dev.nix
    # # ../profiles/work.nix
    ../profiles/desktop/imagework.nix

    # # ../profiles/desktop/games.nix

    # # ../profiles/vgrep.nix

    # ## others
    # # ../profiles/desktop/vnc.nix
    # # ../profiles/desktop/games.nix
    # # ../profiles/vsftp.nix
    # ../profiles/irc.nix
  ];

  myconfig.active-roles = [ "work" "virtualization" "mail" "dev" "irc" ];

  boot.initrd.supportedFilesystems = [ "luks" ];
  boot.initrd.luks.devices = [ {
    device = "/dev/disk/by-uuid/fc9ecff5-e0c5-4cff-bb5c-08a745c76e3c";
    name = "crypted";
    preLVM = true;
    allowDiscards = true;
  } ];
}
