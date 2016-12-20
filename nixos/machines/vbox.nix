{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/quadroFX4800.nix
    ./hardware/grub.nix

    ## software:
    ../profiles/desktop.nix
    ../profiles/xmonad.nix
    # ../profiles/xfce.nix
    # ../profiles/kde.nix
    # ../profiles/virtualization.nix
    # ../profiles/mail.nix
    # ../profiles/dev.nix
    # ../profiles/work.nix
    # ../profiles/imagework.nix
    # ../profiles/games.nix
    ../profiles/openssh.nix
  ];
  virtualisation.virtualbox.guest.enable = true;
  boot.initrd.checkJournalingFS = false;
  services.xserver.displayManager.auto.enable = true;
  services.xserver.displayManager.auto.user = "mhuber";

  # fileSystems."/virtualboxshare" = {
  #   fsType = "vboxsf";
  #   device = "nameofthesharedfolder";
  #   options = "rw";
  # };

  # boot.initrd.availableKernelModules = [ "ata_piix" "ohci_hcd" "ehci_hcd" ];
  # security.sudo = {
  #   enable = true;
  #   security.sudo.configFile = ''
  #     Defaults:root,%wheel env_keep+=LOCALE_ARCHIVE
  #     Defaults:root,%wheel env_keep+=NIX_PATH
  #     Defaults:root,%wheel env_keep+=TERMINFO_DIRS
  #     root        ALL=(ALL) SETENV: ALL
  #     %wheel      ALL=(ALL) NOPASSWD: SETENV: ALL
  #   '';
#   # };
# }
