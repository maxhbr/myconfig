{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/quadroFX4800.nix

    ## software:
    # ../profiles/efi.nix
    ../profiles/grub.nix
    ../profiles/desktop.nix
    ../profiles/xmonad.nix
    ../profiles/xfce.nix
    # ../profiles/kde.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    # ../profiles/dev.nix
    # ../profiles/work.nix
    ../profiles/imagework.nix
    ../profiles/games.nix
    ../profiles/wine.nix
    ../profiles/openssh.nix
  ];
}
