{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/quadroFX4800.nix
    ./hardware/grub.nix

    ## software:
    ../profiles/desktop.nix
    ../profiles/xmonad.nix
    ../profiles/xfce.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    ../profiles/imagework.nix
    ../profiles/games.nix
    ../profiles/wine.nix
    ../profiles/openssh.nix
  ];
}
