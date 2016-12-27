{ config, pkgs, ... }:

{
  imports = [
    ## hardware:
    ./hardware/grub.nix
    ./hardware/quadroFX4800.nix
    ./hardware/steamcontroller.nix

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
