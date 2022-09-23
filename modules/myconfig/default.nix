{ config, lib, pkgs, ... }:
{
  imports = [
    ./myconfig.desktop.nix
    ./myconfig.headless.nix
    ./myconfig.virtualisation.nix
    ./myconfig.imagework.nix
    ./myconfig.cad.nix
  ];
}
