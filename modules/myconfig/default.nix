{ config, lib, pkgs, ... }:
let
  cfg = config.myconfig;
  user = cfg.user;
in {
  imports = [
    ./myconfig.desktop.nix
    ./myconfig.headless.nix
    ./myconfig.virtualisation.nix
    ./myconfig.imagework.nix
    ./myconfig.cad.nix
  ];
}
