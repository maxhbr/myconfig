# see:
# https://discourse.nixos.org/t/running-evolution-without-gnome-is-it-sane-possible/8328
# https://github.com/NixOS/nixpkgs/issues/12756
# https://github.com/NixOS/nixpkgs/pull/17926/files
{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  config = lib.mkIf config.programs.evolution.enable {
    programs.dconf.enable = true;
    programs.seahorse.enable = true;
  };
}
