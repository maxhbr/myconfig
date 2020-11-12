# see:
# https://discourse.nixos.org/t/running-evolution-without-gnome-is-it-sane-possible/8328
# https://github.com/NixOS/nixpkgs/issues/12756
# https://github.com/NixOS/nixpkgs/pull/17926/files
{ config, lib, pkgs, ... }:
let user = config.myconfig.user;
in {
  home-manager.users."${user}" = {
    home.packages = with pkgs; [ gnome3.evolution ];
  };
  services.gnome3.evolution-data-server.enable = true;
  services.gnome3.gnome-keyring.enable = true;
  programs.dconf.enable = true;
  programs.seahorse.enable = true;
}
