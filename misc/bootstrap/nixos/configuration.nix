{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./guest.nix
    ./users.nix
    ./vagrant.nix
    ./mhuber.nix
  ];
  environment.systemPackages = with pkgs; [
    git git-lfs
    vim
    nixops
  ];
}
