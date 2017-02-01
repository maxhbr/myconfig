{ config, pkgs, lib, ... }:

{

  nixpkgs.config = import ../nix/nixpkgs-config.nix;

  imports = [
    ./hardware-configuration.nix
    ./machines
  ];
}
