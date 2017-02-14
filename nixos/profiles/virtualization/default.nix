{ config, pkgs, lib, ... }:

{
  imports = [
    ./docker.nix
    ./vbox.nix
  ];
}
