{ config, lib, pkgs, ... }:

{
  imports = [
    # base configuration, valid for every machine
    ./base
    # custom services
    ./service.deconz.nix
  ];
}
