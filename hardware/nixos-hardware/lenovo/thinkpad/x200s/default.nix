{ config, lib, pkgs, ... }:

{
  imports = [
    ../.
    ../../../common/pc/laptop/hdd
    ../tp-smapi.nix
  ];
}
