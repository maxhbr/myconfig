{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wineFull
    winetricks
  ];
}
