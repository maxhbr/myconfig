{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wineStaging
    winetricks
  ];
}
