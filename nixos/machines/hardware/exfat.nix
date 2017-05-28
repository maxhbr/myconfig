{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    exfat
    # linuxPackages.exfat-nofuse
  ];
}
