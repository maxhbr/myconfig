{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    sundtek
  ];
}
