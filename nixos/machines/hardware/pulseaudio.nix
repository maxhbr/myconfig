{ config, pkgs, ... }:

{
  hardware.pulseaudio.enable = true;
  environment.systemPackages = with pkgs; [
    pavucontrol pamix
  ];
  nixpkgs.config.pulseaudio = true;
}
