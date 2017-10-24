{ config, pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  environment.systemPackages = with pkgs; [
    pavucontrol pamix
  ];
  nixpkgs.config.pulseaudio = true;
}
