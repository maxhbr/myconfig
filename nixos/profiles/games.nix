{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    steam
  ];

  hardware.opengl.driSupport32Bit = true;
}
