{ config, pkgs, ... }:

# Hardware:
#   GeForce Quadro FX 4800

{
  #services.xserver.videoDrivers = ["nvidiaLegacy340"];
  services.xserver.videoDrivers = ["nouveau"];
}
