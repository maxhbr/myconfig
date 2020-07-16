{ ... }:
{
  # These two parameters are the important ones to get the
  # camera working. These will be appended to /boot/config.txt.
  boot.loader.raspberryPi.firmwareConfig = ''
    start_x=1
    gpu_mem=256
  '';
  boot.kernelModules = [ "bcm2835-v4l2" ];
}
