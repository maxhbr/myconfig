{ config, pkgs, ... }:

{
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    memtest86.enable = true;
    # splashImage = ../../static/grub-splashImage.png;
  };
}
