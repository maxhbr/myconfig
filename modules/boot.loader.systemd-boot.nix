{ config, pkgs, lib, ... }:
{
  config = lib.mkIf config.boot.loader.systemd-boot.enable {
    boot.loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        editor = false;
        memtest86.enable = true;
      };
    };
  };
}