{ config, pkgs, ... }:

{
  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [
    xorg.xbacklight
    powertop
  ];

  #networking.wireless.enable = true;

  hardware.bluetooth.enable = false;

  services = {
    logind.extraConfig = "HandleLidSwitch=ignore\nHandlePowerKey=suspend";
    xserver.synaptics = {
      enable = true;
      twoFingerScroll = true;
      vertEdgeScroll = false;
    };
  };
}
