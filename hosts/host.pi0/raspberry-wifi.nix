# based on:
# - https://github.com/illegalprime/nixos-on-arm/blob/master/machines/raspberrypi-zerow/default.nix
{ pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    wirelesstools
    wpa_supplicant
    dhcp
  ];

  networking.networkmanager.enable = lib.mkForce false;

  # when wlan0 is not configured dhcpcd takes 30 extra seconds
  # if you ever want to configure wlan0 just enable this again
  networking.dhcpcd.enable = lib.mkDefault false;

  hardware.firmware = with pkgs; [ raspberrypiWirelessFirmware ];
}
