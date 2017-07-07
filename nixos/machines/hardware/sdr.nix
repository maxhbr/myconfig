{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnuradio-with-packages gqrx hackrf rtl-sdr
  ];
  boot.blacklistedKernelModules = ["dvb_usb_rtl28xxu"];
}
