{ config, pkgs, ... }:

#
#
# Hardware:
#   Thinkpad T470p
#
#

{
  imports = [
    ./notebook-generic.nix
    ./highres.nix
    ./wwan.nix
    ./microcodeIntelFix.nix
  ];

  nix.buildCores = 8;

  hardware.bumblebee.enable = true;
  hardware.opengl = {
    driSupport = true;
    driSupport32Bit = true;
  };

  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';

  services.xserver = {
    videoDrivers = [ "intel" ];
    synaptics = {
      minSpeed = "1";
      maxSpeed = "1";
      accelFactor = "0.15";
      # see: https://major.io/2013/08/24/get-a-rock-solid-linux-touchpad-configuration-for-the-lenovo-x1-carbon/
      additionalOptions = ''
        Option "VertScrollDelta" "-50"
        Option "HorizScrollDelta" "-50"

        # accurate tap-to-click!
        Option "FingerLow" "50"
        Option "FingerHigh" "55"

        Option "AccelerationProfile" "2"
        Option "ConstantDeceleration" "4"
      '';
      buttonsMap = [ 1 3 2 ];
      tapButtons = false;
      fingersMap = [ 0 0 0 ];
    };
  };
}
