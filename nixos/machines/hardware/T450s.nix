{ config, pkgs, ... }:

#
#
# Hardware:
#   Thinkpad T450s
#
#

{
  imports = [
    ./notebook-generic.nix
  ];

  services.xserver = {
    videoDrivers = [ "intel" ];
    synaptics = {
      additionalOptions = ''
        Option "VertScrollDelta" "-50"
        Option "HorizScrollDelta" "-50"
        Option "MinSpeed" "1.2"
        Option "AccelFactor" "0.05"
      '';
      buttonsMap = [ 1 3 2 ];
      tapButtons = false;
      fingersMap = [ 0 0 0 ];
    };
  };

#     services.tlp = {
#       enable = true;
#       extraConfig = ''
# MAX_LOST_WORK_SECS_ON_BAT=15

# # Battery charge thresholds (ThinkPad only, tp-smapi or acpi-call kernel module
# # required). Charging starts when the remaining capacity falls below the
# # START_CHARGE_TRESH value and stops when exceeding the STOP_CHARGE_TRESH value.
# # Main / Internal battery (values in %)
# START_CHARGE_THRESH_BAT0=75
# STOP_CHARGE_THRESH_BAT0=90
# # Ultrabay / Slice / Replaceable battery (values in %)
# START_CHARGE_THRESH_BAT1=75
# STOP_CHARGE_THRESH_BAT1=90
#       '';
#     };
}
