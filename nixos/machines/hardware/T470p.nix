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
}
