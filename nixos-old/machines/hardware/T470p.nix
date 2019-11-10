# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:

#
#
# Hardware:
#   Thinkpad T470p
#
#

{
  imports = [
    ./notebook-generic.nix
    ./ssd.nix
    ./highres.nix
    ./wwan.nix
    ./nixos-hardware/common/cpu/intel
    ./nixos-hardware/common/pc/laptop
    ./nixos-hardware/common/pc/laptop/acpi_call.nix

    # ./bumblebee.nix
    # ./intel-graphics.nix
    ./optimus.nix

    { # config for libinput
      config = lib.mkIf (config.services.xserver.libinput.enable) {
        services.xserver.libinput.accelSpeed = "0.15";
      };
    }
    { # config for synaptics (unused?)
      config = lib.mkIf (config.services.xserver.synaptics.enable) {
        services.xserver.synaptics = {
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
  ];

  nix.buildCores = 8;


  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
}
