{ config, pkgs, ... }:

{
  imports = [
    ../profiles/desktop.nix
    ../profiles/virtualization.nix
    ../profiles/mail.nix
    ../profiles/dev.nix
    ../profiles/work.nix
    ../profiles/imagework.nix
    # ../profiles/games.nix
  ];

  powerManagement.enable = true;

  environment.systemPackages = with pkgs; [
    xorg.xbacklight
  ];

  #networking.wireless.enable = true;
  
  hardware = {
    bluetooth.enable = false;
    opengl.driSupport32Bit = true;
  };

  boot = {
    initrd = {
      supportedFilesystems = [ "luks" ];
      luks.devices = [ {
        device = "/dev/sda2";
        name = "crypted";
        preLVM = true;
        allowDiscards = true;
      } ];
    };
  };

  services = {
    logind.extraConfig = "HandleLidSwitch=ignore";
    xserver.synaptics = {
      additionalOptions = ''
        Option "VertScrollDelta" "-100"
        Option "HorizScrollDelta" "-100"
      '';
      buttonsMap = [ 1 3 2 ];
      enable = true;
      tapButtons = false;
      fingersMap = [ 0 0 0 ];
      twoFingerScroll = true;
      vertEdgeScroll = false;
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
