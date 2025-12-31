{pkgs, lib, config, ...}:
let
  countryCode = "DE";
in
{
  config = {
    # https://community.frame.work/t/any-hope-for-good-speed-on-the-rz717/74655/2
    hardware.wirelessRegulatoryDatabase = true;
    networking.wireless.extraConfig = ''
      country=${countryCode}
    '';
    boot.extraModprobeConfig = ''
      options cfg80211 ieee80211_regdom="${countryCode}"
    '';
    hardware.enableRedistributableFirmware = true;
  };
}