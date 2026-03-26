{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    (
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          # Config that should only apply to the default system, not the specialised ones
        };
      }
    )
  ];
  specialisation = {
    wlp-supplicant = {
      inheritParentConfig = true;
      configuration = {
        myconfig.wifi.backend = "wpa_supplicant";
      };
    };
    iwd = {
      inheritParentConfig = true;
      configuration = {
        myconfig.wifi.backend = "iwd";
      };
    };
  };
}
