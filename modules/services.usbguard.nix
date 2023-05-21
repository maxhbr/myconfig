{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.services.usbguard.enable {
    services.usbguard = {
      IPCAllowedUsers = ["root" "mhuber"];
      rules = ''
        allow with-interface equals { 08:*:* }
      '';
      presentDevicePolicy = "allow";
    };
  };
}
