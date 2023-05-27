{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.services.usbguard.enable {
    services.usbguard = {
      IPCAllowedUsers = ["root" "mhuber"];
      rules = ''
        allow with-interface equals { 08:*:* }

        # Reject devices with suspicious combination of interfaces
        reject with-interface all-of { 08:*:* 03:00:* }
        reject with-interface all-of { 08:*:* 03:01:* }
        reject with-interface all-of { 08:*:* e0:*:* }
        reject with-interface all-of { 08:*:* 02:*:* }
      '';
      presentDevicePolicy = "allow";
    };
  };
}
