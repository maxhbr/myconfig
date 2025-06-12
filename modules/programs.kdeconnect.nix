{ config, lib, ... }: {
  imports = [{ programs.kdeconnect.enable = lib.mkDefault false; }];
  config = lib.mkIf config.programs.kdeconnect.enable {
    networking.firewall = {
      allowedTCPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
      allowedUDPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
    };
    home-manager.sharedModules =
      [{ myconfig.persistence.cache-directories = [ ".config/kdeconnect/" ]; }];
  };
}
