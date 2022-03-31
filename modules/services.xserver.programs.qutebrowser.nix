{ config, lib, pkgs, ... }:
{
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [{
      home.packages = [ pkgs.python3Packages.adblock ];
      programs.qutebrowser = {
        enable = true;
        searchEngines = {
          g = "https://www.google.com/search?hl=en&q={}";
          nw = "https://nixos.wiki/index.php?search={}";
          aw = "https://wiki.archlinux.org/?search={}";
        };
      };
    }];
  });
}
