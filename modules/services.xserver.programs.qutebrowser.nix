{ config, lib, pkgs, ... }: {
  config =
    (lib.mkIf (config.services.xserver.enable && config.myconfig.desktop.full) {
      home-manager.sharedModules = [{
        home.packages = [ pkgs.python3Packages.adblock ];
        programs.qutebrowser = {
          enable = true;
          searchEngines = {
            "DEFAULT" = "https://google.com/search?hl=en&q={}";
            "!g" = "https://www.google.com/search?hl=en&q={}";
            "!gm" = "https://www.google.com/maps?q={}";
            "!d" = "https://duckduckgo.com/?ia=web&q={}";
            "!nw" = "https://nixos.wiki/index.php?search={}";
            "!np" = "https://github.com/NixOS/nixpkgs/search?q={}";
            "!hp" = "https://github.com/nix-community/home-manager/search?q={}";
            "!a" = "https://www.amazon.com/s?k={}";
            "!gh" = "https://github.com/search?o=desc&q={}&s=stars";
            "!gist" = "https://gist.github.com/search?q={}";
            "!aw" = "https://wiki.archlinux.org/?search={}";
          };
        };
      }];
    });
}
