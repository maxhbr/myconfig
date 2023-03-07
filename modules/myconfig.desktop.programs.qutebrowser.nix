{ config, lib, pkgs, ... }: {
  config = lib.mkIf config.myconfig.desktop.enable {
    home-manager.sharedModules = [({config, ...}: lib.mkIf config.programs.qutebrowser.enable{
      programs.qutebrowser = {
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
          "!s" =
            "https://sourcegraph.com/search?patternType=standard&sm=1&q=context:global+{}";
          "!gist" = "https://gist.github.com/search?q={}";
          "!aw" = "https://wiki.archlinux.org/?search={}";
        };
      };
    })];
  };
}
