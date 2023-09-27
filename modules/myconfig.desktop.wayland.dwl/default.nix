# Copyright 2023 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  cfg = config.myconfig;
  mydwl = (pkgs.dwl.overrideAttrs (prev: {
          version = "git";
          src = pkgs.fetchFromGitHub {
            owner = "djpohly";
            repo = "dwl";
            rev = "342850487acf4fc7383429786b9cb05a6a4cdf4f";
            hash = "sha256-qvW09Ge1Qt0Yg3K6TOD1msOxuk+pGucepNiCGhfrQwI=";
          };
          enableXWayland = true;
          # conf = ./config.h;
          patches = [
            (pkgs.fetchpatch {
              url = "https://github.com/djpohly/dwl/compare/main...sevz17:vanitygaps.patch";
              hash = "sha256-vLbdlLtBRUvvbccSpANEzgoPpwb5kxwGV0sZp9aZfvg=";
            })
            (pkgs.fetchpatch {
              url = "https://github.com/djpohly/dwl/compare/main...sevz17:autostart.patch";
              hash = "sha256-/vbVW9BJWmbrqajWpY/mUdYRze7yHOeh4CrBuBGpB0I=";
            })
            (pkgs.fetchpatch {
              url = "https://github.com/djpohly/dwl/compare/main...korei999:rotatetags.patch";
              hash = "sha256-SbtOIWodRrL+pnzaJfa3JMolLZpW5pXy2FXoxjMZC7U=";
            })
            (pkgs.fetchpatch {
              url = "https://github.com/djpohly/dwl/compare/main...NikitaIvanovV:centeredmaster.patch";
              hash = "sha256-k3pHs5E+MyagqvDGXi+HZYiKTG/WJlgqJqes7PN3uNs=";
            })
            # (pkgs.fetchpatch {
            #   url = "https://github.com/djpohly/dwl/compare/main...juliag2:alphafocus.patch";
            #   hash = "sha256-RXkA5jdDaHPKVlWgkiedFBpVXZBkuH66qMAlC6Eb+ug=";
            # })
          ];
      })).override { conf = ./config.h; };
  overlay = (_: super: {
        inherit mydwl;
        mydwl-start = pkgs.writeShellScriptBin "mydwl-start" ''
#!/usr/bin/env bash
exec ${mydwl}/bin/dwl -s ${super.somebar}/bin/somebar
'';
    });
in {
  options.myconfig = with lib; {
    desktop.wayland.dwl = { enable = mkEnableOption "dwl"; };
  };
  config =
    (lib.mkIf (cfg.desktop.wayland.enable && cfg.desktop.wayland.dwl.enable) {
      nixpkgs.overlays = [ overlay ];
      home-manager.sharedModules = [{ home.packages = with pkgs; [ mydwl somebar mydwl-start ]; }];
      services.xserver.windowManager.session = lib.singleton {
        name = "dwl";
        start = "${pkgs.mydwl-start}/bin/mydwl-start";
      };
    });
}
