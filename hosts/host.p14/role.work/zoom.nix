# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
let
  zoom-us-overlay = (self: super: {
    zoom-us = self.nixos-2411.zoom-us.overrideAttrs (old: {
      postFixup = old.postFixup + ''
        wrapProgram $out/bin/zoom-us \
          --set QT_DEBUG_PLUGINS 1 \
          --set XDG_CURRENT_DESKTOP gnome
      '';
    });
  });
  zoom-us = pkgs.zoom-us;
  mk-zoom-auto = name: zoom-cmd: (pkgs.writeShellScriptBin name ''
    set -euo pipefail

    function convert_zoom_link_to_zoommtg() {
        local zoom_link="$1"
        local confno=$(echo "$zoom_link" | sed -n 's/.*\/j\/\([0-9]*\).*/\1/p')
        if [[ "$zoom_link" =~ .*pwd=.* ]]; then
            local pwd=$(echo "$zoom_link" | sed -n 's/.*pwd=\(.*\)/\1/p')
            echo "zoommtg://zoom.us/join?action=join&confno=$confno&pwd=$pwd"
        else
            echo "zoommtg://zoom.us/join?action=join&confno=$confno"
        fi
    }

    function open_zoom_link_in_zoom() {
        local zoom_link="$1"
        local zoommtg_link=$(convert_zoom_link_to_zoommtg "$zoom_link")
        echo "... opening zoom link in zoom ..."
        exec ${zoom-cmd} "$zoommtg_link"
        exit 0
    }

    function test_for_zoom_link() {
        local args="$@"
        # bash check that args has no whitespace
        if [[ "$args" =~ ^https://.*zoom.us.*$ && ! "$args" =~ .*[[:space:]].* ]]; then
            echo "... starting zoom ..."
            open_zoom_link_in_zoom "$args"
        else
            echo "... no zoom link found"
        fi
    }


    function join_from_confno_and_pwd() {
        local confno="$1"
        local pwd="''${2:-}"
        local url='zoommtg://zoom.us/join?action=join&confno='"$confno"
        if [[ ! -z "$pwd" ]]; then
          local url="$url"'&pwd='"$pwd"
        fi
        open_zoom_link_in_zoom "$url"
    }


    if [[ $# -ge 2 ]]; then
        join_from_confno_and_pwd "$@"
    elif [[ $# -gt 0 ]]; then
        test_for_zoom_link "$@"
        exit 0
    else
      echo "testing for secondary clipboard ..."
      test_for_zoom_link "$(${pkgs.wl-clipboard}/bin/wl-paste || true)"

      echo "testing for primary clipboard ..."
      test_for_zoom_link "$(${pkgs.wl-clipboard}/bin/wl-paste -p || true)"

      echo "zoom-auto did not match anything, just running zoom:"
      exec ${zoom-cmd}
    fi
  '');
  zoom-auto = mk-zoom-auto "zoom-auto" "${zoom-us}/bin/zoom-us";
  zoom-auto-master = mk-zoom-auto "zoom-auto-master" "${pkgs.master.zoom-us}/bin/zoom-us";
  # zoom-auto-wl = mk-zoom-auto "zoom-auto-wl" "${pkgs.cage}/bin/cage -- ${zoom-us}/bin/zoom-us"; # does not work
in {
  config = {
    nixpkgs.overlays = [zoom-us-overlay];
    home-manager.sharedModules = [{
      home.packages = [ 
        zoom-us
        zoom-auto
        zoom-auto-master
      ];
      xdg.mimeApps = {
        defaultApplications."x-scheme-handler/zoommtg" =
          [ "us.zoom.Zoom.desktop" ];
      };
    }];
  };
}
