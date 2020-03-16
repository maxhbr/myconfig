# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  resetXrandr = with pkgs; writeScriptBin "resetXrandr" ''
    #!${stdenv.shell}
    sleep 1
    ${pkgs.systemd}/bin/systemctl --user start redshift
    ${pkgs.xorg.xrandr}/bin/xrandr --output DP-2 --brightness 1
  '';
  setupWacom = with pkgs; writeScriptBin "setupWacom" ''
    #!${stdenv.shell}
    set -e
    sleep 1
    getOutput() {
      xrandr=$(${pkgs.xorg.xrandr}/bin/xrandr --listmonitors)
      output=$(printf "$xrandr" | grep '+\*' |  cut -d " " -f 4)
      if [[ -z "$output" ]]; then
        output=$(printf "$xrandr" | head -2 | tail -1 | cut -d " " -f 4)
      fi
      echo "$output" | sed 's%/[0-9]*%%g'
    }
    getWacomIds() {
      ${xf86_input_wacom}/bin/xsetwacom --list devices |
       sed 's/.*id: //' |
       sed 's/\t.*//'
    }
    output=$(getOutput)
    getWacomIds |
       while IFS= read -r id; do
         echo "setup wacom $id to $output"
         ${xf86_input_wacom}/bin/xsetwacom set "$id" MapToOutput "$output"
       done
  '';

in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        autorandr
        setupWacom
      ];
      home.file = {
        ".config/autorandr/postswitch.d/resetXrandr".source = "${resetXrandr}/bin/resetXrandr";
        ".config/autorandr/postswitch.d/setupWacom".source = "${setupWacom}/bin/setupWacom";
      };
    };
    environment = {
      shellAliases = {
        autosetup = "${pkgs.autorandr}/bin/autorandr --change";
        mobile = "${pkgs.autorandr}/bin/autorandr mobile";
      };
    };
    services.autorandr.enable = true;
  };
}
