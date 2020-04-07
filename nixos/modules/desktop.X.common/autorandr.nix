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
      xrandr=$(${xorg.xrandr}/bin/xrandr --listmonitors)
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
  setupHuion = with pkgs; writeScriptBin "setupHuion" ''
    #!${stdenv.shell}
    set -e
    sleep 1
    id=$(${xorg.xinput}/bin/xinput --list --id-only "Tablet Monitor Pen Pen (0)" || true)

    if [[ "$1" == "" ]]; then
        outputs=$(${xorg.xrandr}/bin/xrandr --listmonitors |
                    `# resolution is 1920x1080` grep 'x1080/' |
                    `# DP-2 is the notebook screen` grep -v 'DP-2 ' |
                    awk '{print $NF}')
        nOutputs="$(echo "$outputs" | wc -l)"
    else
        outputs="$1"
        nOutputs=1
    fi

    if [[ "$id" == "" || "$nOutputs" -eq 0 ]]; then
        echo "not connected"
        exit 0
    elif [[ "$nOutputs" -gt 1 ]]; then
        echo "could not identify output"
        echo "options are"
        echo "$outputs"
        exit 0
    fi

    set -x
    ${xorg.xinput}/bin/xinput map-to-output $id $outputs
  '';

in {
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        autorandr
        setupWacom setupHuion
      ];
      home.file = {
        ".config/autorandr/postswitch.d/resetXrandr".source = "${resetXrandr}/bin/resetXrandr";
        ".config/autorandr/postswitch.d/setupWacom".source = "${setupWacom}/bin/setupWacom";
        ".config/autorandr/postswitch.d/setupHuion".source = "${setupWacom}/bin/setupHuion";
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
