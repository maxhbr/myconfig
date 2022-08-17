# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let
  resetXrandr = with pkgs;
    writeShellScriptBin "resetXrandr" ''
      sleep 1
      ${systemd}/bin/systemctl --user start redshift
      ${xorg.xrandr}/bin/xrandr --output DP-2 --brightness 1
    '';
  xrandrUnpan = with pkgs;
    writeShellScriptBin "xrandrUnpan" ''
      set -ex
      ${xorg.xrandr}/bin/xrandr $(${xorg.xrandr}/bin/xrandr --listmonitors |
          grep '^ ' |
          sed 's%/[0-9]*%%g' |
          awk '{print "--output " $4 " --panning " $3 "/tracking:" $3 "/border:0/0/0/0"}')
    '';
  setupWacom = with pkgs;
    writeShellScriptBin "setupWacom" ''
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
  setupHuion = with pkgs;
    writeShellScriptBin "setupHuion" ''
      set -e
      sleep 1
      id=$(${xorg.xinput}/bin/xinput --list --id-only "Tablet Monitor Pen (0)" || true)

      if [[ "$1" == "" ]]; then
          outputs=$(${xorg.xrandr}/bin/xrandr --listmonitors |
                      `# resolution is 1920x1080` grep 'x1080/' |
                      `# DP-2 is the notebook screen` grep -v 'DP-2 ' |
                      `# eDP-1-1 is the notebook screen` grep -v 'eDP-1-1 ' |
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

  myautorandr = let
    autosetup = pkgs.writeShellScriptBin "autosetup" ''
      exec ${pkgs.autorandr}/bin/autorandr --skip-options gamma,panning --change
    '';
    mobile = pkgs.writeShellScriptBin "mobile" ''
      exec ${pkgs.autorandr}/bin/autorandr mobile
    '';
    mautosetup = pkgs.writeShellScriptBin "mautosetup" ''
      ${mobile}/bin/mobile
      exec ${autosetup}/bin/autosetup
    '';

  in pkgs.symlinkJoin {
    name = "myautorandr";
    paths = [ autosetup mobile mautosetup pkgs.autorandr ];
  };

in {
  config = (lib.mkIf config.services.xserver.enable {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        xrandrUnpan
        myautorandr
        setupWacom
        setupHuion
      ];
      home.file = {
        ".config/autorandr/postswitch.d/resetXrandr".source =
          "${resetXrandr}/bin/resetXrandr";
        ".config/autorandr/postswitch.d/setupWacom".source =
          "${setupWacom}/bin/setupWacom";
        ".config/autorandr/postswitch.d/setupHuion".source =
          "${setupHuion}/bin/setupHuion";
      };
    }];
    services.autorandr.enable = true;
  });
}
