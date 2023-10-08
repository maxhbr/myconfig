# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig;
    mkSharescreenBin = bin: args: with pkgs; writeShellScriptBin bin ''
set -euo pipefail

getDevice() {                                                                   
  # get virtual device                                                          
  ${v4l-utils}/bin/v4l2-ctl  --list-devices | grep 'Virtual Camera' -A 1 | grep /dev/ | sed 's%\s%%g'
}

device="$(getDevice)"
exec ${wf-recorder}/bin/wf-recorder ${args} --muxer=v4l2 --file=$device -c rawvideo -x yuyv422
'';
in {
  config = (lib.mkIf cfg.desktop.wayland.enable {
    myconfig.v4l2.enable = true;
    home-manager.sharedModules = [{
      home.packages = [ 
        (mkSharescreenBin "sharescreen" "")
        (mkSharescreenBin "sharescreenarea" "-g \"$(${pkgs.slurp}/bin/slurp)\"")
      ];
    }];
  });
}
