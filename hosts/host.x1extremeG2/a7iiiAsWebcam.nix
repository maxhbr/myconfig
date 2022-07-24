# Copyright 2022 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs,... }:
# based on https://gist.github.com/TheSirC/93130f70cc280cdcdff89faf8d4e98ab
{
  boot.extraModulePackages = [
    config.boot.kernelPackages.v4l2loopback
  ];
  boot.kernelModules = [
    "v4l2loopback"
  ];
  boot.extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 video_nr=9 card_label=a7III
  '';
  home-manager.sharedModules = [{
    home.packages = with pkgs;
      [
        (writeShellScriptBin "a7iii" ''
set -euo pipefail

function connect-camera() {
  ${pkgs.gphoto2}/bin/gphoto2 --stdout --capture-movie 2> ~/tmp/camera.log |
    ${pkgs.ffmpeg}/bin/ffmpeg -i - -vcodec rawvideo -pix_fmt yuv420p -threads 0 -f v4l2 /dev/video9 > /dev/null 2> ~/tmp/encoding-err.log &
  PID_CAM=$!
}

function main() {
	connect-camera && read -n 1 -s -r -p "Press any key to continue" && kill -KILL $PID_CAM
}

main'')];
  }];
}
