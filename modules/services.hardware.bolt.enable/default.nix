# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
# When the Thunderbolt daemon (bolt) is enabled, install the tb_debug
# helper scripts into the primary user's home-manager profile. They read
# the live TB/USB4/Type-C state (overview), collect debugging evidence
# (collect), or watch hotplug events live (watch); all output files are
# written to ~/_tb_debug/.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:

let
  mkTbDebugScript =
    name: pkgs.writeShellScriptBin "tb_debug_${name}" (lib.fileContents ./${"tb_debug_${name}.sh"});
in
{
  config = lib.mkIf config.services.hardware.bolt.enable {
    home-manager.users."${myconfig.user}" = {
      home.packages = map mkTbDebugScript [
        "collect"
        "overview"
        "watch"
      ];
    };
  };
}
