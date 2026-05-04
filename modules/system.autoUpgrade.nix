# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  config,
  lib,
  inputs,
  ...
}:

{
  config = lib.mkIf config.system.autoUpgrade.enable {
    system.autoUpgrade = {
      flake = inputs.self.outPath;
      flags = [ "--print-build-logs" ];
      dates = lib.mkDefault "02:00";
      randomizedDelaySec = lib.mkDefault "45min";
    };
  };
}
