# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  ...
}:
{
  config = lib.mkIf config.system.switch.enable {
    nixpkgs.overlays = [ (import ./overlay.nix) ];
  };
}
