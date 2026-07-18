# Copyright 2016-2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = lib.mkIf config.services.input-remapper.enable {
    nixpkgs.overlays = [ (import ./overlay.nix) ];
    services.input-remapper.enableUdevRules = true;
    home-manager.sharedModules = [
      { home.packages = with pkgs; [ input-remapper ]; }
    ];
  };
}
