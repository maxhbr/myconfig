# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.elixir;
  # elixir =beam.packages.erlangR21.elixir_1_7;
in
{
  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [ { home.packages = with pkgs; [ elixir ]; } ];
  };
}
