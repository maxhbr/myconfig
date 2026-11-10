# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Kanshi wiring for the wayland sessions. Per-host display layouts live in
# `hosts/host.<hostname>/kanshi/*.nix` (one profile per file, written by the
# `kanshi-add-layout` skill or by hand); every `.nix` file in that directory
# is imported here and only appends to `services.kanshi.settings`.
{
  config,
  lib,
  super,
  ...
}:

let
  layoutsDir = toString ./../../hosts + "/host.${super.networking.hostName}/kanshi";
in
{
  imports = lib.optionals (builtins.pathExists layoutsDir) (
    map (n: layoutsDir + "/${n}") (
      lib.filter (lib.hasSuffix ".nix") (builtins.attrNames (builtins.readDir layoutsDir))
    )
  );

  config = lib.mkIf config.services.kanshi.enable {
    # services.kanshi = { systemdTarget = "default.target"; };
  };
}
