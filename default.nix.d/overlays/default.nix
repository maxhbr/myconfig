# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:
{
  config = {
    nixpkgs.overlays = let
        path = ./overlays;
        content = builtins.readDir path;
      in map (n: import (path + ("/" + n)))
              (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                (builtins.attrNames content));
  };
}
