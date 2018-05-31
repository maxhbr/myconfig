# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
let
  path = ../nix/overlays;
  content = builtins.readDir path;
in map (n: import (path + ("/" + n)))
         (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
           (builtins.attrNames content))
