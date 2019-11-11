# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  extraHosts = "${builtins.readFile ./extrahosts}";
in {
  config = {
    networking.extraHosts = "${extraHosts}";
  };
}
