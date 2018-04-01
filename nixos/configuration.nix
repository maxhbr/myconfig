# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

let
  root = /home/mhuber/myconfig;
  myconfig = import (root + "/myconfig.nix") { inherit pkgs; };
in import (root + "/nixos") {
  inherit config pkgs lib;
  otherOverlays = myconfig.overlays;
}
