# Copyright 2016-2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, lib, ... }:

let
  myconfigPath = /home/mhuber/myconfig;
  myconfig = import myconfigPath { inherit pkgs; };
# in import "${myconfig.nixosSource}" {
in import myconfig.nixosSrc {
  inherit config pkgs lib;
  otherOverlays = myconfig.overlays;
}
