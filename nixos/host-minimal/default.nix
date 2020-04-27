# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: let
  importall = import ../lib/helper/importall.nix;
in {
  imports = [
    ../dev.nix
  ] ++ importall ./imports;

  config = {
    networking.hostName = "minimal";
    networking.hostId = "123456";
  };
}
