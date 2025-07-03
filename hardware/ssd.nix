# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  imports = [ ./nixos-hardware/common/pc/ssd ];

  fileSystems."/".options = [
    "noatime"
    "nodiratime"
    "discard"
  ];
}
