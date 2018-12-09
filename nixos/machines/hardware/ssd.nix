# Copyright 2016 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, pkgs, ... }:

{
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
}
