# Copyright 2019-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
{
  config = (
    lib.mkIf config.virtualisation.libvirtd.enable {

    }
  );
}
