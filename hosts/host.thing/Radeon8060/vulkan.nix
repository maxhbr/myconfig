# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, pkgs, ... }:
{
  config = {
    myconfig = {
      hardware.gpu.variant = "amd-no-rocm";
    };
  };
}
