# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ../../hardware/Radeon8060S.nix
  ];

  config = {
    boot.kernelParams = [
      "amd_iommu=off"
      "amdgpu.gttsize=131072"
      "ttm.pages_limit=33554432"
    ];
  };
}
