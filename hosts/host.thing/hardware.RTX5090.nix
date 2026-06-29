# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    {
      myconfig.observability = {
        client = {
          enableDcgmExporter = true;
          dcgmExporterUseContainer = true;
        };
      };
    }
    ./nvidia.dcgm-exporter.nix
    ../../hardware/RTX5090.nix
    ../../hardware/eGPU.nix
  ];
  config = {
  };
}
