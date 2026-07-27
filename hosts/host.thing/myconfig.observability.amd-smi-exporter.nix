# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Enable the amd_smi_exporter on host.thing so the Strix Halo iGPU
# (Radeon 8060S) metrics are pushed into the observability stack.
#
# Only enabled when the GPU is running ROCm — i.e. the `amd` variant.
# The `amd-no-rocm` variant (e.g. gfx1150 without ROCm support) lacks
# the amdsmi runtime the exporter depends on, so we keep it off there.
{ config, lib, ... }:
{
  config = lib.mkIf (builtins.elem "amd" config.myconfig.hardware.gpu.variant) {
    myconfig.observability.client.amdSmiExporter.enable = true;
  };
}
