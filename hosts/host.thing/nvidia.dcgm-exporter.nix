# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Run the NVIDIA DCGM-Exporter as an OCI (Docker) container on host.thing.
#
# This is equivalent to:
#   docker run -d --gpus all --cap-add SYS_ADMIN --rm -p 9400:9400 \
#     nvcr.io/nvidia/k8s/dcgm-exporter:4.5.3-4.8.2-distroless
#
# The container exposes Prometheus metrics at :9400/metrics which are
# scraped by the observability client (client.dcgm-exporter.nix) via
# vmagent and pushed to the central VictoriaMetrics instance.
#
# See: https://github.com/NVIDIA/dcgm-exporter
{
  config,
  lib,
  pkgs,
  ...
}:
let
  dcgmPort = config.myconfig.observability.client.dcgmExporterPort;
in
{
  config = lib.mkIf config.myconfig.observability.client.enableDcgmExporter {
    # Ensure Docker and the NVIDIA container toolkit are available so
    # the container can access GPUs via CDI (--device nvidia.com/gpu=all).
    virtualisation.docker.enable = lib.mkDefault true;
    hardware.nvidia-container-toolkit.enable = lib.mkDefault true;

    virtualisation.oci-containers.containers.dcgm-exporter = {
      image = "nvcr.io/nvidia/k8s/dcgm-exporter:4.5.3-4.8.2-distroless";

      ports = [
        "127.0.0.1:${toString dcgmPort}:9400"
      ];

      extraOptions = [
        "--device=nvidia.com/gpu=all"
        "--cap-add=SYS_ADMIN"
        "--name=dcgm-exporter"
      ];
    };

    # Make sure the container waits for the NVIDIA CDI generator.
    systemd.services."${config.virtualisation.oci-containers.backend}-dcgm-exporter" = {
      wants = [
        "nvidia-container-toolkit-cdi-generator.service"
      ];
      after = [
        "nvidia-container-toolkit-cdi-generator.service"
      ];
    };
  };
}
