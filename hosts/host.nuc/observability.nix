# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ ... }:
{
  myconfig.observability = {
    host.enable = true;
    host.uptime.enable = true;
    client.enable = true;

    # TFA Dostmann AirCO2NTROL Mini (31.5006.02) is plugged into USB
    # on this host (VID:PID 04d9:a052). Exposes `air_co2` (ppm) and
    # `air_temp` (°C); the central Grafana dashboard is provisioned
    # via host.co2.nix.
    client.co2Exporter.enable = true;
  };
}
