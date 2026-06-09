# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ config, lib, ... }:
{
  myconfig.observability = {
    host.enable = true;
    host.uptime.enable = true;
    host.grafana.allowAnonymous = true;
    client.enable = true;

    # TFA Dostmann AirCO2NTROL Mini (31.5006.02) is plugged into USB
    # on this host (VID:PID 04d9:a052). Exposes `air_co2` (ppm) and
    # `air_temp` (°C); the central Grafana dashboard is provisioned
    # via host.co2.nix.
    client.co2Exporter.enable = true;

    # Outdoor weather metrics for Augsburg, fetched from Open-Meteo
    # every 10 minutes and emitted via the node_exporter textfile
    # collector. Central Grafana dashboard provisioned via
    # host.weather.nix (uid `myconfig-weather`).
    client.weatherExporter = {
      enable = true;
      latitude = "48.3705";
      longitude = "10.8978";
      locationLabel = "Augsburg";
    };

    # UniFi controller monitoring via unpoller (Prometheus mode) →
    # vmagent → VictoriaMetrics. The local read-only UniFi admin
    # (username `unpoller`) is created manually in the controller
    # UI at https://192.168.1.1/ — see host.unifi.nix for the full
    # procedure. The password lives in `../priv/` as an agenix
    # secret named `unifi-unpoller-password`; until that overlay
    # provides a source for the secret stub declared below, the
    # exporter will start with the placeholder password and fail
    # to log in (visible in `journalctl -u prometheus-unpoller-exporter`).
    host.unifi = {
      enable = true;
      passwordFile = lib.mkIf (config.age.secrets ? unifi-unpoller-password) (
        config.age.secrets.unifi-unpoller-password.path
      );
    };
  };

  # Stub agenix secret declaration. Real `source = ...` is provided
  # by the private overlay (`priv/hosts/host.nuc/`). When source is
  # absent, `myconfig.secrets.nix` filters this entry out and
  # `config.age.secrets.unifi-unpoller-password` does not exist —
  # the `mkIf` guard above keeps the module evaluable in that state.
  myconfig.secrets."unifi-unpoller-password" = { };
}
