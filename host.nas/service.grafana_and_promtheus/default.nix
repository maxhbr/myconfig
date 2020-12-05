# https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20
{ ... }: {
  imports =
    [ ./service.grafana.nix ./service.promtheus.nix ./service.loki.nix ];
}
