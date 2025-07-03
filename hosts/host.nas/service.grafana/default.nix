# https://christine.website/blog/prometheus-grafana-loki-nixos-2020-11-20
{ ... }:
{
  imports = [
    ./service.grafana.nix
    ./service.promtheus.nix
    ./service.influxdb.nix
    # ./service.loki.nix #  caller=log.go:149 msg="error running loki" err="Must specify compactor config\nerror initialising module: compactor
  ];
}
