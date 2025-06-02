{ pkgs, ... }: {
  imports = [
    ({ config, lib, ... }:
      lib.mkIf config.services.youtrack.enable {
        services.youtrack = {
          address = "10.199.199.1";
          generalParameters = [ ];
          environmentalParameters = {
            listen-port = 8080;
            listen-address = "10.199.199.1";
          };
        };
        networking.firewall.extraStopCommands = ''
          iptables -D nixos-fw -p tcp --source 10.199.199.0/24 --dport ${
            toString config.services.youtrack.listen-port
          } -j nixos-fw-accept || true
          iptables -D nixos-fw -p udp --source 10.199.199.0/24 --dport ${
            toString config.services.youtrack.listen-port
          } -j nixos-fw-accept || true
        '';
      })

  ];
  config = { services.youtrack.enable = false; };
}
