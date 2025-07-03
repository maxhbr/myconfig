{ pkgs, ... }:
{
  imports = [
    (
      { config, lib, ... }:
      lib.mkIf config.services.kanboard.enable {
        services.kanboard = {
          nginx = {
            sslCertificate = "/etc/tls/nginx.crt";
            sslCertificateKey = "/etc/tls/nginx.key";
            # listen = {
            #   port = 443;
            #   address = "10.199.199.1";
            #   ssl = true;
            # };
            listenAddresses = [ "10.199.199.1" ];
            # forceSSL = true;
            onlySSL = true;
          };
        };
        networking.firewall.extraStopCommands = ''
          iptables -D nixos-fw -p tcp --source 10.199.199.0/24 --dport 443 -j nixos-fw-accept || true
          iptables -D nixos-fw -p udp --source 10.199.199.0/24 --dport 443 -j nixos-fw-accept || true
        '';
      }
    )
  ];
  config = {
    services.kanboard.enable = false;
  };
}
