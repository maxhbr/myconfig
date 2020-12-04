# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../hardware/grub.nix
    {
      boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];
      services.btrfs.autoScrub = { enable = true; };
    }
    ../role.headless
    # experimental:
    ./service.grafana_and_promtheus
  ];
  config = {
    networking.hostName = "nuc";
    networking.hostId = "29d93123";

    services = {
      nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;
        virtualHosts."${config.networking.hostName}" = {
          addSSL = true;
          sslCertificate = "/etc/tls/nginx.crt";
          sslCertificateKey = "/etc/tls/nginx.key";
          listen = map (addr: {
            inherit addr;
            port = 443;
            ssl = true;
          }) [ config.networking.hostName ];
        };
      };
    };
    networking.firewall.allowedTCPPorts = [ 443 ];
  };
}
