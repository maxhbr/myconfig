# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, myconfig, ... }: {
  imports = [
    # hardware:
    ./hardware-configuration.nix
    ../../hardware/efi.nix
    ../../hardware/hdd-spinndown.nix
    {
      boot.initrd.supportedFilesystems = [ "btrfs" "luks" ];

      services.btrfs.autoScrub = {
        enable = true;
        # fileSystems = [ "/" "/mnt/v0" ];
      };

      # ########################################################################
      # # USB HDDS:
      # fileSystems."/mnt/tng-backup" = {
      #   device = "/dev/disk/by-uuid/480aaf1f-aae0-469e-911e-13f961b46ec3";
      #   fsType = "ext4";
      #   options =
      #     [ "auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime" ];
      # };
      # fileSystems."/mnt/foto-backup" = {
      #   device = "/dev/disk/by-uuid/a11523b0-e4f5-4af6-8551-2b43989c4781";
      #   fsType = "ext4";
      #   options =
      #     [ "auto,nofail,x-systemd.device-timeout=1,users,rw,discard,noatime" ];
      # };
    }
    ./2x4000-hdds.raid.nix
    # configuration
    ./smarthome
    # ./service.nextcloud.nix
    ./service.unifi.nix
    # ./service.grafana
    ./service.pi-hole.nix

    { # nginx
      config = {
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
              }) (lib.unique ([
                config.networking.hostName
                # (with (import ../lib.nix); (getSecretNoNewline "nas" "ip"))
              ] ++ config.services.nextcloud.config.extraTrustedDomains));
            };
          };
        };

        networking.firewall.allowedTCPPorts = [ 443 ];
      };
    }
    { # haproxy
      config = {
        services.haproxy = {
          enable = true;
          config = ''
            defaults
               mode    http
               option  httpchk

            frontend frontend
               bind *:1443 ssl crt /etc/tls/nginx.pem
               redirect scheme https if !{ ssl_fc }

               acl url_prometheus path_beg /prometheus
               # acl url_prometheus hdr(host) -m beg prometheus.
               use_backend prometheus_backend if url_prometheus

               acl url_deconz path_beg /deconz
               # acl url_deconz hdr(host) -m beg deconz.
               use_backend deconz_backend if url_deconz

               # acl url_stats path_beg /stats
               # # acl url_stats hdr(host) -m beg stats.
               # use_backend stats_backend if url_stats

               default_backend grafana_backend

            backend grafana_backend
               option forwardfor
               http-request set-header X-Forwarded-Port %[dst_port]
               http-request add-header X-Forwarded-Proto https if { ssl_fc }
               option httpchk HEAD / HTTP/1.1\r\nHost:localhost
               server grafana01 ${config.services.grafana.addr}:${
                 toString config.services.grafana.port
               } check inter 2000

            backend prometheus_backend
               server prometheus01 ${config.services.prometheus.listenAddress}:${
                 toString config.services.prometheus.port
               } check inter 2000

            backend deconz_backend
               server deconz01 127.0.0.1:${
                 toString config.myconfig.services.deconz.httpPort
               } check inter 2000

            listen stats
                bind *:1936
                stats enable
                stats uri /
                stats hide-version
                stats auth someuser:password
          '';
        };
        networking.firewall.allowedTCPPorts = [ 1443 1936 ];
      };

    }
    (myconfig.metadatalib.setupNixServe [ "workstation" "vserver" ])
    {
      nix.trustedBinaryCaches =
        [ ("ssh://nix-ssh@" + myconfig.metadatalib.get.hosts.workstation.ip4) ];
    }
    (myconfig.metadatalib.setupAsBackupTarget "/mnt/2x4t/backup"
      [ "x1extremeG2" ])
    (myconfig.metadatalib.fixIp "enp3s0")
    {
      system.activationScripts.mkTlsDir =
        "mkdir -p /etc/tls && chmod 777 /etc/tls";
    }
  ];
  config = {
    myconfig = { headless.enable = true; };

    networking.hostName = "nas";
    networking.hostId = "29d93341";

    virtualisation.docker.enable = true;

    services.logind.extraConfig = ''
      HandlePowerKey=reboot
    '';

    services = {
      snapper = {
        snapshotInterval = "hourly";
        cleanupInterval = "1d";
        filters = null;
        configs = {
          home = {
            subvolume = "/home";
            extraConfig = ''
              ALLOW_USERS="${myconfig.user}"
            '';
          };
        };
      };
    };
  };
}
