# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  lib = pkgs.lib;
  cfg = config.myconfig;
in
{
  options = {
    myconfig.wifi-backend = pkgs.lib.mkOption {
      type = pkgs.lib.types.enum [
        "iwd"
        "wpa_supplicant"
      ];
      default = "iwd";
      description = ''
        Backend for managing WiFi connections.
      '';
    };
  };
  imports = [
    # ./extrahosts
    # ./service.stubby.nix
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      {
        config = lib.mkIf (cfg.wifi-backend == "iwd") {
          networking.networkmanager.wifi.backend = "iwd";
          services.connman.wifi.backend = "iwd";
          networking.wireless.iwd = {
            enable = true;
            settings = {
              IPv6 = {
                Enabled = true;
              };
              Settings = {
                AutoConnect = true;
              };
            };
          };
        };
      }
    )
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      {
        config = lib.mkIf (cfg.wifi-backend == "wpa_supplicant") {
          networking.networkmanager.wifi.backend = "wpa_supplicant";
          services.connman.wifi.backend = "wpa_supplicant";
          home-manager.sharedModules = [
            {
              home.packages = with pkgs; [
                wpa_supplicant
              ];
            }
          ];

          services.dbus.packages = [ pkgs.wpa_supplicant ];
        };

      }
    )
  ];
  config = {
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
    boot.kernel.sysctl."net.ipv6.conf.all.forwarding" = "1";
    environment.interactiveShellInit = ''
      myPorts() { /run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen; }
      killPort() { kill $(${pkgs.lsof}/bin/lsof -t -i:$1); }
    '';

    home-manager.sharedModules = [
      {
        programs.fish = {
          functions = {
            myPorts = "/run/wrappers/bin/sudo ${pkgs.iproute2}/bin/ss -tulpen";
            killPort = "kill $(${pkgs.lsof}/bin/lsof -t -i:$1)";
          };
        };
        home.packages = with pkgs; [
          networkmanagerapplet
          openvpn
          # openconnect
        ];
      }
    ];

    networking = {
      networkmanager = {
        enable = true;
        plugins = with pkgs; [
          networkmanager-openvpn
          # networkmanager-openconnect
        ];
      };
      wireless.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
