# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  lib = pkgs.lib;
  cfg = config.myconfig;
in
{
  options = {
    myconfig.wifi = {
      backend = pkgs.lib.mkOption {
        type = pkgs.lib.types.enum [
          "iwd"
          "wpa_supplicant"
          "none"
        ];
        default = "iwd";
        description = ''
          Backend for managing WiFi connections.
        '';
      };
    };
  };
  imports = [
    # ./extrahosts
    ./service.stubby.nix
    ./service.dhcpcd-optimization.nix
    ./helpers.nix
    (
      {
        pkgs,
        config,
        lib,
        ...
      }:
      {
        config = lib.mkIf (config.networking.networkmanager.enable) {
          networking = {
            networkmanager = {
              plugins = with pkgs; [
                networkmanager-openvpn
                # networkmanager-openconnect
              ];
            };
          };
          home-manager.sharedModules = [
            {
              home.packages = with pkgs; [
                networkmanagerapplet
                openvpn
                # openconnect
              ];
            }
          ];
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
        config = lib.mkIf (cfg.wifi.backend == "iwd") {
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
        config = lib.mkIf (cfg.wifi.backend == "wpa_supplicant") {
          networking = {
            networkmanager.wifi.backend = "wpa_supplicant";
          };
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

    networking = {
      networkmanager.enable = true;
      firewall = {
        enable = true;
        allowPing = false;
      };
    };
  };
}
