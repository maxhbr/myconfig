# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

{
  pkgs,
  config,
  lib,
  myconfig,
  ...
}:
let
  cfg = config.myconfig.wg0.services.dnsmasq;
  wgHosts = myconfig.metadatalib.getOtherWgHosts config.networking.hostName;
  otherAddresses = lib.map (wgHost: "/.${wgHost.name}.wg0.maxhbr.local/${wgHost.ip4}") wgHosts;
in
{
  options.myconfig.wg0.services.dnsmasq = {
    enable = lib.mkEnableOption "dnsmasq service for WireGuard";
    domain = lib.mkOption {
      type = lib.types.str;
      default = "maxhbr.local";
      description = "DNS domain for WireGuard network";
    };
    wgNetwork = lib.mkOption {
      type = lib.types.str;
      default = "10.199.199.0/24";
      description = "WireGuard network CIDR";
    };
    serverIp = lib.mkOption {
      type = lib.types.str;
      default = "10.199.199.1";
      description = "IP address of the WireGuard server";
    };
    dhcpRange = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "10.199.199.2,10.199.199.254,12h" ];
      description = "DHCP range configuration";
    };
  };

  config = lib.mkIf cfg.enable {
    services.dnsmasq.enable = true;
    services.dnsmasq.settings = {
      domain = cfg.domain;
      listen-address = [
        "127.0.0.1"
        cfg.serverIp
      ];
      dhcp-range = cfg.dhcpRange;
      dhcp-leasefile = "/var/lib/dnsmasq/dnsmasq.leases";
      address = [ "/.${cfg.domain}/${cfg.serverIp}" ] ++ otherAddresses;
    };
  };
}
