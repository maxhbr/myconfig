# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Caddy on workstation: reverse-proxy for the deployedServices catalog
# (currently the `rx5500xt` llama-server instance). Mirrors the setup
# used on host.thing — Caddy serves on the wg0 interface only, and
# port 443 is opened on that interface.
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  config = {
    myconfig.deployedServices.configureCaddy = true;
    services.caddy = {
      enable = true;
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
      443
    ];
  };
}
