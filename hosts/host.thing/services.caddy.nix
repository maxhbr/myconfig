# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:

let
  hostName = "${config.networking.hostName}.wg0.maxhbr.local";
in
{
  config = {
    myconfig.deployedServices.configureCaddy = true;
    services.caddy = {
      enable = true;
    };
  };
}
