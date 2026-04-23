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

  # openWebuiPort =
  #   if config.myconfig.ai.container.open-webui.enable then
  #     config.myconfig.ai.container.open-webui.port
  #   else
  #     config.myconfig.ai.open-webui.port;

in
{
  config = {
    # services.searx.settings.server.base_url = lib.mkForce "${hostName}/searx/";
    # services.n8n.environment = {
    #   WEBHOOK_URL = "https://${hostName}/";
    #   N8N_PROXY_HOPS = 1;
    # };
    # containers.n8n.config.services.n8n.environment = {
    #   WEBHOOK_URL = "https://${hostName}/";
    #   N8N_PROXY_HOPS = 1;
    # };

    myconfig.deployedServices.configureCaddy = true;
    services.caddy = {
      enable = true;
    };

    networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
      443
    ];
  };
}
