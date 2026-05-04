# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  myconfig,
  pkgs,
  lib,
  ...
}:
{
  config = {
    myconfig.deployedServices.services = {
      nuc = [
        {
          name = "hass";
          port = 8123;
        }
        {
          name = "deconz";
          port = 8124;
        }
        {
          name = "prometheus";
          port = 9090;
        }
        {
          name = "grafana";
          port = 3000;
        }
        {
          name = "node-exporter";
          port = 9100;
        }
      ];
      thing = [
        {
          name = "n8n";
          port = 5678;
        }
        {
          name = "llama-swap";
          port = 33656;
        }
        {
          name = "llama-swap-2";
          port = 33657;
        }
        {
          name = "litellm";
          port = 4000;
        }
        {
          name = "ollama";
          port = 11434;
        }
        {
          name = "open-webui";
          port = 8080;
        }
        {
          name = "comfyui";
          port = 8188;
        }
        {
          name = "searxng";
          port = 8080;
        }
      ];
    };
  };
}
