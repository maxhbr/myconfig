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
    myconfig.deployedServices = {
      services = {
        # vserver = [
        #   {
        #     name = "litellm";
        #     ip = "litellm.thing.wg0.maxhbr.local";
        #     port = 80;
        #     forceHttps = false;
        #   }
        # ];
        nuc = [
          (rec {
            name = "hass";
            port = 8123;
            redirect = "http://${name}.nuc.wg0.maxhbr.local:${toString port}";
          })
          {
            # Zigbee2MQTT frontend (replaced deCONZ/Phoscon, formerly :8124)
            name = "zigbee2mqtt";
            port = 8080;
          }
          {
            name = "node-red";
            port = 1880;
          }
        ];
        thing = [
          {
            name = "forgejo";
            port = 3000;
          }
          {
            name = "gitolite";
          }
          {
            name = "n8n";
            port = 5678;
          }
          {
            name = "rtx5090";
            port = 33656;
            forceHttps = false;
          }
          {
            name = "gfx1151";
            port = 33657;
            forceHttps = false;
          }
          {
            name = "litellm";
            port = 4000;
            forceHttps = false;
          }
          {
            name = "ollama";
            port = 11434;
            forceHttps = false;
          }
          # {
          #   name = "open-webui";
          #   port = 8888;
          # }
          {
            name = "comfyui";
            port = 8188;
            excludeFromMonitoring = true;
          }
          {
            name = "qdrant";
            port = 6333;
            forceHttps = false;
            excludeFromMonitoring = true;
          }
          # {
          #   name = "headroom";
          #   port = 8787;
          #   forceHttps = false;
          # }
        ];
        p14 = [
          # {
          #   name = "open-webui";
          #   port = 8888;
          # }
          # {
          #   name = "searxng";
          #   port = 28080;
          # }
          # {
          #   name = "searxng-plain";
          #   port = 18080;
          # }
        ];
      };
      center = "vserver";
    };
  };
}
