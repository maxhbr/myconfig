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
          name = "llama-swap";
          port = 33656;
          forceHttps = false;
        }
        {
          name = "llama-swap-2";
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
        {
          name = "open-webui";
          port = 8888;
        }
        {
          name = "comfyui";
          port = 8188;
        }
        {
          name = "qdrant";
          port = 6333;
          forceHttps = false;
        }
      ];
      p14 = [
        {
          name = "open-webui";
          port = 8888;
        }
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
  };
}
