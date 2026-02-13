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
  ai-tmux-session = "ai";
  ai-tmux-session-script = pkgs.writeShellScriptBin "ai-tmux-session" ''
    # if session is not yet created, create it
    if ! tmux has-session -t ${ai-tmux-session}; then
      tmux new-session -d -s ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "btop" C-m
      tmux split-window -h -t ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "nvtop -i" C-m
      tmux split-window -v -t ${ai-tmux-session}
      tmux send-keys -t ${ai-tmux-session}:1 "journalctl -f" C-m
      tmux split-window -v -t ${ai-tmux-session}
    fi
    exec tmux attach-session -t ${ai-tmux-session}
  '';
in
{
  imports = [
    ({
      config =
        lib.mkIf (config.myconfig.ai.container.open-webui.enable || config.myconfig.ai.open-webui.enable)
          (
            let
              port =
                if config.myconfig.ai.container.open-webui.enable then
                  config.myconfig.ai.container.open-webui.port
                else
                  config.myconfig.ai.open-webui.port;
            in
            {
              services.caddy = {
                enable = true;
                virtualHosts."${config.networking.hostName}.wg0.maxhbr.local" = {
                  listenAddresses = [ (myconfig.metadatalib.getWgIp "${config.networking.hostName}") ];
                  hostName = "${config.networking.hostName}.wg0.maxhbr.local";
                  serverAliases = [
                    "${config.networking.hostName}.wg0"
                    (myconfig.metadatalib.getWgIp "${config.networking.hostName}")
                  ];
                  extraConfig = ''
                    reverse_proxy http://localhost:${toString port}
                  '';
                };
              };

              networking.firewall.interfaces."wg0".allowedTCPPorts = lib.optionals config.services.caddy.enable [
                443
              ];
            }
          );
    })
    ({
      config = lib.mkIf (config.myconfig.hardware.gpu.variant == "amd") {
        services.ollama = {
          environmentVariables = {
            HSA_OVERRIDE_GFX_VERSION = "11.5.1";
            HIP_VISIBLE_DEVICES = "1";
            HCC_AMDGPU_TARGET = "gfx1151";
            HSA_ENABLE_SDMA = "1";
          };
          rocmOverrideGfx = "11.5.1";
        };
      };
    })
    (
      {...}: let
        config_json = {
          "$schema" = "https://opencode.ai/config.json";
          "autoupdate" = false;
          "share" = "disabled";
          "permission" = {
            "bash" = "ask";
            "edit" = "ask";
          };
          "provider" = {
            "litellm" = {
              "npm" = "@ai-sdk/openai-compatible";
              "name" = "LiteLLM";
              "options" = {
                "baseURL" = "http://${config.services.litellm.host}:${config.services.litellm.port}/v1"
              };
              "models" = {
                "GLM-4-Flash" = {
                  "name" = "GLM-4-Flash"
                };
              };
            };
          };
          "disabled_providers" = [
            "opencode";
          ];
          "mcp" = {
            "mcp-nixos" = {
              "type" = "local";
              "command" = [ "${lib.getExe pkgs.mcp-nixos}" ];
              "enabled" = true;
            };
            "context7" = {
              "type" = "remote";
              "url" = "https://mcp.context7.com/mcp";
            };
          };
        };
      in {
        myconfig.ai.opencode.enable = true;
        home-manager.sharedModules = [{
          xdg.configFile = {
            "opencode/opencode.json".text = builtins.toJSON config_json;
          };
        }];
      }
    )
  ];

  config = {
    # nixpkgs.config.rocmSupport = false;

    services.litellm = {
      enable = true;
      port = 4000;
      settings.model_list = [
        {
          "model_name" = "GLM-4-Flash";
          "litellm_params" = {
            model = "openai/glm-4"; # Prefix with 'openai/' to use the compatible handler
            api_base = "http://127.0.0.1:22545/v1";
            api_key = "not-needed";
          };
        }
      ];
    };

    myconfig = {
      # services.dmesgMonitor = {
      #   enable = true;
      #   errorSubstrings = [
      #     "Check failed: GPU lost from the bus"
      #     "Assertion failed: status == NV_OK"
      #   ];
      # };
      ai = {
        enable = true;
        # acceleration.vulkan.enable = true;
        inference-cpp = {
          enable = true;
        };
        lmstudio = {
          enable = true;
        };
        alpaca = {
          enable = false;
        };
        open-webui = {
          enable = true;
        };
        vllm = {
          enable = true;
        };
        container = {
          nlm-ingestor = {
            enable = false;
          };
          open-webui = {
            enable = false;
          };
          sillytavern = {
            enable = false;
            host = myconfig.metadatalib.getWgIp "${config.networking.hostName}";
            port = 8888;
          };
          kokoro-fastapi = {
            enable = false;
          };
          lobe-chat = {
            enable = false;
            host = myconfig.metadatalib.getWgIp "${config.networking.hostName}";
          };
        };
      };
    };
    services.ollama = {
      enable = true;
      environmentVariables = {
        OLLAMA_FLASH_ATTENTION = "0";
        OLLAMA_ORIGIN = "*";
      };

      openFirewall = false;
      host = "0.0.0.0";

      loadModels = [
        # "cogito:32b"
        # "deepseek-r1:32b"
        # "gemma3:27b"
        "glm-4.7-flash:q8_0"
        "gpt-oss:20b"
        # # "llama3.2:3b"
        # "llava:7b"
        # "llava:34b"
        # # "magistral:24b"
        # # "openthinker:32b"
        # "phi4"
        # "qwen2.5vl:32b"
        # "qwen3:30b"
        # "qwen3:32b"
        # "qwq:32b"
        # "smollm2:1.7b"
      ];
    };
    # services.tabby = {
    #   enable = false;
    #   acceleration = "cuda";
    #   model = "TabbyML/Qwen2.5-Coder-14B";
    # };

    home-manager.sharedModules = [ { home.packages = [ ai-tmux-session-script ]; } ];
  };
}
