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
    # ./containers.vllm-rocm.nix
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
      { ... }:
      let
        opencodeModels = builtins.listToAttrs (
          lib.map (model: {
            name = model.model_name;
            value = {
              "name" = model.model_name;
            };
          }) config.services.litellm.settings.model_list
        );
      in
      {
        myconfig.ai.opencode.enable = true;
        home-manager.sharedModules = [
          {
            programs.opencode.settings = {
              # "$schema" = "https://opencode.ai/config.json";
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
                    "baseURL" = "http://${config.services.litellm.host}:${toString config.services.litellm.port}/v1";
                  };
                  "models" = opencodeModels;
                };
              };
              "disabled_providers" = [
                "opencode"
              ];
            };
          }
        ];
      }
    )
    {
      config = {
        services.postgresql = {
          enable = true;
          port = 5432;
          ensureDatabases = [ "mydatabase" ];
          authentication = pkgs.lib.mkOverride 10 ''
            #type database DBuser origin-address auth-method
            local all      all     trust
            # ipv4
            host  all      all     127.0.0.1/32   trust
            # ipv6
            host  all      all     ::1/128        trust
          '';
          initialScript = pkgs.writeText "backend-initScript" ''
            CREATE ROLE litellm WITH LOGIN PASSWORD 'litellm' CREATEDB;
            CREATE DATABASE litellm;
            GRANT ALL PRIVILEGES ON DATABASE litellm TO litellm;
          '';
        };
      };
    }
  ];

  config = {
    system.nixos.tags = [ config.myconfig.hardware.gpu.variant ];

    services.litellm = {
      enable = true;
      port = 4000;
      # settings.general_settings = {
      #   store_prompts_in_spend_logs = true;
      #   disable_spend_logs = false;
      #   maximum_spend_logs_retention_period = "30d";
      #   database_url = "postgresql://litellm:litellm@127.0.0.1:${toString config.services.postgresql.port}/litellm";
      # };
      settings.model_list = [
        {
          "model_name" = "GLM-4-Flash";
          "litellm_params" = {
            model = "openai/glm-4"; # Prefix with 'openai/' to use the compatible handler
            api_base = "http://127.0.0.1:22545/v1";
            api_key = "not-needed";
          };
        }
        {
          "model_name" = "Qwen3-Coder-Next";
          "litellm_params" = {
            model = "openai/qwen3-coder";
            api_base = "http://localhost:22546/v1";
            api_key = "not-needed";
            max_tokens = 4096;
          };
        }
        {
          model_name = "Qwen/Qwen3-4B";
          litellm_params = {
            model = "hosted_vllm/Qwen/Qwen3-4B";
            api_base = "https://localhost:8000/v1";
          };
        }
        # ollama
        # ...
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
          enable = false;
        };
        # services = {
        #   llama-server = {
        #     enable = true;
        #   };
        # };
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
        # OLLAMA_FLASH_ATTENTION = lib.mkForce "0";
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

    home-manager.sharedModules = [
      {
        home.packages = [
          ai-tmux-session-script
          (pkgs.writeShellScriptBin "run-qwen3-coder" ''
            "${pkgs.llama-cpp}/bin/llama-server" -m ~/MINE/models/Qwen3-Coder-Next-Q8_0.gguf --port 22546 -c 262144 -fa on -ctk q8_0 -ctv q8_0
          '')
          (pkgs.writeShellScriptBin "run-glm4-flash" ''
            "${pkgs.llama-cpp}/bin/llama-server" -m ~/MINE/models/GLM-4.7-Flash-BF16.gguf --port 22545 -c 202752 -fa on -ctk q8_0 -ctv q8_0
          '')
        ];
      }
    ];
  };
}
