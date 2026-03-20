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
              openWebuiPort =
                if config.myconfig.ai.container.open-webui.enable then
                  config.myconfig.ai.container.open-webui.port
                else
                  config.myconfig.ai.open-webui.port;
              litellmRouteConfig = lib.optionalString config.services.litellm.enable ''
                handle_path /litellm/* {
                  reverse_proxy http://localhost:${toString config.services.litellm.port}
                }
              '';
              ollamaRouteConfig = lib.optionalString config.services.ollama.enable ''
                handle_path /ollama/* {
                  reverse_proxy http://localhost:${toString config.services.ollama.port}
                }
              '';
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
                    ${litellmRouteConfig}
                    ${ollamaRouteConfig}
                    reverse_proxy http://localhost:${toString openWebuiPort}
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
      config = lib.mkIf (builtins.elem "amd" config.myconfig.hardware.gpu.variant) {
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
      {
        myconfig.ai.opencode.enable = true;
        myconfig.ai.localModels = [
          {
            name = "qwen3-5-vulkan";
            port = 22547;
          }
          {
            port = 22546;
          }
          {
            port = 22545;
          }
          {
            name = "sglang";
            port = 30000;
          }
        ];
      }
    )
  ];

  config = {
    system.nixos.tags = config.myconfig.hardware.gpu.variant;

    services.litellm.enable = true;

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
        services = {
          llama-server.instances = {
            qwen3-5-vulkan = {
              enable = true;
              createService = true;
              modelPath = "/home/mhuber/disk/models/Qwen3.5-122B-A10B-MXFP4_MOE.gguf";
              port = 22547;
              contextSize = 202752;
              device = "Vulkan1";
              flashAttention = true;
            };
            qwen3-coder = {
              enable = true;
              modelPath = "/home/mhuber/disk/models/Qwen3-Coder-Next-Q8_0.gguf";
              port = 22546;
              contextSize = 262144;
              flashAttention = true;
              extraArgs = "-ctk q8_0 -ctv q8_0 -ngl all";
            };
            glm4-flash = {
              enable = true;
              modelPath = "/home/mhuber/disk/models/GLM-4.7-Flash-BF16.gguf";
              port = 22545;
              contextSize = 202752;
              flashAttention = true;
              extraArgs = "-ctk bf16 -ctv bf16 -ngl all";
            };
          };
        };
        # container = {
        #   nlm-ingestor = {
        #     enable = false;
        #   };
        #   open-webui = {
        #     enable = false;
        #   };
        #   sillytavern = {
        #     enable = false;
        #     host = myconfig.metadatalib.getWgIp "${config.networking.hostName}";
        #     port = 8888;
        #   };
        #   kokoro-fastapi = {
        #     enable = false;
        #   };
        #   lobe-chat = {
        #     enable = false;
        #     host = myconfig.metadatalib.getWgIp "${config.networking.hostName}";
        #   };
        # };
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
        # # "cogito:32b"
        # # "deepseek-r1:32b"
        # # "gemma3:27b"
        # "glm-4.7-flash:q8_0"
        # "glm-4.7-flash:bf16"
        # "gpt-oss:20b"
        # "gpt-oss:120b"
        # "granite4"
        # "llama4:16x17b"
        # # "llama3.2:3b"
        # # "llava:7b"
        # # "llava:34b"
        # # "magistral:24b"
        # # "openthinker:32b"
        # # "phi4"
        # "qwen3-coder-next:q8_0"
        # # "qwen2.5vl:32b"
        # "qwen3-vl:32b"
        # "qwen3.5:122b"
        # # "qwen3:30b"
        # # "qwen3:32b"
        # # "qwq:32b"
        # # "smollm2:1.7b"
      ];
    };
    # services.tabby = {
    #   enable = false;
    #   acceleration = "cuda";
    #   model = "TabbyML/Qwen2.5-Coder-14B";
    # };

    home-manager.sharedModules = [
      {
        myconfig.persistence.cache-directories = [ ".config/comfy-ui/" ];
        home.packages = [
          ai-tmux-session-script
        ];
      }
    ];

  };
}
