{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  rtxModels = [
    {
      name = "Qwen3.5-9B-Q5_K_M";
      path = "/models/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf";
      ctxSize = 262144;
      aliases = [ "sidekick" ];
      ttl = 300;
    }
    {
      name = "Qwen3.6-35B-A3B-UD-Q5_K_XL";
      path = "/models/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf";
      # mmproj =
      aliases = [
        "Qwen3.6-35B-A3B-UD-Q5_K"
        "Qwen3.6-35B-A3B-UD-Q5"
        "Qwen3.6-35B-A3B-UD"
        "Qwen3.6-35B-A3B"
        "Qwen3.6-35B"
      ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-Q8_0";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf";
      params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
      # mmproj =
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-Q8_0-tweaked";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf";
      params = "--jinja --reasoning-format deepseek -ngl 99 -fa -sm row --temp 0.6 --top-k 20 --top-p 0.95 --min-p 0 -c 40960 -n 32768 --no-context-shift";
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-UD-Q4_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf";
      params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-UD-Q5_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf";
      params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
      aliases = [
        "hermes"
        "opencode-fast"
        "Qwen3.6-27B"
      ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-UD-Q6_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf";
      params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
      # mmproj =
      aliases = [
        "hermes-fallback"
        "opencode-fast-fallback"
      ];
      ttl = 900;
    }
    # {
    #   name = "Qwen3.5-27B-Q8_0:modded";
    #   path = "/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf";
    #   devices = [ "Vulkan0" ];
    #   params = "-c 131072 --threads 4 --batch-size 2048 -np 1 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.0";
    #   ttl = 0;
    # }
    {
      name = "gemma-4-26B-A4B-it-Q8_K_XL";
      path = "/mnt/disk/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf";
      aliases = [
        "gemma-4-26B-A4B-Q8"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-Q6_K_XL";
      path = "/mnt/disk/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q6_K_XL.gguf";
      aliases = [
        # "hermes-fallback"
        "gemma-4-31B-Q6"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-UD-Q4_K_XL";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf";
      ctxSize = 65536;
      mmproj = "/models/gemma-4-31B-it-GGUF/mmproj-F16.gguf";
      params = "--batch-size 2048 --ubatch-size 512 --threads 1 --jinja";
      aliases = [
        "gemma-4:31b-q4"
        "gemma-4-31B-Q4"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-UD-Q4_K_XL-nothink";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf";
      ctxSize = 65536;
      mmproj = "/models/gemma-4-31B-it-GGUF/mmproj-F16.gguf";
      params = "--batch-size 2048 --ubatch-size 512 --threads 1 --chat-template-kwargs '{\"enable_thinking\": false}' --jinja";
      aliases = [
        "gemma-4:31b-q4-nothink"
      ];
      ttl = 300;
    }
  ];

  amdModels = [
    {
      name = "qwen3.5-122B-A10B-Q5_K_M";
      path = "/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
      aliases = [
        "opencode-slow"
        "opencode-fallback"
        "qwen3.5-122B"
      ];
      ttl = 1800;
    }
    {
      name = "Qwen3.6-27B-GGUF-BF16";
      path = "/models/Qwen3.6-27B-GGUF/BF16/Qwen3.6-27B-BF16-00001-of-00002.gguf";
      params = "-ctk f16 -ctv f16 --chat-template-kwargs '{\"preserve_thinking\": true}'";
      aliases = [
        "opencode"
        "Qwen3.6-27B-GGUF"
        "Qwen3.6-27B"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-BF16";
      path = "/models/gemma-4-31B-it-GGUF/BF16/gemma-4-31B-it-BF16-00001-of-00002.gguf";
      params = "-ctk f16 -ctv f16";
      aliases = [
        "gemma-4-31B"
        "gemma-4-31B-BF16"
      ];
      ttl = 300;
    }
    # {
    #   name = "Qwen3.6-27B-UD-Q4_K_XL";
    #   path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf";
    #   params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
    # }
    # {
    #   name = "Qwen3.6-27B-UD-Q5_K_XL";
    #   path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf";
    #   params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
    # }
    # {
    #   name = "Qwen3.6-27B-UD-Q6_K_XL";
    #   path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf";
    #   params = "--chat-template-kwargs '{\"preserve_thinking\": true}'";
    # }
  ];

in
{
  imports = [
    ./services.llama-swap.vllm.nix
  ];
  config = {
    myconfig.ai.llama-swap.models = map (
      model:
      model
      // {
        devices = [
          "Vulkan0"
          "CUDA0"
        ];
        unlistedDevices = [
          "Vulkan1"
        ];
      }
    ) rtxModels;
    services.llama-swap = {
      enable = true;
      port = 33656;
      openFirewall = true;
      listenAddress = "0.0.0.0";
      settings = {
        healthCheckTimeout = 500;
      };
    };

    ############
    containers.llama-swap-33657 = {
      autoStart = true;
      privateNetwork = false;
      # hostAddress = "10.233.10.1";
      # localAddress = "10.233.10.2";

      # Important: cgroup device permissions
      allowedDevices = [
        {
          node = "/dev/dri/renderD128";
          modifier = "rw";
        }
        {
          node = "/dev/dri/card0";
          modifier = "rw";
        }
      ];

      # Important: actual device + driver userspace visibility
      bindMounts = {
        "/dev/dri" = {
          hostPath = "/dev/dri";
          isReadOnly = false;
        };
        "/run/opengl-driver" = {
          hostPath = "/run/opengl-driver";
          isReadOnly = true;
        };
        "/models/" = {
          hostPath = "/models/";
          isReadOnly = true;
        };
      };

      config =
        { ... }:
        {
          imports = [
            ../../modules/myconfig.ai/services.llama-swap.nix
            ../../modules/myconfig.ai/myconfig.localModels.nix
          ];
          hardware.graphics.enable = true;
          myconfig.ai.llama-swap.models =
            let
              allAliasesAndNamesFromAmdModels = lib.concatMap (m: [ m.name ] ++ (m.aliases or [ ])) amdModels;
              fromRtxModels = map (
                {
                  name,
                  path,
                  aliases,
                  params ? "",
                  ...
                }:
                {
                  inherit
                    name
                    path
                    params
                    ;
                  aliases = lib.filter (a: !lib.elem a allAliasesAndNamesFromAmdModels) aliases;
                }
              ) rtxModels;
            in
            fromRtxModels ++ amdModels;
          services.llama-swap = {
            enable = true;
            port = 33657;
            openFirewall = true;
            listenAddress = "0.0.0.0";
            settings = {
              healthCheckTimeout = 500;
            };
          };
        };
    };
    myconfig.ai.localModels = config.containers.llama-swap-33657.config.myconfig.ai.localModels;
    ############
  };
}
