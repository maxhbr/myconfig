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
    myconfig.ai.llama-swap.models = [
      {
        name = "Qwen3.6-35B-A3B-UD-Q5_K_XL";
        path = "/persistent/cache/models/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf";
        devices = [
          "Vulkan0"
          "CUDA0"
          "ROCm0"
        ];
        # mmproj =
        aliases = [
          "hermes"
          "opencode-fast"
          "Qwen3.6-35B-A3B-UD-Q5_K"
          "Qwen3.6-35B-A3B-UD-Q5"
          "Qwen3.6-35B-A3B-UD"
          "Qwen3.6-35B-A3B"
          "Qwen3.6-35B"
        ];
        ttl = 900;
      }
      # {
      #   name = "Qwen3.5-27B-Q8_0:modded";
      #   path = "/persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf";
      #   devices = [ "Vulkan0" ];
      #   params = "-c 131072 --threads 4 --batch-size 2048 -np 1 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.0";
      #   ttl = 0;
      # }
      {
        name = "gemma-4-26B-A4B-it-Q8_K_XL";
        path = "/mnt/disk/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf";
        devices = [
          "Vulkan0"
          "CUDA0"
        ];
        aliases = [
          "gemma-4-26B-A4B-Q8"
        ];
        ttl = 300;
      }
      {
        name = "gemma-4-31B-it-Q6_K_XL";
        path = "/mnt/disk/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q6_K_XL.gguf";
        devices = [
          "Vulkan0"
          "CUDA0"
        ];
        aliases = [
          "hermes-fallback"
          "gemma-4-31B-Q6"
        ];
        ttl = 300;
      }
    ];

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
        "/persistent/cache/models/" = {
          hostPath = "/persistent/cache/models/";
          isReadOnly = true;
        };
      };

      config =
        { pkgs, lib, ... }:
        {
          hardware.graphics.enable = true;
          services.llama-swap = {
            enable = true;
            port = 33657;
            openFirewall = true;
            listenAddress = "0.0.0.0";
            settings = {
              healthCheckTimeout = 500;
              models = {
                "qwen3.5-122B-A10B-Q5_K_M" =
                  let
                    llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
                  in
                  {
                    cmd = ''
                      ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf  -fa on --no-webui
                    '';
                    aliases = [
                      "opencode"
                      "qwen3.5-122B"
                    ];
                    "ttl" = 1800;
                  };
                "qwen3.6-35B-A3B-BF16" =
                  let
                    llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
                  in
                  {
                    cmd = ''
                      ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.6-35B-A3B-GGUF/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf -ctk f16 -ctv f16 -fa on --no-webui
                    '';
                    aliases = [
                      "opencode-fallback"
                      "Qwen3.6-35B-A3B"
                      "Qwen3.6-35B"
                    ];
                    "ttl" = 300;
                  };
                "gemma-4-31B-it-BF16" =
                  let
                    llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
                  in
                  {
                    cmd = ''
                      ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/gemma-4-31B-it-GGUF/BF16/gemma-4-31B-it-BF16-00001-of-00002.gguf -ctk f16 -ctv f16 -fa on --no-webui
                    '';
                    aliases = [
                      "gemma-4-31B"
                      "gemma-4-31B-BF16"
                    ];
                    "ttl" = 300;
                  };
              };
            };
          };
        };
    };
    myconfig.ai.localModels = [
      {
        name = "llama-swap-33657";
        port = 33657;
        models = [
          "qwen3.5-122B-A10B-Q5_K_M"
          "opencode"
          "qwen3.6-35B-A3B-BF16"
          "opencode-fallback"
          "gemma-4-31B-it-BF16"
        ];
      }
    ];
    ############
  };
}
