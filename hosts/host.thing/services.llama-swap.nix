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
    services.llama-swap = {
      enable = true;
      port = 33656;
      openFirewall = true;
      listenAddress = "0.0.0.0";
      settings =
        let
          llama-server = lib.getExe' config.myconfig.ai.inference-cpp.llama-cpp.package "llama-server";
        in
        {
          healthCheckTimeout = 500;
          sendLoadingState = true;
          models = {
            "Vulkan1:qwen3.5-122B-A10B-Q5_K_M" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf --mmproj /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/mmproj-BF16.gguf -fa on --no-webui
              '';
              aliases = [
                "qwen3.5-122B-A10B-Q5_K_M"
                "qwen3.5-122B"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan1"
              ];
              ttl = 1800;
            };
            "CPU:qwen3.5-122B-A10B-Q5_K_M" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf --mmproj /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/mmproj-BF16.gguf -fa on -ngl 0 --no-webui
              '';
              env = [
                "ZENDNNL_MATMUL_ALGO=1" # Blocked AOCL DLP algo for best performance
              ];
              ttl = 1800;
            };
            # "qwen3.5-35B-A3B-Q6_K" = {
            #   cmd = ''
            #     ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-35B-A3B-GGUF/Qwen3.5-35B-A3B-Q6_K.gguf -fa on --no-webui
            #   '';
            #   aliases = [
            #     "qwen3.5-35B"
            #   ];
            #   env = [
            #     "LLAMA_ARG_DEVICE=Vulkan0"
            #   ];
            #   ttl = 300;
            # };
            "Vulkan0:Qwen3.5-27B-Q8_0" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on --no-webui
              '';
              aliases = [
                "Qwen3.5-27B-Q8_0"
                "Qwen3.5-27B"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan0"
              ];
              ttl = 300;
            };
            "Vulkan1:Qwen3.5-27B-Q8_0" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on --no-webui
              '';
              env = [
                "LLAMA_ARG_DEVICE=Vulkan1"
              ];
              ttl = 300;
            };
            "CPU:Qwen3.5-27B-Q8_0" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on -ngl 0 --no-webui
              '';
              env = [
                "ZENDNNL_MATMUL_ALGO=1" # Blocked AOCL DLP algo for best performance
              ];
              ttl = 300;
            };
            "Vulkan1:Qwen3.5-27B-BF16" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/models/cache/Qwen3.5-27B-GGUF/BF16/Qwen3.5-27B-BF16-00001-of-00002.gguf --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on -ctk f16 -ctv f16 --no-webui
              '';
              aliases = [
                "Qwen3.5-27B-BF16"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan1"
              ];
              ttl = 300;
            };
            "CPU:Qwen3.5-27B-BF16" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/BF16/Qwen3.5-27B-BF16-00001-of-00002.gguf --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on -ngl 0 -ctk bf16 -ctv bf16 --no-webui
              '';
              env = [
                "ZENDNNL_MATMUL_ALGO=1" # Blocked AOCL DLP algo for best performance
              ];
              ttl = 300;
            };
          };
        };
      # tls = {
      #   enable =
      #   keyFile =
      #   certFile =
      # };
    };
    systemd.services.llama-swap = {
      # https://github.com/nixos/nixpkgs/issues/441531
      environment.XDG_CACHE_HOME = "/var/cache/llama-swap";
      serviceConfig.CacheDirectory = "llama-swap";
    };
  };
}
