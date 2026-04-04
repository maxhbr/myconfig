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
        name = "Qwen3.5-27B-Q8_0";
        path = "/persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf";
        devices = [
          "CUDA0"
          "ROCm0"
          "Vulkan0"
          "Vulkan1"
        ];
        mmproj = "/persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf";
        aliases = [
          "Qwen3.5-27B-Q8_0"
          "Qwen3.5-27B"
        ];
        ttl = 0;
      }
      {
        name = "Qwen3.5-27B-Q8_0:modded";
        path = "/persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf";
        devices = [ "Vulkan0" ];
        params = "-c 131072 --threads 4 --batch-size 2048 -np 1 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.0";
        ttl = 0;
      }
      {
        name = "qwen3.5-122B-A10B-Q5_K_M";
        path = "/persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
        devices = [ "Vulkan1" ];
        mmproj = "/persistent/cache/models/Qwen3.5-122B-A10B-GGUF/mmproj-BF16.gguf";
        aliases = [
          "opencode"
          "qwen3.5-122B-A10B-Q5_K_M"
          "qwen3.5-122B"
        ];
        ttl = 1800;
      }
      {
        name = "Qwen3.5-27B-BF16";
        path = "/persistent/cache/models/Qwen3.5-27B-GGUF/BF16/Qwen3.5-27B-BF16-00001-of-00002.gguf";
        devices = [ "Vulkan1" ];
        params = "-ctk f16 -ctv f16";
        aliases = [ "Qwen3.5-27B-BF16" ];
        ttl = 300;
      }
      {
        name = "Qwen3.5-27B-BF16:modded";
        path = "/persistent/cache/models/Qwen3.5-27B-GGUF/BF16/Qwen3.5-27B-BF16-00001-of-00002.gguf";
        devices = [ "Vulkan1" ];
        params = "-ctk f16 -ctv f16 -c 131072 --threads 4 --batch-size 2048 -np 1 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.0";
        ttl = 300;
      }
      {
        name = "gemma-4-26B-A4B-it-BF16";
        path = "/mnt/disk/models/gemma-4-26B-A4B-it-GGUF/BF16/gemma-4-26B-A4B-it-BF16-00001-of-00002.gguf";
        devices = [
          "Vulkan1"
        ];
        params = "-ctk f16 -ctv f16";
        aliases = [ "gemma-4-26B-A4B" ];
        ttl = 300;
      }
      {
        name = "gemma-4-26B-A4B-it-Q8_K_XL";
        path = "/mnt/disk/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf";
        devices = [
          "Vulkan0"
          "Vulkan1"
          "CUDA0"
        ];
        aliases = [ 
          "hermes"
          "gemma-4-26B-A4B-Q8"
        ];
        ttl = 300;
      }
      {
        name = "gemma-4-31B-it-BF16";
        path = "/mnt/disk/models/gemma-4-31B-it-GGUF/BF16/gemma-4-31B-it-BF16-00001-of-00002.gguf";
        devices = [
          "Vulkan1"
        ];
        params = "-ctk f16 -ctv f16";
        aliases = [
          "gemma-4-31B"
          "gemma-4-31B-BF16"
        ];
        ttl = 300;
      }
      {
        name = "gemma-4-31B-it-Q6_K_XL";
        path = "/mnt/disk/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q6_K_XL.gguf";
        devices = [
          "Vulkan0"
          "Vulkan1"
          "CUDA0"
        ];
        aliases = [
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
  };
}
