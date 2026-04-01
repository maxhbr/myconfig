{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  llama-cuda-server = lib.getExe' pkgs.llama-cpp "llama-server";
  llama-rocm-server = lib.getExe' pkgs.llama-cpp-rocm "llama-server";
  llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
  hasGpuVariant = v: builtins.elem v config.myconfig.hardware.gpu.variant;
  cudaModels = lib.mkIf (hasGpuVariant "nvidia") {
    "CUDA:Qwen3.5-27B-Q8_0" = {
      cmd = ''
        ${llama-cuda-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=CUDA0"
      ];
      ttl = 300;
    };
    "CUDA:Qwen3.5-27B-Q8_0:mmproj" = {
      cmd = ''
        ${llama-cuda-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=CUDA0"
      ];
      ttl = 300;
    };
  };
  rocmModels = lib.mkIf (hasGpuVariant "amd") {
    "ROCm0:Qwen3.5-27B-Q8_0" = {
      cmd = ''
        ${llama-rocm-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=ROCm0"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 300;
    };
    "ROCm0:Qwen3.5-27B-Q8_0:mmproj" = {
      cmd = ''
        ${llama-rocm-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-F16.gguf -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=ROCm0"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 300;
    };
  };
  vulkanModels = lib.mkIf (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm") {
    "Vulkan1:qwen3.5-122B-A10B-Q5_K_M" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf --gpu-layers 999 -fa on --no-webui
      '';
      aliases = [
        "qwen3.5-122B-A10B-Q5_K_M"
        "qwen3.5-122B"
      ];
      env = [
        "LLAMA_ARG_DEVICE=Vulkan1"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 1800;
    };
    "Vulkan1:qwen3.5-122B-A10B-Q5_K_M:mmproj" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf --gpu-layers 999 --mmproj /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/mmproj-BF16.gguf -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=Vulkan1"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 1800;
    };
    "Vulkan0:Qwen3.5-27B-Q8_0" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 -fa on --no-webui
      '';
      aliases = [
        "Qwen3.5-27B-Q8_0"
        "Qwen3.5-27B"
      ];
      env = [
        "LLAMA_ARG_DEVICE=Vulkan0"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 0;
    };
    "Vulkan0:Qwen3.5-27B-Q8_0:modded" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 -fa on --no-webui -c 131072 --threads 4 --batch-size 2048 -np 1 --temp 0.6 --top-p 0.95 --top-k 20 --min-p 0.0
      '';
      env = [
        "LLAMA_ARG_DEVICE=Vulkan0"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 0;
    };
    "Vulkan0:Qwen3.5-27B-Q8_0:mmproj" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 --mmproj /persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=Vulkan0"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 300;
    };
    "Vulkan1:Qwen3.5-27B-Q8_0" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf --gpu-layers 999 -fa on --no-webui
      '';
      env = [
        "LLAMA_ARG_DEVICE=Vulkan1"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 300;
    };
    "Vulkan1:Qwen3.5-27B-BF16" = {
      cmd = ''
        ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-27B-GGUF/BF16/Qwen3.5-27B-BF16-00001-of-00002.gguf --gpu-layers 999 -fa on -ctk f16 -ctv f16 --no-webui
      '';
      aliases = [
        "Qwen3.5-27B-BF16"
      ];
      env = [
        "LLAMA_ARG_DEVICE=Vulkan1"
        "CUDA_VISIBLE_DEVICES="
      ];
      ttl = 300;
    };
  };
in
{
  config = {
    services.llama-swap = {
      enable = true;
      port = 33656;
      openFirewall = true;
      listenAddress = "0.0.0.0";
      settings =
        let
          llama-server = lib.getExe' config.services.llama-cpp.package "llama-server";
        in
        {
          healthCheckTimeout = 500;
          sendLoadingState = true;
          models = lib.mkMerge [
            cudaModels
            rocmModels
            vulkanModels
          ];
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
    home-manager.users.${myconfig.user} = {
      home.packages =
        let
          scripts = lib.mapAttrs' (model: cfg: {
            name = "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}";
            value = pkgs.writeShellScriptBin "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}" ''
              export PORT=''${1:-33657}
              ${lib.concatStringsSep "\n" (map (e: "export ${e}") cfg.env)}
              set -x
              ${cfg.cmd}
            '';
          }) config.services.llama-swap.settings.models;
        in
        lib.attrValues scripts;
    };
  };
}
