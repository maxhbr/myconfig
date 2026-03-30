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
    # myconfig.ai.localModels = [
    #   {
    #     name = "llama-swap";
    #     port = config.services.llama-swap.port;
    #   }
    # ];
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
            "qwen3.5-122B-A10B-Q5_K_M" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /mnt/disk/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf -fa on --no-webui
              '';
              aliases = [
                "qwen3.5-122B"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan1"
              ];
              ttl = 1800;
            };
            "qwen3.5-35B-A3B-Q6_K" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /mnt/disk/models/Qwen3.5-35B-A3B-GGUF/Qwen3.5-35B-A3B-Q6_K.gguf -fa on --no-webui
              '';
              aliases = [
                "qwen3.5-35B"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan0"
              ];
              ttl = 300;
            };
            "Qwen3.5-27B-Q8_0" = {
              cmd = ''
                ${llama-server} --port ''${PORT} -m /mnt/disk/models/Qwen3.5-27B-GGUF/Qwen3.5-27B-Q8_0.gguf -fa on --no-webui
              '';
              aliases = [
                "Qwen3.5-27B"
              ];
              env = [
                "LLAMA_ARG_DEVICE=Vulkan0"
              ];
              ttl = 300;
            };
            # "other-model" = {
            #   proxy = "http://127.0.0.1:5555";
            #   cmd = "${llama-server} --port 5555 -m /var/lib/llama-cpp/models/other-model.gguf -ngl 0 -c 4096 -np 4 --no-webui";
            #   concurrencyLimit = 4;
            # };
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
