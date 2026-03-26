{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    (
      {
        lib,
        config,
        pkgs,
        ...
      }:
      {
        config = lib.mkIf (config.specialisation != { }) {
          # Config that should only apply to the default system, not the specialised ones
        };
      }
    )
  ];
  specialisation = {
    qwen-35-122 = {
      inheritParentConfig = true;
      configuration = {
        services.llama-cpp = {
          enable = true;
          model = "/mnt/disk/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
          port = 22545;
          extraFlags = ["-fa" "on"];
        };
        systemd.services.llama-cpp = {
          environment = {
            LLAMA_ARG_DEVICE = "Vulkan1";
          };
        };
      };
    };
    llama-swap = {
      inheritParentConfig = true;
      configuration = {
        myconfig.ai.localModels = [
          {
            name = "llama-swap";
            port = 23545;
          }
        ];
        services.llama-swap = {
          enable = true;
          port = 23545;
          openFirewall = false;
          listenAddress = "127.0.0.1";
          settings = 
            let
              llama-server = lib.getExe' config.myconfig.ai.inference-cpp.llama-cpp.package "llama-server";
            in
            {
              healthCheckTimeout = 60;
              models = {
                "qwen3.5-122B-A10B-Q5_K_M" = {
                  cmd = ''
                    ${llama-server} --port ''${PORT} -m /mnt/disk/models/Qwen3.5-122B/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf -fa on --no-webui
                  '';
                  aliases = [
                    "qwen3.5-122B"
                  ];
                  env = [
                    "LLAMA_ARG_DEVICE=Vulkan1"
                  ];
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
      };
    };
  };
}
