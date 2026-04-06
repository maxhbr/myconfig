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
          "Vulkan0"
          "Vulkan1"
          "CUDA0"
          "ROCm0"
        ];
        mmproj = "/persistent/cache/models/Qwen3.5-27B-GGUF/mmproj-BF16.gguf";
        aliases = [
          "hermes"
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
        { node = "/dev/dri/renderD128"; modifier = "rw"; }
        { node = "/dev/dri/card0"; modifier = "rw"; }
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

      config = { pkgs, lib, ... }: {
        hardware.graphics.enable = true;
        services.llama-swap = {
          enable = true;
          port = 33657;
          openFirewall = true;
          listenAddress = "0.0.0.0";
          settings = {
            healthCheckTimeout = 500;
            models = {
              "qwen3.5-122B-A10B-Q5_K_M" = let
                 llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
                in {
                cmd = ''
                  ${llama-vulkan-server} --port ''${PORT} -m /persistent/cache/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf  -fa on --no-webui
                '';
                aliases = [
                  "opencode"
                  "qwen3.5-122B"
                ];
                "ttl" = 1800;
              };
            };
          };
        };

        # users.groups.render.gid = 303;
        # users.groups.video.gid = 44; # adjust if your host uses a different GID
        # users.users.llama = {
        #   isSystemUser = true;
        #   group = "users";
        #   extraGroups = [ "render" "video" ];
        #   home = "/var/lib/llama";
        #   createHome = true;
        # };

        # environment.systemPackages = with pkgs; [
        #   vulkan-tools
        #   mesa-demos

        #   # One of these, depending on your channel:
        #   llama-cpp-vulkan
        #   # or: (llama-cpp.override { vulkanSupport = true; })
        # ];

        # environment.variables = {
        #   # Force RADV explicitly if you want to avoid ambiguity
        #   VK_ICD_FILENAMES = "/run/opengl-driver/share/vulkan/icd.d/radeon_icd.x86_64.json";

        #   # Conservative but useful for troubleshooting
        #   VK_DRIVER_FILES = "/run/opengl-driver/share/vulkan/icd.d/radeon_icd.x86_64.json";

        #   # Make sure shader cache and model cache are writable
        #   HOME = "/var/lib/llama";
        #   XDG_CACHE_HOME = "/var/lib/llama/.cache";
        # };

        # systemd.tmpfiles.rules = [
        #   "d /var/lib/llama 0755 llama users - -"
        #   "d /var/lib/llama/.cache 0755 llama users - -"
        #   "d /var/lib/llama/models 0755 llama users - -"
        # ];

        # systemd.services.llama-server = {
        #   wantedBy = [ "multi-user.target" ];
        #   after = [ "network-online.target" ];
        #   serviceConfig = {
        #     User = "llama";
        #     Group = "users";
        #     WorkingDirectory = "/var/lib/llama";
        #     StateDirectory = "llama";
        #     Restart = "on-failure";
        #     ExecStart = ''
        #       ${pkgs.llama-cpp-vulkan}/bin/llama-server \
        #         -m /var/lib/llama/models/model.gguf \
        #         --host 0.0.0.0 \
        #         --port 8080 \
        #         -ngl 99
        #     '';
        #   };
        # };
      };
    };
    ############
  };
}
