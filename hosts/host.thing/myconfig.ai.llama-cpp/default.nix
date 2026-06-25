{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  modelsPullDir = "/home/mhuber/models";
  gemma4 = import ./gemma4.nix;
  minimaxM2_7 = import ./MiniMax-M2.7-GGUF.nix;
  nemotron3Super = import ./Nemotron-3-Super.nix;
  qwen3_6_27B = import ./Qwen3.6-27B.nix;
  qwen3_6_35B-A3B = import ./Qwen3.6-35B-A3B.nix;
  qwen3_6_27B-multiGpu = qwen3_6_27B.multiGpuModels;
  qwen3_6_35B-A3B-multiGpu = qwen3_6_35B-A3B.multiGpuModels;
  thedrummerSkyfall31B = import ./TheDrummer_Skyfall-31B.nix;
  ornith1_0_35B = import ./Ornith-1.0-35B.nix;
  rtxModels = [
    {
      name = "Qwen3.5-9B-Q5_K_M";
      path = "/models/unsloth-Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf" ];
      };
      ctxSize = 262144;
      aliases = [ "sidekick" ];
      ttl = 300;
    }
  ]
  ++ qwen3_6_27B.rtxModels
  ++ qwen3_6_35B-A3B.rtxModels
  ++ gemma4.rtxModels
  ++ thedrummerSkyfall31B.rtxModels
  ++ ornith1_0_35B.rtxModels;

  amdModels = map (model: model // { params = (model.params or [ ]) ++ [ "--no-mmap" ]; }) (
    [
      {
        name = "NVIDIA-Nemotron-3-Nano-Omni-Q8_0";
        path = "/models/ggml-org-NVIDIA-Nemotron-3-Nano-Omni/nemotron-3-nano-omni-ga_v1.0-Q8_0.gguf";
        pull-models = {
          target_directory = modelsPullDir;
          hf_spec = [ "ggml-org/NVIDIA-Nemotron-3-Nano-Omni/nemotron-3-nano-omni-ga_v1.0-Q8_0.gguf" ];
        };
        ttl = 1800;
      }
      {
        name = "qwen3.5-122B-A10B-Q5_K_M";
        path = "/models/unsloth-Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
        pull-models = {
          target_directory = modelsPullDir;
          hf_spec = [ "unsloth/Qwen3.5-122B-A10B-GGUF/Q5_K_M" ];
        };
        aliases = [ ];
        ttl = 1800;
      }
    ]
    ++ qwen3_6_27B.amdModels
    ++ qwen3_6_35B-A3B.amdModels
    ++ gemma4.amdModels
    ++ minimaxM2_7.amdModels
    ++ nemotron3Super.amdModels
    ++ thedrummerSkyfall31B.amdModels
    ++ ornith1_0_35B.amdModels
  );

  # Package built for the host with ROCm+Vulkan support (variant = "amd").
  # Passed into the container so it reuses the same binary instead of
  # falling back to the plain llama-cpp without GPU backends.
  host-llama-cpp-pkg = config.myconfig.ai.inference-cpp.llama-cpp.package;
in
{
  config = {
    # The amdModels above are served by the `llama-cpp-33657` container,
    # not by the host's own llama-cpp. Their `pull-models` specs would
    # therefore not be picked up by the auto-collector that reads
    # `config.myconfig.ai.llama-cpp.models` at the host level. Surface
    # them here explicitly so `pull-models` on the host still downloads
    # them into ${modelsPullDir} (the container reads `/models/` via a
    # separate bind mount, which is out of scope for this helper).
    myconfig.ai.pull_models.models.${modelsPullDir} = lib.concatMap (m: m.pull-models.hf_spec) (
      builtins.filter (m: (m.pull-models or null) != null) (
        amdModels ++ qwen3_6_27B-multiGpu ++ qwen3_6_35B-A3B-multiGpu
      )
    );

    myconfig.ai.llama-cpp = {
      # Single CUDA0-bound llama-server instance on port 33656 (the new
      # INI-preset-driven router backend). The home-manager
      # llama-server_<Device> wrappers (router.enable = true) still get
      # generated for all three devices so ad-hoc Vulkan0/Vulkan1 access
      # remains available — they just aren't tied to a system service.
      serviceVariant = "llama-server";
      serviceDevice = "CUDA0";
      servicePort = 33656;
      serviceListenAddress = "0.0.0.0";
      serviceOpenFirewall = true;
      serviceProviderName = "rtx5090";
      router.enable = true;
      models = map (
        model:
        model
        // {
          devices = [
            "Vulkan0"
            "CUDA0"
          ];
          unlistedDevices = [
            "Vulkan1"
            "ROCm0"
          ];
        }
      ) rtxModels;
      scriptOnlyModels =
        map (
          model:
          model
          // {
            devices = [
              "Vulkan1"
            ];
          }
        ) amdModels
        # Multi-GPU models already carry their own `devices` list (e.g.
        # "Vulkan0,Vulkan1") and must not have it overridden.
        ++ qwen3_6_27B-multiGpu
        ++ qwen3_6_35B-A3B-multiGpu;
    };

    ############
    # Vulkan-only sibling instance running the llama-server router
    # backend (single llama-server bound to Vulkan0 with an INI preset
    # listing every model). Lives in a container so the host can keep
    # its CUDA-using stack on a different port without GPU library
    # conflicts.
    containers.llama-cpp-33657 = {
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
        "/proc/meminfo" = {
          hostPath = "/proc/meminfo";
          isReadOnly = true;
        };
      };

      config =
        { pkgs, ... }:
        {
          imports = [
            ../../../modules/myconfig.ai/myconfig.ai.llama-cpp
            ../../../modules/myconfig.ai/myconfig.localModels.nix
          ];
          environment.systemPackages = with pkgs; [
            nvtopPackages.amd
            rocmPackages.rocm-smi
          ];
          hardware.graphics.enable = true;
          # Use the host's llama-cpp binary (built with ROCm+Vulkan for
          # variant = "amd") instead of the container's default plain
          # build which lacks GPU backend support.
          services.llama-cpp.package = lib.mkForce host-llama-cpp-pkg;
          myconfig.ai.llama-cpp = {
            serviceVariant = "llama-swap";
            # serviceDevice = "Vulkan0"; # only for serviceVariant llama-server
            servicePort = 33657;
            serviceListenAddress = "0.0.0.0";
            serviceOpenFirewall = true;
            serviceProviderName = "gfx1151";
            models =
              let
                allAliasesAndNamesFromAmdModels = lib.concatMap (m: [ m.name ] ++ (m.aliases or [ ])) amdModels;
                fromRtxModels = map (
                  {
                    name,
                    path,
                    aliases ? [ ],
                    params ? [ ],
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
              map (
                model:
                model
                // {
                  devices = [
                    "Vulkan0"
                    "ROCm0"
                  ];
                }
              ) (fromRtxModels ++ amdModels);
          };
        };
    };
    myconfig.ai.localModels = config.containers.llama-cpp-33657.config.myconfig.ai.localModels;
    ############
  };
}
