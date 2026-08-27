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
  agentsA1 = import ./Agents-A1.nix { inherit modelsPullDir; };
  gemma4 = import ./gemma4.nix { inherit modelsPullDir; };
  minimaxM2_7 = import ./MiniMax-M2.7-GGUF.nix { inherit modelsPullDir; };
  nemotron3Super = import ./Nemotron-3-Super.nix { inherit modelsPullDir; };
  qwen3_6_35B-A3B = import ./Qwen3.6-35B-A3B.nix { inherit modelsPullDir; };
  qwen3_6_35B-A3B-multiGpu = qwen3_6_35B-A3B.multiGpuModels;
  qwen3_8_27B-multiGpu = qwen3_8_27B.multiGpuModels;
  hy3-multiGpu = hy3.multiGpuModels;
  thedrummerSkyfall31B = import ./TheDrummer_Skyfall-31B.nix { inherit modelsPullDir; };
  ornith = import ./Ornith-1.0-35B.nix { inherit modelsPullDir; };
  qwen3_235B = import ./Qwen3-235B-A22B.nix { inherit modelsPullDir; };
  qwen3_8_27B = import ./Qwen3.8-27B.nix { inherit modelsPullDir; };
  hy3 = import ./Hy3-Q2_K_L.nix { inherit modelsPullDir; };
  qwen38_flash_next = import ./Qwen3.8-Flash-Next.nix {
    inherit modelsPullDir;
    package = patched-llama-cpp-pkg;
  };
  # Helper to set the llama-swap group on a list of models.
  withGroup = group: map (m: m // { inherit group; });

  sidekickModel = {
    name = "Qwen3.5-9B-Q5_K_M";
    path = "/models/unsloth-Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf";
    pull-models = {
      target_directory = modelsPullDir;
      hf_spec = [ "unsloth/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf" ];
    };
    ctxSize = 262144;
    aliases = [ "sidekick" ];
    ttl = 300;
  };

  rtxModels = [
    sidekickModel
  ]
  ++ withGroup "MoE" qwen3_6_35B-A3B.rtxModels
  ++ gemma4.rtxModels
  ++ thedrummerSkyfall31B.rtxModels
  ++ agentsA1.rtxModels
  ++ withGroup "dense" qwen3_8_27B.rtxModels;

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
    ++ withGroup "MoE" qwen3_6_35B-A3B.amdModels
    ++ withGroup "MoE" ornith.amdModels
    ++ gemma4.amdModels
    ++ minimaxM2_7.amdModels
    ++ nemotron3Super.amdModels
    ++ thedrummerSkyfall31B.amdModels
    ++ agentsA1.amdModels
    ++ qwen3_235B.amdModels
    ++ qwen3_8_27B.amdModels
    ++ qwen38_flash_next.amdModels
    ++ hy3.amdModels
  );
  fromRtxModels =
    let
      # Gather all names and aliases from AMD models, including aliases
      # declared inside variants (which are invisible at the top level
      # but end up as aliases once variants are unpacked).
      allAliasesAndNamesFromAmdModels = lib.concatMap (
        m:
        [ m.name ]
        ++ (m.aliases or [ ])
        ++ lib.concatMap (v: v.aliases or [ ]) (lib.attrValues (m.variants or { }))
      ) amdModels;
    in
    map
      (
        {
          name,
          path,
          aliases ? [ ],
          params ? [ ],
          group ? "default",
          # KV-cache shape. This block MUST be carried over: the
          # rebuild below is an explicit allowlist, so anything not
          # named here is silently dropped. `ctxSize` used to be, which
          # meant every RTX model re-served on gfx1151 started without
          # `--ctx-size` (llama-server then falls back to the GGUF's
          # n_ctx_train) AND published `contextWindow = null` into
          # `myconfig.ai.localModels`. That null propagates to LiteLLM
          # (no `max_input_tokens` / `max_tokens` on the model entry)
          # and from there to the agents, which then have to guess the
          # context window — see
          # modules/myconfig.ai/docs/debug-litellm-max-output-tokens.md.
          #
          # These values were sized for the RTX 5090's 32 GB; gfx1151
          # has more memory available, so re-using them is safe.
          ctxSize ? null,
          cacheType ? null,
          parallel ? 1,
          kvUnified ? false,
          ...
        }:
        {
          inherit
            name
            path
            params
            group
            ctxSize
            cacheType
            parallel
            kvUnified
            ;
          aliases = lib.filter (a: !lib.elem a allAliasesAndNamesFromAmdModels) aliases;
          # variants are dropped for now
        }
      )
      (
        lib.filter (
          m:
          !lib.elem m.name allAliasesAndNamesFromAmdModels
          # NVFP4 quantisation is NVIDIA-only – cannot load on AMD/GFX
          && !(lib.hasInfix "NVFP4" m.name)
        ) rtxModels
      );

  # Lookup of what the host's inference-cpp hook currently resolves to
  # (the stock nixpkgs build selected by services.llama-cpp.nix for the
  # host's GPU variants).  Used by the container override below.
  host-llama-cpp-pkg = config.myconfig.ai.inference-cpp.llama-cpp.package;

  # PR-27742 patched build for qwen4exp (Qwen3.8-Flash-Next) support.
  # Only the Flash-Next models need this — it is set per-model via the
  # `package` option so the rest of the host (RTX llama-server, other
  # ad-hoc wrappers) keeps the stock nixpkgs build.  The Flash-Next
  # models run on Vulkan0/ROCm0 (container llama-swap) and Vulkan1
  # (host scriptOnlyModels), never on CUDA, so ROCm+Vulkan suffices.
  patched-llama-cpp-pkg = pkgs.llama-cpp-pr-27742.override {
    rocmSupport = true;
    vulkanSupport = true;
    cudaSupport = false;
    blasSupport = false;
  };

  gfx-llama-cpp-config = {
    serviceVariant = "llama-swap";
    # serviceDevice = "Vulkan0"; # only for serviceVariant llama-server
    servicePort = 33657;
    serviceListenAddress = "0.0.0.0";
    serviceOpenFirewall = true;
    serviceProviderName = "gfx1151";
    groups = {
      "Qwen3.8-27B" = {
        swap = true;
        exclusive = false;
      };
      "Qwen3.6-35B-A3B" = {
        swap = true;
        exclusive = false;
      };
      default = {
        swap = true;
        exclusive = true;
      };
    };
    models = map (
      model:
      model
      // {
        devices = [
          "Vulkan0"
          "ROCm0"
        ];
      }
    ) (amdModels ++ fromRtxModels);
  };
  rtx-llama-cpp-config = {
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
      ++ qwen3_8_27B-multiGpu
      ++ qwen3_6_35B-A3B-multiGpu
      ++ hy3-multiGpu;
  };
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
    # Each model entry above keeps its own full, self-contained
    # `pull-models.hf_spec` (e.g. the Q8_0 base GGUF is declared by both
    # `Qwen3.8-27B-Q8_0` and `Qwen3.8-27B-MTP-Q8_0`); the merged list is
    # de-duplicated here so `pull-models` does not run the same
    # `hf download` twice.
    myconfig.ai.pull_models.models.${modelsPullDir} = lib.unique (
      lib.concatMap (m: m.pull-models.hf_spec) (
        builtins.filter (m: (m.pull-models or null) != null) (
          amdModels ++ qwen3_8_27B-multiGpu ++ qwen3_6_35B-A3B-multiGpu ++ hy3-multiGpu
        )
      )
    );

    myconfig.ai.llama-cpp = rtx-llama-cpp-config;

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
          services.llama-cpp.package = lib.mkForce host-llama-cpp-pkg;
          myconfig.ai.llama-cpp = gfx-llama-cpp-config;
        };
    };
    myconfig.ai.localModels = config.containers.llama-cpp-33657.config.myconfig.ai.localModels;
    ############
  };
}
