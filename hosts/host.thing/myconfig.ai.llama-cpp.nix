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
  rtxModels = [
    {
      name = "Qwen3.5-9B-Q5_K_M";
      path = "/models/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.5-9B-GGUF/Qwen3.5-9B-Q5_K_M.gguf" ];
      };
      ctxSize = 262144;
      aliases = [ "sidekick" ];
      ttl = 300;
    }
    {
      name = "Qwen3.6-35B-A3B-UD-Q5_K_XL";
      path = "/models/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-UD-Q5_K_XL.gguf" ];
      };
      variants = {
        tweaked = {
          # https://github.com/nathanlgabriel/paper_code_mapping_assessment/blob/main/README.md
          params = [
            "--split-mode" "layer" "--tensor-split" "1,1.12" "--parallel" "1" "--cache-type-k" "q8_0" "--cache-type-v" "q8_0"
          ];
          ctxSize = 262144;
        };
      };
      aliases = [
        "Qwen3.6-35B-A3B-UD-Q5_K"
        "Qwen3.6-35B-A3B-UD-Q5"
        "Qwen3.6-35B-A3B-UD"
        "Qwen3.6-35B-A3B"
        "Qwen3.6-35B"
        "hermes-fallback"
        "opencode-fast-fallback"
      ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-Q8_0";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-Q8_0.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      aliases = [ ];
      ttl = 900;
      variants = {
        tweaked = {
          params = [
            "--jinja"
            "--reasoning-format"
            "deepseek"
            "-ngl"
            "99"
            "-fa"
            "-sm"
            "row"
            "--temp"
            "0.6"
            "--top-k"
            "20"
            "--top-p"
            "0.95"
            "--min-p"
            "0"
            "-c"
            "40960"
            "-n"
            "32768"
            "--no-context-shift"
          ];
        };
      };
    }
    {
      name = "Qwen3.6-27B-UD-Q4_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q4_K_XL.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      aliases = [ ];
      ttl = 900;
    }
    {
      name = "Qwen3.6-27B-UD-Q5_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q5_K_XL.gguf" ];
      };
      params = [
        "--cache-type-k"
        "q8_0"
        "--cache-type-v"
        "q8_0"
        "--parallel"
        "1"
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
      ];
      aliases = [
        "Qwen3.6-27B"
      ];
      ctxSize = 262144;
      ttl = 900;
      variants = {
        general-tasks = {
          params = [
            "--temp"
            "1.0"
            "--top-p"
            "0.95"
            "--top-k"
            "20"
            "--min-p"
            "0.00"
            "--presence-penalty"
            "1.5"
            "--chat-template-kwargs"
            "{\"enable_thinking\":true,\"preserve_thinking\":false}"
            # TODO: repetition_penalty=1.0
          ];
          aliases = [
            "hermes"
          ];
        };
        precise-coding-tasks = {
          params = [
            "--temp"
            "0.6"
            "--top-p"
            "0.95"
            "--top-k"
            "20"
            "--min-p"
            "0.00"
            "--presence-penalty"
            "0.0"
            "--chat-template-kwargs"
            "{\"enable_thinking\":true,\"preserve_thinking\":true}"
            # TODO: repetition_penalty=1.0
          ];
          aliases = [
            "opencode-fast"
          ];
        };
        instruct-general-tasks = {
          params = [
            "--temp"
            "0.7"
            "--top-p"
            "0.8"
            "--top-k"
            "20"
            "--min-p"
            "0.00"
            "--presence-penalty"
            "1.5"
            "--chat-template-kwargs"
            "{\"enable_thinking\":false}"
            # TODO: repetition_penalty=1.0
          ];
        };
        instruct-reasoning-tasks = {
          params = [
            "--temp"
            "1.0"
            "--top-p"
            "0.95"
            "--top-k"
            "20"
            "--min-p"
            "0.00"
            "--presence-penalty"
            "1.5"
            "--chat-template-kwargs"
            "{\"enable_thinking\":false}"
            # TODO: repetition_penalty=1.0
          ];
        };
        modded = {
          params = [
            "--threads"
            "4"
            "-np"
            "1"
            "--temp"
            "0.6"
            "--top-p"
            "0.95"
            "--top-k"
            "20"
            "--min-p"
            "0.0"
          ];
        };
      };
    }
    {
      name = "Qwen3.6-27B-UD-Q6_K_XL";
      path = "/models/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/Qwen3.6-27B-UD-Q6_K_XL.gguf" ];
      };
      params = [
        "--cache-type-k"
        "q8_0"
        "--cache-type-v"
        "q8_0"
        "--parallel"
        "1"
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      ctxSize = 196608;
      aliases = [ ];
      ttl = 900;
      variants = {
        "65536" = {
          ctxSize = 65536;
        };
        "131072" = {
          ctxSize = 131072;
        };
      };
    }
    {
      name = "gemma-4-31B-it-UD-Q5_K_XL";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q5_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q5_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      aliases = [
        "gemma-4-31B-Q5"
        "gemma-4-dense"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-31B-it-UD-Q4_K_XL";
      path = "/models/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [
          "unsloth/gemma-4-31B-it-GGUF/gemma-4-31B-it-UD-Q4_K_XL.gguf"
          # mmproj sidecars used by `variants.mmproj.mmproj` below.
          "unsloth/gemma-4-31B-it-GGUF/mmproj-F16.gguf"
          "unsloth/gemma-4-31B-it-GGUF/mmproj-BF16.gguf"
          "unsloth/gemma-4-31B-it-GGUF/mmproj-F32.gguf"
        ];
      };
      ctxSize = 65536;
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      variants = {
        mmproj = {
          mmproj = "/models/gemma-4-31B-it-GGUF/mmproj-F16.gguf";
        };
        nothink = {
          params = [
            "--chat-template-kwargs"
            "{\"enable_thinking\":false}"
          ];
        };
      };
      aliases = [
        "gemma-4-31B-Q4"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-26B-A4B-it-UD-Q6_K_XL";
      path = "/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q6_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q6_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      ctxSize = 262144;
      aliases = [
        "gemma-4-26B-Q6"
        "gemma-4-MoE"
      ];
      ttl = 300;
    }
    {
      name = "gemma-4-26B-A4B-it-UD-Q8_K_XL";
      path = "/models/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-26B-A4B-it-GGUF/gemma-4-26B-A4B-it-UD-Q8_K_XL.gguf" ];
      };
      params = [
        "--batch-size"
        "2048"
        "--ubatch-size"
        "512"
        "--threads"
        "1"
        "--jinja"
        "--temp"
        "1.0"
        "--top-p"
        "0.95"
        "--top-k"
        "64"
      ];
      aliases = [
        "gemma-4-26B-Q8"
      ];
      ttl = 300;
    }

  ];

  # Package built for the host with ROCm+Vulkan support (variant = "amd").
  # Passed into the container so it reuses the same binary instead of
  # falling back to the plain llama-cpp without GPU backends.
  host-llama-cpp-pkg = config.myconfig.ai.inference-cpp.llama-cpp.package;

  amdModels = [
    {
      name = "qwen3.5-122B-A10B-Q5_K_M";
      path = "/models/Qwen3.5-122B-A10B-GGUF/Q5_K_M/Qwen3.5-122B-A10B-Q5_K_M-00001-of-00003.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.5-122B-A10B-GGUF/Q5_K_M" ];
      };
      aliases = [
        "opencode-slow"
        "opencode-fallback"
        "qwen3.5-122B"
      ];
      ttl = 1800;
    }
    {
      name = "Qwen3.6-35B-A3B-Q8_0";
      path = "/models/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-Q8_0.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/Qwen3.6-35B-A3B-Q8_0.gguf" ];
      };
      params = [
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      variants = {
        thinking-general = {
          params = [
            "--temp" "1.0"
            "--top-p" "0.95"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "1.5"
            "--repetition-penalty" "1.0"
          ];
        };
        thinking-coding = {
          params = [
            "--temp" "0.6"
            "--top-p" "0.95"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "0.0"
            "--repetition-penalty" "1.0"
          ];
        };
        instruct-general = {
          params = [
            "--temp" "0.7"
            "--top-p" "0.8"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "1.5"
            "--repetition-penalty" "1.0"
          ];
        };
        instruct-reasoning = {
          params = [
            "--temp" "1.0"
            "--top-p" "1.0"
            "--top-k" "40"
            "--min-p" "0.0"
            "--presence-penalty" "2.0"
            "--repetition-penalty" "1.0"
          ];
        };
      };
      aliases = [
        "Qwen3.6-35B-A3B-Q8_0"
        "Qwen3.6-35B-A3B"
      ];
      ttl = 300;
    }
    {
      name = "Qwen3.6-35B-A3B-BF16";
      path = "/models/Qwen3.6-35B-A3B-GGUF/BF16/Qwen3.6-35B-A3B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-35B-A3B-GGUF/BF16" ];
      };
      params = [
        "-ctk"
        "f16"
        "-ctv"
        "f16"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
      variants = {
        thinking-general = {
          params = [
            "--temp" "1.0"
            "--top-p" "0.95"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "1.5"
            "--repetition-penalty" "1.0"
          ];
        };
        thinking-coding = {
          params = [
            "--temp" "0.6"
            "--top-p" "0.95"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "0.0"
            "--repetition-penalty" "1.0"
          ];
        };
        instruct-general = {
          params = [
            "--temp" "0.7"
            "--top-p" "0.8"
            "--top-k" "20"
            "--min-p" "0.0"
            "--presence-penalty" "1.5"
            "--repetition-penalty" "1.0"
          ];
        };
        instruct-reasoning = {
          params = [
            "--temp" "1.0"
            "--top-p" "1.0"
            "--top-k" "40"
            "--min-p" "0.0"
            "--presence-penalty" "2.0"
            "--repetition-penalty" "1.0"
          ];
        };
      };
      aliases = [
        "Qwen3.6-35B-A3B-BF16"
        "Qwen3.6-35B-A3B"
      ];
      ttl = 300;
    }
    {
      name = "Qwen3.6-27B-GGUF-BF16";
      path = "/models/Qwen3.6-27B-GGUF/BF16/Qwen3.6-27B-BF16-00001-of-00002.gguf";
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/Qwen3.6-27B-GGUF/BF16" ];
      };
      params = [
        "-ctk"
        "f16"
        "-ctv"
        "f16"
        "--chat-template-kwargs"
        "{\"preserve_thinking\":true}"
      ];
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
      pull-models = {
        target_directory = modelsPullDir;
        hf_spec = [ "unsloth/gemma-4-31B-it-GGUF/BF16" ];
      };
      params = [
        "-ctk"
        "f16"
        "-ctv"
        "f16"
      ];
      aliases = [
        "gemma-4-31B"
        "gemma-4-31B-BF16"
      ];
      ttl = 300;
    }
  ];

in
{
  imports = [
    ./services.llama-swap.vllm.nix
  ];
  config = {
    # The amdModels above are served by the `llama-cpp-33657` container,
    # not by the host's own llama-cpp. Their `pull-models` specs would
    # therefore not be picked up by the auto-collector that reads
    # `config.myconfig.ai.llama-cpp.models` at the host level. Surface
    # them here explicitly so `pull-models` on the host still downloads
    # them into ${modelsPullDir} (the container reads `/models/` via a
    # separate bind mount, which is out of scope for this helper).
    myconfig.ai.pull_models.models.${modelsPullDir} = lib.concatMap (m: m.pull-models.hf_spec) (
      builtins.filter (m: (m.pull-models or null) != null) amdModels
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
      };

      config =
        { ... }:
        {
          imports = [
            ../../modules/myconfig.ai/myconfig.ai.llama-cpp
            ../../modules/myconfig.ai/myconfig.localModels.nix
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
                    aliases,
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
