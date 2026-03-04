{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.myconfig.ai.ollama;
  ollamaSrc = if cfg.useOllamaFromMaster then pkgs.master else pkgs.nixos-unstable-small;
in
{
  options.myconfig.ai.ollama = with lib; {
    useOllamaFromMaster = mkEnableOption "use ollama from nixpkgs master instead of nixos-unstable-small";
  };
  imports =
    let
      gpuVariants = config.myconfig.hardware.gpu.variant;
      hasVariant = v: builtins.elem v gpuVariants;
    in
    [
      # Ollama package selection: pick the best available GPU backend.
      # Priority: nvidia > amd (rocm) > amd-no-rocm (vulkan).
      # Only one package can be set, so use nested mkIf to ensure exclusivity.
      ({
        config = lib.mkIf (hasVariant "nvidia") {
          services.ollama.package = pkgs.ollama-cuda;
        };
      })
      ({
        config = lib.mkIf (hasVariant "amd" && !hasVariant "nvidia") {
          services.ollama = {
            package = pkgs.ollama-rocm;
            environmentVariables = {
              OLLAMA_LLM_LIBRARY = "rocm";
            };
          };
        };
      })
      ({
        config = lib.mkIf (hasVariant "amd-no-rocm" && !hasVariant "nvidia" && !hasVariant "amd") {
          services.ollama = {
            package = pkgs.ollama-vulkan;
            environmentVariables = {
              OLLAMA_VULKAN = "1";
              RADV_PERFTEST = "sam";
            };
          };
        };
      })

    ];
  config = lib.mkIf config.services.ollama.enable {
    nixpkgs.overlays = [
      (_: prev: {
        ollama = ollamaSrc.ollama;
        ollama-cuda = ollamaSrc.ollama-cuda or prev.ollama-cuda;
        ollama-rocm = ollamaSrc.ollama-rocm or prev.ollama-rocm;
        ollama-vulkan = ollamaSrc.ollama-vulkan or prev.ollama-vulkan;
      })
    ];
    services.ollama = {
      environmentVariables = {
        OLLAMA_KEEP_ALIVE = "5m";
        OLLAMA_FLASH_ATTENTION = "1";
        # OLLAMA_DEBUG = "1";
      };
    };
    services.nextjs-ollama-llm-ui.enable = true;
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          (writeShellApplication {
            name = "ollama-bench";
            runtimeInputs = [ config.services.ollama.package ];
            text = ''
              set -euo pipefail
              model="$1"
              logdir="$HOME/ollama-bench-results"
              mkdir -p "$logdir"
              ollama pull "$model"
              logfile="$logdir/''${model//[^a-zA-Z0-9]/_}-$(date +%Y%m%d-%H%M%S).log"

              {
                ollama show "$model"
                bench -model "$model" -epochs 3 -temperature 0.7
              } | tee "$logfile"
            '';
          })
        ];
      }
    ];
  };
}
