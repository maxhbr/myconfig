{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = lib.mkIf config.services.ollama.enable {
    services.ollama = {
      environmentVariables = {
        OLLAMA_KEEP_ALIVE = "5m";
      };
    };
    services.nextjs-ollama-llm-ui.enable = true;
    home.sharedModules = [
      {
        home.packages = with pkgs; [
          writeShellApplication {
            name = "ollama-bench";
            runtimeInputs = [ config.services.ollama.package ];
            text = ''
              set -euo pipefail
              model="$1"
              mkdir -p "$HOME/ollama-bench-results"
              ollama pull "$model"
              {
                ollama show "$model"
                bench -model "$model" -epochs 3 -temperature 0.7
              } | tee "$HOME/ollama-bench-results/${model}-$(date +%Y%m%d-%H%M%S).log"
            '';
          }
        ];
      }
    ];
  };
}
