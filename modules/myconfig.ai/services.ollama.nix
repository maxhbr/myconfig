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
