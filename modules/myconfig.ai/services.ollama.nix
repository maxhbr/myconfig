{ pkgs, lib, config, ... }: {
  config = lib.mkIf config.services.ollama.enable {
    services.ollama = { environmentVariables = { OLLAMA_KEEP_ALIVE = "5m"; }; };
    services.nextjs-ollama-llm-ui.enable = true;
  };
}
