{ pkgs, lib, config, ... }:
{
  config = lib.mkIf config.services.ollama.enable {
    services.ollama = {
      # environmentVariables = {
      #   OLLAMA_LLM_LIBRARY = "cpu";
      #   HIP_VISIBLE_DEVICES = "0,1";
      # };
    };
    services.nextjs-ollama-llm-ui.enable = true;
  };
}
