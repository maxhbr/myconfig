{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai;
  generateModelConfig =
    model:
    "- model_id: ${model.host}:${toString model.port}\n"
    + "  model_name: ${
        if model.name != null then model.name else "${model.host}:${toString model.port}"
      }\n"
    + "  api_base: http://${model.host}:${toString model.port}/v1\n";
in
{
  options.myconfig = with lib; {
    ai.llm = {
      enable = mkEnableOption "myconfig.ai.llm";
    };
  };

  config = lib.mkIf config.myconfig.ai.llm.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          python313Packages.llm
          python313Packages.llm-ollama
        ];
        xdg.configFile."io.datasette.llm/extra-openai-models.yaml".text =
          if cfg.localModels != [ ] then lib.concatStrings (map generateModelConfig cfg.localModels) else "";
      }
    ];
  };
}
