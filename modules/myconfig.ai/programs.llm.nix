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
    let
      hostPort = "${model.host}:${toString model.port}";
      providerName = if model.name != null then model.name else hostPort;
      modelNames = if model.models != [ ] then model.models else [ providerName ];
    in
    lib.concatStrings (
      lib.map (
        modelName:
        "- model_id: ${hostPort}/${modelName}\n"
        + "  model_name: ${modelName}\n"
        + "  api_base: http://${hostPort}/v1\n"
      ) modelNames
    );
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
