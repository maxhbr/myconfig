{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.myconfig = with lib; {
    ai.aichat = {
      enable = mkEnableOption "myconfig.ai.aichat";
    };
  };
  config = lib.mkIf config.myconfig.ai.aichat.enable {
    home-manager.sharedModules = [
      {
        programs.aichat = {
          enable = true;
          package = pkgs.aichat;
          settings = {
            model = "litellm:Qwen/Qwen3-4B";
            clients =
              lib.optionals config.services.litellm.enable [
                {
                  type = "openai-compatible";
                  name = "litellm";
                  api_base = "http://localhost:${toString config.services.litellm.port}/v1";
                  models = builtins.map (model: {
                    name = model.model_name;
                  }) config.services.litellm.settings.model_list;
                }
              ]
              ++ lib.optionals config.services.ollama.enable [
                {
                  type = "openai-compatible";
                  name = "ollama";
                  api_base = "http://${config.services.ollama.host}:${toString config.services.ollama.port}/v1";
                  models = builtins.map (model: {
                    name = model;
                  }) config.services.ollama.loadModels;
                }
              ]
              ++ lib.optionals (config.myconfig.ai.localModels != [ ]) (
                map (
                  model:
                  let
                    hostPort = "${model.host}:${toString model.port}";
                    providerName = if model.name != null then model.name else hostPort;
                    modelNames = if model.models != [ ] then model.models else [ providerName ];
                  in
                  {
                    type = "openai-compatible";
                    name = "local-${providerName}";
                    api_base = "http://${hostPort}/v1";
                    models = map (modelName: { name = modelName; }) modelNames;
                  }
                ) config.myconfig.ai.localModels
              );
          };
        };
      }
    ];
  };
}
