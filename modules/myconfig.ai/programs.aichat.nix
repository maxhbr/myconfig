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
            model = "localhost:GLM-4-Flash";
            clients = [
              {
                type = "openai-compatible";
                name = "localhost";
                api_base = "http://localhost:4000/v1";
                models = [
                  {
                    name = "GLM-4-Flash";
                  }
                  {
                    name = "Qwen3-Coder-Next";
                  }
                ];
              }
              {
                type = "openai-compatible";
                name = "ollama";
                api_base = "http://localhost:11434/v1";
                models = [
                ];
              }
            ];
          };
        };
      }
    ];
  };
}
