{
  config,
  lib,
  pkgs,
  ...
}:
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
        xdg.configFile."io.datasette.llm/extra-openai-models.yaml".text = ''
          - model_id: localhost:22545
            model_name: localhost:22545
            api_base: http://127.0.0.1:22545/v1
        '';
      }
    ];
  };
}
