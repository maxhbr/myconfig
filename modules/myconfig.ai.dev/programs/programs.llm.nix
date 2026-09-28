{
  config,
  lib,
  pkgs,
  ...
}:
let
  # `myconfig.ai.llmops.localModels` is NOT a dev-tooling option (it is
  # defined by the llmops tree, see
  # ../../myconfig.ai.llmops/myconfig.ai.llmops.localModels.nix), so this
  # binding intentionally reads the llmops attrset.
  cfg = config.myconfig.ai.llmops;
  generateModelConfig =
    model:
    let
      hostPort = "${model.host}:${toString model.port}";
      providerName = if model.name != null then model.name else hostPort;
      # localModels may contain plain strings or `{ name, kind ? null }`
      # submodules (computed kind tag is unused here); flatten both shapes
      # to a string list.
      rawModels = if model.models != [ ] then model.models else [ providerName ];
      modelNames = lib.map (m: if builtins.isAttrs m then m.name else m) rawModels;
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
    ai.dev.llm = {
      enable = mkEnableOption "myconfig.ai.dev.llm";
    };
  };

  config = lib.mkIf config.myconfig.ai.dev.llm.enable {
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          python313Packages.llm
        ];
        xdg.configFile."io.datasette.llm/extra-openai-models.yaml".text =
          if cfg.localModels != [ ] then lib.concatStrings (map generateModelConfig cfg.localModels) else "";
      }
    ];
  };
}
