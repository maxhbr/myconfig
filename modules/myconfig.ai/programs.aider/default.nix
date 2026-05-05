# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:

let
  osconfig = config;
  callLib = file: import file { inherit lib pkgs; };

  # Build a list of "providers", each describing one OpenAI-compatible base
  # URL plus the models exposed there. We then materialize them as aider
  # model settings entries that route via litellm's "openai/<model>" provider
  # but with extra_params.api_base/api_key overriding the endpoint.
  # LiteLLM uses ":" as a tag separator in model ids, so the alias side of
  # the name (which becomes the litellm model id) must not contain ":".
  # The upstream model name is sent verbatim to the server so it can keep ":".
  sanitize = builtins.replaceStrings [ ":" ] [ "_" ];

  mkProvider =
    {
      key,
      baseUrl,
      models,
    }:
    {
      inherit key baseUrl;
      models = lib.map (modelId: {
        upstreamModel = modelId;
        aiderName = "openai/${sanitize key}--${sanitize modelId}";
      }) models;
    };

  localModelProviders = lib.map (
    model:
    let
      hostPort = "${model.host}:${toString model.port}";
      providerName = if model.name != null then model.name else hostPort;
      modelNames =
        let
          raw = if model.models != [ ] then model.models else [ providerName ];
        in
        lib.map (m: if builtins.isAttrs m then m.name else m) raw;
    in
    mkProvider {
      key = "local-${providerName}";
      baseUrl = "http://${hostPort}/v1";
      models = modelNames;
    }
  ) osconfig.myconfig.ai.localModels;

  litellmProvider = lib.optional osconfig.services.litellm.enable (mkProvider {
    key = "litellm";
    baseUrl = "http://${osconfig.services.litellm.host}:${toString osconfig.services.litellm.port}/v1";
    models = lib.map (m: m.model_name) osconfig.services.litellm.settings.model_list;
  });

  ollamaProvider = lib.optional osconfig.services.ollama.enable (mkProvider {
    key = "ollama";
    baseUrl = "http://${osconfig.services.ollama.host}:${toString osconfig.services.ollama.port}/v1";
    models = osconfig.services.ollama.loadModels;
  });

  llamaSwapProvider = lib.optional osconfig.services.llama-swap.enable (mkProvider {
    key = "llama-swap";
    baseUrl = "http://localhost:${toString osconfig.services.llama-swap.port}/v1";
    models = builtins.attrNames osconfig.services.llama-swap.settings.models;
  });

  allProviders = localModelProviders ++ litellmProvider ++ ollamaProvider ++ llamaSwapProvider;

  # Flat list of all (provider, model) pairs.
  allModelEntries = lib.concatMap (
    p:
    lib.map (m: {
      inherit (p) baseUrl;
      inherit (m) aiderName upstreamModel;
    }) p.models
  ) allProviders;

  # Prefer "clean" upstream model names (plain aliases like "opencode")
  # when picking a default to avoid ending up on device-prefixed entries
  # like "Vulkan0:foo".
  defaultModelEntry =
    let
      isCleanName = e: !(lib.hasInfix ":" e.upstreamModel);
      clean = lib.filter isCleanName allModelEntries;
    in
    if clean != [ ] then builtins.head clean else builtins.head allModelEntries;

  # ~/.aider.model.settings.yml content. Aider uses LiteLLM under the hood;
  # by giving the model a name like "openai/<unique>" and setting
  # extra_params.api_base/api_key/model, LiteLLM hits our local server.
  modelSettings = lib.map (e: {
    name = e.aiderName;
    edit_format = "diff";
    use_repo_map = true;
    examples_as_sys_msg = true;
    extra_params = {
      api_base = e.baseUrl;
      api_key = "DUMMY_API_KEY";
      # Tell litellm to forward the upstream model id, not our alias.
      model = e.upstreamModel;
      max_tokens = 4096;
    };
  }) allModelEntries;

  yamlFormat = pkgs.formats.yaml { };
  modelSettingsYaml = yamlFormat.generate "aider.model.settings.yml" modelSettings;

  # ~/.aider.model.metadata.json: gives litellm context-window/cost info for
  # the unknown models we register. Without this, aider prints noisy warnings.
  modelMetadata = lib.listToAttrs (
    lib.map (e: {
      name = e.aiderName;
      value = {
        max_tokens = 4096;
        max_input_tokens = 128000;
        max_output_tokens = 4096;
        input_cost_per_token = 0;
        output_cost_per_token = 0;
        litellm_provider = "openai";
        mode = "chat";
      };
    }) allModelEntries
  );

  modelMetadataJson = pkgs.writeText "aider.model.metadata.json" (builtins.toJSON modelMetadata);

  aiderBwrap = callLib ../fns/sandboxed-app.nix {
    name = "aider";
    pkg = pkgs.aider-chat;
    envVars = {
      AIDER_ANALYTICS_DISABLE = "1";
    };
  };
in
{
  options.myconfig = with lib; {
    ai.aider = {
      enable = mkEnableOption "myconfig.ai.aider";
    };
  };
  config = lib.mkIf config.myconfig.ai.aider.enable {
    home-manager.sharedModules = [
      {
        # Use the upstream home-manager module for ~/.aider.conf.yml.
        # See: https://github.com/nix-community/home-manager/blob/master/modules/programs/aider-chat.nix
        programs.aider-chat = {
          enable = true;
          settings = lib.mkMerge [
            {
              model-settings-file = "~/.aider.model.settings.yml";
              model-metadata-file = "~/.aider.model.metadata.json";
              check-update = false;
              analytics-disable = true;
              show-model-warnings = false;
              auto-commits = false;
              dirty-commits = true;
              gitignore = true;
              pretty = true;
              stream = true;
              suggest-shell-commands = false;
              notifications = false;
            }
            # Pick a reasonable default model when one is available.
            # Use mkDefault so per-host configs can override without conflict.
            (lib.mkIf (allModelEntries != [ ]) {
              model = lib.mkDefault defaultModelEntry.aiderName;
            })
          ];
        };

        # Aider writes per-repo files (.aider.tags.cache.v4, .aider.input.history,
        # .aider.chat.history.md, .aider.llm.history) into the project root, not
        # $HOME, so they don't need to be persisted here.
        home.file.".aider.model.settings.yml".source = modelSettingsYaml;
        home.file.".aider.model.metadata.json".source = modelMetadataJson;
        home.packages = [
          aiderBwrap
          (pkgs.writeShellApplication {
            name = "aider-tmp";
            runtimeInputs = with pkgs; [ coreutils ];
            text = ''
              cd "$(mktemp -d)" && exec ${lib.getExe aiderBwrap} "$@"
            '';
          })
          (pkgs.writeShellApplication {
            name = "aider-worktree";
            runtimeInputs = with pkgs; [
              git
              coreutils
            ];
            text = ''
              if [ ! -d .git ]; then
                echo "Error: Not in a git repository root"
                exit 1
              fi

              timestamp=$(date +%s)
              dirname=$(basename "$(pwd)")
              worktree_name="''${dirname}-aider-''${timestamp}"
              branch_name="aider-''${timestamp}"

              git worktree add -b "''${branch_name}" "../''${worktree_name}" || exit 1
              cd "../''${worktree_name}" && exec ${lib.getExe aiderBwrap} "$@"
            '';
          })
        ];
      }
    ];
  };
}
