# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared bindings for the hermes-agent service and its NixOS-container /
# microvm variants. `service.nix` (native backend), `nixos-container.nix`
# and `microvm.nix` all import this file so the `services.hermes-agent`
# configuration (`hermesServiceCfg`) and the supporting paths/URLs are
# defined in exactly one place.
#
# This is a plain function (not a NixOS module): it is called with the host
# module arguments and returns an attrset of values. The same
# `import ./file { inherit ...; }` pattern is used by modules/myconfig.ai.dev/fns/
# and programs/programs.pi-coding-agent.
#
# All host-specific values (user, group, model, baseUrl, port, HASS URL,
# extraPackages, stateDir, secretsDir, toolsets, telegram) are read from
# `config.myconfig.ai.dev.hermes.*` options declared in service.nix. Override
# them per-host; the defaults live in the option declarations. The model
# route (`model.*`) sits at the top level of the tree because the sandbox
# tiers use it without running a service; everything else is under
# `service.*`.
#
# NOTE: `hermesServiceCfg.settings` declares `compression` twice (once with
# `summary_provider`/`summary_model`, once with `enabled`/`threshold`).
# This is intentional and works because the upstream `services.hermes-agent`
# `settings` option uses a deep-merge type (`recursiveUpdate`), which
# combines both blocks into a single
# `{ enabled, threshold, summary_provider, summary_model }` attrset —
# verified by force-enabling the module.
{
  config,
  lib,
  pkgs,
  myconfig,
  ...
}:
let
  hostConfig = config;
  cfg = config.myconfig.ai.dev.hermes;
  serviceCfg = cfg.service;

  stateDir = serviceCfg.stateDir;
  secretsDir = serviceCfg.secretsDir;
  litellmBaseUrl = cfg.model.baseUrl;

  # When the container backend is enabled, advertise the container's own
  # address to hermes; otherwise bind to localhost. `containers.hermes` is
  # defined by nixos-container.nix — this cross-module reference is safe
  # because NixOS option evaluation is lazy.
  apiServerHost = serviceCfg.apiServerHost;
  hermesServiceCfg = {
    enable = true;
    user = serviceCfg.user;
    group = serviceCfg.group;
    createUser = false;
    stateDir = stateDir;
    settings = {
      model = {
        default = cfg.model.default;
        provider = "custom";
        base_url = litellmBaseUrl;
        api_key = "local-key";
      };
      fallback_model = {
        model = cfg.model.fallback;
        provider = "custom";
        base_url = litellmBaseUrl;
        api_key = "local-key";
      };
      compression = {
        summary_provider = hermesServiceCfg.settings.model.provider;
        summary_model = hermesServiceCfg.settings.model.default;
      };
      custom_providers =
        let
          custom_local_providers = lib.concatMap (
            provider:
            let
              hostPort = "${provider.host}:${toString provider.port}";
              providerName = if provider.name != null then provider.name else hostPort;
              rawModels = if provider.models != [ ] then provider.models else [ providerName ];
              # localModels may contain plain strings or
              # `{ name, kind ? null }` submodules (kind is computed by
              # the publisher and unused here); flatten both shapes.
              modelNames = lib.map (m: if builtins.isAttrs m then m.name else m) rawModels;
            in
            lib.map (modelName: {
              name = "${providerName} / ${modelName}";
              base_url = "http://${hostPort}/v1";
              model = modelName;
              api_key = "local-key";
            }) modelNames
          ) config.myconfig.ai.llmops.localModels;
        in
        custom_local_providers;
      terminal.backend = "local";
      terminal.cwd = "${stateDir}/workspace";
      compression = {
        enabled = true;
        threshold = 0.85;
      };
      toolsets = serviceCfg.toolsets;
    }
    // (lib.optionalAttrs serviceCfg.telegram.enable {
      telegram = {
        require_mention = serviceCfg.telegram.requireMention;
      };
    });
    extraPackages = serviceCfg.extraPackages;
    environmentFiles =
      let
        hermes-api-env = (
          pkgs.writeText "hermes-api-env" ''
            OPENAI_API_KEY=local-key
            ${lib.optionalString (serviceCfg.apiServerPort != null && serviceCfg.apiServerHost != null) ''
              API_SERVER_ENABLED=true
              API_SERVER_PORT=${toString serviceCfg.apiServerPort}
              API_SERVER_HOST=${apiServerHost}
            ''}
            ${lib.optionalString (serviceCfg.hassUrl != null) "HASS_URL=${serviceCfg.hassUrl}"}
          ''
        );
      in
      [
        "${secretsDir}/env"
        "${hermes-api-env}"
      ];
    addToSystemPackages = true;
  };
in
{
  inherit
    hostConfig
    stateDir
    secretsDir
    cfg
    serviceCfg
    litellmBaseUrl
    apiServerHost
    hermesServiceCfg
    ;
}
