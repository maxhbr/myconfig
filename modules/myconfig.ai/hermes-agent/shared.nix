# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared bindings for the hermes-agent service and its NixOS-container
# variant. Both `service.nix` (native backend) and `nixos-container.nix`
# (container backend) import this file so the `services.hermes-agent`
# configuration (`hermesServiceCfg`) and the supporting paths/URLs are
# defined in exactly one place.
#
# This is a plain function (not a NixOS module): it is called with the host
# module arguments and returns an attrset of values. The same `import ./file
# { inherit ...; }` pattern is used by modules/myconfig.ai/fns/ and
# programs.pi-coding-agent.
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
  stateDir = "/home/mhuber/hermes-agent";

  cfg = config.myconfig.ai.hermes;

  # LiteLLM listens on the wg0 IP of `thing` (port 4000) — see
  # hosts/host.thing/default.nix. Connect directly, no Caddy in the path.
  litellmBaseUrl = "http://${myconfig.metadatalib.getWgIp "thing"}:4000/v1";

  # When the container backend is enabled, advertise the container's own
  # address to hermes; otherwise bind to localhost. `containers.hermes` is
  # defined by nixos-container.nix — this cross-module reference is safe
  # because NixOS option evaluation is lazy.
  apiServerHost = if cfg.container.enable then config.containers.hermes.localAddress else "localhost";
  hermesServiceCfg = {
    enable = true;
    user = "mhuber";
    group = "mhuber";
    createUser = false;
    stateDir = stateDir;
    settings = {
      model = {
        default = "hermes";
        provider = "custom";
        base_url = litellmBaseUrl;
        api_key = "local-key";
      };
      fallback_model = {
        model = "hermes-fallback";
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
          ) config.myconfig.ai.localModels;
        in
        custom_local_providers;
      terminal.backend = "local";
      terminal.cwd = "${stateDir}/workspace";
      compression = {
        enabled = true;
        threshold = 0.85;
      };
      toolsets = [ "all" ];
      telegram = {
        require_mention = true;
      };
    };
    extraPackages = with pkgs; [ openhue-cli ];
    environmentFiles =
      let
        hermes-api-env = (
          pkgs.writeText "hermes-api-env" ''
            OPENAI_API_KEY=local-key
            API_SERVER_ENABLED=true
            API_SERVER_PORT=8642
            API_SERVER_HOST=${apiServerHost}
            HASS_URL=http://hass.nuc.wg0.maxhbr.local
          ''
        );
      in
      [
        "/home/mhuber/.hermes-secrets/env"
        "${hermes-api-env}"
      ];
    addToSystemPackages = true;
  };
in
{
  inherit
    hostConfig
    stateDir
    cfg
    litellmBaseUrl
    apiServerHost
    hermesServiceCfg
    ;
}
