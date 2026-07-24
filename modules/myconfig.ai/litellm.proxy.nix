# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# "Remote LiteLLM proxy" pattern: forward all model requests to an
# upstream LiteLLM instance (e.g. thing's LiteLLM via vserver's Caddy
# reverse proxy).
#
# The proxy requires no authentication (no master_key is configured, so
# LiteLLM does not enforce API keys). This allows agent users —
# including the network-isolated "offline" agent, which can only reach
# loopback — to use AI models without credentials. The LiteLLM service
# runs as a dynamic system user, so its outbound requests to the
# upstream are not subject to the offline agent's iptables egress block.
#
# Because a host using this pattern does not register any localModels
# providers, the auto-generated model_list in
# modules/myconfig.ai/services.litellm.nix (a mkDefault) is empty.
# This module supplies the model_list directly with raw-name
# pass-through forward entries. The plain `=` (priority 100) still
# *merges* (concatenates) with other priority-100 contributors — most
# importantly the skainet/ and trustedtokens/ entries registered by
# the tng.nix flake modules (which also use a plain `=`). Using
# `mkForce` (priority 50) would silently discard those contributors.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.litellm.proxy;

  # A model spec is either a bare string (the model name, no metadata) or
  # an attrset { name; contextWindow?; maxOutputTokens?; }. The attrset
  # form lets a caller advertise the model's context window so LiteLLM can
  # do context_window fallback routing and clients can size prompts. These
  # numbers are *not* derivable by LiteLLM from the upstream (its
  # /v1/models is a fixed OpenAI-schema list and the upstream /model/info
  # reports null for these local models); they are scraped from the
  # backends' llama-server `--ctx-size` by
  # hosts/shared.localModels.update.sh. They surface on the proxy's
  # /model/info endpoint (model_info.max_input_tokens /
  # max_output_tokens), never on /v1/models.
  #
  # `contextWindow` deliberately mirrors the field of the same name on
  # myconfig.ai.localModels[].models, so the shared model list
  # (hosts/shared.localModels.litellm.models.nix) can be consumed by both
  # this module and hosts/shared.localModels.litellm.nix unchanged.
  mkForwardEntry =
    m:
    let
      spec = if lib.isString m then { name = m; } else m;
      # Drop unset (null) fields so the string form and metadata-less
      # attrsets produce byte-identical output to the previous behavior.
      modelInfo = lib.filterAttrs (_: v: v != null) {
        max_input_tokens = spec.contextWindow or null;
        max_output_tokens = spec.maxOutputTokens or null;
      };
    in
    {
      model_name = spec.name;
      litellm_params = {
        model = "openai/${spec.name}";
        api_base = cfg.upstreamApiBase;
        api_key = cfg.apiKey;
        request.allowPrivateNetwork = true;
      };
    }
    // lib.optionalAttrs (modelInfo != { }) { model_info = modelInfo; };
in
{
  options.myconfig.ai.litellm.proxy = {
    enable = lib.mkEnableOption "a remote LiteLLM proxy that forwards all models to an upstream LiteLLM instance";

    upstreamApiBase = lib.mkOption {
      type = lib.types.str;
      description = ''
        Upstream LiteLLM API base URL (e.g.
        `http://litellm.thing.vserver.wg0.maxhbr.local:80/v1`).
        Each model is forwarded as `openai/<model>` pointing at this
        base.
      '';
    };

    models = lib.mkOption {
      type =
        with lib.types;
        listOf (
          either str (submodule {
            options = {
              name = lib.mkOption {
                type = str;
                description = "Model name to forward (becomes `openai/<name>`).";
              };
              contextWindow = lib.mkOption {
                type = nullOr int;
                default = null;
                description = ''
                  Context window in tokens. Emitted as
                  `model_info.max_input_tokens`. Scraped from the
                  backend's llama-server `--ctx-size` by
                  hosts/shared.localModels.update.sh; LiteLLM cannot
                  derive it from the upstream. Mirrors the field of the
                  same name on `myconfig.ai.localModels[].models`.
                '';
              };
              maxOutputTokens = lib.mkOption {
                type = nullOr int;
                default = null;
                description = "Max output tokens. Emitted as `model_info.max_output_tokens`.";
              };
            };
          })
        );
      default = [ ];
      description = ''
        Models to forward. Each entry is either a bare model-name string
        or an attrset `{ name; contextWindow?; maxOutputTokens?; }`.
        Each becomes an `openai/<name>` entry pointing at the upstream
        API base; the optional fields add a `model_info` block.
      '';
    };

    apiKey = lib.mkOption {
      type = lib.types.str;
      default = "not-needed";
      description = ''
        API key for the upstream LiteLLM instance. Defaults to
        `"not-needed"` since the upstream typically does not enforce
        authentication.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    services.litellm = {
      enable = true;
      settings.model_list = map mkForwardEntry cfg.models;
    };
  };
}
