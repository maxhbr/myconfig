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

  mkForwardEntry = modelName: {
    model_name = modelName;
    litellm_params = {
      model = "openai/${modelName}";
      api_base = cfg.upstreamApiBase;
      api_key = cfg.apiKey;
      request.allowPrivateNetwork = true;
    };
  };
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
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Model names to forward. Each becomes an `openai/<model>`
        entry pointing at the upstream API base.
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
