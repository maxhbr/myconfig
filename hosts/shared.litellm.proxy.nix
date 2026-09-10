# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared "local LiteLLM proxy" deployment. Imported by hosts that want a
# single, uniform, loopback-only access point to both remote and local
# models (currently f13 and p14).
#
# It listens on localhost:4000 and forwards all model requests to thing's
# LiteLLM via thing's Caddy reverse proxy
# (litellm.thing.wg0.maxhbr.local). This is the counterpart to
# hosts/shared.localModels.litellm.nix: instead of registering thing's
# LiteLLM as *direct* myconfig.ai.localModels providers (which the AI
# client configs would point at thing/vserver, bypassing any local
# proxy), this deploys a *local* LiteLLM whose model_list forwards to
# the same upstream. The two files share the model list defined in
# ./shared.localModels.litellm.models.nix.
#
# Why a local proxy instead of the direct providers?
#   - Single access point: tools/agents talk only to localhost:4000.
#   - Works for network-isolated agent users (e.g. f13's "offline" agent,
#     which can only reach loopback): the LiteLLM service runs as a
#     dynamic system user, so its outbound requests to the upstream are
#     not subject to the agent's iptables egress block.
#   - No credentials required: no master_key is configured, so LiteLLM
#     does not enforce API keys.
#
# Model names match those published by thing's LiteLLM (e.g.
# "gfx1151:hermes", "hermes", "opencode", …) so that tools and agents
# configured for thing work unchanged against this local proxy.
#
# ON TOP of the openai forward list, this file ADDS Anthropic model
# forwarding (see `anthropicModels` below): wildcard routes that let
# Claude-shaped clients (Claude Code hitting `/v1/messages`,
# OpenAI-spec clients requesting `anthropic/<model>`) reach an
# Anthropic-compatible upstream through the same single access point.
# The credentials are a SECRET and therefore live in the private
# repository: the `myconfig.secrets."litellm-anthropic-env"` stub below
# is only materialized (as an env-format file with
# `ANTHROPIC_AUTH_TOKEN=…` and optionally `ANTHROPIC_BASE_URL=…`) when
# the priv repo provides `source = ...`. Everything anthropic is gated
# on that, so a public/CI build (no priv repo) evaluates exactly as
# before — the additive entries only appear once the secret exists.
#
# A host importing this file must NOT also import
# hosts/shared.localModels.{litellm,rtx5090,gfx1151}.nix — those register
# direct providers that would bypass this proxy.
#
# Upstream: thing's Caddy reverse proxy, which forwards to thing's
# LiteLLM. The hostname resolves to thing's wg0 IP via /etc/hosts
# (see modules/myconfig.deployedServices/default.nix). thing is reachable
# directly over wg0, whether the importing host is on the home LAN or
# roaming.
#
# Keep the model list in sync by re-running
# ./hosts/shared.localModels.update.sh (regenerates
# ./shared.localModels.litellm.models.nix).
{
  config,
  lib,
  ...
}:
let
  # The anthropic forwarding turns on only when the priv repo has
  # provisioned the credential secret. `myconfig.secrets` filters out
  # entries without `source` (see modules/myconfig.secrets.nix), so
  # `config.age.secrets ? …` is the reliable presence probe both for
  # real hosts (with priv) and test builds (without priv).
  anthropicSecretName = "litellm-anthropic-env";
  haveAnthropicSecret = config.age.secrets ? ${anthropicSecretName};
in
{
  myconfig.ai.litellm.proxy = {
    enable = true;
    upstreamApiBase = "http://litellm.thing.wg0.maxhbr.local:80/v1";
    # The openai forward list, PLUS the anthropic wildcard entries once
    # the priv repo provisions the credential secret (see below). Exact
    # `model_name` matches always win over the wildcards, so existing
    # behavior is untouched either way.
    models =
      (import ./shared.localModels.litellm.models.nix)
      ++ (lib.optionals haveAnthropicSecret [
        {
          # Claude Code sends bare slugs (`claude-sonnet-4-5`, …) in the
          # `model` field of `/v1/messages`; route them upstream as
          # `anthropic/<slug>`.
          name = "claude-*";
          provider = "anthropic";
        }
        {
          # litellm-convention prefixed names for OpenAI-spec clients:
          # request `anthropic/<model>` → upstream `anthropic/<model>`.
          name = "*";
          modelName = "anthropic/*";
          provider = "anthropic";
        }
      ]);

    # Credentials (and optionally the upstream base URL) come from the
    # priv-provisioned env file — never from this repo. When the secret
    # is absent (public/CI builds), nothing is set and litellm would use
    # its defaults; the wildcard entries are absent too, so no request
    # can even reach the anthropic provider.
    anthropicAuthEnvironmentFile = lib.mkIf haveAnthropicSecret (
      config.myconfig.secrets."${anthropicSecretName}".dest
    );
  };
  # Secret stub: the env-format file (lines like
  # `ANTHROPIC_AUTH_TOKEN=…`, optionally `ANTHROPIC_BASE_URL=…`) is
  # provided by the priv repo via `source = ...`; without it, this stub
  # is inert (no file is created, no EnvironmentFile= is set, the
  # wildcard entries are absent).
  myconfig.secrets."${anthropicSecretName}" = {
    dest = "/run/agenix/${anthropicSecretName}";
  };
}
