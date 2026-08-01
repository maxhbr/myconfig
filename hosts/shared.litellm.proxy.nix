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
# proxy), this deploys a *local* LiteLLM whose model_list forwards to the
# same upstream. The two files share the model list defined in
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
{ ... }:
{
  myconfig.ai.litellm.proxy = {
    enable = true;
    upstreamApiBase = "http://litellm.thing.wg0.maxhbr.local:80/v1";
    models = import ./shared.localModels.litellm.models.nix;
  };
}
