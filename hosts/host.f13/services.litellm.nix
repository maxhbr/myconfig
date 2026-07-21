# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Local LiteLLM proxy on f13. Listens on localhost:4000 and forwards
# all model requests to thing's LiteLLM via vserver's Caddy reverse
# proxy (litellm.thing.vserver.wg0.maxhbr.local).
#
# The proxy requires no authentication (no master_key is configured, so
# LiteLLM does not enforce API keys). This allows agent users —
# including the network-isolated "offline" agent, which can only reach
# loopback — to use AI models without credentials. The LiteLLM service
# runs as a dynamic system user, so its outbound requests to vserver
# are not subject to the offline agent's iptables egress block.
#
# Model names match those published by thing's LiteLLM (e.g.
# "gfx1151:hermes", "hermes", "opencode", …) so that tools and agents
# configured for thing work unchanged against this local proxy.
#
# f13 does NOT import hosts/shared.localModels.litellm.nix. That shared
# file registers thing's LiteLLM as myconfig.ai.localModels providers,
# which (via modules/myconfig.ai/programs.{aichat,opencode,...}) would
# create *direct* provider entries in the AI client configs pointing
# at thing/vserver — bypassing this local proxy and breaking for the
# offline agent, which can only reach loopback. f13's own LiteLLM proxy
# (exposed as the `litellm` provider at localhost:4000) is the single
# access point, so the shared localModels providers are not wanted here.
#
# The model list below mirrors the `models` attribute of the
# `litellm.thing.wg0` provider in hosts/shared.localModels.litellm.nix
# (the output of `curl http://thing.wg0.maxhbr.local:4000/models`).
# Keep it in sync by re-running ./hosts/shared.localModels.update.sh
# and copying the `litellm` model list here.
#
# Upstream: vserver's Caddy reverse proxy, which forwards to thing's
# LiteLLM. The hostname resolves to vserver's wg0 IP via /etc/hosts
# (see modules/myconfig.deployedServices/default.nix). vserver is the
# WireGuard rendezvous host and always reachable by roaming peers
# (f13 is a roaming laptop); thing itself is only directly reachable
# when f13 is on the home LAN (ghost-peer probe), so routing via
# vserver is the robust choice.
{ ... }:
{
  myconfig.ai.litellm.proxy = {
    enable = true;
    upstreamApiBase = "http://litellm.thing.vserver.wg0.maxhbr.local:80/v1";
    models = [
      "gfx1151:Hy3-Q2_K_L"
      "gfx1151:InternScience-Agents-A1-Q4_K_M"
      "gfx1151:InternScience-Agents-A1-Q8_0"
      "gfx1151:MiniMax-M2.7-UD-IQ4_NL"
      "gfx1151:MiniMax-M2.7-UD-IQ4_NL-196k"
      "gfx1151:MiniMax-M2.7-UD-IQ4_NL-49k"
      "gfx1151:MiniMax-M2.7-UD-IQ4_XS"
      "gfx1151:MiniMax-M2.7-UD-IQ4_XS-196k"
      "gfx1151:MiniMax-M2.7-UD-IQ4_XS-49k"
      "gfx1151:MiniMax-M2.7-UD-Q3_K_S"
      "gfx1151:MiniMax-M2.7-UD-Q3_K_S-196k"
      "gfx1151:MiniMax-M2.7-UD-Q3_K_S-49k"
      "gfx1151:NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
      "gfx1151:NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
      "gfx1151:Ornith-1.0-35B-Q8_0"
      "gfx1151:Qwen3-235B-A22B-Instruct-Q2_K_L"
      "gfx1151:Qwen3.5-9B-Q5_K_M"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0-general-tasks"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
      "gfx1151:Qwen3.6-27B-MTP-Q8_0-q8_0"
      "gfx1151:Qwen3.6-27B-Q6_K-MTP"
      "gfx1151:Qwen3.6-27B-Q8_0"
      "gfx1151:Qwen3.6-27B-Q8_0-general-tasks"
      "gfx1151:Qwen3.6-27B-Q8_0-instruct-general-tasks"
      "gfx1151:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
      "gfx1151:Qwen3.6-27B-Q8_0-precise-coding-tasks"
      "gfx1151:Qwen3.6-27B-Q8_0-q8_0"
      "gfx1151:Qwen3.6-27B-UD-Q4_K_XL"
      "gfx1151:Qwen3.6-27B-UD-Q5_K_XL"
      "gfx1151:Qwen3.6-27B-UD-Q6_K_XL"
      "gfx1151:Qwen3.6-35B-A3B-BF16"
      "gfx1151:Qwen3.6-35B-A3B-BF16-instruct-general"
      "gfx1151:Qwen3.6-35B-A3B-BF16-instruct-reasoning"
      "gfx1151:Qwen3.6-35B-A3B-BF16-thinking-coding"
      "gfx1151:Qwen3.6-35B-A3B-BF16-thinking-general"
      "gfx1151:Qwen3.6-35B-A3B-MTP-BF16"
      "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
      "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
      "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
      "gfx1151:Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
      "gfx1151:Qwen3.6-35B-A3B-Q8_0"
      "gfx1151:Qwen3.6-35B-A3B-Q8_0-instruct-general"
      "gfx1151:Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
      "gfx1151:Qwen3.6-35B-A3B-Q8_0-thinking-coding"
      "gfx1151:Qwen3.6-35B-A3B-Q8_0-thinking-general"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K_XL"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-general"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-reasoning"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-coding"
      "gfx1151:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-general"
      "gfx1151:ROCm0:Hy3-Q2_K_L"
      "gfx1151:ROCm0:InternScience-Agents-A1-Q4_K_M"
      "gfx1151:ROCm0:InternScience-Agents-A1-Q8_0"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL-196k"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_NL-49k"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS-196k"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-IQ4_XS-49k"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S-196k"
      "gfx1151:ROCm0:MiniMax-M2.7-UD-Q3_K_S-49k"
      "gfx1151:ROCm0:NVIDIA-Nemotron-3-Nano-Omni-Q8_0"
      "gfx1151:ROCm0:NVIDIA-Nemotron-3-Super-120B-A12B-Q5_K_M"
      "gfx1151:ROCm0:Ornith-1.0-35B-Q8_0"
      "gfx1151:ROCm0:Qwen3-235B-A22B-Instruct-Q2_K_L"
      "gfx1151:ROCm0:Qwen3.5-9B-Q5_K_M"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-general-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-general-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-instruct-reasoning-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-precise-coding-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-MTP-Q8_0-q8_0"
      "gfx1151:ROCm0:Qwen3.6-27B-Q6_K-MTP"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-general-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-general-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-instruct-reasoning-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-precise-coding-tasks"
      "gfx1151:ROCm0:Qwen3.6-27B-Q8_0-q8_0"
      "gfx1151:ROCm0:Qwen3.6-27B-UD-Q4_K_XL"
      "gfx1151:ROCm0:Qwen3.6-27B-UD-Q5_K_XL"
      "gfx1151:ROCm0:Qwen3.6-27B-UD-Q6_K_XL"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-instruct-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-instruct-reasoning"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-thinking-coding"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-BF16-thinking-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-instruct-reasoning"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-coding"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-MTP-BF16-thinking-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-instruct-reasoning"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-coding"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-Q8_0-thinking-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q5_K_XL"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-general"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-instruct-reasoning"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-coding"
      "gfx1151:ROCm0:Qwen3.6-35B-A3B-UD-Q8_K_XL-MTP-thinking-general"
      "gfx1151:ROCm0:TheDrummer_Skyfall-31B-v4.2-Q6_K"
      "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q6_K_XL"
      "gfx1151:ROCm0:gemma-4-26B-A4B-it-UD-Q8_K_XL"
      "gfx1151:ROCm0:gemma-4-26B-A4B-it-qat-q4_0"
      "gfx1151:ROCm0:gemma-4-31B-it-BF16"
      "gfx1151:ROCm0:gemma-4-31B-it-UD-Q4_K_XL"
      "gfx1151:ROCm0:gemma-4-31B-it-UD-Q5_K_XL"
      "gfx1151:ROCm0:gemma-4-31B-it-qat-q4_0"
      "gfx1151:ROCm0:qwen3.5-122B-A10B-Q5_K_M"
      "gfx1151:TheDrummer_Skyfall-31B-v4.2-Q6_K"
      "gfx1151:gemma-4-26B-A4B-it-UD-Q6_K_XL"
      "gfx1151:gemma-4-26B-A4B-it-UD-Q8_K_XL"
      "gfx1151:gemma-4-26B-A4B-it-qat-q4_0"
      "gfx1151:gemma-4-31B-it-BF16"
      "gfx1151:gemma-4-31B-it-UD-Q4_K_XL"
      "gfx1151:gemma-4-31B-it-UD-Q5_K_XL"
      "gfx1151:gemma-4-31B-it-qat-q4_0"
      "gfx1151:hermes"
      "gfx1151:hermes-fallback"
      "gfx1151:opencode"
      "gfx1151:opencode-fallback"
      "gfx1151:opencode-fast"
      "gfx1151:opencode-fast-fallback"
      "gfx1151:opencode-slow-fallback"
      "gfx1151:qwen3.5-122B-A10B-Q5_K_M"
      "gfx1151:sidekick"
      "hermes"
      "hermes-fallback"
      "localhost:22545:localhost:22545"
      "localhost:22546:localhost:22546"
      "opencode"
      "opencode-fallback"
      "opencode-fast"
      "opencode-fast-fallback"
      "opencode-slow-fallback"
      "rtx5090:InternScience-Agents-A1-Q4_K_M"
      "rtx5090:InternScience-Agents-A1-Q4_K_M-mmproj"
      "rtx5090:Qwen3.5-9B-Q5_K_M"
      "rtx5090:Qwen3.6-27B-Q6_K-MTP"
      "rtx5090:Qwen3.6-27B-Q6_K-MTP-full-ctx"
      "rtx5090:Qwen3.6-27B-UD-Q4_K_XL"
      "rtx5090:Qwen3.6-27B-UD-Q5_K_XL"
      "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-general-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-general-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-instruct-reasoning-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q5_K_XL-precise-coding-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q6_K_XL"
      "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-general-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-instruct-general-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-instruct-reasoning-tasks"
      "rtx5090:Qwen3.6-27B-UD-Q6_K_XL-precise-coding-tasks"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-general"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-instruct-reasoning"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-coding"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-MTP-thinking-general"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-general"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-instruct-reasoning"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-coding"
      "rtx5090:Qwen3.6-35B-A3B-UD-Q5_K_XL-thinking-general"
      "rtx5090:TheDrummer_Skyfall-31B-v4.2-Q6_K"
      "rtx5090:gemma-4-26B-A4B-it-UD-Q6_K_XL"
      "rtx5090:gemma-4-26B-A4B-it-UD-Q8_K_XL"
      "rtx5090:gemma-4-26B-A4B-it-qat-q4_0"
      "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-mmproj"
      "rtx5090:gemma-4-26B-A4B-it-qat-q4_0-nothink"
      "rtx5090:gemma-4-31B-it-UD-Q4_K_XL"
      "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-mmproj"
      "rtx5090:gemma-4-31B-it-UD-Q4_K_XL-nothink"
      "rtx5090:gemma-4-31B-it-UD-Q5_K_XL"
      "rtx5090:gemma-4-31B-it-qat-q4_0"
      "rtx5090:gemma-4-31B-it-qat-q4_0-mmproj"
      "rtx5090:gemma-4-31B-it-qat-q4_0-nothink"
      "rtx5090:hermes"
      "rtx5090:hermes-fallback"
      "rtx5090:opencode"
      "rtx5090:opencode-fallback"
      "rtx5090:opencode-fast-fallback"
      "rtx5090:sidekick"
      "sidekick"
    ];
  };
}
