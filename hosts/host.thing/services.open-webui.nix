# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Configure Open WebUI's "Manage OpenAI API Connections" with the local
# OpenAI-compatible endpoints running on this host:
#
#   * litellm  -> http://127.0.0.1:4000/v1
#   * rtx5090  -> http://127.0.0.1:33656/v1   (CUDA0, host service)
#   * gfx1151  -> http://127.0.0.1:33657/v1   (Vulkan0, nixos-container)
#
# Open WebUI reads these from the env vars OPENAI_API_BASE_URLS and
# OPENAI_API_KEYS (semicolon-separated, one entry per connection). The keys
# are mandatory placeholders for endpoints that don't require auth.
#
# Note: these settings are PersistentConfig in Open WebUI, i.e. they are
# read from the env on first start and then stored in the database. To make
# subsequent env changes take effect, either reset the connections in the
# admin UI or set ENABLE_PERSISTENT_CONFIG=False.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.open-webui;

  # `host` may be a wildcard (e.g. "0.0.0.0") for external exposure;
  # rewrite to localhost for in-host clients.
  litellmHost =
    if config.services.litellm.host == "0.0.0.0" then "localhost" else config.services.litellm.host;
  litellmUrl = "http://${litellmHost}:${toString config.services.litellm.port}/v1";
  rtx5090Url = "http://127.0.0.1:${toString config.myconfig.ai.llama-cpp.servicePort}/v1";
  gfx1151Url = "http://127.0.0.1:${toString config.containers.llama-cpp-33657.config.myconfig.ai.llama-cpp.servicePort}/v1";

  connections = [
    {
      url = litellmUrl;
      key = "sk-litellm-local";
    }
    {
      url = rtx5090Url;
      key = "sk-rtx5090-local";
    }
    {
      url = gfx1151Url;
      key = "sk-gfx1151-local";
    }
  ];

  joinSemi = lib.concatMapStringsSep ";";
in
{
  config = lib.mkIf (config.myconfig.ai.enable && cfg.enable) {
    services.open-webui.environment = {
      ENABLE_OPENAI_API = "True";
      OPENAI_API_BASE_URLS = joinSemi (c: c.url) connections;
      OPENAI_API_KEYS = joinSemi (c: c.key) connections;
    };
  };
}
