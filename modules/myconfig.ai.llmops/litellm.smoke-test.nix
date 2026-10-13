# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `litellm-smoke-test`: a helper script that smoke-tests models
# deployed on a LiteLLM server (default http://localhost:4000). It
# discovers the models via GET /v1/models and is incremental: models
# recorded in a state file by a previous run are skipped, so only new
# models are probed (--force retests everything). Each probed model
# gets one minimal chat-completion request, with retries and
# exponential backoff to give on-demand backends (llama-swap /
# llama-server router behind thing's LiteLLM) time to load the model,
# and a CSV report plus a log file are written next to it.
#
# The script lives in ./bin/litellm-smoke-test.sh and is embedded via
# `builtins.readFile` so writeShellApplication's shellcheck pass covers
# it at build time.
{
  lib,
  pkgs,
  config,
  ...
}:
let
  litellmSmokeTest = pkgs.writeShellApplication {
    name = "litellm-smoke-test";
    runtimeInputs = with pkgs; [
      curl
      jq
      gawk
    ];
    text = builtins.readFile ./bin/litellm-smoke-test.sh;
  };
in
{
  config = lib.mkIf (config.myconfig.ai.llmops.enable && config.services.litellm.enable) {
    home-manager.sharedModules = [
      {
        home.packages = [ litellmSmokeTest ];
      }
    ];
  };
}
