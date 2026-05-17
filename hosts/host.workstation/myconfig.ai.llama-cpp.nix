# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# llama-cpp deployment for workstation.
#
# Serves Qwen3.5-9B-UD-Q4_K_XL (downloaded by the pull-models helper
# into ~/models/) via the llama-server router backend, bound to the
# AMD RX 5500 XT (ROCm0). The model is exposed under the alias
# "sidekick" so downstream tools (litellm, opencode, ...) can route
# to it by name.
{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
let
  modelsDir = "/home/mhuber/models";
in
{
  config = {
    # Pull Qwen3.5-9B-UD-Q4_K_XL.gguf into ~/models/Qwen3.5-9B-GGUF/.
    # `pull-models` skips the download when the target file already
    # exists, so this is safe to re-run.
    myconfig.ai.pull_models = {
      enable = true;
      models = {
        "${modelsDir}" = [
          "unsloth/Qwen3.5-9B-GGUF/Qwen3.5-9B-UD-Q4_K_XL.gguf"
        ];
      };
    };

    # llama-cpp router backend: a single llama-server bound to ROCm0
    # serves every model declared below; the OpenAI `model` field at
    # request time picks which INI section is active.
    myconfig.ai.inference-cpp.enable = true;
    myconfig.ai.llama-cpp = {
      serviceVariant = "llama-server";
      serviceDevice = "Vulkan0";
      servicePort = 33656;
      serviceListenAddress = "0.0.0.0";
      serviceOpenFirewall = true;
      # Publish this instance as `rx5500xt` (the GPU model) so the
      # localModels provider name is backend-agnostic and human-
      # readable, matching the pattern used on `thing`
      # (`rtx5090` / `gfx1151`).
      serviceProviderName = "rx5500xt";
      router.enable = true;
      models = [
        {
          name = "Qwen3.5-9B-UD-Q4_K_XL";
          path = "${modelsDir}/Qwen3.5-9B-GGUF/Qwen3.5-9B-UD-Q4_K_XL.gguf";
          devices = [ "Vulkan0" ];
          aliases = [ "sidekick" ];
          ttl = 300;
        }
      ];
    };
  };
}
