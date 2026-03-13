# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.myconfig.ai.llmServers;

  modelScript =
    model:
    pkgs.writeShellScriptBin model.name ''
      set -x
      ${
        lib.optionalString (model.device != null) "exec LLAMA_ARG_DEVICE=${model.device}"
      } "${pkgs.llama-cpp}/bin/llama-server" \
        -m "${model.modelPath}" \
        --port ${toString model.port} \
        -c ${toString model.contextSize} \
        ${lib.optionalString (model.flashAttention or false) "-fa on"} \
        ${lib.optionalString (model.continuousBatching or false) "-cb on"} \
        ${model.extraArgs or ""} \
        "$@"
    '';

in
{
  options.myconfig.ai.llmServers = {
    enable = lib.mkEnableOption "LLM server scripts";

    models = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Name of the script (also used as command name)";
            };

            modelPath = lib.mkOption {
              type = lib.types.str;
              description = "Path to the GGUF model file";
            };

            port = lib.mkOption {
              type = lib.types.int;
              description = "Port for the LLM server";
            };

            contextSize = lib.mkOption {
              type = lib.types.int;
              description = "Context window size";
            };

            device = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Device to use (e.g., 'Vulkan1', 'CUDA0'); null for default";
            };

            flashAttention = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Enable flash attention";
            };

            continuousBatching = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Enable continuous batching";
            };

            extraArgs = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "Additional arguments to pass to llama-server";
            };
          };
        }
      );
      default = [ ];
      description = "List of LLM model configurations";
    };
  };

  config = lib.mkIf cfg.enable {
    home-manager.sharedModules = [
      {
        home.packages = map modelScript cfg.models;
      }
    ];
  };
}
