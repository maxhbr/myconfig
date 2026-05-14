# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Option declarations for the llama-cpp module.
#
# These are deliberately backend-agnostic: they describe the set of
# models the host wants to expose plus per-device hints. The llama-swap
# integration in ./llama-swap.nix consumes these options, and any future
# alternative (e.g. a llama-swap replacement) should consume the same
# shape.
{ lib, ... }:
{
  options.myconfig.ai.llama-cpp = with lib; {
    router = {
      enable = mkEnableOption "per-device llama-server router scripts driven by INI presets";

      service.enable = mkEnableOption "systemd services for each generated router (implementation deferred; option declared for forward compat)";

      modelsMax = mkOption {
        type = types.int;
        default = 1;
        description = "Value for llama-server --models-max (1 = one model loaded at a time per device).";
      };

      basePort = mkOption {
        type = types.int;
        default = 22600;
        description = "Default port used by llama-server_<Device> when invoked without arguments.";
      };
    };

    models = mkOption {
      type = types.listOf (
        types.submodule {
          options = {
            name = mkOption {
              type = types.str;
              description = "Model name used as identifier in the llama-swap model key";
            };
            path = mkOption {
              type = types.str;
              description = "Path to the GGUF model file";
            };
            devices = mkOption {
              type = types.listOf types.str;
              default = [ "Vulkan0" ];
              description = "List of devices to run this model on (e.g. 'Vulkan0', 'CUDA0', 'ROCm0')";
            };
            unlistedDevices = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Devices that generate llama-swap entries with unlisted = true (accessible only via direct script)";
            };
            params = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Additional llama-server parameters";
            };
            aliases = mkOption {
              type = types.listOf types.str;
              default = [ ];
              description = "Aliases for this model in llama-swap";
            };
            ttl = mkOption {
              type = types.int;
              default = 300;
              description = "Time-to-live in seconds before the model is unloaded";
            };
            ctxSize = mkOption {
              type = types.nullOr types.int;
              default = null;
              description = "Context size (--ctx-size) for llama-server; null to use the model default";
            };
            variants = mkOption {
              type = types.attrsOf (
                types.submodule {
                  options = {
                    aliases = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      description = "Aliases for this model in llama-swap";
                    };
                    ctxSize = mkOption {
                      type = types.nullOr types.int;
                      default = null;
                      description = "Context size (--ctx-size) for llama-server; null to use the model default";
                    };
                    params = mkOption {
                      type = types.listOf types.str;
                      default = [ ];
                      description = "Additional llama-server parameters appended to the parent model params";
                    };
                    mmproj = mkOption {
                      type = types.nullOr types.str;
                      default = null;
                      description = "Path to mmproj file; when set, a :mmproj variant is auto-generated";
                    };
                  };
                }
              );
              default = { };
              description = "Named variants of this model. Each variant generates a ${model.name}-${variant_name} entry with its params merged on top of the parent params";
            };
          };
        }
      );
      default = [ ];
      description = "Declarative model definitions that are expanded into llama-swap model entries per device";
    };
  };
}
