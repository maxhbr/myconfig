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
let
  inherit (lib) types mkOption;

  modelSubmodule = types.submodule {
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
      tags = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          User-provided tags for this model. Propagated by the
          publishers (router.nix / llama-swap.nix) to every entry
          derived from this model: the unpacked base entry, every
          variant generated from it (after the variant's own
          `tags`), and every alias (base- or variant-attached).

          These are merged with the computed classification tag
          (`base` / `variant`) and lineage tags (the parent model
          names) into a single deduplicated list, both in the
          llama-server `tags=` INI key and in the
          `myconfig.ai.localModels.<provider>.models[*].tags` field
          surfaced to downstream tools.
        '';
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
      parallel = mkOption {
        type = types.int;
        default = 1;
        description = "Number of parallel requests, increases the set ctx-size by its value";
      };
      cacheType = mkOption {
        type = types.nullOr (
          types.enum [
            "f16"
            "q8_0"
            "q5_1"
            "q4_0"
          ]
        );
        default = null;
        description = "KV cache quantisation type; null to use the model default";
      };
      pull-models = mkOption {
        type = types.nullOr (
          types.submodule {
            options = {
              target_directory = mkOption {
                type = types.path;
                description = ''
                  Directory into which the `pull-models` helper should
                  download this model. Auto-collected into
                  `myconfig.ai.pull_models.models` (keyed by this
                  path) when `myconfig.ai.pull_models.enable` is on.
                '';
              };
              hf_spec = mkOption {
                type = types.listOf types.str;
                description = ''
                  HuggingFace model specs for the `pull-models` helper.
                  A list of strings, each one of: `"org/repo"` (full
                  repo), `"org/repo/file.ext"` (single file), or
                  `"org/repo/subdir"` (subdir/*). Useful when a
                  single model has companion sidecars (e.g. mmproj
                  files) that should be downloaded together. See
                  `myconfig.ai.pull_models.models` for the spec
                  format.
                '';
              };
            };
          }
        );
        default = null;
        description = ''
          Optional metadata describing how `myconfig.ai.pull_models`
          should download this model. When set, each element of
          `hf_spec` is appended to
          `myconfig.ai.pull_models.models.<target_directory>`.
        '';
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
              tags = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = ''
                  Additional user-provided tags for this variant.
                  Appended to the parent model's `tags` to form the
                  full user-tag set propagated to this variant entry
                  and its aliases. See the parent model's `tags`
                  option for the merge semantics.
                '';
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
              cacheType = mkOption {
                type = types.nullOr (
                  types.enum [
                    "f16"
                    "q8_0"
                    "q5_1"
                    "q4_0"
                  ]
                );
                default = null;
                description = "KV cache quantisation type; null to use the model default";
              };
            };
          }
        );
        default = { };
        description = "Named variants of this model. Each variant generates a <model.name>-<variant_name> entry with its params merged on top of the parent params";
      };
    };
  };
in
{
  options.myconfig.ai.llama-cpp = with lib; {
    # Which system service backend (if any) serves the models declared
    # in `models`. `null` means no service is started by this module
    # (hosts may still set the underlying `services.*.enable` directly).
    #
    # - "llama-swap"   -> auto-enable `services.llama-swap` and feed it
    #                     the per-(model, device) wrappers (existing
    #                     behaviour; this is the multi-GPU, swapping
    #                     deployment).
    # - "llama-server" -> auto-enable the upstream nixpkgs
    #                     `services.llama-cpp` with the INI preset
    #                     generated for `serviceDevice`. A single
    #                     device serves every model on it; the runtime
    #                     `model` field of the OpenAI API picks which
    #                     section of the INI is active. Requires
    #                     `serviceDevice` to be set.
    serviceVariant = mkOption {
      type = types.nullOr (
        types.enum [
          "llama-swap"
          "llama-server"
        ]
      );
      default = null;
      description = ''
        Pick which system service backend serves `myconfig.ai.llama-cpp.models`:
          - null: no service activated by this module
          - "llama-swap":   enable services.llama-swap (per-(model, device) wrappers)
          - "llama-server": enable upstream services.llama-cpp with the INI preset
                            generated for `serviceDevice`
      '';
    };

    # Required iff serviceVariant == "llama-server". Picks which device
    # (e.g. "CUDA0", "Vulkan0") the single llama-server instance binds
    # to. The generated INI for that device is then the only set of
    # models the service exposes.
    serviceDevice = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Device to bind the llama-server service to when
        serviceVariant == "llama-server" (e.g. "CUDA0", "Vulkan0",
        "ROCm0"). Must appear in at least one model's `devices` or
        `unlistedDevices` list.
      '';
      example = "CUDA0";
    };

    # Backend-agnostic service-side networking. Routed into
    # `services.llama-swap.{port,listenAddress,openFirewall}` when
    # serviceVariant == "llama-swap", and into
    # `services.llama-cpp.{port,host,openFirewall}` when serviceVariant
    # == "llama-server". `mkDefault` is used on the wiring so hosts can
    # still reach into the backend-specific options directly when they
    # need to.
    serviceListenAddress = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Listen address for the active llama-cpp service backend (llama-swap or llama-server).";
    };
    servicePort = mkOption {
      type = types.port;
      default = 33656;
      description = "Listen port for the active llama-cpp service backend (llama-swap or llama-server).";
    };
    serviceOpenFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open the firewall for the active llama-cpp service backend's port.";
    };

    # User-facing provider name published into `myconfig.ai.localModels`
    # (consumed by litellm, opencode, aichat, ...). The default keeps
    # the legacy implementation-revealing scheme
    # ("llama-server-${port}" / "llama-swap-${port}") so existing hosts
    # see no change. Hosts that want a backend-agnostic, user-friendly
    # name (e.g. by GPU device: "rtx5090", "gfx1151") should override
    # this option.
    serviceProviderName = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Provider name used for the `myconfig.ai.localModels` entry this
        module publishes (and therefore the prefix LiteLLM emits for
        every served model, e.g. `${"\${serviceProviderName}"}:hermes`).
        Defaults to a port-suffixed implementation name
        ("llama-server-${"\${servicePort}"}" or "llama-swap-${"\${servicePort}"}")
        when null, preserving legacy behaviour.
      '';
      example = "rtx5090";
    };

    router = {
      enable = mkEnableOption "per-device llama-server router scripts driven by INI presets (home-manager wrappers, independent of `serviceVariant`)";

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
      type = types.listOf modelSubmodule;
      default = [ ];
      description = "Declarative model definitions that are expanded into llama-swap model entries per device";
    };
    scriptOnlyModels = mkOption {
      type = types.listOf modelSubmodule;
      default = [ ];
      description = "Declarative model definitions that are exposed as scripts";
    };
  };
}
