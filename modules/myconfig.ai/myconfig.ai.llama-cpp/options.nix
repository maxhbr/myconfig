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
      group = mkOption {
        type = types.str;
        default = "default";
        description = ''
          Group name for llama-swap grouping. Models in the same group
          share swapping behaviour (controlled by the group's `swap`
          and `exclusive` settings). The default group "default" is
          used when no group is specified.
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
        description = ''
          Context a SINGLE request may use, in tokens (llama.cpp's
          `n_ctx_seq`); null to use the model default.

          This is deliberately *not* `--ctx-size` verbatim: llama-server's
          `--ctx-size` is the size of the whole KV cache, which it then
          divides by the slot count unless the cache is unified
          (`src/llama-context.cpp`: `n_ctx_seq = kv_unified ? n_ctx :
          n_ctx / n_seq_max`). The generated command line therefore emits
          `--ctx-size (ctxSize * parallel)` when `kvUnified = false` and
          plain `--ctx-size ctxSize` when `kvUnified = true`, so that
          `ctxSize` means the same thing either way.
        '';
      };
      parallel = mkOption {
        type = types.int;
        default = 1;
        description = ''
          Number of llama-server slots (`--parallel`), i.e. how many
          requests are served concurrently.

          Leaving this at 1 emits NO `--parallel` flag, which selects
          llama.cpp's *auto* mode: 4 slots with a unified KV cache
          (`tools/server/server.cpp`: `if (params.n_parallel < 0) {
          n_parallel = 4; kv_unified = true; }`). Setting it explicitly
          turns unified KV OFF unless `kvUnified` is also set, and then
          the KV cache costs `ctxSize * parallel` tokens.
        '';
      };
      kvUnified = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Pass `--kv-unified`: use ONE KV buffer shared by all slots
          instead of statically partitioning it per slot.

          Only meaningful together with an explicit `parallel > 1`
          (llama.cpp already enables it implicitly when `--parallel` is
          omitted). With it, `parallel` concurrent requests share a pool
          of `ctxSize` tokens and each may address all of it, instead of
          each getting a fixed `ctxSize` slice — same per-request
          context at 1/parallel of the KV memory. The trade-off is that
          concurrent long-context requests compete for the pool.
        '';
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
      tensorSplit = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Comma-separated tensor-split ratios passed as `--tensor-split` to
          llama-server (e.g. "2,3"). Required when a `devices` entry contains
          a comma (multi-device), e.g. "Vulkan0,Vulkan1". The module asserts
          at evaluation time that `tensorSplit` is non-null whenever a
          multi-device string is present.
        '';
        example = "2,3";
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
      mlock = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = ''
          Override the global `mlock` setting for this model.
          `null` to inherit the global default; `false` to disable
          `mlock` even if the global enables it; `true` to enable it
          even if the global disables it.
        '';
      };
      # Per-model llama.cpp package override. When null the package is
      # selected from the device string (Vulkan -> llama-cpp-vulkan,
      # ROCm -> llama-cpp-rocm, CUDA -> llama-cpp+cudaSupport). When set,
      # the given package's `llama-server` / `llama-bench` binaries are
      # used instead — e.g. a pinned fork for one backend without
      # moving the other backends off upstream. Such models are also
      # excluded from the router (see the description below).
      serverPackage = mkOption {
        type = types.nullOr types.package;
        default = null;
        description = ''
          Override the llama.cpp package used to serve and benchmark
          this model. When null (default), the package is auto-selected
          from the device string. When set (e.g. to a pinned fork), the
          per-(model, device) script wrappers and llama-swap entries for
          this model use `serverPackage.bin/llama-server` and
          `serverPackage.bin/llama-bench` instead of the device-default
          build from `pkgs.llama-cpp-vulkan` / `pkgs.llama-cpp-rocm` /
          `pkgs.llama-cpp` (CUDA). The package must provide
          `llama-server` and `llama-bench`.

          Models with a non-null `serverPackage` are **excluded from
          the router** (the `llama-server` service backend and the
          `llama-server_<Device>` INI-preset wrappers): the router runs
          a single `services.llama-cpp.package` binary for every
          section in its INI preset and cannot mix per-model packages.
          They remain available via llama-swap and the
          per-(model, device) script wrappers.

          Use this for models that require a patched or custom
          llama.cpp build (e.g. the PR-27742 `qwen4exp` build for
          Qwen3.8-Flash-Next, or the Nathanw1014 strix-halo-vulkan
          fork) without forcing the entire host onto that build.
        '';
      };
      # Per-model extra environment variables, exported around the
      # llama-server / llama-bench run in ADDITION to the device-specific
      # vars (LLAMA_ARG_DEVICE, CUDA_VISIBLE_DEVICES suppression). Use
      # for backend tuning that should NOT be applied globally.
      extraEnv = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = ''
          Extra environment variables exported around this model's
          llama-server / llama-bench runs, on top of the device-specific
          vars. Keys are variable names, values are their string values.
          Use for backend-specific tuning scoped to one model (e.g.
          `HSA_ENABLE_SDMA = "0"` for a single ROCm candidate) that
          should NOT be applied to other ROCm models.
        '';
      };
      # Explicit --no-mmap control. null = pass params through as-is
      # (the legacy behaviour: --no-mmap is present iff it is in
      # `params`). true = force --no-mmap on. false = strip --no-mmap
      # so the model is served with mmap+mlock.
      noMmap = mkOption {
        type = types.nullOr types.bool;
        default = null;
        description = ''
          Control the `--no-mmap` flag explicitly, independent of the
          free-form `params` list.
          null (default): pass `params` through verbatim; `--no-mmap` is
            present iff it appears in `params`.
          true: ensure `--no-mmap` is in the effective flags (added if
            not already in `params`).
          false: strip `--no-mmap` from `params` so the model is served
            with mmap + mlock (the GGUF is mmap'd then mlock'd into
            RAM), which can reduce resident memory for large models.
        '';
      };
      # Pinned SHA-256 of the target model file (the LFS oid, hex).
      # Logged in the server startup banner for provenance. Not
      # verified at runtime (hashing a multi-GB file at startup would
      # add minutes of I/O); verify out-of-band with `sha256sum`.
      sha256 = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          Pinned SHA-256 (hex digest, the LFS oid) of the target model
          file. Logged in the server startup banner for provenance /
          audit. Not verified at startup (verifying a multi-GB file
          would add minutes of I/O); verify out-of-band with
          `sha256sum <path>`.
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
              parallel = mkOption {
                type = types.nullOr types.int;
                default = null;
                description = "Number of llama-server slots (--parallel); null to inherit from the parent model";
              };
              kvUnified = mkOption {
                type = types.nullOr types.bool;
                default = null;
                description = "Share one KV buffer across slots (--kv-unified); null to inherit from the parent model";
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

    # Vendored chat templates exposed to model declarations so hosts can
    # reference a pinned, store-path-stable template file with
    # `--jinja --chat-template <path>` without pointing at a mutable
    # checkout. See ./templates/ for the pinned assets and their
    # provenance files.
    chatTemplates = mkOption {
      type = types.submodule {
        options = {
          qwen38-sharp = mkOption {
            type = types.path;
            default = ./templates/sharp.jinja;
            defaultText = lib.literalExpression "./templates/sharp.jinja";
            description = ''
              Path to the vendored Qwen3.8 "sharp" jinja chat template
              (PieBru, template_version `qwen3.8-froggeric-v22.3`, MIT).
              Used by Qwen3.8 profiles that need OpenAI-compatible
              tool-call templating via `--jinja --chat-template <this
              path>`. The file is kept byte-identical to upstream; see
              `./templates/sharp.jinja.provenance` for the pinned SHA-256,
              upstream URL and re-pinning instructions.
            '';
          };
        };
      };
      default = { };
      description = "Vendored chat templates exposed to model declarations.";
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

    groups = mkOption {
      type = types.attrsOf (
        types.submodule {
          options = {
            swap = mkOption {
              type = types.bool;
              default = true;
              description = ''
                Controls model swapping behaviour within the group.
                True: only one model runs at a time (models swap in/out).
                False: all models in the group can run simultaneously.
              '';
            };
            exclusive = mkOption {
              type = types.bool;
              default = false;
              description = ''
                Controls how the group affects other groups.
                True: causes all other groups to unload when this group
                runs a model.
                False: does not affect other groups (allows concurrent
                models across groups).
              '';
            };
            persistent = mkOption {
              type = types.bool;
              default = false;
              description = ''
                Prevents other groups from unloading the models in this
                group. Does not affect individual model behaviour within
                the group.
              '';
            };
          };
        }
      );
      default = {
        default = {
          swap = true;
          exclusive = false;
          persistent = false;
        };
      };
      description = ''
        Group settings for llama-swap. Keys are group names that must
        match the `group` attribute on models. The "default" group is
        always present with sensible defaults; override or extend as
        needed. Members are auto-derived from model `group` attributes.
      '';
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
