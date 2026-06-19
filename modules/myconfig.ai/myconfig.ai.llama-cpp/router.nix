# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Per-device llama-server router backend for the llama-cpp module.
#
# Two distinct (but related) features live here:
#
#  1. `router.enable` — home-manager wrappers `llama-server_<Device>`
#     that launch llama-server with the device-specific INI preset.
#     Independent of which (if any) system service runs.
#
#  2. `serviceVariant == "llama-server"` — wire the same INI into the
#     upstream nixpkgs `services.llama-cpp` module so a system-managed
#     systemd unit serves the models for `serviceDevice`. Also
#     publishes `myconfig.ai.localModels` so downstream tools (litellm,
#     opencode, ...) can discover the served models.
#
# The companion variant `serviceVariant == "llama-swap"` lives in
# ./llama-swap.nix.
#
# Per-device wrappers run
#
#   llama-server --models-preset <generated.ini> --models-max <N>
#                --port <port> [extra args]
#
# where `<generated.ini>` contains every model whose `devices ++
# unlistedDevices` list includes `<Device>`. The router lets the user
# (or an upstream proxy) switch between models at runtime via the
# OpenAI-compatible `model` field while keeping a fixed port per
# device.
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-cpp;
  rcfg = cfg.router;

  llamaLib = import ./lib {
    inherit lib pkgs;
    diffusionLlamaCpp = cfg.diffusionLlamaCpp;
  };
  inherit (llamaLib) variants devices router;

  hasGpuVariant = devices.mkHasGpuVariant { inherit config options; };
  guardDevice = devices.mkGuardDevice hasGpuVariant;
  inherit (devices) backendForDevice;

  unpackedModels = variants.unpackModels cfg.models;

  # Every device declared anywhere (in `devices` or `unlistedDevices`)
  # by any model, filtered by host hardware capability so e.g. a Vulkan
  # device on a CUDA-only host disappears.
  allDevices = lib.unique (
    lib.concatMap (m: builtins.filter guardDevice (m.devices ++ m.unlistedDevices)) unpackedModels
  );

  # Models that include `device` in either `devices` or
  # `unlistedDevices`. The "unlisted" distinction has no router
  # analogue — every section in the INI is API-reachable. We fold both
  # lists in so the router exposes every model the user marked for
  # this device.
  modelsForDevice =
    device: builtins.filter (m: builtins.elem device (m.devices ++ m.unlistedDevices)) unpackedModels;

  # `[*]` shared defaults applied to every model unless overridden in
  # a per-model section. Mirrors the flags that scripts.nix bakes into
  # each generated wrapper.
  globalKeys = {
    gpu-layers = "all";
    flash-attn = "on";
    mlock = true;
    metrics = true;
    no-webui = true;
    # Disable per-slot *context checkpoints* (-ctxcp / --ctx-checkpoints
    # / --swa-checkpoints, default 32 in llama.cpp b9xxx). This is the
    # mechanism that actually hard-aborts on our SWA models. During
    # update_slots, when n_swa forces a partial-cache reuse the server
    # tries to restore a saved context checkpoint via
    # common_prompt_checkpoint::load_tgt -> llama_state_seq_set_data_ext;
    # when the saved blob can't be reloaded into the KV cache it
    # GGML_ABORTs instead of degrading to a full re-prefill:
    #
    #   slot update_slots: ... n_swa = 1024
    #   slot update_slots: ... Checking checkpoint with [..] against ..
    #   state_read_meta: failed to find available cells in kv cache
    #   common.cpp:2093: checkpoint size mismatch: expected N, got 0
    #   state_seq_set_data: error loading state: failed to restore kv cache
    #   -> ggml_abort, the whole llama-server process dies and never recovers
    #
    # With ctx-checkpoints = 0 no checkpoints are ever created
    # (server-context.cpp: `do_checkpoint = n_ctx_checkpoints > 0`), so
    # the restore branch is never taken and the slot falls back to full
    # prompt re-processing (`do_reset`) instead of aborting. Drop this
    # once the upstream restore path degrades gracefully instead of
    # aborting (see ggml-org/llama.cpp PR #15293).
    ctx-checkpoints = 0;

    # Also disable the idle-slot prompt save/restore path
    # (--cache-idle-slots, default-on). It has a separate but related
    # abort site (slot_save_and_clear / state restore) that can fire
    # under the same SWA + RAM-cache-pressure conditions. Keeping it off
    # leaves ordinary same-prompt KV reuse intact.
    cache-idle-slots = false;

    # Increase the server-side slot timeout (seconds). The default (30 s)
    # is hit when a slot waits longer than that for the next generated
    # token — which happens routinely on large Qwen3.6 prompts that need
    # full re-prefilling (SWA forces a reset): the prefill alone can take
    # tens of seconds at > 1 k tokens/s on 40 k-token contexts.
    # See https://github.com/ggml-org/llama.cpp/pull/22907
    timeout = 600;

    sleep-idle-seconds = 1800;
  };

  # Lineage tags for a model entry itself (not for its aliases).
  # `_baseName` and `_kind` come from
  # `lib/variants.nix:unpackContainedVariants`:
  #   - base    -> []
  #   - variant -> [ _baseName ]
  modelLineageTags = m: lib.optional (m._kind or null == "variant") m._baseName;

  # Lineage tags for an alias of a model: the parent's own name
  # first, then (only when the parent is a variant) the base it came
  # from. An alias of a base gets one tag; an alias of a variant gets
  # two.
  aliasLineageTags = m: [ m.name ] ++ modelLineageTags m;

  # Final `tags` list published for a model entry: lineage first, then
  # the device's llama.cpp backend ("cuda" / "rocm" / "vulkan", from
  # `devices.backendForDevice`) so tag-based routing can pin requests
  # to a specific backend without parsing the device string, then the
  # user-provided tags (already pre-merged with the parent's tags in
  # `lib/variants.nix` for variants). Deduped while preserving the
  # first occurrence. Aliases reuse this with the alias-specific
  # lineage. `device` is the resolved llama-cpp device string
  # (e.g. "CUDA0") that this entry is being served on.
  modelTags =
    device: m:
    lib.unique (
      modelLineageTags m
      ++ (lib.optional (backendForDevice device != null) (backendForDevice device))
      ++ (m._userTags or [ ])
    );
  aliasTags =
    device: m:
    lib.unique (
      aliasLineageTags m
      ++ (lib.optional (backendForDevice device != null) (backendForDevice device))
      ++ (m._userTags or [ ])
    );

  # Build a single `[<model.name>]` section.
  #
  # The `tags` key is llama-server's first-class `--tags` flag — a
  # comma-separated string that llama-server splits into the per-
  # model `tags[]` array surfaced in `GET /v1/models`. We assemble a
  # list of tags here and join with "," at emission time so a future
  # caller can add more tags without restructuring the schema. Tags
  # emitted in order, deduped:
  #   - the `_kind` computed by
  #     `lib/variants.nix:unpackContainedVariants` ("base" / "variant"),
  #     so external tools can distinguish a top-level model from a
  #     variants.<n>-generated one without re-parsing the name;
  #   - lineage tags (`modelLineageTags`): a variant additionally
  #     carries the name of its base model;
  #   - the llama.cpp backend for `device` ("cuda" / "rocm" / "vulkan"),
  #     for backend-aware tag routing; and
  #   - user-provided `tags` from the model declaration (merged with
  #     the parent's `tags` for variants — see `lib/variants.nix`).
  # Aliases don't get their own section (llama-server registers them
  # via the `alias = ` key of the parent section) — they piggyback on
  # the parent's INI tags. Their lineage tag set (which includes the
  # parent's name on top of the base) is published separately via
  # `myconfig.ai.localModels` below; the user-tag portion is identical
  # to the parent's.
  sectionFor =
    device: m:
    let
      translated = router.translateParamsToIni m.params;
      backendTag = backendForDevice device;
      tagsList = lib.unique (
        (lib.optional (m ? _kind && m._kind != null) m._kind)
        ++ (modelLineageTags m)
        ++ (lib.optional (backendTag != null) backendTag)
        ++ (m._userTags or [ ])
      );
      keys = {
        model = m.path;
      }
      // lib.optionalAttrs (m.ctxSize != null) { ctx-size = (m.ctxSize * m.parallel); }
      // lib.optionalAttrs (m.cacheType != null) {
        cache-type-k = m.cacheType;
        cache-type-v = m.cacheType;
        spec-draft-type-k = m.cacheType;
        spec-draft-type-v = m.cacheType;
      }
      // lib.optionalAttrs (m.parallel > 1) { parallel = m.parallel; }
      // lib.optionalAttrs (m.aliases != [ ]) { alias = lib.concatStringsSep "," m.aliases; }
      // lib.optionalAttrs (tagsList != [ ]) { tags = lib.concatStringsSep "," tagsList; }
      // translated.keys;
    in
    {
      name = m.name;
      inherit keys;
      unhandled = translated.unhandled;
    };

  # Shared { globals, sections } structure for a device. Used both for
  # the home-manager wrappers' on-disk INI and for the
  # `services.llama-cpp.modelsPreset` attrset.
  iniDataForDevice = device: {
    globals = globalKeys;
    sections = map (sectionFor device) (modelsForDevice device);
  };

  iniForDevice =
    device:
    router.renderIni (
      {
        name = "llama-cpp-router-${device}.ini";
      }
      // iniDataForDevice device
    );

  scriptForDevice =
    device:
    router.mkRouterScript {
      inherit device;
      port = rcfg.basePort;
      iniFile = iniForDevice device;
      modelsMax = rcfg.modelsMax;
    };

  # Skip devices with no models on this host (after guardDevice
  # filtering). Without this we'd emit empty INI files / routers.
  effectiveDevices = builtins.filter (d: modelsForDevice d != [ ]) allDevices;

  allRouterScripts = map scriptForDevice effectiveDevices;

  hmEnabled = lib.hasAttrByPath [ "home-manager" "sharedModules" ] options;

  # --- llama-server service backend --------------------------------------
  #
  # When `serviceVariant == "llama-server"`, configure the upstream
  # nixpkgs `services.llama-cpp` module with the INI preset generated
  # for `serviceDevice`. We also publish `myconfig.ai.localModels` so
  # discovery-style consumers see the served models.

  isLlamaServerService = cfg.serviceVariant == "llama-server";
  serviceDevice = cfg.serviceDevice;

  serviceModels = if isLlamaServerService then modelsForDevice serviceDevice else [ ];
  serviceModelNames = map (m: m.name) serviceModels;
  serviceModelAliases = lib.unique (lib.concatMap (m: m.aliases) serviceModels);

  # `models` for myconfig.ai.localModels: one tagged entry per model,
  # plus one tagged entry per alias (so consumers see every
  # API-reachable name as a top-level model). The service serves
  # exactly one model at a time, but every section in the INI is
  # selectable via the OpenAI `model` field, so we publish them all.
  #
  # `_kind` was attached to each unpacked model by
  # `lib/variants.nix:unpackContainedVariants` ("base" or "variant").
  # Aliases are emitted here as additional `{ name; kind = "alias"; tags; }`
  # entries — they have no llama-cpp section of their own, they just
  # piggyback on a base/variant via llama-server's `alias = ` INI key.
  # Lineage `tags`:
  #   - base    -> []
  #   - variant -> [ <baseName> ]
  #   - alias of base    -> [ <base.name> ]
  #   - alias of variant -> [ <variant.name>, <base.name> ]
  serviceLocalModelsEntries =
    (map (m: {
      name = m.name;
      kind = m._kind;
      tags = modelTags serviceDevice m;
    }) serviceModels)
    ++ (lib.concatMap (
      m:
      map (a: {
        name = a;
        kind = "alias";
        tags = aliasTags serviceDevice m;
      }) m.aliases
    ) serviceModels);

  serviceModelsPresetFile = if isLlamaServerService then iniForDevice serviceDevice else null;
in
{
  config = lib.mkMerge [
    # --- assertions: serviceVariant <-> serviceDevice consistency --------
    {
      assertions = [
        {
          assertion = !isLlamaServerService || serviceDevice != null;
          message = "myconfig.ai.llama-cpp.serviceVariant = \"llama-server\" requires myconfig.ai.llama-cpp.serviceDevice to be set (e.g. \"CUDA0\").";
        }
        {
          assertion =
            !isLlamaServerService
            || serviceDevice == null
            || (serviceModels != [ ] && guardDevice serviceDevice);
          message = "myconfig.ai.llama-cpp.serviceDevice = \"${toString serviceDevice}\" has no models on this host (no model declares it in devices/unlistedDevices, or the host lacks the required GPU variant).";
        }
        {
          assertion = cfg.serviceVariant != "llama-swap" || cfg.serviceDevice == null;
          message = "myconfig.ai.llama-cpp.serviceDevice must be null when serviceVariant = \"llama-swap\" (llama-swap is multi-device by design).";
        }
      ];
    }

    # --- home-manager wrappers (router.enable, independent of service) ---
    (lib.mkIf rcfg.enable (
      lib.optionalAttrs hmEnabled {
        home-manager.sharedModules = [
          {
            home.packages = allRouterScripts;
          }
        ];
      }
    ))

    # --- llama-server system service backend -----------------------------
    (lib.mkIf isLlamaServerService {
      services.llama-cpp = {
        enable = true;
        # `services.llama-cpp.package` is set by services.llama-cpp.nix
        # via `lib.mkDefault` (it picks a multi-backend llama-cpp build
        # appropriate for the host's GPU variants). We don't override
        # it here — at runtime the LLAMA_ARG_DEVICE env var (set below)
        # selects which backend that build uses. Hosts that need a
        # different package can set `services.llama-cpp.package`
        # explicitly with the usual mkForce / mkOverride mechanism.
        openFirewall = cfg.serviceOpenFirewall;
        settings = {
          host = cfg.serviceListenAddress;
          port = cfg.servicePort;
          models-preset = serviceModelsPresetFile;
          models-max = rcfg.modelsMax;
        };
      };

      # llama-server picks the device via $LLAMA_ARG_DEVICE.
      # `CUDA_VISIBLE_DEVICES=` (empty) is required for Vulkan/ROCm so
      # CUDA libs don't fight the active backend. `devices.envForDevice`
      # returns "KEY=VALUE" strings; convert them to the
      # systemd `environment` attrset shape.
      #
      # HOME / XDG_CACHE_HOME: the upstream services.llama-cpp unit runs
      # as a DynamicUser with no $HOME, so the multi-backend build's
      # Vulkan backend resolves its shader cache to `//.cache` and logs
      #   "Failed to create //.cache for shader cache (Read-only file
      #    system)---disabling."
      # on every start. Point HOME at the existing CacheDirectory
      # (/var/cache/llama-cpp) so the pipeline cache lands in a writable,
      # persisted location and warmup is faster on subsequent starts.
      systemd.services.llama-cpp.environment = {
        HOME = "/var/cache/llama-cpp";
        XDG_CACHE_HOME = "/var/cache/llama-cpp";
      }
      // lib.listToAttrs (
        map (
          kv:
          let
            parts = lib.splitString "=" kv;
            k = builtins.head parts;
            v = lib.concatStringsSep "=" (builtins.tail parts);
          in
          {
            name = k;
            value = v;
          }
        ) (devices.envForDevice serviceDevice)
      );

      myconfig.ai.localModels = [
        {
          name =
            if cfg.serviceProviderName != null then
              cfg.serviceProviderName
            else
              "llama-server-${toString cfg.servicePort}";
          host = "localhost";
          port = cfg.servicePort;
          models = serviceLocalModelsEntries;
        }
      ];
    })
  ];
}
