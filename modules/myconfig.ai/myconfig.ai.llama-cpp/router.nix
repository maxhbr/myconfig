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

  llamaLib = import ./lib { inherit lib pkgs; };
  inherit (llamaLib) variants devices router;

  hasGpuVariant = devices.mkHasGpuVariant { inherit config options; };
  guardDevice = devices.mkGuardDevice hasGpuVariant;

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
  };

  # Build a single `[<model.name>]` section.
  sectionFor =
    m:
    let
      translated = router.translateParamsToIni m.params;
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
    sections = map sectionFor (modelsForDevice device);
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

  # `models` for myconfig.ai.localModels: one entry per model with its
  # aliases attached. The service serves exactly one model at a time,
  # but every section in the INI is selectable via the OpenAI `model`
  # field, so we publish them all.
  serviceLocalModelsEntries = map (m: {
    name = m.name;
    aliases = m.aliases;
  }) serviceModels;

  serviceModelsPreset =
    if isLlamaServerService then router.toModelsPreset (iniDataForDevice serviceDevice) else null;
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
        host = cfg.serviceListenAddress;
        port = cfg.servicePort;
        openFirewall = cfg.serviceOpenFirewall;
        modelsPreset = serviceModelsPreset;
        # `--models-max` is not a first-class option on the nixpkgs
        # module; pass it through `extraFlags`. Defaults to 1 (the
        # single-llama-server-per-device deployment).
        extraFlags = [
          "--models-max"
          (toString rcfg.modelsMax)
        ];
      };

      # llama-server picks the device via $LLAMA_ARG_DEVICE.
      # `CUDA_VISIBLE_DEVICES=` (empty) is required for Vulkan/ROCm so
      # CUDA libs don't fight the active backend. `devices.envForDevice`
      # returns "KEY=VALUE" strings; convert them to the
      # systemd `environment` attrset shape.
      systemd.services.llama-cpp.environment = lib.listToAttrs (
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
