# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Per-device llama-server router backend for the llama-cpp module.
#
# When `myconfig.ai.llama-cpp.router.enable = true`, generates one
# `llama-server_<Device>` home-manager wrapper per device that has at
# least one model declared in `myconfig.ai.llama-cpp.models`. Each
# wrapper runs
#
#   llama-server --models-preset <generated.ini> --models-max <N>
#                --port <port> [extra args]
#
# where `<generated.ini>` contains every model whose `devices ++
# unlistedDevices` list includes `<Device>`. The router lets the user
# (or an upstream proxy) switch between models at runtime via the
# OpenAI-compatible `model` field while keeping a fixed port per
# device.
#
# Coexists with ./llama-swap.nix — both backends consume the same
# `models` option. The systemd / localModels publication side is
# scaffolded under `router.service.enable` but not yet implemented.
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
      // lib.optionalAttrs (m.ctxSize != null) { ctx-size = m.ctxSize; }
      // lib.optionalAttrs (m.aliases != [ ]) { alias = lib.concatStringsSep "," m.aliases; }
      // translated.keys;
    in
    {
      name = m.name;
      inherit keys;
      unhandled = translated.unhandled;
    };

  iniForDevice =
    device:
    router.renderIni {
      name = "llama-cpp-router-${device}.ini";
      globals = globalKeys;
      sections = map sectionFor (modelsForDevice device);
    };

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
in
{
  config = lib.mkIf rcfg.enable (
    lib.mkMerge [
      (lib.optionalAttrs hmEnabled {
        home-manager.sharedModules = [
          {
            home.packages = allRouterScripts;
          }
        ];
      })

      # Service variant — option declared in options.nix; implementation
      # is a follow-up. When wired up this branch should also publish
      # `myconfig.ai.localModels` entries (one per device, with port +
      # model name list).
      (lib.mkIf rcfg.service.enable {
        # TODO: generate systemd.user.services.llama-server-<device>
        #       + myconfig.ai.localModels entries
      })
    ]
  );
}
