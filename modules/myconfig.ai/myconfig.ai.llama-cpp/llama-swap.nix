# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# llama-swap backend for the llama-cpp module.
#
# Consumes `config.myconfig.ai.llama-cpp.models` and wires it into
# - `services.llama-swap.settings.{models,groups}`
# - `myconfig.ai.localModels` (so other modules can discover the served models)
# - home-manager packages for each generated llama-server / llama-bench
#   wrapper, plus aggregate `llama-bench-all` / `llama-bench_<device>`
#   runners.
#
# Gated on `config.services.llama-swap.enable`, so hosts that don't run
# llama-swap simply leave their declared models inert. When
# `myconfig.ai.llama-cpp.serviceVariant = "llama-swap"`, this module
# also auto-enables `services.llama-swap.enable` (mkDefault) so the
# host only needs to set port / listenAddress / openFirewall.
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-cpp;
  llamaLib = import ./lib { inherit lib pkgs; };
  inherit (llamaLib) variants scripts;

  hasGpuVariant = llamaLib.devices.mkHasGpuVariant { inherit config options; };
  guardDevice = llamaLib.devices.mkGuardDevice hasGpuVariant;
  inherit (llamaLib.devices) deviceIndex;
  inherit (scripts) mkLlamaScript mkLlamaBenchScript mkLlamaBenchAggregate;

  unpackedModels = variants.unpackModels cfg.models;

  # --- llama-swap model entries -------------------------------------------

  # Generate a single llama-swap model entry backed by a shell application.
  mkModelEntry =
    {
      model,
      device,
      isFirstDevice ? false,
      unlisted ? false,
    }:
    let
      script = mkLlamaScript { inherit model device; };
      modelKey =
        (if unlisted then "unlisted:" else "") + (if isFirstDevice then "" else "${device}:") + model.name;
    in
    {
      "${modelKey}" = {
        cmd = "${lib.getExe script} \${PORT}";
        ttl = model.ttl;
        inherit unlisted;
        aliases = lib.optionals (isFirstDevice && unlisted == false) model.aliases;
      };
    };

  # Generate model entries for a list of devices, optionally marking them
  # as unlisted.
  mkDeviceEntries =
    {
      model,
      devices,
      unlisted ? false,
    }:
    let
      firstDevice = if devices != [ ] then builtins.head devices else null;
    in
    map (
      device:
      let
        isFirstDevice = device == firstDevice;
      in
      mkModelEntry {
        inherit
          model
          device
          isFirstDevice
          unlisted
          ;
      }
    ) devices;

  mkModelEntries =
    model:
    mkDeviceEntries {
      inherit model;
      devices = builtins.filter guardDevice model.devices;
    }
    ++ mkDeviceEntries {
      inherit model;
      devices = builtins.filter guardDevice model.unlistedDevices;
      unlisted = true;
    };

  allModels = lib.mkMerge (lib.concatMap mkModelEntries unpackedModels);

  # --- home-manager scripts ----------------------------------------------

  mkScriptDeviceEntries =
    {
      model,
      devices,
    }:
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) ([
        (mkLlamaScript { inherit model device; })
        (mkLlamaBenchScript { inherit model device; })
      ])
    ) devices;

  mkScriptEntries =
    model:
    mkScriptDeviceEntries {
      inherit model;
      devices = model.devices;
    }
    ++ mkScriptDeviceEntries {
      inherit model;
      devices = model.unlistedDevices;
    };

  allScripts = lib.concatMap mkScriptEntries unpackedModels;

  # Names of all generated llama-bench-* scripts.
  benchScriptNames = lib.concatMap (
    s: lib.optional (lib.hasPrefix "llama-bench_" s.name) s.name
  ) allScripts;

  # All distinct devices that have at least one generated
  # llama-bench_<device>_* script.
  benchDevices = lib.unique (
    lib.concatMap (
      n:
      let
        # Strip the "llama-bench_" prefix, then take everything up to
        # the next "_".
        rest = lib.removePrefix "llama-bench_" n;
        parts = lib.splitString "_" rest;
      in
      lib.optional (parts != [ ]) (builtins.head parts)
    ) benchScriptNames
  );

  # The catch-all aggregate that runs every generated bench script.
  llamaBenchAll = mkLlamaBenchAggregate {
    name = "llama-bench-all";
    scripts = benchScriptNames;
    runtimeInputs = allScripts;
  };

  # Per-device aggregates: llama-bench_CUDA0, llama-bench_Vulkan0, ...
  # NOTE: runtimeInputs is intentionally `allScripts` (not the filtered
  # subset) to match the historical behaviour and preserve closure-equal
  # output paths between refactors.
  llamaBenchPerDevice = map (
    device:
    mkLlamaBenchAggregate {
      name = "llama-bench_${device}";
      scripts = builtins.filter (n: lib.hasPrefix "llama-bench_${device}_" n) benchScriptNames;
      runtimeInputs = allScripts;
    }
  ) benchDevices;

  # --- llama-swap groups (one per physical GPU) --------------------------

  # Build groups: one group per physical GPU (by device index) so models
  # on different GPUs can run simultaneously. Within the same GPU,
  # models swap (they share VRAM).
  allGroups =
    let
      mkGpuPairs =
        { model, devices }:
        lib.concatMap (
          device:
          lib.optionals (guardDevice device) ([
            {
              gpu = deviceIndex device;
              key = "${device}:${model.name}";
            }
          ])
        ) devices;
      gpuModelPairs = lib.concatMap (
        model:
        mkGpuPairs {
          inherit model;
          devices = model.devices;
        }
        ++ mkGpuPairs {
          inherit model;
          devices = model.unlistedDevices;
        }
      ) unpackedModels;
      gpuIndices = lib.unique (map (p: p.gpu) gpuModelPairs);
      membersFor = idx: map (p: p.key) (builtins.filter (p: p.gpu == idx) gpuModelPairs);
    in
    lib.listToAttrs (
      map (idx: {
        name = "gpu${idx}";
        value = {
          swap = true;
          exclusive = false;
          members = membersFor idx;
        };
      }) gpuIndices
    );

  # --- localModels names ------------------------------------------------
  #
  # Each unpacked model contributes:
  #   - one `{ name = ...; kind = m._kind; }` entry per device it runs on
  #     (firstDevice is unprefixed; subsequent devices get a "<dev>:" prefix
  #     so they remain distinct in the published list). `_kind` is "base"
  #     or "variant", propagated from `lib/variants.nix:unpackContainedVariants`.
  #   - one `{ name = alias; kind = "alias"; }` entry per declared alias.
  #     llama-swap itself only registers aliases against the first-device
  #     entry (see `mkModelEntry`), but the alias is still resolvable on
  #     the served port, so we publish it once at the registry level.

  mkModelEntriesForDevices =
    { model, devices }:
    let
      firstDevice = if devices != [ ] then builtins.head devices else null;
    in
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) [
        {
          name = if device == firstDevice then model.name else "${device}:${model.name}";
          kind = model._kind;
        }
      ]
    ) devices;

  allModelEntries = lib.concatMap (
    model:
    mkModelEntriesForDevices {
      inherit model;
      devices = model.devices;
    }
    ++ mkModelEntriesForDevices {
      inherit model;
      devices = model.unlistedDevices;
    }
    ++ (map (a: {
      name = a;
      kind = "alias";
    }) model.aliases)
  ) unpackedModels;
in
{
  config = lib.mkMerge [
    # Auto-enable services.llama-swap when the host opts in via the
    # serviceVariant enum, and route the backend-agnostic
    # `myconfig.ai.llama-cpp.service{Port,ListenAddress,OpenFirewall}`
    # options into the corresponding `services.llama-swap.*` fields.
    # `mkDefault` keeps explicit `services.llama-swap.*` overrides
    # winning (useful in containers where the inner config wants to
    # set things directly).
    (lib.mkIf (cfg.serviceVariant == "llama-swap") {
      services.llama-swap = {
        enable = lib.mkDefault true;
        port = lib.mkDefault cfg.servicePort;
        listenAddress = lib.mkDefault cfg.serviceListenAddress;
        openFirewall = lib.mkDefault cfg.serviceOpenFirewall;
      };
    })

    (lib.mkIf config.services.llama-swap.enable {
      myconfig.ai.localModels = [
        (
          let
            port = config.services.llama-swap.port;
          in
          {
            name =
              if cfg.serviceProviderName != null then cfg.serviceProviderName else "llama-swap-${toString port}";
            models = allModelEntries;
            port = port;
          }
        )
      ];

      services.llama-swap = {
        settings = {
          sendLoadingState = true;
          models = allModels;
          groups = allGroups;
        };
      };

      systemd.services.llama-swap = {
        # https://github.com/nixos/nixpkgs/issues/441531
        environment.XDG_CACHE_HOME = "/var/cache/llama-swap";
        serviceConfig.CacheDirectory = "llama-swap";
      };
    })
  ];
}
