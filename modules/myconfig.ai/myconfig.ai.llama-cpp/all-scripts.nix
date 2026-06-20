# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  options,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-cpp;
  llamaLib = import ./lib {
    inherit lib pkgs;
    diffusionLlamaCpp = cfg.diffusionLlamaCpp;
  };
  inherit (llamaLib) variants scripts;

  hasGpuVariant = llamaLib.devices.mkHasGpuVariant { inherit config options; };
  guardDevice = llamaLib.devices.mkGuardDevice hasGpuVariant;
  inherit (llamaLib.devices) deviceIndex;
  inherit (scripts) mkLlamaScript mkLlamaBenchScript mkLlamaBenchAggregate;

  unpackedModels = variants.unpackModels (cfg.models ++ cfg.scriptOnlyModels);

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

  hmEnabled = lib.hasAttrByPath [ "home-manager" "sharedModules" ] options;
in
{
  config = lib.mkMerge [
    (lib.optionalAttrs hmEnabled {
      home-manager.sharedModules = lib.mkIf (unpackedModels != [ ]) [
        {
          home.packages = allScripts ++ [ llamaBenchAll ] ++ llamaBenchPerDevice;
          myconfig.persistence.cache-directories = [
            "benchmarks/llama-bench"
          ];
        }
      ];
    })
  ];
}
