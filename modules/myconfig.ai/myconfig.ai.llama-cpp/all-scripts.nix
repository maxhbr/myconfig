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
  llamaLib = import ./lib { inherit lib pkgs; };
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

  # --- duplicate wrapper detection ---------------------------------------
  # Every (model, device) pair from `models` AND `scriptOnlyModels` (via
  # `devices` + `unlistedDevices`) generates a `llama-server_<Device>_<Name>`
  # wrapper. Two entries with the same (device, name) produce two different
  # derivations claiming the same `bin/<name>`, which makes home-manager's
  # `buildEnv` fail with "two given paths contain a conflicting subpath".
  # Fail at eval time with a precise message instead.
  duplicateScriptKeys =
    let
      guardedPairs = lib.concatMap (
        m:
        map (device: {
          inherit device;
          name = m.name;
        }) (builtins.filter guardDevice (m.devices ++ m.unlistedDevices))
      ) unpackedModels;
      counts = builtins.foldl' (
        acc: p:
        let
          key = "${p.device}_${p.name}";
        in
        acc // { ${key} = (acc.${key} or 0) + 1; }
      ) { } guardedPairs;
    in
    builtins.filter (k: counts.${k} > 1) (builtins.attrNames counts);
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

    {
      assertions = [
        {
          assertion = duplicateScriptKeys == [ ];
          message = ''
            myconfig.ai.llama-cpp: duplicate (device, model) wrapper entries:
              ${lib.concatStringsSep "\n  " (map (k: "llama-server_${k}") duplicateScriptKeys)}
            Each (model, device) pair in `models` and `scriptOnlyModels`
            (via `devices` and `unlistedDevices`) generates one
            `llama-server_<Device>_<Name>` home-manager wrapper; two entries
            with the same (device, name) collide in buildEnv with "two given
            paths contain a conflicting subpath". Remove the duplicate entry
            (usually a model present in both `models` (via `unlistedDevices`)
            and `scriptOnlyModels`) or give one of them a distinct `name`.
          '';
        }
      ];
    }
  ];
}
