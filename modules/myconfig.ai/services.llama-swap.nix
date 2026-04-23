# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  myconfig,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-swap;

  hasGpuVariant = v: builtins.elem v config.myconfig.hardware.gpu.variant;

  # Determine whether a given device string is supported by the current hardware
  guardDevice =
    device:
    if lib.hasPrefix "Vulkan" device then
      (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm")
    else if lib.hasPrefix "ROCm" device then
      (hasGpuVariant "amd")
    else if lib.hasPrefix "CUDA" device then
      (hasGpuVariant "nvidia")
    else
      false;

  # Select the correct llama-server binary for a device
  llamaServerFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-server"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-server"
    else
      lib.getExe' pkgs.llama-cpp "llama-server";
  llamaBenchFor =
    device:
    if lib.hasPrefix "Vulkan" device then
      lib.getExe' pkgs.llama-cpp-vulkan "llama-bench"
    else if lib.hasPrefix "ROCm" device then
      lib.getExe' pkgs.llama-cpp-rocm "llama-bench"
    else
      lib.getExe' pkgs.llama-cpp "llama-bench";

  # Build environment variables for a device
  envForDevice =
    device:
    [ "LLAMA_ARG_DEVICE=${device}" ]
    ++ lib.optional (
      lib.hasPrefix "Vulkan" device || lib.hasPrefix "ROCm" device
    ) "CUDA_VISIBLE_DEVICES=";

  # Build a shell application that launches llama-server for a specific model+device combo.
  # Usage: <script> <port> [extra-args...]
  mkLlamaScript =
    {
      model,
      device,
      suffix ? "",
      extraArgs ? "",
    }:
    let
      server = llamaServerFor device;
      safeName = lib.replaceStrings [ ":" ] [ "-" ] "${model.name}${suffix}";
      scriptName = "llama-server_${device}_${safeName}";
      envExports = lib.concatStringsSep "\n" (map (e: "export ${e}") (envForDevice device));
    in
    pkgs.writeShellApplication {
      name = scriptName;
      runtimeInputs = [ ];
      text = ''
        ${envExports}
        ${server} --port "$1" -m "${model.path}" --gpu-layers 999 -fa on --no-webui ${model.params} ${extraArgs} "''${@:2}"
      '';
    };

  mkLlamaBenchScript =
    {
      model,
      device,
    }:
    let
      bench = llamaBenchFor device;
      safeName = lib.replaceStrings [ ":" ] [ "-" ] "${model.name}";
      scriptName = "llama-bench_${device}_${safeName}";
      envExports = lib.concatStringsSep "\n" (map (e: "export ${e}") (envForDevice device));
    in
    pkgs.writeShellApplication {
      name = scriptName;
      runtimeInputs = [ ];
      text = ''
        ${envExports}
        dir="$HOME/benchmarks/llama-bench-logs"
        mkdir -p "$dir"
        exec &> >(tee -a "$dir/${scriptName}.log")
        set -x
        ${bench} -m "${model.path}" ${model.params} -d 0,4096,8192,16384,32768 -p 2048 -n 32 -ub 2048 -mmp 0
      '';
    };

  # Generate a single llama-swap model entry backed by a shell application
  mkModelEntry =
    {
      model,
      device,
      isFirstDevice ? false,
      suffix ? "",
      extraArgs ? "",
    }:
    let
      script = mkLlamaScript {
        inherit
          model
          device
          suffix
          extraArgs
          ;
      };
      modelKey = "${device}:${model.name}${suffix}";
    in
    {
      "${modelKey}" = {
        cmd = "${lib.getExe script} \${PORT}";
        ttl = model.ttl;
      }
      // lib.optionalAttrs (model.aliases != [ ] && suffix == "" && isFirstDevice) {
        inherit (model) aliases;
      };
    };

  # Generate all model entries for a single model input across all its devices
  mkModelEntries =
    model:
    let
      eligibleDevices = builtins.filter guardDevice model.devices;
      firstDevice = if eligibleDevices != [ ] then builtins.head eligibleDevices else null;
    in
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        let
          isFirstDevice = device == firstDevice;
        in
        [
          # Base model entry
          (mkModelEntry { inherit model device isFirstDevice; })
        ]
        ++ lib.optionals (model.mmproj != null) [
          # mmproj variant
          (mkModelEntry {
            inherit model device;
            suffix = ":mmproj";
            extraArgs = ''--mmproj "${model.mmproj}"'';
          })
        ]
      )
    ) model.devices;

  # Collect all script derivations for home-manager packages
  mkScriptEntries =
    model:
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        [
          (mkLlamaScript { inherit model device; })
          (mkLlamaBenchScript { inherit model device; })
        ]
        ++ lib.optionals (model.mmproj != null) [
          (mkLlamaScript {
            inherit model device;
            suffix = ":mmproj";
            extraArgs = ''--mmproj "${model.mmproj}"'';
          })
        ]
      )
    ) model.devices;

  allScripts = lib.concatMap mkScriptEntries cfg.models;

  # Generate all models from the input list
  allModels = lib.mkMerge (lib.concatMap mkModelEntries cfg.models);

  # Extract the numeric suffix from a device string (e.g. "Vulkan0" -> "0", "ROCm1" -> "1").
  # Devices with the same index share the same physical GPU, so they must be in the same group.
  deviceIndex =
    device:
    let
      m = builtins.match ".*([0-9]+)$" device;
    in
    if m != null then builtins.head m else device;

  # Build groups: one group per physical GPU (by device index) so models on different GPUs
  # can run simultaneously. Within the same GPU, models swap (they share VRAM).
  allGroups =
    let
      # Collect (gpuIndex, modelKey) pairs for all eligible model entries
      gpuModelPairs = lib.concatMap (
        model:
        lib.concatMap (
          device:
          lib.optionals (guardDevice device) (
            [
              {
                gpu = deviceIndex device;
                key = "${device}:${model.name}";
              }
            ]
            ++ lib.optional (model.mmproj != null) {
              gpu = deviceIndex device;
              key = "${device}:${model.name}:mmproj";
            }
          )
        ) model.devices
      ) cfg.models;

      # Group model keys by GPU index
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

  # Collect all model names/keys exposed by this llama-swap instance for localModels registration
  allModelNames = lib.concatMap (
    model:
    lib.concatMap (
      device:
      lib.optionals (guardDevice device) (
        [ "${device}:${model.name}" ]
        ++ lib.optional (model.mmproj != null) "${device}:${model.name}:mmproj"
      )
    ) model.devices
    ++ model.aliases
  ) cfg.models;
in
{
  imports = [ ];
  options.myconfig.ai.llama-swap = with lib; {
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
              default = [ ];
              description = "List of devices to run this model on (e.g. 'Vulkan0', 'CUDA0', 'ROCm0')";
            };
            mmproj = mkOption {
              type = types.nullOr types.str;
              default = null;
              description = "Path to mmproj file; when set, a :mmproj variant is auto-generated";
            };
            params = mkOption {
              type = types.str;
              default = "";
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
          };
        }
      );
      default = [ ];
      description = "Declarative model definitions that are expanded into llama-swap model entries per device";
    };
  };
  config = lib.mkIf config.services.llama-swap.enable {
    myconfig.ai.localModels = [
      {
        name = "llama-swap";
        models = allModelNames;
        port = config.services.llama-swap.port;
      }
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
    home-manager.sharedModules = [
      {
        home.packages = allScripts;
        myconfig.persistence.cache-directories = [ "benchmarks/llama-bench-logs" ];
      }
    ];
  };
}
