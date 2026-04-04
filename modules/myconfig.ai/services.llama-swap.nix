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

  # Build environment variables for a device
  envForDevice =
    device:
    [ "LLAMA_ARG_DEVICE=${device}" ]
    ++ lib.optional (
      lib.hasPrefix "Vulkan" device || lib.hasPrefix "ROCm" device
    ) "CUDA_VISIBLE_DEVICES=";

  # Generate a single llama-swap model entry
  mkModelEntry =
    {
      model,
      device,
      isFirstDevice ? false,
      suffix ? "",
      extraArgs ? "",
    }:
    let
      server = llamaServerFor device;
      modelKey = "${device}:${model.name}${suffix}";
    in
    {
      "${modelKey}" = {
        cmd = ''
          ${server} --port ''${PORT} -m "${model.path}" --gpu-layers 999 -fa on --no-webui ${model.params} ${extraArgs}
        '';
        env = envForDevice device;
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

  # Generate all models from the input list
  allModels = lib.mkMerge (lib.concatMap mkModelEntries cfg.models);
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
    services.llama-swap = {
      settings = {
        sendLoadingState = true;
        models = allModels;
      };
    };

    systemd.services.llama-swap = {
      # https://github.com/nixos/nixpkgs/issues/441531
      environment.XDG_CACHE_HOME = "/var/cache/llama-swap";
      serviceConfig.CacheDirectory = "llama-swap";
    };
    home-manager.sharedModules = [
      {
        home.packages =
          let
            scripts = lib.mapAttrs' (model: modelCfg: {
              name = "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}";
              value = pkgs.writeShellScriptBin "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}" ''
                export PORT=''${1:-33657}
                ${lib.concatStringsSep "\n" (map (e: "export ${e}") modelCfg.env)}
                set -x
                ${modelCfg.cmd}
              '';
            }) config.services.llama-swap.settings.models;
          in
          lib.attrValues scripts;
      }
    ];
  };
}
