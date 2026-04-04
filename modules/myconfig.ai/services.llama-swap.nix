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
  mkLlamaCppScriptName = {name, device, file}: "llama-server_${name}_${device}";

  mkLlamaCppScript = args@{name, device, file}: let
      llama-cuda-server = lib.getExe' pkgs.llama-cpp "llama-server";
      llama-rocm-server = lib.getExe' pkgs.llama-cpp-rocm "llama-server";
      llama-vulkan-server = lib.getExe' pkgs.llama-cpp-vulkan "llama-server";
      llama-server = if lib.strings.hasPrefix "Vulkan" device
                     then llama-cpp-vulkan
                     else if lib.strings.hasPrefix "ROCm" device
                          then llama-rocm-server
                          else llama-cuda-server;
    in writeShellApplication {
      name = mkLlamaCppScriptName args;

      runtimeInputs = [
      ];

      text = ''
        ${llama-server} --port $1 -m "${file}" --gpu-layers 999 -fa on --no-webui
      '';
    };

  hasGpuVariant = v: builtins.elem v config.myconfig.hardware.gpu.variant;
  guardLlamaCppScript = device:
      if lib.strings.hasPrefix "Vulkan" device
      then (hasGpuVariant "amd" || hasGpuVariant "amd-no-rocm")
      else if lib.strings.hasPrefix "ROCm" device
           then (hasGpuVariant "amd")
           else if lib.strings.hasPrefix "CUDA" device
               then (hasGpuVariant "nvidia")
               else false;
in 
{
  imports = [
  ];
  options.myconfig = with lib; {
    ai = {
      localModelApps = mkOption {
        type = types.listOf (
          types.submodule {
            options = {
              name = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "model alias";
              };
              devices = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "Devices to provide scripts for the CARD";
              };
            };
          }
        );
        default = [ ];
        description = "List of local model server instances (e.g. llama-cpp) available for AI tools";
      };
    };
  };
  config = lib.mkIf config.services.llama-swap.enable {
    services.llama-swap = {
      settings = {
        sendLoadingState = true;
      };
    };

    systemd.services.llama-swap = {
      # https://github.com/nixos/nixpkgs/issues/441531
      environment.XDG_CACHE_HOME = "/var/cache/llama-swap";
      serviceConfig.CacheDirectory = "llama-swap";
    };
    home-manager.sharedModules = [{
      home.packages =
        let
          scripts = lib.mapAttrs' (model: cfg: {
            name = "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}";
            value = pkgs.writeShellScriptBin "llama-manual-${lib.replaceStrings [ ":" ] [ "_" ] model}" ''
              export PORT=''${1:-33657}
              ${lib.concatStringsSep "\n" (map (e: "export ${e}") cfg.env)}
              set -x
              ${cfg.cmd}
            '';
          }) config.services.llama-swap.settings.models;
        in
        lib.attrValues scripts;
    }];
  };
}

