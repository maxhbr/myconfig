# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.services.llama-server;
  instancesWithKeys = map (key: {
    inherit key;
    value = cfg.instances.${key};
  }) (builtins.attrNames cfg.instances);
  enabledInstances = builtins.filter (inst: inst.value.enable) instancesWithKeys;
  createScript =
    inst:
    pkgs.writeShellScriptBin "llama-server-${inst.key}" ''
      ${lib.concatMapStrings (env: "export ${env}\n") (
        [
          (lib.optionalString (inst.value.device != null) "LLAMA_ARG_DEVICE=${inst.value.device}")
        ]
        ++ inst.value.extraEnvironment
      )}
      exec ${pkgs.llama-cpp}/bin/llama-server \
        -m ${inst.value.modelPath} \
        --port ${toString inst.value.port} \
        -c ${toString inst.value.contextSize} \
        ${lib.optionalString inst.value.flashAttention "-fa on"} \
        ${lib.optionalString inst.value.continuousBatching "-cb on"} \
        ${inst.value.extraArgs} \
        "$@"
    '';
  scripts = builtins.map createScript enabledInstances;
  getScriptByName = name: builtins.elemAt (builtins.filter (s: s.pname == name) scripts) 0;
in
{
  options.myconfig.ai.services = with lib; {
    llama-server = {
      instances = mkOption {
        type = types.attrsOf (
          types.submodule {
            options = {
              enable = mkEnableOption "this llama-server instance";

              modelPath = mkOption {
                type = types.str;
                description = "Path to the GGUF model file";
              };

              port = mkOption {
                type = types.int;
                description = "Port for the LLM server";
              };

              contextSize = mkOption {
                type = types.int;
                default = 2048;
                description = "Context window size";
              };

              device = mkOption {
                type = types.nullOr types.str;
                default = null;
                description = "Device to use (e.g., 'Vulkan1', 'CUDA0'); null for default";
              };

              flashAttention = mkOption {
                type = types.bool;
                default = false;
                description = "Enable flash attention";
              };

              continuousBatching = mkOption {
                type = types.bool;
                default = false;
                description = "Enable continuous batching";
              };

              extraEnvironment = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "Additional environment variables";
              };

              extraArgs = mkOption {
                type = types.str;
                default = "";
                description = "Additional arguments to pass to llama-server";
              };
            };
          }
        );
        default = { };
        description = "Named llama-server instances";
      };
    };
  };

  config = lib.mkIf (builtins.any (inst: inst.value.enable) instancesWithKeys) {
    home-manager.sharedModules = [
      {
        home.packages = scripts;
        systemd.user.services = builtins.listToAttrs (
          builtins.map (inst: {
            name = "llama-server-${inst.key}";
            value = {
              Unit = {
                Description = "LLM server: ${inst.key}";
                Wants = [ "graphical-session.target" ];
                After = [ "graphical-session.target" ];
                PartOf = [ "graphical-session.target" ];
                Requisite = [ "graphical-session.target" ];
              };
              Service = {
                ExecStart = "${createScript inst}/bin/llama-server-${inst.key}";
                Restart = "on-failure";
                Environment = [
                  (lib.optionalString (inst.value.device != null) "LLAMA_ARG_DEVICE=${inst.value.device}")
                ]
                ++ inst.value.extraEnvironment;
              };
              Install = {
                WantedBy = [ "graphical-session.target" ];
              };
            };
          }) enabledInstances
        );
      }
    ];
  };
}
