# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.hermes — host-level (native) hermes-agent gateway backend.
#
# Declares the `myconfig.ai.hermes.*` options, imports the upstream
# `inputs.hermes-agent` NixOS module, and (when `enable = true` and the
# container backend is disabled) runs `services.hermes-agent` natively on
# the host. The containerized backend lives in `nixos-container.nix`.
#
# The shared `hermesServiceCfg` attrset and supporting paths/URLs live in
# `shared.nix` so both backends stay in sync.
{
  config,
  lib,
  pkgs,
  inputs,
  myconfig,
  ...
}:
let
  shared = import ./shared.nix {
    inherit
      config
      lib
      pkgs
      myconfig
      ;
  };
  inherit (shared) hermesServiceCfg stateDir;
  cfg = config.myconfig.ai.hermes;
in
{
  options = {
    myconfig.ai.hermes = {
      enable = lib.mkEnableOption "Hermes agent configuration";
      container = {
        enable = lib.mkEnableOption "Hermes gateway container";
        autostart = lib.mkEnableOption "Autostart Hermes gateway container";
      };
      microvm = {
        enable = lib.mkEnableOption "Hermes gateway microvm";
        autostart = lib.mkEnableOption "Autostart Hermes gateway microvm";
      };
    };
  };

  imports = [
    inputs.hermes-agent.nixosModules.default
  ];

  config = lib.mkIf cfg.enable {
    myconfig.persistence.directories = [ "hermes-agent" ];
    environment.sessionVariables = {
      HERMES_HOME = "${stateDir}/.hermes";
    };
    home-manager.users.mhuber =
      { pkgs, ... }:
      {
        home.packages = [
          inputs.hermes-agent.packages.${pkgs.system}.default
        ];
      };
    # Run the native host service only when neither the container nor the
    # microvm backend is active — those backends run hermes inside their own
    # isolated environment instead.
    services.hermes-agent = lib.mkIf (!cfg.container.enable && !cfg.microvm.enable) hermesServiceCfg;
  };
}
