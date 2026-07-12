# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.hermes — host-level (native) hermes-agent gateway backend.
#
# Declares the `myconfig.ai.hermes.*` options, imports the upstream
# `inputs.hermes-agent` NixOS module, and (when `enable = true` and the
# container/microvm backend is disabled) runs `services.hermes-agent`
# natively on the host. The containerized backends live in
# `nixos-container.nix` and `microvm.nix`.
#
# The shared `hermesServiceCfg` attrset and supporting paths/URLs live in
# `shared.nix` so all backends stay in sync. Host-specific values (model
# name, base URL, port, HASS URL, extra packages) are options with
# sensible defaults — override them per-host.
{
  config,
  lib,
  pkgs,
  inputs,
  myconfig,
  ...
}:
let
  # Default model base URL: LiteLLM on `thing`'s wg0 IP (port 4000).
  # See hosts/host.thing/default.nix and hosts/host.thing/services.litellm.nix.
  defaultModelBaseUrl = "http://${myconfig.metadatalib.getWgIp "thing"}:4000/v1";

  shared = import ./shared.nix {
    inherit
      config
      lib
      pkgs
      myconfig
      ;
  };
  inherit (shared) hermesServiceCfg;
  cfg = config.myconfig.ai.hermes;
in
{
  options.myconfig.ai.hermes = with lib; {
    enable = mkEnableOption "Hermes agent configuration";

    stateDir = mkOption {
      type = types.str;
      default = "/home/mhuber/hermes-agent";
      description = ''
        State directory for hermes-agent. Contains the .hermes/ subdir
        (HERMES_HOME) and the workspace/.
      '';
    };

    model = {
      default = mkOption {
        type = types.str;
        default = "hermes";
        description = ''
          Default model name. Routed through the model provider's base_url
          (LiteLLM by default, which maps "hermes" → gfx1151:hermes on
          thing).
        '';
      };
      fallback = mkOption {
        type = types.str;
        default = "hermes-fallback";
        description = "Fallback model name (used when the primary fails).";
      };
      baseUrl = mkOption {
        type = types.str;
        default = defaultModelBaseUrl;
        defaultText = literalExpression ''"http://''${myconfig.metadatalib.getWgIp \"thing\"}:4000/v1"'';
        description = ''
          Base URL for the model provider. Defaults to LiteLLM on
          `thing`'s wg0 IP. Override per-host to point at a different
          OpenAI-compatible endpoint.
        '';
      };
    };

    apiServerPort = mkOption {
      type = types.port;
      default = 8642;
      description = "Port for the hermes API server (the gateway's REST API).";
    };

    hassUrl = mkOption {
      type = types.nullOr types.str;
      default = "http://hass.nuc.wg0.maxhbr.local";
      description = ''
        Home Assistant URL (passed to the hermes agent via env).
        Set to null to omit HASS_URL entirely.
      '';
    };

    extraPackages = mkOption {
      type = types.listOf types.package;
      default = with pkgs; [ openhue-cli ];
      defaultText = literalExpression "[ pkgs.openhue-cli ]";
      description = ''
        Extra packages available on the hermes-agent service PATH.
      '';
    };

    container = {
      enable = mkEnableOption "Hermes gateway container";
      autostart = mkEnableOption "Autostart Hermes gateway container";
    };
    microvm = {
      enable = mkEnableOption "Hermes gateway microvm";
      autostart = mkEnableOption "Autostart Hermes gateway microvm";
    };
  };

  imports = [
    inputs.hermes-agent.nixosModules.default
  ];

  config = lib.mkIf cfg.enable {
    myconfig.persistence.directories = [ "hermes-agent" ];
    environment.sessionVariables = {
      HERMES_HOME = "${cfg.stateDir}/.hermes";
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
