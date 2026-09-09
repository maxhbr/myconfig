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
  # Auto-detect local LiteLLM proxy (same pattern as opencode and pi).
  # Falls back to `thing`'s wg0 IP when no local proxy is present.
  # NOTE: The local-proxy auto-detection only works for the native backend
  # (services.hermes-agent running on the host). The container and microvm
  # backends have isolated network namespaces where 127.0.0.1 does not reach
  # the host's litellm. Hosts using those backends should override
  # model.baseUrl explicitly or configure the container/microvm to forward
  # the host's litellm port.
  localLiteLlmBaseUrl =
    if config.services.litellm.enable then
      let
        # Rewrite wildcard bind to localhost for in-host clients
        # (same pattern as opencode/pi modules)
        litellmHost =
          if config.services.litellm.host == "0.0.0.0" then "127.0.0.1" else config.services.litellm.host;
      in
      "http://${litellmHost}:${toString config.services.litellm.port}/v1"
    else
      null;

  defaultModelBaseUrl =
    if localLiteLlmBaseUrl != null then
      localLiteLlmBaseUrl
    else
      "http://${myconfig.metadatalib.getWgIp "thing"}:4000/v1";

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

    user = mkOption {
      type = types.str;
      default = myconfig.user;
      defaultText = literalExpression "myconfig.user";
      description = "User that runs the hermes-agent service.";
    };

    group = mkOption {
      type = types.str;
      default = myconfig.user;
      defaultText = literalExpression "myconfig.user";
      description = "Group that runs the hermes-agent service.";
    };

    stateDir = mkOption {
      type = types.str;
      default = "/home/${myconfig.user}/hermes-agent";
      defaultText = literalExpression ''"/home/''\${myconfig.user}/hermes-agent"'';
      description = ''
        State directory for hermes-agent. Contains the .hermes/ subdir
        (HERMES_HOME) and the workspace/.
      '';
    };

    secretsDir = mkOption {
      type = types.str;
      default = "/home/${myconfig.user}/.hermes-secrets";
      defaultText = literalExpression ''"/home/''\${myconfig.user}/.hermes-secrets"'';
      description = ''
        Directory containing hermes secret files (e.g. the env file with
        API keys). Mounted read-only into the container/microvm backends.
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
        defaultText = literalExpression ''
          "http://127.0.0.1:4000/v1" when services.litellm is enabled,
          otherwise "http://<thing-wg0-ip>:4000/v1"
        '';
        description = ''
          Base URL for the model provider. Auto-detects the local LiteLLM
          proxy (localhost:4000) when `services.litellm.enable` is true
          (same pattern as opencode and pi-coding-agent). Falls back to
          LiteLLM on `thing`'s wg0 IP when no local proxy is present.
          Override per-host to point at a different OpenAI-compatible
          endpoint.
        '';
      };
    };

    apiServerPort = mkOption {
      type = types.nullOr types.port;
      default = 8642;
      description = ''
        Port for the hermes API server (the gateway's REST API).
        Set to null to omit the API server endpoint env lines
        (API_SERVER_ENABLED, API_SERVER_PORT, API_SERVER_HOST).
      '';
    };

    apiServerHost = mkOption {
      type = types.nullOr types.str;
      default =
        if config.myconfig.ai.hermes.container.enable then
          config.containers.hermes.localAddress
        else
          "localhost";
      defaultText = literalExpression ''"localhost"'';
      description = ''
        Host/address the hermes API server binds to, passed via
        API_SERVER_HOST. Defaults to the container's local address when
        the container backend is enabled, otherwise "localhost". Set to
        null to omit the API server endpoint env lines entirely.
      '';
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

    toolsets = mkOption {
      type = types.listOf types.str;
      default = [ "all" ];
      description = ''
        Toolsets enabled for the hermes agent. Defaults to [ "all" ].
        Restrict per-host (e.g. a headless server might drop browser tools).
      '';
    };

    telegram = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable the Telegram messaging integration. Only hosts with a
          Telegram bot token (in the secrets env file) should enable this.
        '';
      };
      requireMention = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether the bot requires @mention to respond in group chats.
          Ignored when telegram.enable is false.
        '';
      };
    };

    container = {
      enable = mkEnableOption "Hermes gateway container";
      autostart = mkEnableOption "Autostart Hermes gateway container";
      hostAddress = mkOption {
        type = types.str;
        default = "192.168.111.10";
        description = "Host-side IPv4 address for the container network.";
      };
      localAddress = mkOption {
        type = types.str;
        default = "192.168.111.11";
        description = "Container-side IPv4 address for the container network.";
      };
      hostAddress6 = mkOption {
        type = types.str;
        default = "fc00::1";
        description = "Host-side IPv6 address for the container network.";
      };
      localAddress6 = mkOption {
        type = types.str;
        default = "fc00::2";
        description = "Container-side IPv6 address for the container network.";
      };
    };
    microvm = {
      enable = mkEnableOption "Hermes gateway microvm";
      autostart = mkEnableOption "Autostart Hermes gateway microvm";
      vcpu = mkOption {
        type = types.ints.positive;
        default = 2;
        description = "Number of virtual CPU cores for the hermes microvm.";
      };
      mem = mkOption {
        type = types.ints.positive;
        # avoid QEMU "memory is exactly 2GB" hang, microvm.nix#171
        default = 2049;
        description = "Amount of RAM (in MB) for the hermes microvm.";
      };
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
    home-manager.users."${cfg.user}" =
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
