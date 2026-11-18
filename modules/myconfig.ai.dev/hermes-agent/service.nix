# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.hermes — host-level (native) hermes-agent gateway backend.
#
# Declares the `myconfig.ai.dev.hermes.*` options, imports the upstream
# `inputs.hermes-agent` NixOS module, and (when `service.enable = true` and
# neither the container nor the microvm backend is selected) runs
# `services.hermes-agent` natively on the host. The containerized backends
# live in `nixos-container.nix` and `microvm.nix`.
#
# Two switches, on purpose:
#
#   enable          — the hermes CLI for this host, nothing else. This is
#                     the tier the sandbox integrations need (mysbx,
#                     ai.microvm agents) and it is what a host wants when
#                     hermes should only ever run inside a sandbox.
#   service.enable  — the gateway, run by the backend selected with
#                     `service.container.enable` / `service.microvm.enable`
#                     (native systemd service when neither is set). Selecting
#                     a backend implies this, and this implies `enable`.
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
  cfg = config.myconfig.ai.dev.hermes;

  # The host-side CLI launcher: upstream package with the seeding `bin/hermes`
  # prepended (the same symlinkJoin trick ./mysbx.nix uses for the sandbox
  # tier; named `hermes-agent-cli-…` to tell it from the mysbx launcher).
  # `hermes-agent` / `hermes-acp` stay untouched.
  hermesCliSeeded = pkgs.symlinkJoin {
    name = "hermes-agent-cli-${lib.getVersion hermesUpstreamPkg}";
    paths = [ hermesUpstreamPkg ];
    inherit (hermesUpstreamPkg) meta;
    postBuild = ''
      mv "$out/bin/hermes" "$out/bin/.hermes-upstream"
      cat >"$out/bin/hermes" <<EOF
      #! ${pkgs.runtimeShell} -e
      ${lib.getExe cliSeeding.seed} || true
      exec -a "\$0" "$out/bin/.hermes-upstream" "\$@"
      EOF
      chmod +x "$out/bin/hermes"
    '';
    passthru.unwrapped = hermesUpstreamPkg;
  };

  hermesUpstreamPkg = inputs.hermes-agent.packages.${pkgs.stdenv.hostPlatform.system}.default;

  # The host `~/.hermes/config.yaml` seeding (./seed-cli-config.nix for the
  # merge rules). Unlike the sandbox tier the host launcher ALSO registers
  # the local LiteLLM proxy as a NAMED provider entry, so the model picker
  # (`/model`) knows the proxy and its model list — the host config was
  # previously incomplete: no provider configured at all, and the picker
  # had no litellm entry.
  cliSeeding = import ./seed-cli-config.nix {
    inherit lib pkgs;
    baseUrl = cfg.model.baseUrl;
    apiKey = "local-key";
    defaultModel = cfg.model.default;
    providerEntry = {
      key = "${config.networking.hostName}-litellm";
      name = "LiteLLM (${config.networking.hostName})";
      models =
        if config.services.litellm.enable then
          (lib.unique (lib.map (m: m.model_name) config.services.litellm.settings.model_list))
        else
          [ cfg.model.default ];
    };
  };
in
{
  options.myconfig.ai.dev.hermes = with lib; {
    enable = mkEnableOption "the hermes CLI for this host (installs the CLI only; the gateway is `service.enable`)";

    # Deliberately NOT under `service`: the sandbox tiers
    # (mysbx, ai.microvm agents) run the hermes CLI against the same model
    # route as the host gateway without running a gateway themselves.
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

    service = {
      enable = mkEnableOption "the hermes-agent gateway (native systemd service, NixOS container or microvm — see `service.container`/`service.microvm`)";

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
          if config.myconfig.ai.dev.hermes.service.container.enable then
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
  };

  imports = [
    inputs.hermes-agent.nixosModules.default
  ];

  config = lib.mkMerge [
    # Selecting a backend implies the gateway it runs, and the gateway implies
    # the CLI — `service.enable` on its own is never a no-op.
    {
      myconfig.ai.dev.hermes.service.enable = lib.mkIf (
        config.myconfig.ai.dev.hermes.service.container.enable
        || config.myconfig.ai.dev.hermes.service.microvm.enable
      ) (lib.mkDefault true);
    }
    {
      myconfig.ai.dev.hermes.enable = lib.mkIf config.myconfig.ai.dev.hermes.service.enable (
        lib.mkDefault true
      );
    }

    # CLI tier: the binary for the invoking user, no service. The package
    # is the SEEDED launcher (the host twin of the mysbx one,
    # ./mysbx.nix + ./seed-cli-config.nix): hermes keeps `config.yaml`,
    # `.env`, `auth.json`, `state.db` and `sessions/` in ONE root, so the
    # config cannot be a `home.file` entry — it would fight the CLI's own
    # writes. Instead `bin/hermes` merges the LiteLLM route and a named
    # `<hostname>-litellm` provider entry into `~/.hermes/config.yaml` on
    # every launch (idempotent; a user-set `model.default` and any other
    # provider entries survive), so a host session knows the local proxy
    # as a provider and never drops into the first-run wizard.
    (lib.mkIf cfg.enable {
      home-manager.users."${myconfig.user}" =
        { pkgs, ... }:
        {
          home.packages = [ hermesCliSeeded ];
        };
    })

    # Gateway tier: state, HERMES_HOME and one backend.
    (lib.mkIf cfg.service.enable {
      myconfig.persistence.directories = [ "hermes-agent" ];
      environment.sessionVariables = {
        HERMES_HOME = "${cfg.service.stateDir}/.hermes";
      };
      # Run the native host service only when neither the container nor the
      # microvm backend is active — those backends run hermes inside their
      # own isolated environment instead.
      services.hermes-agent = lib.mkIf (
        !cfg.service.container.enable && !cfg.service.microvm.enable
      ) hermesServiceCfg;
    })
  ];
}
