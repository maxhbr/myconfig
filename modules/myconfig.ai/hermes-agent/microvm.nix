# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.hermes — microvm.nix backend.
#
# When `myconfig.ai.hermes.enable` and `myconfig.ai.hermes.microvm.enable`
# are both true, runs the hermes-agent gateway inside a microvm.nix
# MicroVM (a real VM with its own kernel, managed as a systemd service on
# the host). This gives stronger isolation than the NixOS-container backend
# (nixos-container.nix) at the cost of a per-VM kernel.
#
# The VM runs hermes in *native* mode (the VM itself is the isolation
# boundary, so no nested container is needed). It reuses `hermesServiceCfg`
# from shared.nix so the hermes configuration stays identical across all
# three backends — only the `API_SERVER_HOST` differs (bound to `0.0.0.0`
# here so the host can reach it via a forwarded port).
#
# The host-side `microvm` machinery is imported unconditionally (so the
# `microvm.*` options exist on every host), but neutralized with
# `microvm.host.enable = lib.mkDefault false` so that merely importing this
# module does *not* load tap/vhost_net modules, enable KSM or create the
# `microvm` user on hosts that don't use the backend. Hosts that opt in set
# `myconfig.ai.hermes.microvm.enable = true`, which flips
# `microvm.host.enable` to `true`.
#
# In addition to the VM itself, this module installs a `hermes-microvm`
# wrapper on the host that runs the hermes CLI pointed at the gateway's
# OpenAI-compatible API server (forwarded to host 127.0.0.1:8642), so the
# user can interact with the agent living inside the VM from the host
# shell.
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
  inherit (shared)
    hermesServiceCfg
    stateDir
    hostConfig
    ;
  cfg = config.myconfig.ai.hermes;

  # The hermes CLI binary (same package the native backend installs).
  hermesPkg = inputs.hermes-agent.packages.${pkgs.system}.default;

  # Host-side wrapper that runs the hermes CLI against the gateway running
  # inside the microvm. The gateway exposes an OpenAI-compatible API
  # (POST /v1/chat/completions etc.) on the forwarded host port 127.0.0.1:8642
  # — see `forwardPorts` below. The hermes CLI reads `OPENROUTER_BASE_URL` as
  # its model base_url fallback (the only base_url env var in cli.py:main)
  # and `OPENAI_API_KEY` for the key. The gateway has no `API_SERVER_KEY`
  # set, so auth is disabled and any key value is accepted.
  hermes-microvm = pkgs.writeShellApplication {
    name = "hermes-microvm";
    runtimeInputs = [ hermesPkg ];
    text = ''
      # Fail early with a helpful message if the microvm is not running.
      if ! /run/current-system/sw/bin/systemctl is-active --quiet microvm@hermes.service; then
        echo "The hermes microvm is not running." >&2
        echo "Start it with:  sudo systemctl start microvm@hermes" >&2
        echo "(autostart is controlled by myconfig.ai.hermes.microvm.autostart)" >&2
        exit 1
      fi

      # Point the hermes CLI at the gateway inside the VM. The gateway
      # advertises model "hermes-agent" (see its /v1/models endpoint).
      export OPENROUTER_BASE_URL="http://127.0.0.1:${toString cfg.apiServerPort}/v1"
      export OPENAI_API_KEY="local-key"
      exec hermes --model hermes-agent "$@"
    '';
  };
in
{
  imports = [
    inputs.microvm.nixosModules.host
  ];

  config = lib.mkMerge [
    # Neutralize the upstream default `microvm.host.enable = true` so that
    # importing this module on hosts that don't use the microvm backend has
    # no effect (no kernel modules, no KSM, no `microvm` user).
    { microvm.host.enable = lib.mkDefault false; }

    (lib.mkIf (cfg.enable && cfg.microvm.enable) {
      microvm.host.enable = true;

      # Host-side `hermes-microvm` CLI wrapper (see `let` block above).
      home-manager.users.mhuber =
        { pkgs, ... }:
        {
          home.packages = [ hermes-microvm ];
        };

      microvm.vms.hermes = {
        autostart = cfg.microvm.autostart;
        # The guest is a full NixOS system evaluated from this repo. Its
        # `config` is a NixOS module; microvm.nix auto-imports the guest
        # `microvm` module and evaluates it with the host's nixpkgs.
        config =
          {
            config,
            pkgs,
            lib,
            ...
          }:
          let
            # hermes' API server must bind to 0.0.0.0 inside the VM so the
            # host can reach it via the forwarded port (a localhost-bound
            # server is not reachable through QEMU user-mode port
            # forwarding, which targets the guest's 10.0.2.15 address).
            hermesMicrovmEnv = pkgs.writeText "hermes-microvm-api-env" ''
              OPENAI_API_KEY=local-key
              ${lib.optionalString (cfg.apiServerPort != null) ''
                API_SERVER_ENABLED=true
                API_SERVER_PORT=${toString cfg.apiServerPort}
                API_SERVER_HOST=0.0.0.0
              ''}
              ${lib.optionalString (cfg.hassUrl != null) "HASS_URL=${cfg.hassUrl}"}
            '';
          in
          {
            imports = [
              inputs.hermes-agent.nixosModules.default
            ];

            networking.hostName = "hermes";
            # Open the guest firewall for the API port so forwarded traffic
            # from the host reaches the hermes API server.
            networking.firewall.allowedTCPPorts = [ cfg.apiServerPort ];

            # ── microvm guest configuration ─────────────────────────────
            microvm = {
              # qemu with KVM acceleration on x86_64 Linux (f13 is a
              # Framework AMD laptop). cloud-hypervisor is an alternative.
              hypervisor = "qemu";
              vcpu = 2;
              mem = 2048;

              # Share the host's /nix/store read-only so the VM needs no
              # store disk image and boots fast. storeOnDisk auto-disables
              # because a share with source "/nix/store" is present.
              shares = [
                {
                  tag = "nix-store";
                  source = "/nix/store";
                  mountPoint = "/nix/store";
                  proto = "virtiofs";
                  readOnly = true;
                }
                {
                  # Persistent hermes state lives on the host (mirrors the
                  # container backend's bind-mount). For fully VM-local
                  # state, replace this share with a `microvm.volumes`
                  # entry (auto-created ext4 image) mounted at stateDir.
                  tag = "hermes-state";
                  source = stateDir;
                  mountPoint = stateDir;
                  proto = "virtiofs";
                }
                {
                  # Secrets stay on the host (agenix-managed) and are
                  # exposed read-only to the VM.
                  tag = "hermes-secrets";
                  source = "/home/mhuber/.hermes-secrets";
                  mountPoint = "/home/mhuber/.hermes-secrets";
                  proto = "virtiofs";
                  readOnly = true;
                }
              ];

              # SLiRP user-mode networking: the VM NATs through the host,
              # so it can reach `thing`'s LiteLLM (wg0) and other hosts the
              # host can route to. No host bridge or tap setup needed.
              interfaces = [
                {
                  type = "user";
                  id = "qemu";
                  mac = "02:00:00:01:01:01";
                }
              ];

              # Expose the hermes API on the host's localhost.
              forwardPorts = [
                {
                  from = "host";
                  host.address = "127.0.0.1";
                  host.port = cfg.apiServerPort;
                  guest.port = cfg.apiServerPort;
                }
              ];
            };

            # ── hermes service (native, inside the VM) ──────────────────
            # Reuse the shared hermes config, only overriding the env so
            # the API server binds to 0.0.0.0 (see comment above).
            services.hermes-agent = hermesServiceCfg // {
              environmentFiles = [
                "/home/mhuber/.hermes-secrets/env"
                "${hermesMicrovmEnv}"
              ];
            };

            # The hermes service runs as `mhuber` (createUser = false in
            # hermesServiceCfg), so create the user/group inside the VM.
            # UID matches the host so shared files keep consistent ownership.
            users.users.mhuber = {
              isNormalUser = true;
              home = "/home/mhuber";
              createHome = true;
              uid = hostConfig.users.users.mhuber.uid or 1000;
              extraGroups = [ "mhuber" ];
            };
            users.groups.mhuber = { };

            system.stateVersion = "25.11";
          };
      };
    })
  ];
}
