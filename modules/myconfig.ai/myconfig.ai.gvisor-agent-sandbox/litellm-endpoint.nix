# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.gvisor-agent-sandbox — sandbox-reachable LiteLLM endpoint.
#
# The host LiteLLM proxy is loopback-only on purpose
# (`services.litellm.host = mkForce "127.0.0.1"` in ../services.litellm.nix),
# so a rootless Podman sandbox cannot reach it directly: with pasta (podman's
# rootless default) the sandbox's own 127.0.0.1 is the container's loopback, not
# the host's, and runsc runs its own netstack on top of the pasta netns.
#
# This file used to expose the proxy through a member-less bridge (`agentsbr0`,
# 192.168.84.1) plus a socket-activated `systemd-socket-proxyd` bound to it.
# That did NOT work, and the reason is worth remembering:
#
#   pasta re-opens the sandbox's outbound connections in the host network
#   namespace, but it BINDS those outbound sockets to the host's default-route
#   interface (pasta(1) `--outbound-if4` default). A connection from the
#   sandbox to the host-local bridge address therefore egresses that
#   default-route interface (the LAN NIC or the VPN tun) toward its gateway,
#   instead of being locally delivered to the host. It never reaches the bridge
#   listener. The socket's `BindToDevice = <bridge>` was a second, independent
#   barrier: it rejected any connection not arriving on the bridge. Either way
#   the endpoint was unreachable from a sandbox — observed as
#   `curl: (7) ... after 0 ms` (the VPN default-route case) or a timeout (the
#   LAN default-route case). See ./docs/debug-model-endpoint-routing.md.
#
# The working mechanism is pasta's `--map-host-loopback` (pasta(1)): it
# translates a chosen address to the host's 127.0.0.1 *before* egress, using
# pasta's local-traffic bypass, so the connection is locally delivered on the
# host loopback (a trusted firewall interface) straight to LiteLLM. runsc's
# netstack just routes the chosen address to the tap; pasta does the rest. This
# is NOT the `-T` listener mechanism (which is invisible to runsc's netstack)
# and NOT `host.containers.internal` (`--map-guest-addr`, which maps to the
# host global address, not the loopback).
#
# The actual pasta options are baked into the `agent-session` wrapper as
# `AGENT_SANDBOX_NETWORK` in ./default.nix — a `pasta:--map-gw,--map-host-loopback,
# <address>` podman network spec: `--map-gw` suppresses podman's own
# `--no-map-gw` (which would disable the loopback mapping), and
# `--map-host-loopback <address>` picks the translated address. `doctor` probes
# the endpoint through that same network, so it exercises the real path.
#
# This module therefore only contributes the *address and port* of the endpoint
# (the `--map-host-loopback` target), the read-only base URL
# (`litellm.endpoint`) and the `--env-file` for `agent-session start`. The host
# LiteLLM proxy itself is not touched and stays loopback-only.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;
  lcfg = cfg.litellm;

  address = lcfg.address;
  port = toString lcfg.port;

  enabled = cfg.enable && lcfg.enable;
in
{
  options.myconfig.ai.gvisor-agent-sandbox.litellm = with lib; {
    enable = mkOption {
      type = types.bool;
      default = config.services.litellm.enable;
      defaultText = literalExpression "config.services.litellm.enable";
      description = ''
        Make the loopback-only host LiteLLM proxy reachable from sandboxes by
        advertising a host address that pasta maps to the host loopback (see
        ./default.nix, `AGENT_SANDBOX_NETWORK`). On by default whenever the host
        runs LiteLLM at all.
      '';
    };

    port = mkOption {
      type = types.port;
      default = config.services.litellm.port;
      defaultText = literalExpression "config.services.litellm.port";
      description = ''
        Port of the host LiteLLM proxy. The sandbox endpoint uses this same
        port: `--map-host-loopback` translates only the address (to 127.0.0.1),
        keeping the port, so the connection lands on the loopback proxy.
      '';
    };

    address = mkOption {
      type = types.str;
      default = "192.168.84.1";
      description = ''
        Host address the sandbox connects to, which pasta's
        `--map-host-loopback` translates to the host's 127.0.0.1. It does NOT
        need to be assigned to any host interface — it is a pure translation
        target, intercepted by pasta before egress — but it must be stable,
        non-loopback, and not collide with a network the sandbox routes for
        real: not `myconfig.ai.microvm.subnet`, nor the host LAN or WireGuard
        ranges.
      '';
    };

    endpoint = mkOption {
      type = types.str;
      readOnly = true;
      default = "http://${address}:${port}/v1";
      defaultText = literalExpression ''"http://''${address}:''${port}/v1"'';
      description = ''
        OpenAI-compatible base URL of the endpoint, as seen from inside a
        sandbox (pasta maps `address` to the host loopback). Read-only; also
        written to `~/.config/agent-sandbox/litellm.env` for
        `agent-session start --env-file`.
      '';
    };
  };

  config = lib.mkIf enabled {
    assertions = [
      {
        assertion = config.services.litellm.enable;
        message = ''
          myconfig.ai.gvisor-agent-sandbox.litellm.enable is on, but
          services.litellm is not enabled — the sandbox endpoint would map to
          127.0.0.1:${port}, where nothing listens.
        '';
      }
    ];

    # Ready-made `--env-file` for `agent-session start`. Contains no secret:
    # the API key stays out of the Nix store and out of the session state.
    home-manager.sharedModules = [
      {
        xdg.configFile."agent-sandbox/litellm.env".text = ''
          OPENAI_BASE_URL=${lcfg.endpoint}
        '';
      }
    ];
  };
}
