# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.gvisor-agent-sandbox — sandbox-reachable LiteLLM endpoint.
#
# The host-side forwarder that carries the loopback-only LiteLLM proxy into a
# container sandbox lives in ../../myconfig.ai.dev.litellm-forwarder.nix, which
# this module turns on and configures; that file documents the mechanism
# (`--map-guest-addr` plus a port-scoped `systemd-socket-proxyd`), why it is
# port-scoped, and which alternatives do not work.
#
# This module contributes the tier's view of it: the *address, forward port and
# endpoint URL* used by `agent-gvisor` (whose pasta network spec,
# `AGENT_GVISOR_NETWORK`, is baked in ./default.nix), the in-sandbox loopback
# relay, and the `--env-file` for `agent-gvisor start`.
{
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.ai.dev.gvisor-agent-sandbox;
  lcfg = cfg.litellm;

  address = lcfg.address;
  port = toString lcfg.port;
  forwardPort = toString lcfg.forwardPort;

  enabled = cfg.enable && lcfg.enable;
in
{
  options.myconfig.ai.dev.gvisor-agent-sandbox.litellm = with lib; {
    enable = mkOption {
      type = types.bool;
      default = config.services.litellm.enable;
      defaultText = literalExpression "config.services.litellm.enable";
      description = ''
        Make the loopback-only host LiteLLM proxy reachable from sandboxes by
        running a port-scoped forwarder
        (`myconfig.ai.dev.litellm-forwarder`, which this option turns on) and
        advertising a host address that pasta maps to the host's global address
        (see ./default.nix, `AGENT_GVISOR_NETWORK`). On by default whenever the
        host runs LiteLLM at all.
      '';
    };

    port = mkOption {
      type = types.port;
      default = config.services.litellm.port;
      defaultText = literalExpression "config.services.litellm.port";
      description = ''
        Port of the host LiteLLM proxy (on `127.0.0.1`). The forwarder proxies
        to this port; the sandbox never connects to it directly.
      '';
    };

    forwardPort = mkOption {
      type = types.port;
      default = lcfg.port + 10000;
      defaultText = literalExpression "config.services.litellm.port + 10000";
      description = ''
        Port the port-scoped forwarder listens on (`0.0.0.0:${"forwardPort"}` →
        `127.0.0.1:${"port"}`). This is the port the sandbox connects to. It
        MUST differ from `port` so the `0.0.0.0` wildcard bind does not collide
        with LiteLLM's own `127.0.0.1:${"port"}` listener.

        The NixOS firewall trusts `lo` and drops this port on every other
        interface (it is not added to `allowedTCPPorts`), so the forwarder is
        reachable only from the host and from sandboxes (whose pasta
        connections are local), never from external hosts.
      '';
    };

    address = mkOption {
      type = types.str;
      default = "192.168.84.1";
      description = ''
        Host address the sandbox connects to, which pasta's `--map-guest-addr`
        translates to the host's global address (the address on the
        default-route interface). It does NOT need to be assigned to any host
        interface — it is a pure translation target — but it must be stable,
        non-loopback, and not collide with a network the sandbox routes for
        real: not `myconfig.ai.dev.microvm.subnet`, nor the host LAN or WireGuard
        ranges.

        Unlike pasta's `--map-host-loopback` (which translates an address to
        `127.0.0.1`, exposing every loopback port), `--map-guest-addr` maps to
        the host's global address, so loopback-ONLY services stay unreachable.
      '';
    };

    loopbackForward = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Also serve the endpoint on the sandbox's OWN `127.0.0.1:${"port"}`, by
        relaying it there from inside the sandbox
        (`/bin/agent-gvisor-init`, baked into the image).

        The host loopback is not reachable from a sandbox and cannot be made
        reachable from the outside: runsc runs its own network stack, so only a
        process INSIDE the sandbox can bind a port that sandboxed processes see
        on `127.0.0.1` (this is why pasta's `-T` does not work here). The relay
        is that process; it forwards to `address:${"forwardPort"}`, which is
        the path the sandbox can already use.

        It therefore grants no additional reach — it only lets configuration
        that names `http://127.0.0.1:${"port"}` verbatim (host agent configs,
        `OPENAI_BASE_URL`, MCP servers, a hand-typed `curl`) work unchanged,
        including everything the `home.rewriteEndpoints` rules do not catch.
      '';
    };

    endpoint = mkOption {
      type = types.str;
      readOnly = true;
      default = "http://${address}:${forwardPort}/v1";
      defaultText = literalExpression ''"http://''${address}:''${forwardPort}/v1"'';
      description = ''
        OpenAI-compatible base URL of the endpoint, as seen from inside a
        sandbox (pasta maps `address` to the host's global address, where the
        port-scoped forwarder listens on `forwardPort`). Read-only; also
        written to `~/.config/agent-gvisor/litellm.env` for
        `agent-gvisor start --env-file`.
      '';
    };
  };

  config = lib.mkIf enabled {
    # The host side, shared with the podman-gvisor backend of mysbx: this tier
    # decides the port pair and the advertised address, the forwarder module
    # runs the proxy and checks the two invariants (LiteLLM enabled, distinct
    # ports).
    myconfig.ai.dev.litellm-forwarder = {
      enable = true;
      inherit (lcfg) port forwardPort address;
    };

    # Ready-made `--env-file` for `agent-gvisor start`. Contains no secret:
    # the API key stays out of the Nix store and out of the session state.
    home-manager.sharedModules = [
      {
        xdg.configFile."agent-gvisor/litellm.env".text = ''
          OPENAI_BASE_URL=${lcfg.endpoint}
        '';
      }
    ];
  };
}
