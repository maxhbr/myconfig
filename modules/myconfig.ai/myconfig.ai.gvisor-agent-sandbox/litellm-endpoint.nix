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
# === History: why the old approaches did NOT work ===
#
# This file used to expose the proxy through a member-less bridge (`agentsbr0`,
# 192.168.84.1) plus a socket-activated `systemd-socket-proxyd` bound to it.
# That did NOT work, and the reason is worth remembering:
#
#   pasta re-opens the sandbox's outbound connections in the host network
#   namespace, but it BINDS those outbound sockets to the host's default-route
#   interface (pasta(1) `--outbound-if4` default, applied unconditionally in
#   tcp_bind_outbound() via SO_BINDTODEVICE). A connection from the sandbox to
#   the host-local bridge address therefore egresses that default-route
#   interface (the LAN NIC or the VPN tun) toward its gateway, instead of being
#   locally delivered to the host. It never reaches the bridge listener. The
#   socket's `BindToDevice = <bridge>` was a second, independent barrier: it
#   rejected any connection not arriving on the bridge. Either way the endpoint
#   was unreachable from a sandbox — observed as `curl: (7) ... after 0 ms`.
#
# A follow-up attempt used pasta's `--map-host-loopback`: it translates a chosen
# address to the host's 127.0.0.1 *before* egress, using pasta's local-traffic
# bypass, so the connection IS locally delivered (127.0.0.0/8 is special in the
# kernel — always locally delivered regardless of SO_BINDTODEVICE). This worked
# for reachability but has an isolation flaw: `--map-host-loopback` is
# *address-scoped, not port-scoped* — ALL ports on the mapped address translate
# to 127.0.0.1, so a hostile agent could reach ANY service on the host's
# loopback, not just LiteLLM. (pasta's nat_outbound() translates the address
# only; the port is always preserved: `tgt->eport = ini->oport`.) There is no
# port-scoped variant of `--map-host-loopback` in pasta.
#
# === Current mechanism: --map-guest-addr + port-scoped forwarder ===
#
# The current approach closes that loophole by NOT mapping to the loopback at
# all. Instead:
#
#   1. A socket-activated `systemd-socket-proxyd` forwarder listens on
#      `0.0.0.0:${forwardPort}` and forwards to `127.0.0.1:${port}` (the
#      loopback-only LiteLLM proxy). The forward port differs from the LiteLLM
#      port so the `0.0.0.0` wildcard bind does not collide with LiteLLM's own
#      `127.0.0.1:${port}` listener.
#
#   2. The `agent-gvisor` wrapper bakes `AGENT_GVISOR_NETWORK` as a
#      `pasta:--map-guest-addr,<address>` podman network spec (in ./default.nix).
#      `--map-guest-addr` translates <address> to the *guest's assigned address
#      on the host* — by default the host's global address (the address on the
#      default-route interface). That address IS on the SO_BINDTODEVICE
#      interface, so the connection IS locally delivered (unlike the bridge,
#      which was on a different interface). The port is kept unchanged.
#
#   3. The sandbox therefore connects to `<address>:${forwardPort}`, which pasta
#      translates to `<host-global-addr>:${forwardPort}`, where the forwarder
#      accepts and proxies to `127.0.0.1:${port}` (LiteLLM).
#
# Why this is port-scoped:
#   * The sandbox can reach `<address>:${forwardPort}` (the forwarder → LiteLLM).
#   * It can also reach other ports on the host's global address — but only
#     services that already bind to `0.0.0.0` (i.e., are already
#     network-accessible). Loopback-ONLY services (bound to `127.0.0.1`) are
#     NOT reachable, because `--map-guest-addr` maps to the host's global
#     address, not to `127.0.0.1`. This is the key isolation improvement over
#     `--map-host-loopback`, which exposed every loopback port.
#   * The forwarder itself is on `0.0.0.0:${forwardPort}` (all interfaces), but
#     the NixOS firewall trusts `lo` and drops `${forwardPort}` on every other
#     interface (it is not in `allowedTCPPorts`), so only local connections
#     (the sandbox via pasta, and host processes) can reach it — external hosts
#     cannot. See `networking.firewall.trustedInterfaces = [ "lo" ]` in NixOS.
#
# Why `--map-gw` is gone: the old spec used `pasta:--map-gw,--map-host-loopback,
# <address>`. `--map-gw` is a podman-only flag (NOT a real pasta option) that
# suppresses podman's default `--no-map-gw`. It was believed necessary so that
# `--map-host-loopback` would take effect, but the pasta source shows this is
# unnecessary: `--map-host-loopback`/`--map-guest-addr` set `map_host_loopback`/
# `map_guest_addr` directly, and the `--no-map-gw` finalization only fills in
# the *default* (gateway) mapping when `map_host_loopback` is still unspecified.
# An explicit mapping is never overridden by `--no-map-gw`. Dropping `--map-gw`
# therefore changes nothing and removes one address→loopback exposure path.
#
# This module contributes the forwarder (the host-side port-scoped proxy), the
# *address, forward-port and endpoint URL* used by `agent-gvisor`, and the
# `--env-file` for `agent-gvisor start`. The actual pasta network spec is baked
# into the `agent-gvisor` wrapper in ./default.nix.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;
  lcfg = cfg.litellm;

  address = lcfg.address;
  port = toString lcfg.port;
  forwardPort = toString lcfg.forwardPort;

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
        running a port-scoped forwarder and advertising a host address that
        pasta maps to the host's global address (see ./default.nix,
        `AGENT_GVISOR_NETWORK`). On by default whenever the host runs LiteLLM
        at all.
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
        real: not `myconfig.ai.microvm.subnet`, nor the host LAN or WireGuard
        ranges.

        Unlike the old `--map-host-loopback` (which translated this address to
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
    assertions = [
      {
        assertion = config.services.litellm.enable;
        message = ''
          myconfig.ai.gvisor-agent-sandbox.litellm.enable is on, but
          services.litellm is not enabled — the forwarder would proxy to
          127.0.0.1:${port}, where nothing listens.
        '';
      }
      {
        assertion = lcfg.forwardPort != lcfg.port;
        message = ''
          myconfig.ai.gvisor-agent-sandbox.litellm.forwardPort must differ from
          port: the forwarder binds to 0.0.0.0:${forwardPort}, which would
          collide with LiteLLM's own 127.0.0.1:${port} listener if they were
          equal.
        '';
      }
    ];

    # Port-scoped forwarder: 0.0.0.0:${forwardPort} → 127.0.0.1:${port}.
    # Socket-activated, so it costs nothing until a sandbox connects, and it
    # exits again after an idle period. The 0.0.0.0 bind is safe because the
    # NixOS firewall drops ${forwardPort} on non-loopback interfaces (it is not
    # in allowedTCPPorts).
    #
    # `Accept` MUST stay at its default (`no`): systemd-socket-proxyd inherits
    # the LISTENING socket and accepts connections itself (systemd-socket-proxyd(8):
    # "support for socket activation with Accept=no"). With `Accept = true`
    # systemd passes an already-accepted CONNECTION socket instead, the proxy
    # fails on it and exits, and the client sees the TCP handshake succeed and
    # then an immediate reset — which is exactly how this looked from inside a
    # sandbox: `curl: (56) Recv failure: Connection reset by peer`, and through
    # the in-sandbox relay `socat[7] E read(5, …): Connection reset by peer`.
    systemd.sockets.agent-litellm-forward = {
      description = "Socket for the agent LiteLLM port-scoped forwarder";
      wantedBy = [ "sockets.target" ];
      socketConfig.ListenStream = "0.0.0.0:${forwardPort}";
    };

    systemd.services.agent-litellm-forward = {
      description = "Agent LiteLLM port-scoped forwarder (0.0.0.0:${forwardPort} -> 127.0.0.1:${port})";
      requires = [
        "agent-litellm-forward.socket"
        "litellm.service"
      ];
      after = [
        "agent-litellm-forward.socket"
        "litellm.service"
      ];
      serviceConfig = {
        Type = "notify";
        ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd --exit-idle-time=5min 127.0.0.1:${port}";
        ProtectSystem = "strict";
        PrivateTmp = true;
        PrivateDevices = true;
        # Needs the host network namespace to reach the loopback-only proxy.
        PrivateNetwork = false;
      };
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
