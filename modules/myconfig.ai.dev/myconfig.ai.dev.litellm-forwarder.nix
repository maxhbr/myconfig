# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.litellm-forwarder — the host-side, port-scoped forwarder that
# makes the loopback-only LiteLLM proxy reachable from a container sandbox.
#
# The host LiteLLM proxy is loopback-only on purpose
# (`services.litellm.host = mkForce "127.0.0.1"` in ../myconfig.ai/services.litellm.nix),
# so a rootless Podman sandbox cannot reach it directly: with pasta (podman's
# rootless default) the sandbox's own 127.0.0.1 is the container's loopback, not
# the host's, and runsc runs its own netstack on top of the pasta netns.
#
# === Mechanism: --map-guest-addr + port-scoped forwarder ===
#
#   1. A socket-activated `systemd-socket-proxyd` forwarder listens on
#      `0.0.0.0:${forwardPort}` and forwards to `127.0.0.1:${port}` (the
#      loopback-only LiteLLM proxy). The forward port differs from the LiteLLM
#      port so the `0.0.0.0` wildcard bind does not collide with LiteLLM's own
#      `127.0.0.1:${port}` listener.
#
#   2. The sandbox runs with a `pasta:--map-guest-addr,<address>` podman network
#      spec (`AGENT_GVISOR_NETWORK` of the gvisor tier,
#      `MYSBX_GVISOR_PASTA_SPEC` of mysbx's podman-gvisor backend).
#      `--map-guest-addr` translates <address> to the *guest's assigned address
#      on the host* — by default the host's global address (the address on the
#      default-route interface). That address IS on the interface pasta binds
#      its outbound sockets to (`--outbound-if4`, applied unconditionally via
#      SO_BINDTODEVICE in pasta's tcp_bind_outbound()), so the connection IS
#      locally delivered. The port is kept unchanged.
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
#     address, not to `127.0.0.1`. This is the key isolation property over
#     pasta's `--map-host-loopback`, which is address-scoped rather than
#     port-scoped (its nat_outbound() translates the address only, keeping the
#     port: `tgt->eport = ini->oport`) and therefore exposes EVERY loopback
#     port of the host.
#   * The forwarder itself is on `0.0.0.0:${forwardPort}` (all interfaces), but
#     the NixOS firewall trusts `lo` and drops `${forwardPort}` on every other
#     interface (it is not in `allowedTCPPorts`), so only local connections
#     (the sandbox via pasta, and host processes) can reach it — external hosts
#     cannot. See `networking.firewall.trustedInterfaces = [ "lo" ]` in NixOS.
#
# A member-less bridge plus a `BindToDevice`d listener does NOT work as a
# substitute: pasta re-opens the sandbox's outbound connections in the host
# network namespace but binds them to the host's default-route interface, so a
# connection to a host-local bridge address egresses that interface toward its
# gateway instead of being locally delivered — observed as
# `curl: (7) ... after 0 ms`.
#
# Consumers of this module:
#   * ./sandboxes/myconfig.ai.gvisor-agent-sandbox/litellm-endpoint.nix — the
#     `agent-gvisor` tier, which additionally seeds sandbox homes with rewritten
#     endpoints and relays the endpoint onto the sandbox's own loopback.
#   * ./mysbx/default.nix — the `podman-gvisor` backend of mysbx, which pins the
#     pasta spec into the `mysbx` wrapper and hands the endpoint to the
#     sandboxed agents as container environment.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.dev.litellm-forwarder;

  port = toString cfg.port;
  forwardPort = toString cfg.forwardPort;
in
{
  options.myconfig.ai.dev.litellm-forwarder = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Run the port-scoped forwarder that makes the loopback-only host
        LiteLLM proxy reachable from a container sandbox, and advertise a host
        address that pasta's `--map-guest-addr` maps to the host's global
        address.

        Off by default: the sandbox tiers that need it turn it on
        (`myconfig.ai.dev.gvisor-agent-sandbox.litellm.enable` and the
        `podman-gvisor` backend of `myconfig.ai.dev.mysbx`).
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
      default = cfg.port + 10000;
      defaultText = literalExpression "config.services.litellm.port + 10000";
      description = ''
        Port the forwarder listens on (`0.0.0.0:${"forwardPort"}` →
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
        real: not `myconfig.ai.dev.microvm.subnet`, nor the host LAN or
        WireGuard ranges.
      '';
    };

    endpoint = mkOption {
      type = types.str;
      readOnly = true;
      default = "http://${cfg.address}:${forwardPort}/v1";
      defaultText = literalExpression ''"http://''${address}:''${forwardPort}/v1"'';
      description = ''
        OpenAI-compatible base URL of the endpoint, as seen from inside a
        sandbox (pasta maps `address` to the host's global address, where the
        forwarder listens on `forwardPort`). Read-only.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.services.litellm.enable;
        message = ''
          myconfig.ai.dev.litellm-forwarder.enable is on, but services.litellm
          is not enabled — the forwarder would proxy to 127.0.0.1:${port},
          where nothing listens.
        '';
      }
      {
        assertion = cfg.forwardPort != cfg.port;
        message = ''
          myconfig.ai.dev.litellm-forwarder.forwardPort must differ from port:
          the forwarder binds to 0.0.0.0:${forwardPort}, which would collide
          with LiteLLM's own 127.0.0.1:${port} listener if they were equal.
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
    # then an immediate reset (`curl: (56) Recv failure: Connection reset by
    # peer` from inside a sandbox).
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
  };
}
