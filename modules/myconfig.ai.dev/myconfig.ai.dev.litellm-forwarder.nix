# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.dev.litellm-forwarder — the host-side, port-scoped forwarder that
# makes the loopback-only LiteLLM proxy reachable from a container sandbox.
#
# The host LiteLLM proxy is loopback-only on purpose
# (`services.litellm.host = mkForce "127.0.0.1"` in ../myconfig.ai.llmops/services.litellm.nix),
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
#      `MYSBX_PODMAN_PASTA_SPEC` of mysbx's podman-gvisor backend).
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
# Consumer of this module:
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
  # The socket path is fixed to the directory the sandbox binds
  # (docs/design/config.md D20, bd myconfig-mo3.2.6); only the file name
  # is configurable.
  socketPath = "/run/mysbx-egress/${cfg.unixSocket.name}";

  # The confinement both forwarder halves share: no privileges, no home, no
  # writable system tree, and — the point of the whole block — a DESTINATION
  # fence, so even a compromised forwarder cannot reach the LAN, the VPN or
  # a cloud-metadata endpoint. `RestrictAddressFamilies` keeps AF_UNIX for
  # socket activation and `sd_notify`, and AF_INET for the loopback target.
  #
  # The IP filter is bidirectional (it also screens INBOUND sources), which
  # is harmless for the Unix half (AF_UNIX is never IP-filtered) and
  # intended for none of them: the TCP half's INGRESS arrives through the
  # socket unit's cgroup (Accept=no means PID1 owns the listening socket),
  # so a non-loopback client — pasta re-originates the sandbox connection
  # from the host's global address — is not screened here. What this fence
  # bounds is what the forwarder may CONNECT TO. bd myconfig-jq2 verifies
  # that the podman-gvisor endpoint still works with it on.
  forwarderHardening = {
    DynamicUser = true;
    NoNewPrivileges = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateDevices = true;
    IPAddressAllow = "localhost";
    IPAddressDeny = "any";
    RestrictAddressFamilies = [
      "AF_UNIX"
      "AF_INET"
    ];
  };
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

        Off by default: the podman backends of `myconfig.ai.dev.mysbx`
        turn it on.
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

    unixSocket = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Also run a Unix-socket forwarder
          (`agent-litellm-forward-unix`) whose socket the bubblewrap
          `network = "proxy-only"` profile binds into the sandbox
          (`docs/design/config.md` D20). The socket is the ONE host
          channel such a sandbox has; the in-sandbox `mysbx-egress-
          relay` forwards loopback TCP clients to it.

          Off by default: the mysbx module turns it on wherever it turns
          the profile on. Independent of `enable` so a host can have the
          socket channel without the TCP listener (and vice versa); the
          socket serves only the loopback LiteLLM proxy.
        '';
      };

      name = mkOption {
        type = types.strMatching "[A-Za-z0-9._-]+";
        default = "litellm.sock";
        description = ''
          File name of the Unix socket inside the FIXED directory
          `/run/mysbx-egress` (the mysbx bubblewrap `proxy-only` backend
          binds exactly that directory read-only into the sandbox,
          docs/design/config.md D20, bd myconfig-mo3.2.6). One plain
          file-name component — letters, digits, `.`, `_`, `-`, but not
          `.` or `..` — so the path can never escape the directory.
          The socket file is mode 0666; the directory is created by
          systemd at 0755, root-owned.
        '';
      };
    };
  };

  config = lib.mkMerge [
    # The destination is the loopback LiteLLM proxy: whether the TCP
    # forwarder, the Unix-socket forwarder or both are on, that proxy
    # must exist, and the TCP listener's port must not collide with it.
    (lib.mkIf cfg.enable {
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
        serviceConfig = forwarderHardening // {
          Type = "notify";
          ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd --exit-idle-time=5min 127.0.0.1:${port}";
          # Needs the host network namespace to reach the loopback-only
          # proxy; the shared fence's destination rule is what bounds it.
          PrivateNetwork = false;
        };
      };
    })

    # The Unix-socket half (bd myconfig-mo3.2.4): the host side of the
    # bubblewrap `network = "proxy-only"` channel (config.md D20). The
    # socket is the ONE host channel such a sandbox has — the mysbx
    # backend binds its DIRECTORY into the sandbox, and the in-sandbox
    # `mysbx-egress-relay` forwards loopback TCP clients to it.
    #
    # The microvm precedent (sandboxes/myconfig.ai.microvm/network.nix):
    # one Unix socket → systemd-socket-proxyd → 127.0.0.1:<port>,
    # destination-FIXED, with the process itself restricted to the host
    # loopback so a compromised forwarder cannot reach the LAN, the VPN
    # or the metadata service.
    #
    # `Accept` stays `no` (the same rule as the TCP pair): socket-proxyd
    # inherits the LISTENING socket and accepts connections itself.
    # Gated on `unixSocket.enable` ALONE (not the TCP half's
    # `cfg.enable`): the profile runs on plain bubblewrap, where the
    # port-scoped TCP forwarder is not needed, so a host can have the
    # socket channel without the TCP listener.
    (lib.mkIf cfg.unixSocket.enable {
      assertions = [
        {
          assertion = config.services.litellm.enable;
          message = ''
            myconfig.ai.dev.litellm-forwarder.unixSocket.enable is on, but
            services.litellm is not enabled — the socket would forward to
            127.0.0.1:${port}, where nothing listens.
          '';
        }
        {
          assertion = cfg.unixSocket.name != "." && cfg.unixSocket.name != "..";
          message = ''
            myconfig.ai.dev.litellm-forwarder.unixSocket.name must be a plain
            file name, not `.` or `..` (got `${cfg.unixSocket.name}`).
          '';
        }
      ];

      systemd.sockets.agent-litellm-forward-unix = {
        description = "Unix socket for the agent LiteLLM egress forwarder";
        wantedBy = [ "sockets.target" ];
        socketConfig = {
          # The directory is created by systemd (no ExecStartPre mkdir)
          # at an EXPLICIT 0755, root-owned: the world-writable SOCKET
          # (0666) inside it is the only relaxation, and a sandbox that
          # binds the directory read-only sees exactly the one socket
          # the mysbx backend expects. The Rust side refuses a real run
          # unless the directory is a non-symlink, root-owned and not
          # group/world-writable.
          DirectoryMode = "0755";
          ListenStream = socketPath;
          # Mode 0666: the mysbx `proxy-only` sandbox binds the socket's
          # directory and connects as the sandbox user; the socket is no
          # wider than LiteLLM's own loopback TCP port (any host process
          # could already curl it), and `/run/mysbx-egress` is root-owned
          # 0755, so the world-writable SOCKET is the only relaxation.
          SocketMode = "0666";
          # A stale socket file would shadow the listener on the next
          # start.
          RemoveOnStop = true;
          Accept = false;
        };
      };

      systemd.services.agent-litellm-forward-unix = {
        description = "Agent LiteLLM Unix-socket egress forwarder (${socketPath} -> 127.0.0.1:${port})";
        requires = [
          "agent-litellm-forward-unix.socket"
          "litellm.service"
        ];
        after = [
          "agent-litellm-forward-unix.socket"
          "litellm.service"
        ];
        serviceConfig = forwarderHardening // {
          # --exit-idle-time mirrors the TCP forwarder: the unit exits
          # after an idle period, so an enabled-but-unused forwarder
          # really runs nothing once its first connection is done.
          ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd --exit-idle-time=5min 127.0.0.1:${port}";
        };
      };
    })
  ];
}
