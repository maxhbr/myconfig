# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.gvisor-agent-sandbox — bridge-only LiteLLM forwarding endpoint.
#
# The host LiteLLM proxy is loopback-only on purpose
# (`services.litellm.host = mkForce "127.0.0.1"` in ../services.litellm.nix),
# so a rootless Podman sandbox cannot reach it: with pasta (podman's rootless
# default) the sandbox's own 127.0.0.1 is the container's loopback, not the
# host's.
#
# This file adds the same construction the microVM tier uses (see
# ../myconfig.ai.microvm/network.nix §16): a private, member-less bridge
# carrying one stable host address, and a socket-activated
# `systemd-socket-proxyd` bound ONLY to that address, forwarding to
# 127.0.0.1:<port>. The main LiteLLM proxy is not touched and stays
# loopback-only.
#
# Why a member-less bridge and not just the container network:
#   * Rootless Podman has NO host-visible bridge — with pasta the sandbox
#     lives in a user network namespace and its outbound connections are
#     re-opened by pasta *in the host namespace*. A host-local address is
#     therefore reachable from the sandbox, while the host's 127.0.0.1 is
#     not. The bridge exists solely to provide that stable, non-loopback,
#     non-LAN address, independent of DHCP, of which NIC is up, and of the
#     pasta/slirp4netns and podman-version differences around
#     `host.containers.internal`.
#   * Binding to it (`BindToDevice` + `FreeBind`) keeps the endpoint off
#     0.0.0.0 and off the LAN, which is the property that matters.
#
# Firewall: a dedicated GVISOR_AGENT_SANDBOX_INPUT chain accepts packets to
# <address>:<port> only from `lo` (where pasta-originated connections to a
# host-local address arrive) and from the bridge itself, and drops everything
# else addressed to it, so the endpoint cannot be reached from the LAN even if
# something routes packets for that address to this host.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;
  lcfg = cfg.litellm;

  bridge = lcfg.bridgeName;
  address = lcfg.address;
  port = toString lcfg.port;

  enabled = cfg.enable && lcfg.enable;

  firewallExtraCommands = ''
    # ==== myconfig.ai.gvisor-agent-sandbox: LiteLLM endpoint =============
    iptables -N GVISOR_AGENT_SANDBOX_INPUT 2>/dev/null || iptables -F GVISOR_AGENT_SANDBOX_INPUT
    iptables -C INPUT -d ${address} -j GVISOR_AGENT_SANDBOX_INPUT 2>/dev/null \
      || iptables -A INPUT -d ${address} -j GVISOR_AGENT_SANDBOX_INPUT
    # Rootless sandboxes reach the endpoint via pasta, which re-opens the
    # connection in the host namespace; routing to a host-local address sends
    # it over `lo`.
    iptables -A GVISOR_AGENT_SANDBOX_INPUT -i lo -p tcp --dport ${port} -j ACCEPT
    iptables -A GVISOR_AGENT_SANDBOX_INPUT -i ${bridge} -p tcp --dport ${port} -j ACCEPT
    # Never reachable from the LAN / any other interface (fail closed).
    iptables -A GVISOR_AGENT_SANDBOX_INPUT -j DROP
  '';

  firewallExtraStopCommands = ''
    # ==== myconfig.ai.gvisor-agent-sandbox: remove LiteLLM endpoint ======
    iptables -D INPUT -d ${address} -j GVISOR_AGENT_SANDBOX_INPUT 2>/dev/null || true
    iptables -F GVISOR_AGENT_SANDBOX_INPUT 2>/dev/null || true
    iptables -X GVISOR_AGENT_SANDBOX_INPUT 2>/dev/null || true
  '';
in
{
  options.myconfig.ai.gvisor-agent-sandbox.litellm = with lib; {
    enable = mkOption {
      type = types.bool;
      default = config.services.litellm.enable;
      defaultText = literalExpression "config.services.litellm.enable";
      description = ''
        Expose the loopback-only host LiteLLM proxy to the sandboxes through a
        bridge-only forwarding endpoint. On by default whenever the host runs
        LiteLLM at all.
      '';
    };

    port = mkOption {
      type = types.port;
      default = config.services.litellm.port;
      defaultText = literalExpression "config.services.litellm.port";
      description = ''
        Port of the host LiteLLM proxy. The forwarder listens on this same
        port on `address` and forwards to `127.0.0.1:<port>`.
      '';
    };

    bridgeName = mkOption {
      type = types.str;
      default = "agentsbr0";
      description = ''
        Member-less bridge providing the stable host address the endpoint is
        bound to. Must differ from `myconfig.ai.microvm.bridgeName`.
      '';
    };

    address = mkOption {
      type = types.str;
      default = "192.168.84.1";
      description = ''
        Host address on `bridgeName`, and the only address the LiteLLM
        forwarder listens on. Must not collide with
        `myconfig.ai.microvm.subnet` or any LAN in use.
      '';
    };

    prefixLength = mkOption {
      type = types.int;
      default = 24;
      description = "Prefix length of `address` on `bridgeName`.";
    };

    endpoint = mkOption {
      type = types.str;
      readOnly = true;
      default = "http://${address}:${port}/v1";
      defaultText = literalExpression ''"http://''${address}:''${port}/v1"'';
      description = ''
        OpenAI-compatible base URL of the endpoint, as seen from inside a
        sandbox. Read-only; also written to
        `~/.config/agent-sandbox/litellm.env` for
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
          services.litellm is not enabled — the forwarder would have nothing
          to forward to on 127.0.0.1:${port}.
        '';
      }
      {
        assertion = !config.myconfig.ai.microvm.enable || bridge != config.myconfig.ai.microvm.bridgeName;
        message = ''
          myconfig.ai.gvisor-agent-sandbox.litellm.bridgeName (${bridge}) collides
          with myconfig.ai.microvm.bridgeName; the two tiers must not share a
          bridge.
        '';
      }
    ];

    # Member-less private bridge, created with the standard udev/systemd
    # bridge mechanism (NOT systemd-networkd), exactly like the microVM tier.
    networking.bridges.${bridge}.interfaces = [ ];

    networking.interfaces.${bridge}.ipv4.addresses = [
      {
        inherit address;
        inherit (lcfg) prefixLength;
      }
    ];

    # NetworkManager must not try to manage this bridge.
    networking.networkmanager.unmanaged = [ "interface-name:${bridge}" ];

    # No IPv6 policy is implemented for this endpoint, so keep IPv6 off the
    # bridge entirely. The sysctl key only applies if the interface already
    # exists when systemd-sysctl runs, and the bridge is created later (at
    # `network.target`), hence the oneshot that re-applies it.
    boot.kernel.sysctl."net.ipv6.conf.${bridge}.disable_ipv6" = 1;

    systemd.services."gvisor-agent-sandbox-${bridge}-disable-ipv6" = {
      description = "Disable IPv6 on the agent sandbox bridge ${bridge}";
      after = [ "${bridge}-netdev.service" ];
      wants = [ "${bridge}-netdev.service" ];
      wantedBy = [ "network.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.procps}/bin/sysctl -w net.ipv6.conf.${bridge}.disable_ipv6=1";
      };
    };

    networking.firewall.extraCommands = firewallExtraCommands;
    networking.firewall.extraStopCommands = firewallExtraStopCommands;

    # Socket-activated endpoint bound ONLY to the bridge address.
    # `BindToDevice` (SO_BINDTODEVICE) needs the DEVICE to exist at bind time
    # and `FreeBind` does not cover that, so the socket must be ordered after
    # — and require — the bridge's netdev unit; otherwise it fails once early
    # in boot (sockets.target) and never retries.
    systemd.sockets.gvisor-agent-sandbox-litellm-proxy = {
      description = "Bridge-only LiteLLM forwarding endpoint for gVisor agent sandboxes";
      wantedBy = [ "sockets.target" ];
      after = [ "${bridge}-netdev.service" ];
      requires = [ "${bridge}-netdev.service" ];
      socketConfig = {
        ListenStream = "${address}:${port}";
        BindToDevice = bridge;
        FreeBind = true;
        Accept = false;
      };
    };

    systemd.services.gvisor-agent-sandbox-litellm-proxy = {
      description = "Forward ${address}:${port} to the loopback LiteLLM proxy";
      requires = [ "gvisor-agent-sandbox-litellm-proxy.socket" ];
      after = [ "gvisor-agent-sandbox-litellm-proxy.socket" ];
      serviceConfig = {
        ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${port}";
        # This process only shuffles bytes between two sockets.
        DynamicUser = true;
        NoNewPrivileges = true;
        PrivateTmp = true;
        ProtectSystem = "strict";
        ProtectHome = true;
      };
    };

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
