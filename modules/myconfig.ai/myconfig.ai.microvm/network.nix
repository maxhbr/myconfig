# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — private bridge, firewall policy & bridge-only
# LiteLLM forwarder (PHASE: plan §12–§16, §31 proxy-only profile, §33 DNS).
#
# Everything in this file is gated behind `lib.mkIf cfg.enable`, so a
# disabled feature (the secure default) produces zero config side effects.
#
#   §12  A NetworkManager-COMPATIBLE private bridge `cfg.bridgeName`
#        (agentbr0) carrying the host address `cfg.gatewayAddress`/<prefix>
#        on `cfg.subnet`. We do NOT migrate to systemd-networkd (§46). The
#        bridge and the per-slot TAP interfaces are declared unmanaged in
#        NetworkManager so NM never fights microvm.nix over them.
#
#   §13  Default network policy = PROXY-ONLY (§31): the only egress a guest
#        gets is guest -> <gatewayAddress>:<litellmPort>. Everything else is
#        denied — all other host ports, host LAN, RFC1918 / CGNAT / loopback
#        / link-local / multicast / reserved ranges, the cloud-metadata IP,
#        inter-VM (TAP-to-TAP) traffic and the general internet.
#
#   §14  Implemented with the EXISTING NixOS firewall (no nftables/iptables
#        backend migration, §46). Dedicated chains AGENT_MICROVM_INPUT /
#        _FORWARD / _OUTPUT are created idempotently in
#        `networking.firewall.extraCommands`, scoped to the bridge / subnet /
#        TAPs, and torn down again in `extraStopCommands`.
#
#   §15  IPv6 is disabled on the bridge (MVP limitation): no equivalent IPv6
#        firewall policy is implemented, so we simply prevent IPv6 from being
#        configured on the bridge at all.
#
#   §16  A bridge-only LiteLLM forwarding endpoint:
#        `systemd.sockets.agent-litellm-proxy` listens ONLY on
#        <gatewayAddress>:<litellmPort> (never 0.0.0.0 / the LAN) and hands
#        connections to `systemd-socket-proxyd`, which forwards them to the
#        loopback-only LiteLLM proxy on 127.0.0.1:<litellmPort>. The main
#        LiteLLM proxy stays loopback-only and is NOT touched here.
#
# The insecure-relaxation flags (allowInterVmTraffic / allowPrivateNetworks /
# allowPublicInternet) gate the corresponding *allow* rules; all three
# default to false, so the default policy is deny-all-except-proxy.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Shared, deterministic slot table (§4) — imported rather than re-derived
  # so the TAP interface names we hand to NetworkManager's unmanaged list
  # always match the TAPs guest.nix actually creates.
  slots = (import ./slots.nix { inherit lib; }).mkSlots cfg.slotCount;

  bridge = cfg.bridgeName;
  gateway = cfg.gatewayAddress;
  subnet = cfg.subnet;
  port = toString cfg.litellmPort;

  # Prefix length of the private subnet (e.g. "192.168.83.0/24" -> 24). Used
  # for the host-side bridge address.
  prefixLength = lib.toInt (lib.last (lib.splitString "/" subnet));

  # Non-routable / special-use IPv4 ranges that a proxy-only guest must never
  # reach: RFC1918, CGNAT (100.64/10), loopback, link-local, multicast and
  # the reserved 240/4 block. 169.254/16 covers the cloud-metadata IP, which
  # is ALSO blocked explicitly below for defence in depth (§13).
  privateRanges = [
    "10.0.0.0/8"
    "172.16.0.0/12"
    "192.168.0.0/16"
    "100.64.0.0/10"
    "127.0.0.0/8"
    "169.254.0.0/16"
    "224.0.0.0/4"
    "240.0.0.0/4"
  ];

  # Verdicts driven by the insecure-relaxation flags. Default (all false) →
  # deny-all-except-proxy.
  interVmVerdict = if cfg.allowInterVmTraffic then "ACCEPT" else "DROP";
  privateVerdict = if cfg.allowPrivateNetworks then "ACCEPT" else "DROP";
  internetVerdict = if cfg.allowPublicInternet then "ACCEPT" else "DROP";

  privateRuleLines = lib.concatMapStringsSep "\n" (
    range: "    iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${range} -j ${privateVerdict}"
  ) privateRanges;

  # --- firewall setup (runs after NixOS' own rules) -----------------------
  firewallExtraCommands = ''
        # ==== myconfig.ai.microvm: dedicated agent-sandbox chains ============
        # Idempotently (re)create the chains: on a fresh run -N succeeds; on a
        # reload where extraStopCommands did not run, -F clears stale rules.
        iptables -N AGENT_MICROVM_INPUT 2>/dev/null || iptables -F AGENT_MICROVM_INPUT
        iptables -N AGENT_MICROVM_FORWARD 2>/dev/null || iptables -F AGENT_MICROVM_FORWARD
        iptables -N AGENT_MICROVM_OUTPUT 2>/dev/null || iptables -F AGENT_MICROVM_OUTPUT

        # Hook the dedicated chains from the built-ins, scoped to the bridge so
        # no other traffic is touched. -C guards make the -I inserts idempotent.
        iptables -C INPUT -i ${bridge} -j AGENT_MICROVM_INPUT 2>/dev/null \
          || iptables -I INPUT 1 -i ${bridge} -j AGENT_MICROVM_INPUT
        iptables -C FORWARD -i ${bridge} -j AGENT_MICROVM_FORWARD 2>/dev/null \
          || iptables -I FORWARD 1 -i ${bridge} -j AGENT_MICROVM_FORWARD
        iptables -C FORWARD -o ${bridge} -j AGENT_MICROVM_FORWARD 2>/dev/null \
          || iptables -I FORWARD 1 -o ${bridge} -j AGENT_MICROVM_FORWARD
        iptables -C OUTPUT -o ${bridge} -j AGENT_MICROVM_OUTPUT 2>/dev/null \
          || iptables -I OUTPUT 1 -o ${bridge} -j AGENT_MICROVM_OUTPUT

        # --- INPUT: guest -> host -------------------------------------------
        # Unconditional cloud-metadata block FIRST, so no later ACCEPT (incl.
        # ESTABLISHED) or future relaxation can ever shadow it (defence in
        # depth; also covered by the 169.254/16 FORWARD rule).
        iptables -A AGENT_MICROVM_INPUT -d 169.254.169.254 -j DROP
        iptables -A AGENT_MICROVM_INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
        # Proxy-only exception (§13/§31): guests may reach ONLY the bridge-local
        # LiteLLM forwarder on <gateway>:<litellmPort>.
        iptables -A AGENT_MICROVM_INPUT -s ${subnet} -d ${gateway} -p tcp --dport ${port} -j ACCEPT
        # Deny every other host port / service reachable via the bridge.
        iptables -A AGENT_MICROVM_INPUT -j DROP

        # --- FORWARD: guest -> {other guests, host LAN, internet} -----------
        # Cloud-metadata IP is ALWAYS blocked FIRST, regardless of relaxation
        # flags, so no later ACCEPT can shadow it.
        iptables -A AGENT_MICROVM_FORWARD -d 169.254.169.254 -j DROP
        iptables -A AGENT_MICROVM_FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT
        # No TAP-to-TAP / guest-to-guest forwarding unless explicitly relaxed
        # (allowInterVmTraffic). Placed before the 192.168/16 rule below, which
        # would otherwise subsume the bridge subnet.
        #
        # NOTE: same-bridge L2 (guest<->guest) frames only reach iptables
        # FORWARD when net.bridge.bridge-nf-call-iptables=1 (needs br_netfilter);
        # both are enabled in the config section below so this rule actually
        # fires once TAPs are attached to the bridge in a later phase. For
        # additional defence in depth, per-port L2 isolation (bridge `isolated`
        # flag) should be set at TAP-attachment time.
        iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${subnet} -j ${interVmVerdict}
        # Private / special-use ranges (host LAN, VPN peers, RFC1918, CGNAT,
        # loopback, link-local, multicast, reserved) unless allowPrivateNetworks.
    ${privateRuleLines}
        # General internet unless allowPublicInternet. Permits legitimate egress
        # from the bridge subnet only when allowPublicInternet=true.
        iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -j ${internetVerdict}
        # Unconditional terminal DROP (fail CLOSED). The built-in FORWARD policy
        # is ACCEPT (networking.firewall.filterForward defaults false) and the
        # host has net.ipv4.ip_forward=1, so anything falling out the bottom of
        # this chain would otherwise be routed out the WAN. Every rule above is
        # scoped to -s ${subnet}; this catch-all denies packets that match none
        # of them — a guest spoofing a non-subnet source IP, the -o ${bridge}
        # (LAN -> guest) direction, and any unexpected protocol — mirroring
        # AGENT_MICROVM_INPUT's terminal DROP.
        iptables -A AGENT_MICROVM_FORWARD -j DROP

        # --- OUTPUT: host -> guest ------------------------------------------
        # The host is trusted; permit host-originated traffic to guests over the
        # bridge (SSH/console management in later phases).
        iptables -A AGENT_MICROVM_OUTPUT -j ACCEPT
  '';

  # --- §12 attach each per-slot TAP to the bridge -------------------------
  # microvm.nix's `type = "tap"` `tap-up` only *creates* the tap
  # (`ip tuntap add … mode tap` + `ip link set … up`); it does NOT enslave it
  # to any bridge — bridging is the host's responsibility for `type = "tap"`.
  # Without this the guest interface has NO L2 path to the bridge / gateway
  # (192.168.83.1), so the guest IP (192.168.83.10…) is unreachable and the
  # launcher's SSH-readiness wait always times out even though the guest
  # boots and sshd runs (plan §12 "Attach each slot TAP"; validation B1
  # "TAP ∈ bridge / TAP enslaved").
  #
  # One oneshot per slot enslaves `vm-agent-<n>` to the bridge. It is ordered
  # AFTER microvm.nix created the tap (`microvm-tap-interfaces@<slot>`) and
  # BEFORE the VM boots (`microvm@<slot>`), and is `partOf` the VM so it is
  # re-run whenever the VM (re)starts — important because `tap-down` deletes
  # and `tap-up` recreates the tap on every restart, dropping bridge
  # membership. On VM stop `tap-down` removes the tap (and thus its bridge
  # port) automatically, so no explicit detach is needed.
  tapAttachServices = builtins.listToAttrs (
    map (
      slot:
      lib.nameValuePair "agent-microvm-attach-${slot.name}" {
        description = "Enslave TAP ${slot.tap} to bridge ${bridge} for microVM ${slot.name}";
        after = [ "microvm-tap-interfaces@${slot.name}.service" ];
        requires = [ "microvm-tap-interfaces@${slot.name}.service" ];
        before = [ "microvm@${slot.name}.service" ];
        partOf = [ "microvm@${slot.name}.service" ];
        wantedBy = [ "microvm@${slot.name}.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${lib.getExe' pkgs.iproute2 "ip"} link set ${slot.tap} master ${bridge}";
        };
      }
    ) slots
  );

  # --- firewall teardown (must mirror the setup above) --------------------
  firewallExtraStopCommands = ''
    # ==== myconfig.ai.microvm: remove dedicated agent-sandbox chains =====
    iptables -D INPUT -i ${bridge} -j AGENT_MICROVM_INPUT 2>/dev/null || true
    iptables -D FORWARD -i ${bridge} -j AGENT_MICROVM_FORWARD 2>/dev/null || true
    iptables -D FORWARD -o ${bridge} -j AGENT_MICROVM_FORWARD 2>/dev/null || true
    iptables -D OUTPUT -o ${bridge} -j AGENT_MICROVM_OUTPUT 2>/dev/null || true
    iptables -F AGENT_MICROVM_INPUT 2>/dev/null || true
    iptables -F AGENT_MICROVM_FORWARD 2>/dev/null || true
    iptables -F AGENT_MICROVM_OUTPUT 2>/dev/null || true
    iptables -X AGENT_MICROVM_INPUT 2>/dev/null || true
    iptables -X AGENT_MICROVM_FORWARD 2>/dev/null || true
    iptables -X AGENT_MICROVM_OUTPUT 2>/dev/null || true
  '';
in
{
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      { systemd.services = tapAttachServices; }
      {
        # --- §12 private bridge (NetworkManager-compatible) -------------------
        # Create the bridge with no static members; per-slot TAPs are attached
        # by microvm.nix / the launcher (later phase). This uses the standard
        # udev/systemd bridge mechanism, NOT systemd-networkd (§46).
        networking.bridges.${bridge}.interfaces = [ ];

        # Host-side address on the bridge (the guests' gateway + the address the
        # LiteLLM forwarder binds).
        networking.interfaces.${bridge}.ipv4.addresses = [
          {
            address = gateway;
            prefixLength = prefixLength;
          }
        ];

        # Keep NetworkManager out of the way: it must not try to manage the
        # bridge or the per-slot TAP interfaces, or it will fight microvm.nix.
        networking.networkmanager.unmanaged = [
          "interface-name:${bridge}"
        ]
        ++ map (s: "interface-name:${s.tap}") slots;

        # --- §15 disable IPv6 on the bridge ----------------------------------
        # No IPv6 firewall policy is implemented yet, so prevent IPv6 from being
        # configured on the bridge at all (MVP limitation).
        #
        # The boot.kernel.sysctl key below only applies if the bridge already
        # exists when systemd-sysctl runs at boot; a scripted/NM-created bridge is
        # brought up later, so we ALSO re-apply the key from a oneshot ordered
        # after the bridge's -netdev service, which is when the interface actually
        # exists. (L2 link-local IPv6 between guests remains out of scope for the
        # MVP — see §44; verify with `ip -6 addr show ${bridge}` per §40.)
        boot.kernel.sysctl."net.ipv6.conf.${bridge}.disable_ipv6" = 1;

        systemd.services."agent-microvm-${bridge}-disable-ipv6" = {
          description = "Disable IPv6 on the agent microVM bridge ${bridge}";
          after = [ "${bridge}-netdev.service" ];
          wants = [ "${bridge}-netdev.service" ];
          wantedBy = [ "network.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            # Re-apply the per-interface sysctl now that the bridge exists.
            ExecStart = "${pkgs.procps}/bin/sysctl -w net.ipv6.conf.${bridge}.disable_ipv6=1";
          };
        };

        # --- §13/§14 firewall: proxy-only default policy ---------------------
        # Make same-bridge (guest<->guest) L2 frames traverse iptables FORWARD so
        # the inter-VM DROP rule is actually enforced once TAPs are attached in a
        # later phase. Without br_netfilter + bridge-nf-call-iptables, bridged
        # frames are L2-switched and never reach the FORWARD chain.
        boot.kernelModules = [ "br_netfilter" ];
        boot.kernel.sysctl."net.bridge.bridge-nf-call-iptables" = 1;

        networking.firewall.extraCommands = firewallExtraCommands;
        networking.firewall.extraStopCommands = firewallExtraStopCommands;

        # --- §16 bridge-only LiteLLM forwarder -------------------------------
        # Socket-activated endpoint bound ONLY to the bridge address, never to
        # 0.0.0.0 / the LAN. FreeBind lets it bind before the bridge address is
        # assigned during boot; BindToDevice pins it to the bridge interface.
        systemd.sockets.agent-litellm-proxy = {
          description = "Bridge-only LiteLLM forwarding endpoint for agent microVMs";
          wantedBy = [ "sockets.target" ];
          socketConfig = {
            ListenStream = "${gateway}:${port}";
            BindToDevice = bridge;
            FreeBind = true;
            Accept = false;
          };
        };

        systemd.services.agent-litellm-proxy = {
          description = "Forward <bridge>:<litellmPort> to the loopback LiteLLM proxy";
          requires = [ "agent-litellm-proxy.socket" ];
          after = [ "agent-litellm-proxy.socket" ];
          serviceConfig = {
            # Forward accepted connections to the loopback-only LiteLLM proxy.
            ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${port}";
            # Hardening (§16). This process only shuffles bytes between two
            # sockets, so it needs no filesystem, home or elevated privileges.
            DynamicUser = true;
            NoNewPrivileges = true;
            PrivateTmp = true;
            ProtectSystem = "strict";
            ProtectHome = true;
          };
        };
      }
    ]
  );
}
