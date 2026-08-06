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
#        Since improvement ticket 3 C the policy is selected by the NAMED
#        profile `myconfig.ai.microvm.networkProfile`
#        (offline / proxy-only / package-access / internet) rather than by
#        three independent booleans. The capability table lives in
#        ./network-profiles.nix and is resolved ONCE in default.nix
#        (`_module.args.agentNetwork`), so this firewall and the guest-side
#        proxy/DNS configuration in guest.nix always agree.
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
# Invariants that hold in EVERY profile (not configurable):
#   * cloud-metadata 169.254.169.254 dropped first, in INPUT and FORWARD;
#   * guest<->guest dropped (this chain's inter-VM rule, plus the per-TAP L2
#     `isolated` flag from ticket 3 A, which is the real enforcement);
#   * private / special-use IPv4 ranges dropped (host LAN, VPN peers, RFC1918,
#     CGNAT, loopback, link-local, multicast, reserved) — the only exception is
#     a resolver the operator EXPLICITLY listed in `dnsServers`;
#   * terminal DROP at the end of INPUT and FORWARD (fail closed).
{
  config,
  lib,
  pkgs,
  # The effective resource-class table (see default.nix).
  agentResourceClasses,
  # The ONE resolved network decision (profile + capability flags + effective
  # DNS servers), from default.nix (`_module.args.agentNetwork`).
  agentNetwork,
  ...
}:
let
  cfg = config.myconfig.ai.microvm;

  # Shared, deterministic slot table (§4) — imported rather than re-derived
  # so the TAP interface names we hand to NetworkManager's unmanaged list
  # always match the TAPs guest.nix actually creates.
  # The slot pool of the effective resource classes (ticket 5 A). The class
  # table comes from default.nix (`_module.args.agentResourceClasses`), so every
  # module builds the SAME pool.
  slots = (import ./slots.nix { inherit lib; }).mkSlots agentResourceClasses;

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

  # --- profile-derived capabilities (ticket 3 C) --------------------------
  caps = agentNetwork.caps;
  profile = agentNetwork.profile;

  # --- transport-derived capabilities (lightweight plan phase 6) -----------
  # The ONE transport decision, resolved in default.nix from the table in
  # ./network-profiles.nix. `transport.hostBridge` / `.hostFirewall` /
  # `.bridgeLitellm` are what gate EVERYTHING in this file: under the `vsock`
  # transport there is no TAP, no bridge, no firewall chain and no bridge-only
  # LiteLLM socket — the model API is carried by the per-VM AF_VSOCK forwarder
  # below instead.
  transport = agentNetwork.transportCaps;
  vsockPort = toString agentNetwork.vsockLitellmPort;

  # DNS servers the guests may use under the `internet` profile, split by
  # whether they are the host itself on the bridge (INPUT) or off-host and
  # therefore routed (FORWARD).
  dnsServers = lib.optionals caps.dns agentNetwork.dnsServers;
  hostDnsServers = lib.filter (a: a == gateway) dnsServers;
  routedDnsServers = lib.filter (a: a != gateway) dnsServers;

  # Replies to routed guest traffic need conntrack ACCEPTs in FORWARD; in the
  # profiles that route NOTHING we deliberately do not even allow those, so the
  # chain is a pure DROP for forwarded packets.
  forwardConntrack = caps.internetEgress || routedDnsServers != [ ];

  indent = lines: lib.concatMapStringsSep "\n" (l: "    ${l}") lines;

  # Host-local ports a guest may reach on the bridge address, per profile.
  inputAllowLines =
    lib.optional caps.litellm "iptables -A AGENT_MICROVM_INPUT -s ${subnet} -d ${gateway} -p tcp --dport ${port} -j ACCEPT"
    ++ lib.optional caps.packageProxy "iptables -A AGENT_MICROVM_INPUT -s ${subnet} -d ${gateway} -p tcp --dport ${packageProxyPort} -j ACCEPT"
    ++ lib.concatMap (addr: [
      "iptables -A AGENT_MICROVM_INPUT -s ${subnet} -d ${addr} -p udp --dport 53 -j ACCEPT"
      "iptables -A AGENT_MICROVM_INPUT -s ${subnet} -d ${addr} -p tcp --dport 53 -j ACCEPT"
    ]) hostDnsServers;

  # Explicit DNS policy (`internet` profile): ONLY the configured resolvers,
  # and only on port 53. Placed before the private-range drops because an
  # operator may legitimately point at a resolver inside a private range — an
  # explicit allow, never a blanket one.
  dnsForwardLines = lib.concatMap (addr: [
    "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${addr} -p udp --dport 53 -j ACCEPT"
    "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${addr} -p tcp --dport 53 -j ACCEPT"
  ]) routedDnsServers;

  # Everything else on port 53 is dropped, so a guest cannot pick its own
  # resolver / DNS tunnel even when general egress is allowed.
  dnsDenyLines = lib.optionals caps.internetEgress [
    "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -p udp --dport 53 -j DROP"
    "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -p tcp --dport 53 -j DROP"
  ];

  privateRuleLines = map (
    range: "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${range} -j DROP"
  ) privateRanges;

  internetLines = lib.optional caps.internetEgress "iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -j ACCEPT";

  # Auditable egress: rate-limited log of what the terminal DROP below kills.
  logLines = lib.optional caps.logDrops (
    "iptables -A AGENT_MICROVM_FORWARD -m limit --limit 5/min --limit-burst 10"
    + " -j LOG --log-prefix \"agent-microvm-drop: \" --log-level info"
  );

  packageProxyPort = toString (if cfg.packageProxyPort == null then 0 else cfg.packageProxyPort);

  # --- NAT (only the `internet` profile) ---------------------------------
  # Masquerade the guest subnet so `internetEgress` is FUNCTIONAL egress and
  # not just a firewall verdict. Deliberately not `networking.nat` (which needs
  # an externalInterface and would fight the host's own NAT config): a
  # dedicated chain hooked from POSTROUTING, torn down again symmetrically.
  natSetup = lib.optionalString caps.nat ''
    iptables -t nat -N AGENT_MICROVM_NAT 2>/dev/null || iptables -t nat -F AGENT_MICROVM_NAT
    iptables -t nat -C POSTROUTING -s ${subnet} -j AGENT_MICROVM_NAT 2>/dev/null \
      || iptables -t nat -I POSTROUTING 1 -s ${subnet} -j AGENT_MICROVM_NAT
    # Only leaving the bridge subnet is masqueraded; guest<->guest is
    # dropped in FORWARD (and at L2) long before it could be NATed.
    iptables -t nat -A AGENT_MICROVM_NAT -s ${subnet} ! -d ${subnet} -j MASQUERADE
  '';

  natTeardown = lib.optionalString caps.nat ''
    iptables -t nat -D POSTROUTING -s ${subnet} -j AGENT_MICROVM_NAT 2>/dev/null || true
    iptables -t nat -F AGENT_MICROVM_NAT 2>/dev/null || true
    iptables -t nat -X AGENT_MICROVM_NAT 2>/dev/null || true
  '';

  # --- firewall setup (runs after NixOS' own rules) -----------------------
  # Rendered from the effective network PROFILE (see ./network-profiles.nix):
  # the invariants below are unconditional, and only the marked ACCEPT blocks
  # are profile-dependent. Effective profile: ${profile}.
  firewallExtraCommands = ''
            # ==== myconfig.ai.microvm: dedicated agent-sandbox chains ============
            # network profile: ${profile}
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
    ${natSetup}
            # --- INPUT: guest -> host -------------------------------------------
            # Unconditional cloud-metadata block FIRST, so no later ACCEPT (incl.
            # ESTABLISHED) or future relaxation can ever shadow it (defence in
            # depth; also covered by the 169.254/16 FORWARD rule).
            iptables -A AGENT_MICROVM_INPUT -d 169.254.169.254 -j DROP
            # Replies to HOST-initiated control traffic (ssh / console). Required in
            # every profile, including `offline`, since the host manages the VM.
            iptables -A AGENT_MICROVM_INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT
            # Profile-dependent host-local ACCEPTs: the bridge-only LiteLLM endpoint
            # (all profiles except `offline`), the explicit package proxy
            # (`package-access`) and the configured host resolver (`internet`).
            # `offline` adds NOTHING here.
    ${indent inputAllowLines}
            # Deny every other host port / service reachable via the bridge.
            iptables -A AGENT_MICROVM_INPUT -j DROP

            # --- FORWARD: guest -> {other guests, host LAN, internet} -----------
            # Cloud-metadata IP is ALWAYS blocked FIRST, in every profile, so no
            # later ACCEPT can shadow it.
            iptables -A AGENT_MICROVM_FORWARD -d 169.254.169.254 -j DROP
    ${indent (
      lib.optional forwardConntrack "iptables -A AGENT_MICROVM_FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT"
    )}
            # No TAP-to-TAP / guest-to-guest forwarding, in ANY profile. Placed
            # before the 192.168/16 rule below, which would otherwise subsume the
            # bridge subnet.
            #
            # NOTE: same-bridge L2 (guest<->guest) frames only reach iptables
            # FORWARD when net.bridge.bridge-nf-call-iptables=1 (needs br_netfilter);
            # both are enabled in the config section below so this rule actually
            # fires once TAPs are attached to the bridge. This IPv4 rule is the
            # second line of defence only: every guest TAP is additionally marked
            # `isolated` at attach time (see tapAttachServices), which blocks
            # guest<->guest frames in the bridge itself — including ARP, IPv6 ND
            # and every other EtherType that iptables cannot see.
            iptables -A AGENT_MICROVM_FORWARD -s ${subnet} -d ${subnet} -j DROP
            # Explicit DNS policy (`internet` profile only): exactly the configured
            # resolvers, port 53 only. An operator-listed resolver inside a private
            # range is allowed HERE on purpose — an explicit allow, not a blanket
            # one — hence before the private-range drops.
    ${indent dnsForwardLines}
            # Private / special-use ranges (host LAN, VPN peers, RFC1918, CGNAT,
            # loopback, link-local, multicast, reserved) are dropped in EVERY
            # profile: no profile grants access to the host LAN or VPN peers.
    ${indent privateRuleLines}
            # Any other port-53 destination is dropped even under `internet`, so a
            # guest cannot choose its own resolver or tunnel over DNS.
    ${indent dnsDenyLines}
            # General internet: only the `internet` profile adds this ACCEPT, and
            # only together with the NAT rule above, so egress actually works
            # instead of being a firewall verdict that black-holes.
    ${indent internetLines}
            # Rate-limited audit log of what the terminal DROP below kills
            # (`internet` profile).
    ${indent logLines}
            # Unconditional terminal DROP (fail CLOSED). The built-in FORWARD policy
            # is ACCEPT (networking.firewall.filterForward defaults false) and the
            # host has net.ipv4.ip_forward=1, so anything falling out the bottom of
            # this chain would otherwise be routed out the WAN. Every ACCEPT above is
            # scoped to -s ${subnet}; this catch-all denies packets that match none
            # of them — a guest spoofing a non-subnet source IP, the -o ${bridge}
            # (LAN -> guest) direction, and any unexpected protocol — mirroring
            # AGENT_MICROVM_INPUT's terminal DROP.
            iptables -A AGENT_MICROVM_FORWARD -j DROP

            # --- OUTPUT: host -> guest ------------------------------------------
            # The host is trusted; permit host-originated traffic to guests over the
            # bridge (ssh / console management). Unchanged by the profile: this is
            # the control channel, not guest egress.
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
  #
  # --- per-TAP LAYER 2 isolation (ticket 3 A / open item A1) --------------
  # Enslaving alone leaves every guest port in the same L2 broadcast domain:
  # `br_netfilter` + `bridge-nf-call-iptables` gets bridged IPv4 frames into
  # the FORWARD chain (where the inter-VM DROP fires), but iptables does NOT
  # filter ARP — nor IPv6 ND, nor any non-IP EtherType. A hostile guest could
  # therefore still ARP-spoof the gateway or a co-resident guest and MITM
  # host<->guest traffic (the unpinned `agent-microvm ssh` / `--attach`
  # sessions included).
  #
  # `bridge link set dev <tap> isolated on` closes that at L2: the kernel
  # bridge refuses to forward frames between two isolated ports, in EITHER
  # direction and for EVERY EtherType, before any netfilter hook runs. Ports
  # marked isolated can still talk to non-isolated ports and to the bridge
  # itself, so guest<->host (gateway, LiteLLM forwarder, SSH) keeps working.
  #
  # Applied ONLY to guest TAP ports. The bridge's own host-facing interface is
  # deliberately NOT isolated (it is the bridge master, not a port — isolating
  # host connectivity would break the proxy-only egress the design depends on).
  tapAttachServices = builtins.listToAttrs (
    map (
      slot:
      lib.nameValuePair "agent-microvm-attach-${slot.name}" {
        description = "Enslave + L2-isolate TAP ${slot.tap} on bridge ${bridge} for microVM ${slot.name}";
        after = [ "microvm-tap-interfaces@${slot.name}.service" ];
        requires = [ "microvm-tap-interfaces@${slot.name}.service" ];
        before = [ "microvm@${slot.name}.service" ];
        partOf = [ "microvm@${slot.name}.service" ];
        wantedBy = [ "microvm@${slot.name}.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          # Order matters: `isolated` is a bridge-PORT flag, so the interface
          # must already be a port of the bridge. Both steps run on every VM
          # (re)start, because the tap is destroyed and recreated each time.
          ExecStart = [
            "${lib.getExe' pkgs.iproute2 "ip"} link set ${slot.tap} master ${bridge}"
            "${lib.getExe' pkgs.iproute2 "bridge"} link set dev ${slot.tap} isolated on"
          ];
        };
      }
    ) slots
  );

  # --- PER-VM AF_VSOCK model forwarder (lightweight plan phase 6) ----------
  # The host half of the LITERAL phase-6 design:
  #
  #   guest agent -> 127.0.0.1:<litellmPort>            (unchanged endpoint)
  #     -> guest TCP->VSOCK bridge                      (../guest.nix)
  #     -> AF_VSOCK CID 2 (the host), port <litellmPort>
  #     -> cloud-hypervisor's per-VM mux socket
  #        <stateRoot>/<slot>/notify.vsock_<litellmPort>
  #     -> THIS socket unit -> systemd-socket-proxyd
  #     -> 127.0.0.1:<litellmPort>                      (the loopback LiteLLM proxy)
  #
  # WHY A UNIX SOCKET AND NOT AN AF_VSOCK LISTENER: with cloud-hypervisor the
  # guest's VSOCK device is implemented in the VMM, not by the host kernel's
  # vhost-vsock. The host never sees an AF_VSOCK address at all: a
  # guest-initiated connection to host CID 2 port N is delivered by the VMM to
  # the Unix socket `<vsock socket>_<N>` next to the mux socket it was started
  # with (the same convention microvm.nix itself relies on for the guest's
  # systemd notify socket, `notify.vsock_8888`). Listening on that path IS
  # listening on the guest's VSOCK port — and it is strictly stronger than an
  # AF_VSOCK listener would be, because a Unix socket in the VM's own state
  # directory cannot be reached from anywhere except that one VM's VMM process.
  #
  # ONE LISTENER PER VM, which is the plan's own requirement ("The host listener
  # must validate the expected guest CID or use one listener per slot"): each
  # slot's forwarder is a separate unit bound to a separate path inside that
  # slot's state directory, so slot A's guest cannot reach slot B's forwarder
  # (it cannot address slot B's socket at all — there is no shared namespace and
  # no CID to spoof).
  #
  # DESTINATION-FIXED, HOST-LOOPBACK-ONLY: the forwarder's ONLY argument is
  # `127.0.0.1:<litellmPort>`. It is not a CONNECT proxy, it reads nothing from
  # the connection, and the sandboxing below (`IPAddressAllow=localhost` +
  # `IPAddressDeny=any`, `RestrictAddressFamilies=AF_UNIX AF_INET`, DynamicUser,
  # ProtectSystem=strict) makes "some other host port" unreachable for the
  # forwarder itself, not merely unrequestable by the guest.
  # The two per-slot unit tables (sockets + services), generated from the SAME
  # slot pool the VMs are generated from.
  vsockForwarderSockets = builtins.listToAttrs (
    map (
      slot:
      let
        socketPath = "${cfg.stateRoot}/${slot.name}/notify.vsock_${vsockPort}";
      in
      lib.nameValuePair "agent-litellm-vsock-${slot.name}" {
        description = "AF_VSOCK LiteLLM endpoint for microVM ${slot.name} (guest CID ${toString slot.cid})";
        # Present whenever the host's VMs may run — the guest connects on demand,
        # long after boot, and the listener costs one inode plus one systemd
        # socket until then. Ordered after the VM's install unit because that is
        # what creates `<stateRoot>/<slot>` (the directory the socket lives in);
        # `ExecStartPre` additionally creates it, so an unlaunched slot cannot
        # leave the listener dead.
        wantedBy = [ "microvms.target" ];
        wants = [ "install-microvm-${slot.name}.service" ];
        after = [ "install-microvm-${slot.name}.service" ];
        socketConfig = {
          ExecStartPre = "${lib.getExe' pkgs.coreutils "mkdir"} -p ${lib.escapeShellArg "${cfg.stateRoot}/${slot.name}"}";
          ListenStream = socketPath;
          # cloud-hypervisor runs as microvm.nix's `microvm` user, whose primary
          # group is `kvm`, so the VMM (and ONLY it, plus root) may connect. Not
          # world-accessible: an unprivileged host user must not be able to reach
          # the model endpoint through a guest's socket.
          SocketUser = "root";
          SocketGroup = "kvm";
          SocketMode = "0660";
          # A stale socket file would shadow the listener on the next start.
          RemoveOnStop = true;
          Accept = false;
        };
      }
    ) slots
  );

  vsockForwarderServices = builtins.listToAttrs (
    map (
      slot:
      lib.nameValuePair "agent-litellm-vsock-${slot.name}" {
        description = "Forward microVM ${slot.name}'s AF_VSOCK model port to the loopback LiteLLM proxy";
        requires = [ "agent-litellm-vsock-${slot.name}.socket" ];
        after = [ "agent-litellm-vsock-${slot.name}.socket" ];
        serviceConfig = {
          # DESTINATION-FIXED: the loopback LiteLLM proxy, and nothing else.
          ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd 127.0.0.1:${toString cfg.litellmPort}";
          DynamicUser = true;
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          # HOST-TCP-ONLY, enforced on the FORWARDER (not merely on what the
          # guest can ask for): the process may only talk to the host loopback,
          # so even a compromised forwarder cannot reach the LAN, the VPN, the
          # metadata service or another host service. AF_UNIX is the socket it is
          # activated on; AF_INET is the loopback destination. AF_VSOCK is
          # deliberately absent — the VSOCK side is the VMM's Unix socket, not a
          # kernel vsock address.
          IPAddressAllow = "localhost";
          IPAddressDeny = "any";
          RestrictAddressFamilies = [
            "AF_UNIX"
            "AF_INET"
          ];
          RestrictNamespaces = true;
          RestrictRealtime = true;
          LockPersonality = true;
          MemoryDenyWriteExecute = true;
          SystemCallArchitectures = "native";
          SystemCallFilter = [
            "@system-service"
            "~@privileged"
            "~@resources"
          ];
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
    ${natTeardown}
  '';
in
{
  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      # --- lightweight plan phase 6: the PER-VM AF_VSOCK model forwarder ----
      # The ONLY thing this file produces under the `vsock` transport: no bridge,
      # no TAP, no firewall chain, no bridge-only socket. One listener per VM,
      # destination-fixed to the loopback LiteLLM proxy.
      (lib.mkIf transport.vsockLitellm {
        systemd.sockets = vsockForwarderSockets;
        systemd.services = vsockForwarderServices;
      })

      # Everything below is the `tap` transport (the historical shape, and the
      # only one that can carry DNS / NAT / a package proxy). Gated on the
      # transport's own capability flags rather than on a transport NAME, so
      # adding a transport cannot silently re-enable half of it.
      (lib.mkIf transport.hostBridge { systemd.services = tapAttachServices; })
      (lib.mkIf transport.hostBridge {
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
          # `wants` (not `requires`): unlike the LiteLLM forwarder socket below
          # (whose `BindToDevice` FAILS hard without the bridge device, so a
          # missing bridge must be a visible socket failure), a missing bridge
          # here only leaves IPv6 un-disabled on it — a cosmetic hardening gap,
          # not a broken listener — so a `wants` pull-in is the right strength.
          wantedBy = [ "network.target" ];
          serviceConfig = {
            Type = "oneshot";
            RemainAfterExit = true;
            # Re-apply the per-interface sysctl now that the bridge exists.
            ExecStart = "${pkgs.procps}/bin/sysctl -w net.ipv6.conf.${bridge}.disable_ipv6=1";
          };
        };

      })

      (lib.mkIf transport.hostFirewall {
        # --- §13/§14 firewall: proxy-only default policy ---------------------
        # Make same-bridge (guest<->guest) L2 frames traverse iptables FORWARD so
        # the inter-VM DROP rule is actually enforced once TAPs are attached in a
        # later phase. Without br_netfilter + bridge-nf-call-iptables, bridged
        # frames are L2-switched and never reach the FORWARD chain.
        #
        # Under the `vsock` transport (lightweight plan phase 6) NONE of this
        # exists — and it does not have to: a guest with no network interface has
        # nothing to filter, which is the whole point of the transport. The
        # AGENT_MICROVM_* chains are the second line of defence for a guest that
        # CAN address the bridge; a vsock guest cannot address anything but its
        # own loopback.
        boot.kernelModules = [ "br_netfilter" ];
        boot.kernel.sysctl."net.bridge.bridge-nf-call-iptables" = 1;

        networking.firewall.extraCommands = firewallExtraCommands;
        networking.firewall.extraStopCommands = firewallExtraStopCommands;
      })

      (lib.mkIf transport.bridgeLitellm {
        # --- §16 bridge-only LiteLLM forwarder -------------------------------
        # Socket-activated endpoint bound ONLY to the bridge address, never to
        # 0.0.0.0 / the LAN. BindToDevice pins the listener to the bridge
        # interface (SO_BINDTODEVICE); FreeBind (IP_FREEBIND) lets it bind the
        # <gateway>:<port> address before it is assigned to the bridge.
        #
        # BOOT ORDERING (do not drop): `BindToDevice` requires the bridge
        # DEVICE to exist at bind time — `SO_BINDTODEVICE` fails with ENODEV
        # otherwise, and `FreeBind` (IP_FREEBIND) does NOT cover it (it covers
        # the *address*, not the *device*). The socket is `wantedBy
        # sockets.target`, which starts early in boot — BEFORE the
        # `${bridge}-netdev.service` (`wantedBy network.target`) creates the
        # bridge. Without an explicit `after`/`requires` against that netdev
        # unit the socket therefore failed once at boot and never retried, so
        # the bridge endpoint had no listener and every guest -> LiteLLM
        # connection was refused (the worker died ~2s in). `requires` +
        # `after` make systemd defer the socket until the bridge exists, so
        # `SO_BINDTODEVICE` succeeds; `requires` (not merely `wants`) also
        # turns a missing bridge into a VISIBLE failure of this socket rather
        # than a silent dead listener.
        systemd.sockets.agent-litellm-proxy = {
          description = "Bridge-only LiteLLM forwarding endpoint for agent microVMs";
          wantedBy = [ "sockets.target" ];
          after = [ "${bridge}-netdev.service" ];
          requires = [ "${bridge}-netdev.service" ];
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
      })
    ]
  );
}
