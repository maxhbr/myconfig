# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.microvm — THE network-profile capability table (improvement
# ticket 3 C).
#
# `myconfig.ai.microvm.networkProfile` replaces the three ambiguous booleans
# (allowPublicInternet / allowPrivateNetworks / allowInterVmTraffic) with four
# NAMED, coherent profiles. This file is the single source of truth for what
# each profile means; network.nix renders the firewall from it and guest.nix
# derives the guest-side proxy/DNS configuration from it, so the host policy
# and the guest configuration can never disagree.
#
# The capability flags are deliberately POSITIVE ("what is additionally
# allowed"), because everything not listed is denied by the terminal DROPs in
# network.nix. Two properties hold in EVERY profile and are therefore not
# expressible here:
#
#   * guest<->guest isolation — per-TAP L2 `isolated` (ticket 3 A) plus the
#     IPv4 inter-VM FORWARD DROP. There is no way to relax it.
#   * cloud-metadata (169.254.169.254) and private/special-use IPv4 ranges are
#     always dropped (RFC1918, CGNAT, loopback, link-local, multicast,
#     reserved) — a guest can never reach the host LAN or VPN peers.
#
# Capability flags:
#
#   litellm         guest may reach the bridge-only LiteLLM endpoint
#                   <gatewayAddress>:<litellmPort> (the model API).
#   packageProxy    guest may reach the explicit host package proxy
#                   <gatewayAddress>:<packageProxyPort>, and gets http_proxy /
#                   https_proxy pointed at it. NOT general egress: the proxy is
#                   a single host-controlled TCP port.
#   dns             guest may reach the configured DNS servers on port 53
#                   (udp+tcp) and is configured to use them. All other port-53
#                   egress stays blocked.
#   internetEgress  guest may be routed to public IPv4 addresses (private
#                   ranges and metadata remain blocked).
#   nat             host masquerades the guest subnet, so `internetEgress` is
#                   functional rather than a mere firewall verdict.
#   logDrops        rate-limited LOG of dropped forwarded guest packets, so
#                   egress attempts are auditable in the journal.
#
# THE TRANSPORT DECISION (lightweight plan phase 6, the LITERAL objective)
# ------------------------------------------------------------------------
# The capability flags above say WHAT a guest may reach; the TRANSPORT table
# below says HOW the model API gets there — and, with it, whether the guest has
# an ordinary network interface at all:
#
#   tap    the guest has a TAP interface on the private bridge, a static IPv4
#          and a default route; the host carries the bridge, the
#          AGENT_MICROVM_* firewall chains and the bridge-only LiteLLM socket.
#   vsock  the guest has NO network interface (loopback only). The model API
#          travels guest 127.0.0.1:<litellmPort> -> AF_VSOCK -> a PER-VM host
#          forwarder -> 127.0.0.1:<litellmPort> (the loopback LiteLLM proxy).
#          There is no TAP, no bridge, no guest networkd, no static IP and no
#          host firewall chain to get wrong.
#
# `resolveTransport` is the ONE place that decides, and it is deliberately
# narrow: VSOCK replaces the network only for the CLOSED, model-API-only
# profile (`proxy-only`) and only when the host selected the `vsock`
# capability. `package-access`/`internet` NEED ordinary IP networking (that is
# what they are), and `offline` has no model API to carry, so both keep the
# `tap` shape. This is what keeps the module on ONE code path: every consumer
# asks the resolved transport's capability flags, nobody re-derives the
# condition.
{ lib }:
rec {
  none = {
    litellm = false;
    packageProxy = false;
    dns = false;
    internetEgress = false;
    nat = false;
    logDrops = false;
  };

  capabilities = {
    # Only the control traffic strictly required to manage the VM (host ->
    # guest SSH / console, and the replies to it). No model API, no DNS, no
    # package proxy, no routing, no other guests, no host services.
    offline = none;

    # THE SECURE DEFAULT. The only egress a guest gets is the bridge-only
    # LiteLLM endpoint; arbitrary DNS, the public internet, private networks,
    # metadata endpoints, other guests and unrelated host services are denied.
    proxy-only = none // {
      litellm = true;
    };

    # LiteLLM plus controlled package access through ONE explicit host proxy
    # port. Deliberately implemented WITHOUT routing, NAT or DNS: the guest
    # cannot open arbitrary connections, it can only talk to the host proxy,
    # which decides what to fetch.
    package-access = none // {
      litellm = true;
      packageProxy = true;
    };

    # Complete, functional egress: routing + NAT/masquerading + an explicit DNS
    # policy + metadata/private-network blocking + guest isolation + drop
    # logging. INSECURE; requires `acknowledgeInsecureNetwork = true`.
    internet = none // {
      litellm = true;
      dns = true;
      internetEgress = true;
      nat = true;
      logDrops = true;
    };
  };

  names = lib.attrNames capabilities;

  # --- the TRANSPORT table (lightweight plan phase 6) ---------------------
  # Positive flags again ("what this transport builds"), so a consumer never has
  # to test the transport NAME:
  #
  #   guestInterface  the guest gets a `microvm.interfaces` TAP, a static IPv4,
  #                   a default route and systemd-networkd. FALSE means
  #                   loopback-only — invariant 6 as an absence of a device
  #                   rather than as a firewall verdict.
  #   hostBridge      the host creates the private bridge, enslaves + L2-isolates
  #                   the per-slot TAPs, marks them unmanaged in NetworkManager
  #                   and loads br_netfilter.
  #   hostFirewall    the host renders the AGENT_MICROVM_INPUT/_FORWARD/_OUTPUT
  #                   chains (and, under `internet`, the NAT chain).
  #   bridgeLitellm   the host's bridge-only LiteLLM socket + the guest's
  #                   loopback -> <gatewayAddress> forwarder.
  #   vsockLitellm    the PER-VM host AF_VSOCK -> 127.0.0.1:<litellmPort>
  #                   forwarder + the guest's loopback -> AF_VSOCK bridge.
  #   tapSsh          an SSH daemon bound to a guest NETWORK interface can be
  #                   reached at all. False without an interface, which is why
  #                   the VSOCK control channel is the only one a vsock-transport
  #                   guest has (see ../guest.nix / ../launcher.nix).
  noTransport = {
    guestInterface = false;
    hostBridge = false;
    hostFirewall = false;
    bridgeLitellm = false;
    vsockLitellm = false;
    tapSsh = false;
  };

  transports = {
    # THE HISTORICAL SHAPE, and still the only one that can carry DNS, NAT or a
    # package proxy: a TAP on the private bridge, the firewall chains and the
    # bridge-only LiteLLM endpoint.
    tap = noTransport // {
      guestInterface = true;
      hostBridge = true;
      hostFirewall = true;
      bridgeLitellm = true;
      tapSsh = true;
    };

    # THE LITERAL PHASE-6 SHAPE: no network interface in the guest at all, the
    # model API over AF_VSOCK to a per-VM host forwarder that only ever connects
    # to 127.0.0.1:<litellmPort>. Strictly stronger than `tap` + `proxy-only`:
    # the guest cannot address anything but its own loopback, so LAN, VPN,
    # metadata, DNS, other guests and every other host port are unreachable by
    # CONSTRUCTION rather than by firewall rule.
    vsock = noTransport // {
      vsockLitellm = true;
    };
  };

  transportNames = lib.attrNames transports;

  # The ONE transport decision. `vsockCapability` is
  # `agentCapabilities.vsock` (../default.nix).
  resolveTransport =
    {
      profile,
      vsockCapability,
    }:
    if vsockCapability && profile == "proxy-only" then "vsock" else "tap";

  forTransport = transport: transports.${transport};

  # Profiles that widen the guest's reach beyond the model API and therefore
  # require the explicit `acknowledgeInsecureNetwork` opt-in.
  insecureProfiles = [
    "package-access"
    "internet"
  ];

  forProfile = profile: capabilities.${profile};
}
