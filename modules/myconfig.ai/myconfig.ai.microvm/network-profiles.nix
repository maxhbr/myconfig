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

  # Profiles that widen the guest's reach beyond the model API and therefore
  # require the explicit `acknowledgeInsecureNetwork` opt-in.
  insecureProfiles = [
    "package-access"
    "internet"
  ];

  forProfile = profile: capabilities.${profile};
}
