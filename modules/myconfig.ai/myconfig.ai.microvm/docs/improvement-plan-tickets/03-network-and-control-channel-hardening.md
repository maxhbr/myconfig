# Ticket 3: Harden Guest Networking and the Control Channel

## Goal

Strengthen the isolation boundary between simultaneously running MicroVM guests and replace ambiguous networking options with explicit, coherent profiles.

The secure default must remain proxy-only access to the host-side LiteLLM endpoint.

## Prerequisites

- Tickets 1 and 2 are complete.

## Scope

Update at least:

```text
modules/myconfig.ai/myconfig.ai.microvm/network.nix
modules/myconfig.ai/myconfig.ai.microvm/guest.nix
modules/myconfig.ai/myconfig.ai.microvm/launcher.nix
modules/myconfig.ai/myconfig.ai.microvm/default.nix
```

## Part A: Add per-TAP Layer 2 isolation

### Problem

IP filtering through `br_netfilter` does not by itself prevent ARP spoofing or direct Layer 2 communication between guests on the same bridge.

### Required implementation

When attaching every guest TAP interface to the agent bridge, enable bridge-port isolation:

```bash
ip link set "$tap" master agentbr0
bridge link set dev "$tap" isolated on
```

Use the actual bridge and TAP names from the current module.

Do not apply guest-port isolation to the bridge’s host-facing interface.

### Validation

Verify with:

```bash
bridge link show
```

Each active guest TAP must report isolation enabled.

With two guests running, verify that guest A cannot:

- Reach guest B over IPv4.
- Reach guest B over IPv6.
- Exchange direct Ethernet traffic with guest B.
- Poison guest B or the host through ARP.
- Intercept host-to-guest-B SSH traffic.

Both guests must still reach the explicitly allowed LiteLLM endpoint.

## Part B: Implement trustworthy interactive attachment

### Preferred result

Use deterministic per-slot SSH host keys for interactive access and VSOCK for noninteractive control traffic.

### Deterministic SSH host keys

For every predeclared slot:

1. Provision a unique SSH host key.
2. Make the private key available only to that slot.
3. Generate a host-side known-hosts file.
4. Enable strict host-key verification in the launcher:

```bash
ssh \
  -o StrictHostKeyChecking=yes \
  -o UserKnownHostsFile=/var/lib/agent-microvms/known_hosts \
  ...
```

Do not share one private host key across all guests.

### VSOCK preparation

Assign every concurrently runnable slot a unique VSOCK CID.

Reserve VSOCK for later batch job readiness, status, cancellation, and results.

Do not use reserved CIDs.

## Part C: Replace network booleans with profiles

Introduce:

```nix
myconfig.ai.microvm.networkProfile = "proxy-only";
```

Support exactly these profiles:

```text
offline
proxy-only
package-access
internet
```

### `offline`

Allow only control traffic strictly required to manage the VM.

Block LiteLLM, public internet, metadata endpoints, private networks, and other guests.

### `proxy-only`

Allow:

- DHCP as required.
- Host control traffic as required.
- The host-side LiteLLM forwarding endpoint.

Block:

- Arbitrary DNS.
- Public internet.
- Private networks.
- Metadata endpoints.
- Other guests.
- Unrelated host services.

This remains the default.

### `package-access`

Allow LiteLLM and controlled package access through an explicit host proxy or egress gateway.

Do not implement this as unrestricted public internet.

### `internet`

Implement complete functional egress:

- IP forwarding.
- NAT or masquerading.
- DNS policy.
- Metadata blocking.
- Private-network blocking unless explicitly allowed.
- Guest-to-guest isolation.
- Logging policy.

Do not expose an `internet` option that only changes a firewall verdict.

## Migration requirements

If old booleans are public module options:

1. Keep them temporarily with deprecation warnings.
2. Translate safe combinations to profiles.
3. Reject ambiguous or unsafe combinations.
4. Document the migration path.
5. Do not silently make an existing configuration less restrictive.

## Acceptance criteria

- Every active guest TAP has bridge-port isolation enabled.
- Two guests cannot communicate at Layer 2, IPv4, or IPv6.
- Interactive SSH uses strict host-key verification.
- Every slot has a unique host identity and VSOCK CID.
- `proxy-only` remains the default.
- `internet` includes working routing, NAT, DNS, and filtering.
- Existing safe configurations migrate with warnings rather than silent behavior changes.

## Validation

Use real running MicroVMs and packet captures where useful:

```bash
bridge link show
ip link show
iptables-save
nft list ruleset
tcpdump -ni agentbr0
tcpdump -ni <tap-interface>
```

Test metadata and private-network blocking, including:

```text
169.254.169.254
10.0.0.0/8
172.16.0.0/12
192.168.0.0/16
another active guest
a public IP
a public DNS server
```
