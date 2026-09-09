# Proposal: a durable guard against the missing on-link subnet route

Status: **proposed, not implemented.** Written after the 2026-09-03 incident
in [`debug-sandbox-enetunreach.md`](./debug-sandbox-enetunreach.md); implement
only if that failure recurs (see "When to implement").

## The problem this guards against

When the host's default-route interface carries a `noprefixroute` address and
its on-link subnet route is missing from table `main`, pasta copies the
address flags verbatim (`nl_addr_dup()`), so the container netns gets no
subnet route either, the copied default route is rejected ("Nexthop has
invalid gateway") and `nl_route_dup()` swallows that error by design. runsc
then reproduces the resulting address-only netns in its netstack, and every
sandbox loses all off-link connectivity while `127.0.0.1` keeps working.

Observed on f13: route absent at 19:45, back (NetworkManager's own
`proto kernel … metric 600`) at 06:39 without intervention. It is transient
and self-healing, which is exactly what makes it expensive: sandboxes break
and recover for no reason visible to their user.

`agent-gvisor doctor` now *detects* the state in one line. A guard would
*prevent* it.

## Option A (recommended): re-add the subnet route on the host

Fix the host, once, for every consumer — pasta, microVMs, containers and the
host itself — instead of teaching each one to cope.

New module `modules/myconfig.networking.subnetRouteGuard.nix`, defining
`options.myconfig.networking.subnetRouteGuard.enable`, enabled where
`myconfig.networking.preferWired` is (that module's radio toggling on every
wired carrier change is the suspected trigger).

Sketch:

- A `writeShellApplication` that, for every interface with a global IPv4
  address and no route covering that address' prefix in table `main`, adds
  `<prefix> dev <if> proto kernel scope link src <addr> metric <default-route
  metric of that if>`. Idempotent (`ip route replace`, or tolerate `EEXIST`),
  and a no-op when the route is present — which is almost always.
- Triggered exactly like `wg-roaming-*` already is in
  `modules/myconfig.wireguard/default.nix`: a `oneshot` unit plus
  `networking.networkmanager.dispatcherScripts` on
  `up | dhcp4-change | connectivity-change`, with
  `startLimitIntervalSec = 0` (dispatcher events arrive in bursts and would
  otherwise trip `StartLimitBurst`). Reuse that pattern verbatim rather than
  inventing a second style.
- No timer. The state is event-driven; a polling loop would only add a
  window in which the route is wrong and paper over the actual NM bug.

Cost: one small module and a systemd unit that does nothing 99.9 % of the
time. Risk: it re-adds a route NetworkManager believes it owns — keep the
metric identical to NM's so a later NM insert is a no-op rather than a
duplicate.

### Before implementing: find out who drops the route

The guard treats a symptom. Spend one round on the cause first, because it
may be a plain misconfiguration:

```bash
journalctl -u NetworkManager --since -1d | grep -iE 'route|wlan0' | less
nmcli -f ipv4.route-metric,ipv4.never-default,ipv4.ignore-auto-routes \
      con show "<wifi-connection>"
ip monitor route            # leave running across a wired plug/unplug cycle
```

If `ip monitor route` shows the deletion coinciding with
`prefer-wired.service` toggling the radio, that is the whole story and the
guard is the right fix. If NM never adds the route at all, fix the connection
profile instead and drop this proposal.

## Option B: make pasta configure the tap itself

Bake an explicit address and gateway into the podman network spec, in
`modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix`:

```nix
AGENT_GVISOR_NETWORK = "pasta:--map-guest-addr,${addr},-a,<ns-addr>,-g,<gw>";
```

`-a`/`-g` imply `--no-copy-addrs`/`--no-copy-routes`: pasta then sets up a
single address of its own (via `nl_addr_set()`, which sets only
`IFA_F_NODAD` — never `IFA_F_NOPREFIXROUTE`) and only the default gateway, so
the kernel creates the prefix route and the gateway resolves.

Rejected as the primary fix:

- it needs a *static* namespace address and gateway, which the module cannot
  know and which stop being true the moment the host changes network;
- it only rescues gVisor sandboxes, leaving every other pasta/netns consumer
  on the same host broken;
- `--map-guest-addr` maps to "the address assigned with `-a`", so the litellm
  endpoint mapping would silently follow whatever address is hardcoded.

Worth keeping in mind as an escape hatch for a host where Option A is
impossible (no NetworkManager, immutable network config).

## When to implement

Implement Option A when the missing subnet route is observed a **second**
time — i.e. `agent-gvisor doctor` reports "no IPv4 default route in the
sandbox netstack" again, or `ip route show dev <default-route-if>` lacks the
subnet while an address is configured. One occurrence is not worth a
permanent host-level workaround.

## How to verify a guard works

1. Reproduce the broken host state deliberately:
   ```bash
   sudo ip route del 192.168.1.0/24 dev wlan0
   ```
2. The guard's unit must re-add it within one dispatcher event
   (`systemctl start <guard>.service` for the direct test), and
   `ip route show dev wlan0` must list subnet *and* default again.
3. `agent-gvisor doctor` must show both routes under
   `routes (IPv4, /proc/net/route)` and reach the model endpoint.
4. Without the guard, step 1 alone must break `doctor` in exactly the way
   `debug-sandbox-enetunreach.md` describes — that is the regression test for
   the whole chain.
