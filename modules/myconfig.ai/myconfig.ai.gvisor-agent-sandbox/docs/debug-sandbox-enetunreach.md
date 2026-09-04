# Debug: the model endpoint is unreachable from a sandbox (`ENETUNREACH`)

## Symptom

`agent-gvisor doctor` starts a sandbox fine, but every check that leaves the
sandbox fails *instantly*:

```
agent-gvisor: sandbox works
agent-gvisor: checking the sandbox network (interfaces and routes it sees)
interfaces: lo wlan0
routes (IPv4, /proc/net/route):
no IPv4 default route in the sandbox netstack
…
socat[6] W connect(5, AF=2 192.168.84.1:14000, 16): Network is unreachable
```

`ENETUNREACH` plus `after 0 ms` in curl's message: no packet ever left. The
loopback relay still *starts* (its listener lives inside gVisor's netstack,
which always has a working `lo`), it just cannot reach its target, so the
relay warning is a consequence, not an independent failure — as are both
model-endpoint warnings.

## Root cause (confirmed 2026-09-03 on f13)

A host interface whose IPv4 address carries `noprefixroute` while its
**on-link subnet route is missing**. On f13:

```
$ ip -4 -o addr show dev wlan0
4: wlan0  inet 192.168.1.180/24 brd 192.168.1.255 scope global dynamic noprefixroute wlan0
$ ip -4 route show
default via 192.168.1.1 dev wlan0 proto dhcp src 192.168.1.180 metric 600
…                                   # no 192.168.1.0/24 route at all
```

The host itself still works (its route cache resolves the gateway), but the
copy into the container netns does not:

1. **pasta clones the default-route interface** into the netns. `nl_addr_dup()`
   (`netlink.c`) copies the address message **verbatim** — it strips only
   `IFA_LABEL` and `IFA_CACHEINFO` and ORs in `IFA_F_NODAD`, so
   `IFA_F_NOPREFIXROUTE` survives. The netns tap therefore gets
   `192.168.1.180/24 noprefixroute` and, like on the host, **no subnet route**.
2. **The copied default route is rejected.** With no on-link route the kernel
   refuses `default via 192.168.1.1`:
   ```
   $ unshare -rn sh -c 'ip link add d0 type dummy; ip link set d0 up
       ip addr add 192.168.1.180/24 dev d0 noprefixroute
       ip route add default via 192.168.1.1 dev d0'
   Error: Nexthop has invalid gateway.        # rc=2; without noprefixroute: rc=0
   ```
   `nl_route_dup()` ignores exactly this: `if (rc < 0 && rc != -EEXIST && rc !=
   -ENETUNREACH && rc != -EHOSTUNREACH) return rc;` — by design, silently.
3. The netns now has a tap **with an address and zero IPv4 routes**.
4. `runsc --network=sandbox` copies precisely that into its netstack
   (`collectLinksAndRoutes()`), so the sandbox has `lo` plus an address-only
   interface: loopback works, every off-link connect is `ENETUNREACH`.

Nothing in this chain is agent-gvisor's or gVisor's doing; the sandbox is the
first consumer strict enough to notice the broken host route.

### Fix

Restore the subnet route on the host — reactivate the connection
(NetworkManager: `nmcli con up <name>`, or `systemctl restart
NetworkManager`), or as a stop-gap:

```bash
sudo ip route add 192.168.1.0/24 dev wlan0 proto kernel scope link \
  src 192.168.1.180 metric 600
```

`RTNETLINK answers: File exists` means the route is already back — check
with `ip route show dev wlan0` before adding anything. Then re-run
`agent-gvisor doctor`; the network probe must list the subnet *and* the
default route.

**The missing route is transient.** On f13 it was absent at 19:45 and present
again (`proto kernel … metric 600`, i.e. NetworkManager's own copy) at 06:39
the next morning, without manual intervention — a reassociation or DHCP
renewal restored it. So a sandbox that cannot reach the model endpoint *today*
may work after the next Wi-Fi reconnect, and vice versa. This host arbitrates
two interfaces on the same subnet (`myconfig.networking.preferWired` toggles
the Wi-Fi radio whenever a wired link appears), which is a plausible trigger
for NetworkManager dropping and not re-adding that route. If it recurs often,
the durable options are a NetworkManager dispatcher guard that re-adds the
subnet route, or pinning pasta's namespace address with `-a`/`-g` (both imply
`--no-copy-addrs`/`--no-copy-routes`, so pasta configures the tap itself and
never copies the `noprefixroute` flag).

## Ruled out along the way

- **Configuration and the baked wrapper.** `litellm.{port,forwardPort,address,
  endpoint,loopbackForward}` evaluate to `4000 / 14000 / 192.168.84.1 /
  http://192.168.84.1:14000/v1 / true`, and the installed wrapper carries
  `AGENT_GVISOR_NETWORK=pasta:--map-guest-addr,192.168.84.1`.
- **The host side of the model endpoint.** `/proc/net/tcp` shows LiteLLM on
  `127.0.0.1:4000` and the socket-activated forwarder on `0.0.0.0:14000`.
- **gVisor 20260817.0 (the SIGWINCH bump, `8c960494ea`) losing routes.**
  Reproduced with the *exact* runsc binary the host uses: in a `unshare -rn`
  netns with an address plus a default route, both `runsc run` and the
  podman-style `runsc create` + `runsc start` split log `Setting routes
  [… 0.0.0.0/0 via … nic 2 …]`, and the container sees them in
  `/proc/net/route`. A point-to-point address (`ip addr add 10.0.0.2 peer
  10.0.0.1`) works too, i.e. `patches/gvisor-remove-p2p-addresses.patch` still
  applies correctly to the new tree. The release does move link creation from
  `NetworkCreateLinksAndRoutes` to `SetNetworkArgs` + a deferred
  `ConfigureNetwork` (`runsc/sandbox/network.go`, `runsc/boot/loader.go`),
  which *can* skip networking silently ("SetNetworkArgs called after sandbox
  started … ignoring") — but not on the create/start path podman uses.

### Reproducing the runsc-side experiment

```bash
mkdir -p /tmp/gvtest/rootfs/bin && cd /tmp/gvtest
cp "$(nix eval --raw nixpkgs#busybox.outPath)/bin/busybox" rootfs/bin/ && ln -sf busybox rootfs/bin/sh
runsc spec   # then set process.args to dump /proc/net/dev and /proc/net/route
# and point linux.namespaces[type=network].path at /proc/<pid>/ns/net of the
# shell below — runsc creates a fresh, EMPTY netns when the path is missing,
# which fakes the very bug you are hunting
unshare -rn sh -c '
  ip link set lo up
  ip link add dummy0 type dummy
  ip addr add 192.168.1.180/24 dev dummy0
  ip link set dummy0 up
  ip route add default via 192.168.1.1 dev dummy0
  ip neigh add 192.168.1.1 lladdr 02:00:00:00:00:01 dev dummy0 nud permanent
  runsc --ignore-cgroups --network=sandbox --debug --debug-log=/tmp/gvtest/log.txt \
        --root=/tmp/gvtest/root run test'
grep -aE "Setting up network, config|Setting routes|Skipping|No usable" /tmp/gvtest/log.txt
```

`--rootless` cannot be used (`sandbox network isn't supported with
--rootless`); being uid 0 inside a `unshare -r` user namespace is enough.
Dropping the `ip addr add`, or adding it with `noprefixroute`, reproduces the
empty-route sandbox from the top of this document.
