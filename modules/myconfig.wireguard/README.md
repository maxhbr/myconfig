# `myconfig.wireguard` — WireGuard mesh client

Option-based NixOS module that turns a host into a WireGuard mesh
participant against the rendezvous server defined in
`hosts/metadata.json`. It builds the peer list automatically from the
metadata, falls back to the rendezvous host as a catch-all router, and
provides a runtime probe so roaming laptops can opportunistically use
direct LAN endpoints when they're at home.

The matching server side (NAT/MASQUERADE for the wg subnet, dnsmasq for
`*.<wg>.maxhbr.local`) lives in
`hosts/host.vserver/service.wireguard-server/`.

---

## Topology

```
                       ┌────────────────────────┐
                       │  vserver (rendezvous)  │
                       │  10.199.199.1          │
                       │  public IP             │
                       │  dnsmasq, NAT, catch-  │
                       │  all 10.199.199.0/24   │
                       └─────────┬──────────────┘
                                 │ public Internet
                                 │
                ┌────────────────┼────────────────────┐
                │                │                    │
                │       home LAN 192.168.1.0/24       │
                │                │                    │
        ┌───────▼──────┐ ┌───────▼──────┐ ┌───────────▼─────┐
        │ thing        │ │ nas          │ │ nuc, r6c, ...   │
        │ 192.168.1.60 │ │ 192.168.1.9  │ │ (LAN-anchored,  │
        │ 10.199.199.60│ │ 10.199.199.6 │ │  fixIp)         │
        └──────────────┘ └──────────────┘ └─────────────────┘
                ╲                ╲                  ╱
                 ╲                ╲                ╱  direct LAN
                  ╲────── full mesh on home LAN ──╱   wg endpoints

        ┌──────────────┐                     ┌──────────────┐
        │ f13 (laptop) │ ── roams between ── │ Pixel9 (mob.)│
        └──────────────┘                     └──────────────┘
```

* **Single source of truth**: `hosts/metadata.json`. Each host has
  `network` (LAN id, e.g. `home`), an optional `ip4` (LAN IP), and
  `wireguard.<wg>.{ip4,pubkey}`. The rendezvous host is named in
  `metadata.networks.<wg>.peer`.
* **Full mesh** in cryptokey-routing terms: every host has every other
  host as a wg peer. There is no "client / server" role at the protocol
  level.
* **LAN optimization**: if both ends share `network`, the peer entry
  uses the LAN `ip4` as endpoint, so traffic stays on the LAN.
* **Off-LAN reach**: the rendezvous peer carries a wider
  `allowedIPs = ["10.199.199.0/24"]` so any wg subnet traffic that
  doesn't have a more-specific `/32` peer falls through to vserver,
  which forwards via its `net.ipv4.ip_forward = 1` + NAT setup.

---

## How peers are derived

`getWgPeersFor wgInterface thisHostName roaming` (file-local in
`default.nix`) walks every other host in metadata that has both
`wireguard.<wg>.ip4` and `wireguard.<wg>.pubkey` and emits one peer
entry per host with these rules:

| Condition                                              | `allowedIPs`         | `endpoint`                | `persistentKeepalive` |
| ------------------------------------------------------ | -------------------- | ------------------------- | --------------------- |
| Same `network` as me, and peer has `ip4` (LAN-anchored)| `[<peer-wg-ip>/32]`  | `<peer-lan-ip>:51820`     | _(none — LAN)_        |
| Different `network`, peer has `ip4` (typically vserver)| `[<peer-wg-ip>/32]`  | `<peer-pub-ip>:51820`     | `25` _(NAT keepalive)_|
| Peer is the rendezvous host                            | + `10.199.199.0/24`  | (as above)                | (as above)            |
| Peer has no `ip4` (e.g. phone)                         | `[<peer-wg-ip>/32]`  | _(none — peer initiates)_ | _(none)_              |
| `roaming = true` and peer is on my `network`           | `[]` _(ghost peer)_  | _(none)_                  | _(none)_              |

The kernel does **longest-prefix-match** on `allowedIPs`, so a per-peer
`/32` always wins over the rendezvous catch-all when we *do* have a
direct path. When we don't, packets for that peer's wg IP fall through
to the rendezvous and get forwarded.

---

## Per-host roles

### LAN-anchored host

A wired box on the home LAN (workstation, nuc, nas, r6c, thing, …).
Idiomatic config:

```nix
imports = [
  (myconfig.metadatalib.fixIp "enp0s20u2")  # static LAN IP, gateway, DNS
];
myconfig.wireguard.wg0 = {
  enable           = true;
  privateKeySource = ../../priv/secrets/wireguard.<host>.age;
};
```

Effect:

* `fixIp` opens `UDP/51820` only on the named LAN interface, so the
  port is exposed on the home LAN but **not** on any other network the
  host might be plugged into in the future.
* The static peer list has direct LAN endpoints for all other home
  hosts, plus the rendezvous endpoint for off-LAN reach.

### Roaming host

A laptop or phone that doesn't have a stable LAN identity (DHCP, Wi-Fi
SSIDs change, IPs differ between networks). Config:

```nix
myconfig.wireguard.wg0 = {
  enable           = true;
  privateKeySource = ../../priv/secrets/wireguard.<host>.age;
  roaming          = true;
};
```

Effect:

* The static peer list emits same-LAN peers as **ghosts**
  (no `endpoint`, empty `allowedIPs`). Only the rendezvous peer has
  routable allowedIPs (`10.199.199.0/24`), so off-LAN all wg traffic
  flows through vserver.
* The `wg-roaming-<wg>.service` + `.timer` + a NetworkManager
  dispatcher hook detect "we're on the home LAN" and patch the ghost
  peers in via `wg set` (endpoint + per-peer `/32`). When we leave the
  LAN, the script clears the per-peer allowedIPs so traffic falls back
  to the rendezvous catch-all.
* The host firewall does **not** open UDP/51820 globally, so the wg
  port is never exposed on untrusted Wi-Fi.

### Rendezvous host (vserver)

Configured separately under `hosts/host.vserver/service.wireguard-server/`.
Not managed by this module. Its job:

* Listen for wg handshakes on a public IP.
* Carry every other host as a peer with `[<wg-ip>/32]` allowedIPs.
* MASQUERADE the wg subnet (`10.199.199.0/24`) out to the internet, so
  it can both relay between mesh peers and act as a VPN exit.
* Run dnsmasq on `10.199.199.1` to serve `*.<wg>.maxhbr.local` for the
  whole mesh.

---

## The roaming probe in detail

The bash script generated by `mkRoamingScript` (file-local) does the
minimum needed to flip between two modes:

**Mode detection** — strict and fast, no ICMP/DNS round-trip:

```bash
ip route show default | grep -qE "via $home_gw( |$)"
```

`$home_gw` is `metadata.networks.<thisHost.network>.defaultGateway`.
If the kernel's default route points at that IP, we're on the home LAN
(L3 segment); otherwise we're not.

**Mode application** — for each LAN-anchored peer of this host:

* `on-lan`:
  ```
  wg set <wg> peer <pubkey> endpoint <lan-ip>:51820 \
                            allowed-ips <wg-ip>/32
  ```
* `off-lan`:
  ```
  wg set <wg> peer <pubkey> allowed-ips ""
  ```
  Clears the cryptokey route for this peer; traffic for `<wg-ip>` then
  falls through to the rendezvous's catch-all (longest-prefix match).
  Endpoint is left as last set; harmless without a route.

**Idempotency** — `/run/wg-roaming/<wg>.mode` records the last applied
mode. The script no-ops when the mode hasn't changed.

**Triggers**:

* `wantedBy = [ "multi-user.target" ]` and `after =
  [ "wireguard-<wg>.service" "network-online.target" ]` so the very
  first invocation runs at boot, after wg has come up.
* A timer with `OnBootSec = 30s; OnUnitActiveSec = 60s` catches Wi-Fi
  changes that don't fire any other event.
* A NetworkManager dispatcher hook fires immediately on
  `up | down | connectivity-change | dhcp4-change | dhcp6-change`.

---

## Operational checks

```bash
# Current peer state (endpoints, allowed-ips, handshake age).
sudo wg show wg0

# Roaming probe state and recent mode flips.
journalctl -u wg-roaming-wg0.service -b
cat /run/wg-roaming/wg0.mode

# Force a probe right now.
sudo systemctl start wg-roaming-wg0.service

# Confirm packets actually take the LAN path on a same-LAN pair.
sudo tcpdump -ni <lan-iface> 'udp port 51820 and host <peer-lan-ip>' &
ping -c5 <peer-wg-ip>
```

A LAN-direct flow looks like UDP/51820 packets between the two LAN IPs;
a vserver-relayed flow looks like UDP/51820 packets to/from vserver's
public IP. Latency is also a clean tell — same-LAN is typically
sub-millisecond, vserver-relayed is at least 2× the home → vserver RTT.

---

## Trade-offs and design notes

* **No global UDP/51820 firewall opening**. Originally the module
  opened the port on `networking.firewall.allowedUDPPorts`. That
  exposed the wg listen port on every Wi-Fi a laptop joined.
  Now the opening lives in `metadatalib.fixIp`, which is only called
  by hosts with a static LAN identity — exactly the set that should
  accept inbound LAN handshakes. Roaming hosts never expose 51820.
* **Catch-all on rendezvous, /32 on LAN peers**. Putting the catch-all
  on the *LAN* peers would make them look reachable from anywhere,
  breaking off-LAN behavior. Putting it only on the rendezvous gives
  us "use direct path when available, otherwise relay" for free via
  longest-prefix-match.
* **Default-route probe vs. ICMP probe**. ICMP can be blocked, has
  RTT, and depends on the gateway being reachable as opposed to
  *being* the gateway. The default-route check is local, immediate,
  and exactly captures "am I on this L3 segment".
* **`roaming = false` is correct for stationary Wi-Fi hosts too**.
  As long as the host has a stable presence on the home LAN (DHCP
  reservation or static IP via `fixIp`), the static peer list with
  LAN endpoints works fine for it. Roaming mode only matters when the
  host actually leaves the home network.
* **WireGuard endpoint roaming** still helps even without `roaming =
  true`: the kernel retargets a peer's endpoint based on the source
  address of inbound packets, so a laptop that briefly dropped Wi-Fi
  recovers automatically once any peer reaches it. Roaming mode is the
  belt-and-suspenders that handles the "no peer can reach me right
  now" case (i.e. just after waking on a foreign Wi-Fi).
* **Why per-key `lib.mkMerge` instead of one big `config = ...`**.
  Writing `config = lib.mkMerge (mapAttrsToList ... config.myconfig.wireguard)`
  triggers infinite recursion in NixOS's lazy module evaluator: the
  *shape* of `config` ends up depending on the option whose merge is
  being computed. Splitting per top-level key (`myconfig.secrets`,
  `networking.wireguard.interfaces`, `systemd.services`, …) breaks the
  cycle because each assignment only depends on values, not on what
  *other* keys exist.

---

## Extending

### Add a new wg host

1. Generate a key pair on the host:
   ```bash
   umask 077
   wg genkey | tee /tmp/private | wg pubkey > /tmp/public
   ```
2. Append to `hosts/metadata.json` under `hosts.<name>`:
   ```json
   "wireguard": {
     "wg0": {
       "ip4":    "10.199.199.<n>",
       "pubkey": "<public key>"
     }
   }
   ```
3. Encrypt the private key as an age file in the private repo and
   reference it from the host config:
   ```nix
   myconfig.wireguard.wg0 = {
     enable           = true;
     privateKeySource = ../../priv/secrets/wireguard.<name>.age;
     # roaming = true;  # if it's a laptop
   };
   ```
4. Redeploy the new host *and* the rendezvous (vserver), so vserver
   learns the new peer's pubkey + wg IP. Other mesh hosts redeploy at
   their own pace; until they do, traffic to the new peer relays via
   vserver.

### Add a new wg interface (e.g. `wg1` for a separate group)

Add `metadata.networks.wg1 = { allowedIPs = ["..."]; peer = "<rendezvous>"; }`,
then per-host `wireguard.wg1.{ip4,pubkey}`. Hosts that should join the
new mesh add a second submodule:

```nix
myconfig.wireguard.wg1 = {
  enable           = true;
  privateKeySource = ../../priv/secrets/wireguard.<host>.wg1.age;
};
```

The two interfaces are independent: separate subnets, separate
rendezvous, separate roaming probes.

### Enable roaming on another laptop

Add this in the laptop's host config in **this** (public) repo, next to
the other one-liners:

```nix
myconfig.wireguard.wg0.roaming = true;
```

The matching `enable = true; privateKeySource = ...` lives in the
private repo. Both halves merge into the same submodule via the NixOS
module system.

---

## Migrating from `metadatalib.setupAsWireguardClient`

Earlier the helper was called from a per-host module, typically in the
private repo, like this:

```nix
# OLD — function-style helper
{ myconfig, ... }:
{
  imports = [
    (myconfig.metadatalib.setupAsWireguardClient
       "wg0"
       ../../secrets/wireguard.<host>.age)
  ];
}
```

The helper has been removed. Replace each call site with the option
assignment:

```nix
# NEW — option-based module
{ ... }:
{
  myconfig.wireguard.wg0 = {
    enable           = true;
    privateKeySource = ../../secrets/wireguard.<host>.age;
  };
}
```

Search-and-replace recipe for the private repo:

```bash
rg -l 'metadatalib\.setupAsWireguardClient'
# inspect each hit; rewrite the imports = [ ... ] line(s) into the
# myconfig.wireguard.<wg> = { enable; privateKeySource; } block above.
```

If a host needs `roaming = true`, prefer setting that in the public
repo (next to the other host-shape config) so it's visible without the
private repo. The private and public assignments merge automatically:
the private repo provides `enable + privateKeySource`, the public repo
provides `roaming`, and the host ends up with all three.

The new option also exposes `listenPort`, `mtu`, `dnsServer`, and
`privateKeyFile` if you need to override defaults — see the option
docstrings in `default.nix` for details.
