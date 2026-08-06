# Debug: `runsc start` fails on OpenVPN `tun0` (`EADDRNOTAVAIL`)

## Symptom

On host `f13` (which runs OpenVPN, creating a `tun0` point-to-point
interface), every `agent-session start` aborts before the container is
started:

```
❱ agent-session start --name local-litellm-dashboard --repo (pwd)
…
agent-session: warning: memory/cpu/pids limits not enforced, the runtime ignores cgroups
starting container: setting up network: creating interfaces from net namespace "/proc/3928114/ns/net": removing link addresses for interface "tun0": removing address 10.16.23.62/32 from device "tun0": cannot assign requested address
Error: `/nix/store/3mg1xryg6rxblw25spn7px630jw0gimw-gvisor-20260406.0/bin/runsc --ignore-cgroups start ad39063ce817a9f4c506a4d5a10376175da9427a38f0845bece9192665814887` failed: exit status 128
```

The container is never created; the session state is left behind and must
be cleaned up with `agent-session destroy <name> --force`.

## Root cause

Two things conspire:

### 1. runsc `--network=sandbox` (the default) scrapes and strips host interfaces

gVisor's default network mode is `--network=sandbox`.  In that mode
`runsc/sandbox/network.go` (`setupNetwork` → `createInterfacesAndRoutesFromNS`)
does the following during `runsc start`:

1. Joins the sandbox process's own network namespace
   (`/proc/<sandbox-pid>/ns/net`).
2. Guards against the *root* netns with `isRootNetNS()`, which checks
   whether `/proc/sys/net/core/dev_weight` exists (the gVisor comment
   claims it "only exists in root network namespace").
3. Enumerates **every** UP, non-loopback interface in that namespace
   (`collectLinksAndRoutes`), collecting their addresses.
4. For each interface, calls `removeLinkAddresses` → `netlink.AddrDel`
   (`ip addr del <ip/prefix> dev <iface>`) to **strip the address off the
   host NIC** and move it into gVisor's own userspace netstack.

Step 4 is the immediate failure: `tun0` carries a **point-to-point**
address — `10.16.23.62 peer 10.16.23.61/32` — but `collectLinksAndRoutes`
reads it via `net.Interfaces().Addrs()`, which returns a plain
`10.16.23.62/32` `*net.IPNet` **without the peer address**.
`netlink.AddrDel` then sends a netlink `RTM_DELADDR` carrying only the
local `/32`; the kernel cannot match it against the real p2p entry and
returns `EADDRNOTAVAIL` ("cannot assign requested address").  The
fallback check `isAddressOnInterface` confirms the address is still
present, so the error propagates and `runsc start` exits 128.

Relevant gVisor source (pinned `gvisor-20260406.0`,
`/nix/store/jihwgldwd5n7kcs18a4lvc11x4vwm5vr-source`):

```go
// runsc/sandbox/network.go

func removeLinkAddresses(linkName string, addresses []boot.IPWithPrefix) error {
    ifaceLink, err := netlink.LinkByName(linkName)
    …
    for _, addr := range addresses {
        ipNet := &net.IPNet{
            IP:   addr.Address,
            Mask: net.CIDRMask(addr.PrefixLen, addrBitLength(addr.Address)),
        }
        if err := removeAddress(ifaceLink, ipNet.String()); err != nil {
            // … re-check, then:
            return fmt.Errorf("removing address %v from device %q: %w", ipNet, linkName, err)
        }
    }
    return nil
}

func removeAddress(source netlink.Link, ipAndMask string) error {
    addr, err := netlink.ParseAddr(ipAndMask)
    if err != nil { return err }
    return netlink.AddrDel(source, addr)   // ← fails EADDRNOTAVAIL for p2p /32
}
```

### 2. The sandbox process lands in a namespace that contains `tun0`

In a *correctly* configured rootless setup, Podman + pasta would create a
fresh, isolated network namespace for the container (containing only `lo`
+ a pasta tap device), and runsc would scrape just those — never `tun0`.

On `f13` that isolation is not happening: the error proves the sandbox
process's netns **does** contain `tun0` (a host interface).  The
`isRootNetNS()` guard that should catch this case is **unreliable inside
a user namespace**: rootless Podman always runs the container in a user
namespace, and in that context `unix.Access("/proc/sys/net/core/dev_weight",
F_OK)` can return `ENOENT` even when the netns *is* the host's, so the
guard returns `false` and runsc proceeds to scrape `tun0`.

> **Note:** `/proc/sys/net/core/dev_weight` *does* exist in the host root
> netns on this kernel (7.1.6, `CONFIG_NET_NS=y`), as confirmed with
> `ls /proc/sys/net/core/dev_weight`.  The gVisor assumption that it only
> exists in the root netns is outdated for modern kernels, and the
> user-namespace interaction breaks the check from the other direction.

Whether the sandbox is in the literal host netns or a namespace that
somehow inherited `tun0` could not be conclusively determined from within
the agent sandbox (no access to the host's podman state or `agent-session
doctor` output).  Either way, the *observable* effect is: runsc sees
`tun0` and crashes trying to strip its p2p address.

## Evidence

### The failing interface

```
$ ip -d addr show tun0
18: tun0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1500 …
    tun type tun pi off vnet_hdr off persist off …
    inet 10.16.23.62 peer 10.23.61/32 scope global noprefixroute tun0
       valid_lft forever preferred_lft forever
    inet6 fe80::e6ea:814d:cb6c:eea1/64 scope link stable-privacy …
```

`POINTOPOINT` + `peer` confirms it is a point-to-point tunnel (OpenVPN).
The user confirmed: *"It seems to conflict with my openvpn."*

### The runsc invocation

From the error, the exact command is:

```
/nix/store/3mg1xryg6rxblw25spn7px630jw0gimw-gvisor-20260406.0/bin/runsc \
    --ignore-cgroups start <container-id>
```

No `--network` flag → runsc defaults to `--network=sandbox` → the
scrape-and-strip path described above.

### runsc flags confirm the modes

```
$ runsc flags | grep -A1 '\-network value'
    -network value
        specifies which network to use: sandbox (default), host, none.
```

- `sandbox` (default): scrape interfaces, strip addresses, build gVisor
  netstack — **fails on `tun0`**.
- `host`: *"Nothing to do here"* — forward network syscalls to the kernel,
  no scraping.
- `none`: loopback only, no external network.

### Podman rootless default is pasta (not host)

```
$ grep -n 'DefaultRootlessNetworkCmd' \
    …/vendor/go.podman.io/common/pkg/config/default.go
278:    DefaultRootlessNetworkCmd: "pasta",
```

So Podman *should* create an isolated pasta netns.  The `pasta` binary is
present in the podman helper dir (`…/libexec/podman/pasta` → passt).
The fact that `tun0` is still visible means the isolation is not taking
effect for the runsc sandbox process — likely a Podman/runsc interaction
around `PostConfigureNetNS` (rootless containers set an empty-path
NetworkNamespace in the OCI spec, so the *runtime* creates the netns,
then Podman post-configures it; with gVisor's separate sandbox process
this can race or fail).

## Recommended fix (implemented in this repo)

**Set runsc `--network=host`** so gVisor forwards network syscalls to the
kernel (which uses the netns Podman + pasta already set up) and skips the
scrape/strip/socket step entirely.  This is a one-flag workaround that
works regardless of whether the sandbox is in the host netns or a pasta
netns.

### Changes made

1. **`bin/agent-session`** — the rootless default for
   `AGENT_PODMAN_RUNTIME_FLAGS` changed from `ignore-cgroups` to
   `ignore-cgroups network=host`, with an explanatory comment.  The
   `doctor` error hints and `usage()` text were updated to mention the
   `tun0` failure mode and the `network=host` default.

2. **`default.nix`** — added a `runscNetwork` option
   (`types.enum ["sandbox" "host" "none"]`, default `"host"`) wired into
   `sessionEnv` as `AGENT_PODMAN_RUNTIME_FLAGS = "ignore-cgroups
   network=${cfg.runscNetwork}"` (via `--set-default`, so still
   overridable per-invocation).

### Verification

The built `agent-session-configured` wrapper for `test-f13` correctly
bakes in the flag:

```
makeCWrapper '…/agent-session' \
    --set-default 'AGENT_PODMAN_RUNTIME_FLAGS' 'ignore-cgroups network=host' \
    …

setenv("AGENT_PODMAN_RUNTIME_FLAGS", "ignore-cgroups network=host", 0)
```

So `agent-session start` now runs `runsc --ignore-cgroups --network=host
start <id>`, which hits `case config.NetworkHost: // Nothing to do here`
and never touches `tun0`.

> **Caveat:** `--network=host` means gVisor does **not** run its own
> netstack; the kernel handles the container's networking directly.  This
> is slightly less isolated than `--network=sandbox` would be (if it
> worked), but the sandbox still has gVisor's syscall filtering and
> filesystem isolation, and the network path is the same one the
> `litellm-bridge.nix` design already assumes (pasta re-opens connections
> in the host namespace).

### Trade-off: why not `--network=none`?

`--network=none` also avoids the scrape but gives the container **no
external network at all** (loopback only) — the agent could not reach the
LiteLLM bridge or the internet.  Not viable.

## Upstream patch (for gVisor / the vendoring branch)

The deeper fix belongs in gVisor's `removeLinkAddresses`: it must include
the **peer address** when deleting a point-to-point address, so that
`netlink.AddrDel` matches the kernel's `IFA_LOCAL`/`IFA_ADDRESS` pair.

Conceptual diff against `runsc/sandbox/network.go`:

```go
// collectLinksAndRoutes must also capture the peer address for p2p
// interfaces (tun, wireguard, etc.) and pass it through to
// removeLinkAddresses, which must set netlink.Addr.Peer before calling
// AddrDel.

func removeLinkAddresses(linkName string, addresses []boot.IPWithPrefix) error {
    ifaceLink, err := netlink.LinkByName(linkName)
    if err != nil {
        return fmt.Errorf("getting link for interface %q: %w", linkName, err)
    }
    for _, addr := range addresses {
        nlAddr := &netlink.Addr{IPNet: &net.IPNet{
            IP:   addr.Address,
            Mask: net.CIDRMask(addr.PrefixLen, addrBitLength(addr.Address)),
        }}
        if addr.PeerAddress != nil {                 // ← NEW
            nlAddr.Peer = addr.PeerAddress            // ← NEW
        }
        if err := netlink.AddrDel(ifaceLink, nlAddr); err != nil {
            …
        }
    }
    return nil
}
```

This requires threading a `PeerAddress` field through `boot.IPWithPrefix`
and `collectLinksAndRoutes` (using `netlink.AddrList` instead of
`net.Interfaces().Addrs()` to see `IFA_ADDRESS`/`IFA_LOCAL`).  Until that
lands, `--network=host` (this repo's fix) is the reliable workaround.

A second upstream concern is `isRootNetNS()`: the `/proc/sys/net/core/
dev_weight` check is unreliable on modern kernels and inside user
namespaces, so the guard does not prevent runsc from processing the host
netns.  A more robust check (e.g. comparing the netns inode against
`/proc/1/ns/net`, or checking for the `genetlink` family) would be
worth filing upstream.
