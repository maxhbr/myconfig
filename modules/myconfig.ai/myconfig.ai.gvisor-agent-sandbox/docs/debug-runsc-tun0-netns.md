# Debug: `runsc start` fails with `EADDRNOTAVAIL` on a `tun0` address

## Symptom

On a host running OpenVPN (or WireGuard — anything creating a point-to-point
`tun` device), every `agent-gvisor start` aborts before the container runs:

```
❱ agent-gvisor start --name local-litellm-dashboard --repo (pwd)
…
agent-gvisor: warning: memory/cpu/pids limits not enforced, the runtime ignores cgroups
starting container: setting up network: creating interfaces from net namespace "/proc/3928114/ns/net": removing link addresses for interface "tun0": removing address 10.16.23.62/32 from device "tun0": cannot assign requested address
Error: `/nix/store/…-gvisor-20260406.0/bin/runsc --ignore-cgroups start ad39063ce817…` failed: exit status 128
```

The container is never created; the session state is left behind and must be
cleaned up with `agent-gvisor destroy <name> --force`.

## Root cause

Two independent, individually-reasonable behaviours combine into a crash.

### 1. pasta names its tap after the host's default-route interface, and clones its addresses

Podman's rootless default network backend is pasta
(`DefaultRootlessNetworkCmd: "pasta"`, `go.podman.io/common/pkg/config/default.go`).
pasta creates a **fresh, correctly isolated** network namespace for the
container — but by default it makes that namespace *look like* the host's
egress path. From `pasta(1)`:

```
-I, --ns-ifname name
    Name of tap interface to be created in target namespace.
    By default, the same interface name as the external, routable interface is used.

--no-copy-addrs (DEPRECATED)
    Default is to copy all the addresses, except for link-local ones, from the
    interface from the outer namespace to the target namespace.
```

With an OpenVPN full-tunnel the host's default-route interface **is** `tun0`.
So pasta creates a tap **named `tun0`** inside the sandbox netns and copies
`tun0`'s addresses onto it — **including the point-to-point pair**
(`IFA_LOCAL 10.16.23.62`, `IFA_ADDRESS 10.16.23.61`).

> This is why the sandbox netns contains something called `tun0`. It is *not*
> the host's `tun0`, and netns isolation is **not** broken — see
> [Why this is not an isolation failure](#why-this-is-not-an-isolation-failure).

### 2. runsc deletes those addresses without their peer

runsc's default `--network=sandbox` mode moves the netns's addresses into
gVisor's own userspace netstack. In `runsc/sandbox/network.go` it:

1. collects addresses with `net.Interfaces().Addrs()` — which reports **only
   `IFA_LOCAL`**, silently dropping the peer;
2. deletes them with `netlink.ParseAddr(ipNet.String())` → `netlink.AddrDel`.

The reconstructed `netlink.Addr` therefore has `Peer == nil`. In
`vishvananda/netlink`'s `addrHandle`, `Peer == nil` means `IFA_ADDRESS` is set
equal to `IFA_LOCAL`:

```go
// addr_linux.go
var peerAddrData []byte
if addr.Peer != nil {
    peerAddrData = addr.Peer.IP.To4()
} else {
    peerAddrData = localAddrData        // ← IFA_ADDRESS := IFA_LOCAL
}
req.AddData(nl.NewRtAttr(unix.IFA_ADDRESS, peerAddrData))
```

The kernel matches `RTM_DELADDR` on the `(IFA_LOCAL, IFA_ADDRESS)` pair. It
holds `(10.16.23.62, 10.16.23.61)`; runsc sends `(10.16.23.62, 10.16.23.62)`.
Nothing matches → **`EADDRNOTAVAIL`** → `runsc start` exits 128.

Note the same file's `AddrList` *does* populate `Peer` correctly, which is the
basis of the fix:

```go
// addr_linux.go, parseAddr()
} else {
    addr.IPNet = local
    addr.Peer = dst          // ← peer preserved when IFA_ADDRESS != IFA_LOCAL
}
```

## Why this is not an isolation failure

It is tempting to conclude that the sandbox is running in the **host** netns
(how else would it see `tun0`?). It is not, and gVisor's own control flow
proves it. In `createInterfacesAndRoutesFromNS`
(`runsc/sandbox/network.go:318-334`):

```go
restore, err := joinNetNS(nsPath)          // join the sandbox netns
…
isRoot, err := isRootNetNS()
if isRoot {
    return fmt.Errorf("cannot run with network enabled in root network namespace")
}
args, err := collectLinksAndRoutes(conf, disableIPv6)   // ← reached
removeLinkAddresses(…)                                  // ← error originates here
```

The root-netns guard runs **before** the code that failed. Since the observed
error comes from `removeLinkAddresses`, `isRootNetNS()` must have returned
`false`, i.e. `/proc/sys/net/core/dev_weight` was **not** visible in the joined
namespace. That file *does* exist in this host's root netns:

```
$ ls -la /proc/sys/net/core/dev_weight
-rw-r--r-- 1 … /proc/sys/net/core/dev_weight
$ readlink /proc/self/ns/net
net:[4026531833]        # the initial/host netns
```

Therefore the joined namespace was **not** the host netns. The isolation was
working; only the *contents* pasta put in it were surprising.

This distinction matters, because the two diagnoses lead to opposite fixes: a
broken-isolation diagnosis invites `--network=host` (which would hand the
untrusted workload the host kernel's network stack), whereas the real cause is
a narrow address-deletion bug that can simply be fixed.

## Evidence

The host interface pasta cloned:

```
$ ip -d addr show tun0
18: tun0: <POINTOPOINT,MULTICAST,NOARP,UP,LOWER_UP> mtu 1500 qdisc fq_codel state UNKNOWN group default qlen 500
    link/none  promiscuity 0 allmulti 0 minmtu 68 maxmtu 65535
    tun type tun pi off vnet_hdr off persist off numtxqueues 1 numrxqueues 1 […]
    inet 10.16.23.62 peer 10.16.23.61/32 scope global noprefixroute tun0
       valid_lft forever preferred_lft forever
    inet6 fe80::e6ea:814d:cb6c:eea1/64 scope link stable-privacy proto kernel_ll
       valid_lft forever preferred_lft forever
```

(`[…]` marks elided attributes.) `POINTOPOINT` plus `peer` is exactly the
shape that breaks the deletion.

The runsc invocation carries no `--network`, so it defaults to `sandbox`:

```
runsc --ignore-cgroups start <container-id>
```

```
$ runsc flags | grep -A1 'network value'
  -network value
        specifies which network to use: sandbox (default), host, none. […]
```

## The fix

`nix/patches/gvisor-remove-p2p-addresses.patch`, applied to `pkgs.gvisor` from
`nix/overlay.nix`.

`removeLinkAddresses` now keeps the netlink view of the link
(`netlink.AddrList`, which populates `Addr.Peer`) and deletes the kernel's own
entry verbatim, so `IFA_ADDRESS` carries the peer. Addresses not found in that
list fall back to the previous behaviour, so nothing else changes.

This keeps runsc's default `--network=sandbox` — i.e. the sandbox keeps
**gVisor's userspace netstack**, which is a large part of gVisor's value: the
untrusted workload never issues socket syscalls against the host kernel's
TCP/IP stack.

### Why not `--network=host`

`--network=host` also avoids the crash — `setupNetwork` does nothing at all in
that mode — but it is the wrong trade:

- It replaces netstack with `hostinet` (`runsc/boot/loader.go`), so the
  untrusted agent's sockets are serviced directly by the **host kernel's**
  network stack.
- It *expands* the sentry's own seccomp allowlist by ~17 syscalls
  (`socket`, `bind`, `connect`, `sendmsg`, `setsockopt`, `ioctl(SIOC*)`, …),
  several matched unconditionally.
- It lets the workload `bind()`/`listen()` in that netns and enumerate the
  namespace's real interfaces via `/proc/net/*` and `SIOCGIFCONF`.
- runsc's own flag help says it plainly: *"Using network inside the sandbox is
  more secure because it's isolated from the host network."*

It also would have invalidated the reasoning in `README.md` and
`litellm-bridge.nix`, which is written on the premise that runsc runs its own
network stack.

### Why not `--network=none`

`setupNetwork`'s `NetworkNone` branch builds only `DefaultLoopbackLink`, so the
sandbox would have **no egress at all** — it could reach neither the LiteLLM
bridge nor the internet. Not viable for a coding agent.

### Why not reconfigure pasta

Giving pasta an explicit, non-p2p address (`--network 'pasta:-I,eth0,-a,…'`) or
switching to `slirp4netns` would also dodge the bad address, and both are
legitimate. They were not chosen because they fix the symptom for *this*
topology while leaving runsc unable to handle p2p addresses in general, and
because hard-coding a sandbox subnet risks colliding with host networks
(`agentsbr0` 192.168.84.0/24, the microVM subnet, the VPN itself). The patch is
topology-independent.

### There is no flag that keeps netstack but skips the stripping

Worth recording so it is not re-litigated: the only code paths in
`createInterfacesAndRoutesFromNS` that bypass `removeLinkAddresses` are the XDP
modes (`XDPModeRedirect`/`XDPModeTunnel`, which need root and eBPF) and
`--network=plugin` (a third-party stack). Hence patching.

## Escape hatch

If a host ever needs to bypass the netstack anyway, the upstream script already
supports it at runtime — no NixOS option is involved:

```bash
AGENT_GVISOR_PODMAN_RUNTIME_FLAGS='ignore-cgroups network=host' agent-gvisor start …
```

Be aware this carries every downside listed under
[Why not `--network=host`](#why-not---networkhost).

## Verification

```
$ nix build …#nixosConfigurations.test-f13.pkgs.gvisor     # patch applies, gVisor + tests build
$ grep -c 'listing addresses for interface' <patched>/bin/.runsc-wrapped
1
$ grep -c 'listing addresses for interface' <unpatched>/bin/.runsc-wrapped
0
```

End-to-end (`agent-gvisor start` on the affected host) still needs to be run
on `f13` itself; it could not be exercised from inside an agent sandbox, which
has neither the container image nor a rootless Podman store.

## Upstream

The patch is a candidate for gVisor upstream as-is. The one-line framing:
`collectLinksAndRoutes` gathers addresses through `net.Interfaces().Addrs()`,
which cannot represent a peer address, so the deletion path must consult
`netlink.AddrList` instead of reconstructing the address from a string.

A second, separate upstream concern found while diagnosing: `isRootNetNS()`
detects the root netns by probing `/proc/sys/net/core/dev_weight`, with the
comment *"only exists in root network namespace"*. That assumption is fragile
on modern kernels; comparing the netns inode against `/proc/1/ns/net` would be
more robust. Not needed for this bug — the guard behaved correctly here — but
worth reporting.
