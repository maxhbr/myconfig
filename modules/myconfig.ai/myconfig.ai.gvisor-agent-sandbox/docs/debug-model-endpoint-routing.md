# Debug: the model endpoint is unreachable from a runsc sandbox

## Symptom

`agent-session doctor` on `f13` reports the sandbox itself works, but the
model endpoint is not reachable from inside it:

```
model endpoint:  http://192.168.84.1:4000/v1
agent-session: running a throwaway sandbox container
Linux 4.4.0 x86_64 GNU/Linux
uid=1000(mhuber) gid=1000(mhuber) groups=1000(mhuber)
agent-session: sandbox works
agent-session: checking model endpoint http://192.168.84.1:4000/v1 from inside a sandbox
curl: (7) Failed to connect to 192.168.84.1:4000 after 0 ms: Could not connect to server
HTTP 000
agent-session: warning: model endpoint http://192.168.84.1:4000/v1 is NOT reachable from a sandbox.
…
On NixOS check the bridge endpoint:
  ip addr show agentsbr0
  systemctl status gvisor-agent-sandbox-litellm-proxy.socket
```

The doctor's own hint (check `agentsbr0` / the socket) is a **red herring**:
the host-side chain is healthy. The break is on the sandbox side.

## Root cause

The endpoint was exposed through a **member-less bridge** (`agentsbr0`,
`192.168.84.1/24`) plus a socket-activated `systemd-socket-proxyd` bound to
`192.168.84.1:4000`, modelled on the microVM tier's bridge. That construction
is sound for microVMs — they are L2 members of their bridge — but **not** for a
rootless Podman + runsc sandbox, which reaches the host through **pasta**, not
through the bridge. Two independent barriers make the bridge address
unreachable from such a sandbox:

1. **pasta binds its outbound sockets to the host's default-route interface.**
   `pasta(1)` `--outbound-if4` defaults to "the interface given by the default
   route". When pasta re-opens the sandbox's connection to `192.168.84.1` in
   the host network namespace, that socket is bound (`SO_BINDTODEVICE`) to the
   default-route interface (the LAN NIC `wlan0`, or the WireGuard `wg0` when a
   VPN is up). A connection to the host-local `192.168.84.1` then **egresses
   that interface** toward its gateway instead of being locally delivered to
   the host. It never reaches the bridge listener. (`--no-copy-addrs` copies
   addresses only from that one default-route interface, so `192.168.84.1` is
   *not* a local address of the sandbox — this is not a connect-to-self.)

2. **The listening socket had `BindToDevice = agentsbr0`.** Even if a connection
   were locally delivered, `SO_BINDTODEVICE` on the listener rejects any SYN not
   arriving on `agentsbr0`. The sandbox's connection arrives via pasta (the
   default-route interface / loopback), not via `agentsbr0`.

The exact failure mode depends on which interface is the default route: egress
to the LAN (`wlan0`) hangs until `curl`'s timeout (28); egress to the VPN
(`wg0`) returns an immediate `ECONNREFUSED` (7) after 0 ms — the latter is the
observed symptom, so the doctor was most likely run while WireGuard was the
default route. Either way the endpoint is unreachable.

## Evidence

All commands were run from a `myconfig.ai.jail` bubblewrap jail on `f13`, which
**shares the host network namespace** (the loopback and all interfaces are the
host's), so it can probe the exact host-side path the sandbox's pasta-reopened
connection would take. (`podman`/`runsc`/`agent-session` are not available
inside the jail, so the sandbox itself could not be driven from here — see
*Verification*.)

The host-side chain is up — the bridge carries the address and the socket
forwards to the loopback LiteLLM proxy:

```
$ curl -sS -o /dev/null -w 'HTTP %{http_code}\n' http://192.168.84.1:4000/v1/models
HTTP 200
$ curl -sS -o /dev/null -w 'HTTP %{http_code}\n' http://127.0.0.1:4000/v1/models
HTTP 200
$ grep '192.168.84' /proc/net/fib_trie   # 192.168.84.1 is a host-LOCAL address
…
           +-- 192.168.84.0/24 2 0 2
              +-- 192.168.84.0/31 1 0 0
                 |-- 192.168.84.0
                 |-- 192.168.84.1
                    /32 host LOCAL
$ cat /proc/net/route   # default route is wlan0; agentsbr0=192.168.84/24, agentbr0=192.168.83/24, wg0=10.199.199/24
Iface Destination Gateway …
wlan0 00000000      0101A8C0 …   # default via 192.168.1.1
wg0   00C7C70A      00000000 …   # 10.199.199.0/24
agentbr0 0053A8C0  00000000 …   # 192.168.83.0/24 (microVM bridge)
agentsbr0 0054A8C0 00000000 …   # 192.168.84.0/24 (gvisor bridge)
```

Forcing the egress interface reproduces the bug and isolates `BindToDevice` —
only connections arriving on `agentsbr0` reach the listener:

```
$ for i in '' agentsbr0 wlan0 lo wg0 agentbr0; do
    curl -sS -o /dev/null -w "$i -> HTTP %{http_code} (%{time_total}s)\n" \
      ${i:+--interface $i} --connect-timeout 5 http://192.168.84.1:4000/v1/models
  done
 -> HTTP 200 (0.009s)              # default route: local delivery via agentsbr0
agentsbr0 -> HTTP 200 (0.008s)
wlan0 -> HTTP 000 (5.002s)         # curl (28) timeout  — egress LAN, no listener
lo -> HTTP 000 (5.003s)            # curl (28) timeout  — BindToDevice rejects lo
wg0 -> HTTP 000 (0.000s)           # curl (7) after 0 ms — the sandbox symptom
agentbr0 -> HTTP 000 (3.068s)      # curl (7) — wrong bridge
```

`--interface wlan0` forces `SO_BINDTODEVICE=wlan0` on the *outgoing* socket,
which is exactly what pasta's `--outbound-if4` default does. Forcing it against
the loopback proxy shows a bound socket egresses the bound interface even for a
local destination (no local delivery):

```
$ curl --interface wlan0 -sS -o /dev/null -w 'HTTP %{http_code}\n' http://127.0.0.1:4000/v1/models
HTTP 000   # timeout — a wlan0-bound socket to 127.0.0.1 egresses wlan0, not lo
```

pasta(1) confirms the relevant defaults:

```
--map-host-loopback addr
    Translate addr to refer to the host. Packets from the guest to addr will
    be redirected to the host. On the host such packets will appear to have
    both source and destination of 127.0.0.1 or ::1.
    … if the option is specified multiple times, the last one takes effect.
    Default is to translate the guest's default gateway address, unless
    --no-map-gw is given, in which case no address is mapped.

--no-copy-addrs (DEPRECATED)
    Default is to copy all the addresses, except for link-local ones, from
    the interface [with the default route] from the outer namespace to the
    target namespace.   # i.e. only the default-route interface's addresses

--outbound-if4 name
    Bind IPv4 outbound sockets to host interface name …
    By default, the interface given by the default route is selected.
```

Podman's pasta argument builder (`vendor/.../libnetwork/pasta/pasta_linux.go`,
`createPastaArgs`) appends `--config-net`, the user's `pasta_options`/`--network
pasta:` options, then `-t none -u none -T none -U none` and **`--no-map-gw`**
— unless the pseudo-option `--map-gw` is present, in which case `--no-map-gw` is
suppressed (and `--map-gw` itself is removed before pasta sees it). The
podman-run(1) man page documents the syntax and the `--map-gw` escape hatch:

```
pasta[:OPTIONS,…]    options are comma-separated pasta(1) options
pasta:--map-gw       Allow the container to directly reach the host using the
                    gateway address.
```

## The fix (v2: port-scoped)

The bridge fix (v1) used pasta's `--map-host-loopback`, which worked for
reachability but has an **isolation flaw**: `--map-host-loopback` is
*address-scoped, not port-scoped* — ALL ports on the mapped address translate
to `127.0.0.1`, so a hostile agent could reach ANY service on the host's
loopback, not just LiteLLM. (pasta's `nat_outbound()` translates the address
only; the port is always preserved: `tgt->eport = ini->oport`. There is no
port-scoped variant of `--map-host-loopback`.)

The current fix (v2) closes that loophole by NOT mapping to the loopback at
all. Instead it uses `--map-guest-addr` + a **port-scoped forwarder**:

1.  A socket-activated `systemd-socket-proxyd` listens on `0.0.0.0:${forwardPort}`
    (default `14000`) and forwards to `127.0.0.1:${port}` (the loopback-only
    LiteLLM proxy). The forward port differs from the LiteLLM port so the
    `0.0.0.0` wildcard bind does not collide with LiteLLM's own listener. The
    NixOS firewall trusts `lo` and drops `${forwardPort}` on every other
    interface (it is not in `allowedTCPPorts`), so the forwarder is reachable
    only from the host and from sandboxes, never from external hosts.
2.  The `agent-session` wrapper bakes `AGENT_SANDBOX_NETWORK =
    "pasta:--map-guest-addr,${address}"`. `--map-guest-addr` translates
    `<address>` to the *guest's assigned address on the host* — by default the
    host's global address (the address on the default-route interface). That
    address IS on the `SO_BINDTODEVICE` interface, so the connection IS
    locally delivered (unlike the old bridge, which was on a different
    interface). The port is kept unchanged.
3.  The sandbox connects to `<address>:${forwardPort}`, which pasta translates
    to `<host-global-addr>:${forwardPort}`, where the forwarder accepts and
    proxies to `127.0.0.1:${port}` (LiteLLM).

**Why this is port-scoped.** The sandbox can reach `<address>:${forwardPort}`
(the forwarder → LiteLLM) and other ports on the host's global address — but
only services that already bind to `0.0.0.0` (i.e., are already
network-accessible). Loopback-ONLY services (bound to `127.0.0.1`) are **not**
reachable, because `--map-guest-addr` maps to the host's global address, not
to `127.0.0.1`. This is the key isolation improvement over `--map-host-loopback`.

**Why `--map-gw` was dropped.** The old spec was
`pasta:--map-gw,--map-host-loopback,${address}`. `--map-gw` is a podman-only
flag (NOT a real pasta option) that suppresses podman's default `--no-map-gw`.
It was believed necessary so that `--map-host-loopback` would take effect, but
the pasta source (`conf.c`, `conf_nat()` + finalization at line ~1881) shows
this is unnecessary: `--map-host-loopback`/`--map-guest-addr` set
`map_host_loopback`/`map_guest_addr` directly, and `--no-map-gw` only fills in
the *default* (gateway) mapping when `map_host_loopback` is still unspecified.
An explicit mapping is never overridden by `--no-map-gw`. Dropping `--map-gw`
changes nothing and removes one address→loopback exposure path.

Concretely (in this repo):

- `litellm-endpoint.nix`: adds the `forwardPort` option, the socket-activated
  `systemd-socket-proxyd` forwarder (`0.0.0.0:${forwardPort}` →
  `127.0.0.1:${port}`), the `forwardPort != port` assertion, and the
  `litellm.endpoint` (now `http://${address}:${forwardPort}/v1`). The old
  `--map-host-loopback` target address and port options are kept (the address is
  now the `--map-guest-addr` target).
- `default.nix` bakes `AGENT_SANDBOX_NETWORK =
  "pasta:--map-guest-addr,${cfg.litellm.address}"` into the `agent-session`
  wrapper, and updates `litellmBase` / `rewriteEndpoints` to use `forwardPort`.
- `bin/agent-session` `doctor` message updated to describe the new mechanism.

## Verification

Done from the jail (the host netns is shared):

- Config snapshot, before → after: the `agentsbr0` bridge, the
  `gvisor-agent-sandbox-litellm-proxy` socket/service, the `agentsbr0` firewall
  rule and the IPv6-disable service are all gone; `litellm.{address,port,
  endpoint}` and the `agent-sandbox/litellm.env` file are byte-identical.
- `nix flake check --no-build` passes (only a pre-existing `workmux`
  `meta.mainProgram` warning).
- `./build-pkg-for-host.sh agent-session-configured f13` builds the wrapper;
  the binary bakes in `AGENT_SANDBOX_NETWORK =
  pasta:--map-gw,--map-host-loopback,192.168.84.1` (and the existing
  `AGENT_SANDBOX_MODEL_ENDPOINT`), and execs the unwrapped script in which the
  `AGENT_SANDBOX_NETWORK` default and the `doctor` `net_args` are present.
- `bash -n` and `shellcheck -x` pass on `bin/agent-session`; `nixfmtall.sh`
  makes no further changes.

The end-to-end check — `agent-session doctor` reporting `HTTP 200` from inside a
sandbox — needs `podman`/`runsc`/the loaded image, none of which exist inside
the bubblewrap jail (same constraint as `debug-runsc-tun0-netns.md`). On `f13`:

```bash
agent-session doctor
# expect:
#   agent-session: checking model endpoint http://192.168.84.1:14000/v1 from inside a sandbox
#   HTTP 200
#   agent-session: model endpoint reachable from the sandbox
agent-session shell <name> -c 'curl -sS -o /dev/null -w "HTTP %{http_code}\n" "$OPENAI_BASE_URL/models"'
```

If it still fails, check (a) the host proxy is up on the loopback
(`curl 127.0.0.1:4000/v1/models` → 200), (b) the forwarder socket is active
(`systemctl status agent-litellm-forward.socket` and `curl 0.0.0.0:14000/v1/models`
→ 200 from the host), and (c) the wrapper actually set the network
(`agent-session doctor`'s model probe runs `podman … --network
pasta:--map-guest-addr,192.168.84.1 …`; `AGENT_SANDBOX_NETWORK='pasta:--map-guest-addr,192.168.84.1,--debug'`
shows the options reached pasta).

## Upstream patch (gvisor-agent-sandbox)

The repo-specific NixOS wiring (`litellm-endpoint.nix`, the `sessionEnv` in
`default.nix`) does not apply upstream. The upstream-able part is the
`bin/agent-session` change — a default `--network` from `AGENT_SANDBOX_NETWORK`,
and `doctor` probing through it:

```diff
diff --git a/bin/agent-session b/bin/agent-session
@@ start options:
-  --network MODE              Podman network mode; omit to use the rootless default
+  --network MODE              Podman network mode; omit for the rootless
+                              default ($AGENT_SANDBOX_NETWORK if set)
@@ environment:
   AGENT_SANDBOX_MODEL_ENDPOINT  Model API base URL as seen from a sandbox;
                                 `doctor` probes it from inside one
+  AGENT_SANDBOX_NETWORK         Default --network mode for sessions and for the
+                                `doctor` endpoint probe, e.g. a "pasta:..." spec
+                                mapping the model endpoint to the host loopback
@@ cmd_start() {
-  local detach=false memory=8g cpus=4 pids_limit=2048 network=''
+  local detach=false memory=8g cpus=4 pids_limit=2048 network="${AGENT_SANDBOX_NETWORK-}"
@@ cmd_doctor() …
   if [[ -n "${AGENT_SANDBOX_MODEL_ENDPOINT-}" ]]; then
+    # Probe the endpoint from a sandbox using the SAME network sessions use, so
+    # the check exercises the real path (e.g. pasta --map-host-loopback) rather
+    # than podman's rootless default, which cannot reach a host-only endpoint.
+    local -a net_args=()
+    [[ -z "${AGENT_SANDBOX_NETWORK-}" ]] || net_args=(--network "$AGENT_SANDBOX_NETWORK")
     log "checking model endpoint $AGENT_SANDBOX_MODEL_ENDPOINT from inside a sandbox"
     if podman_c run --rm \
       …
       --userns=keep-id \
+      "${net_args[@]}" \
       "$DEFAULT_IMAGE" \
       /bin/sh -c "curl … \"$AGENT_SANDBOX_MODEL_ENDPOINT\""; then
       log "model endpoint reachable from the sandbox"
     else
       log "$(
         printf 'warning: model endpoint %s is NOT reachable from a sandbox.\n' \
           "$AGENT_SANDBOX_MODEL_ENDPOINT"
-        printf 'The endpoint must be a HOST address the sandbox can route to;\n'
-        printf 'the host loopback is not one, and neither is the sandbox loopback:\n'
-        printf '  - runsc runs its own network stack, so a pasta "-T" listener in\n'
-        printf '    the container netns is invisible to the sandbox loopback;\n'
-        printf '  - host.containers.internal maps to the host GLOBAL address\n'
-        printf '    (pasta --map-guest-addr), not to the host loopback.\n'
-        printf 'On NixOS check the bridge endpoint:\n'
-        printf '  ip addr show agentsbr0\n'
-        printf '  systemctl status gvisor-agent-sandbox-litellm-proxy.socket'
+        printf 'runsc runs its own network stack: neither the host loopback nor\n'
+        printf 'a pasta "-T" listener in the container netns is visible to it, and\n'
+        printf 'host.containers.internal maps to the host global address, not the\n'
+        printf 'loopback. The supported path is pasta --map-host-loopback, which\n'
+        printf 'translates a chosen address to the host loopback; set it via the\n'
+        printf 'AGENT_SANDBOX_NETWORK env (a "pasta:..." podman network spec, e.g.\n'
+        printf 'pasta:--map-gw,--map-host-loopback,<endpoint-host>). Also confirm\n'
+        printf 'the host litellm proxy is up on 127.0.0.1:<port>.'
       )"
     fi
   fi
```

Applied here as a local patch to the vendored `bin/agent-session` (already
patched for `start --force` / home-seeding — see `README.md`); once the
`gvisor-agent-sandbox` vendoring branch lands, this can go upstream verbatim.
