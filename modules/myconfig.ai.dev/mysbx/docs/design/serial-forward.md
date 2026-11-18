<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: forwarding serial devices through ser2net

Status: reference guide. No mysbx code change is required — this is a
host-side setup that works with every mysbx backend as a TCP client.

Use this guide when a mysbx sandbox needs to flash, observe or debug
an embedded target that is connected to the host via a serial device
(`/dev/ttyACM*`, `/dev/ttyUSB*`).

## The problem

None of the mysbx VM backends (`podman-krun`, `krun`,
`microvm-qemu`, `microvm-ch`) can forward raw character devices
(`/dev/tty*`) into the guest. The bubblewrap and nono backends *can*
bind a device path directly, but that ties the sandbox to the host
kernel, the host user's privileges, and a single machine.

ser2net solves all three constraints: it exposes a serial port as a
plain TCP stream on `127.0.0.1:<port>`. The sandbox dials that port
over whatever network path its backend provides. No device bind, no
root, no host-kernel dependency inside the sandbox.

## Architecture

```
┌──────────────────────────────────────────────────────┐
│                     HOST                             │
│                                                      │
│  ┌──────────┐    TCP   ┌────────────────┐           │
│  │ sandbox  │◄────────►│  ser2net :20108│           │
│  │ (mysbx)  │  telnet  │                │           │
│  │          │          │  /dev/ttyACM0  │           │
│  └──────────┘          └────────────────┘           │
│                                                      │
│  ┌──────────────────────────────────────────────┐   │
│  │             /dev/ttyACM0 (USB-serial)        │   │
│  └──────────────────────────────────────────────┘   │
└──────────────────────────────────────────────────────┘
```

The serial device lives on the host. ser2net listens on a loopback
port and converts every incoming TCP byte stream into serial-port
bytes and vice versa. The sandbox connects as a plain TCP client —
it does not see the device node, it does not need device permissions,
and it speaks the same protocol it would on a direct `/dev/tty*`
bind.

## Host setup

Upstream NixOS has **no `services.ser2net` module** (as of nixpkgs
26.05). Host `thing` carries a small local module,
`hosts/host.thing/services.ser2net.nix`, that defines the systemd unit
and generates the ser2net 4.x YAML config (ser2net ≥ 4 no longer accepts
the legacy one-line format). It is enabled through:

```nix
myconfig.host.thing.ser2net = {
  enable = true;
  ports = [
    {
      port = 20108;        # TCP port on 127.0.0.1
      device = "/dev/ttyACM0";
      baud = "115200";     # defaults shown
      parity = "N";
      dataBits = "8";
      stopBits = "1";
      rfc2217 = true;      # telnet accepter with RFC 2217 negotiation
    }
  ];
  timeout = 0;            # idle timeout; 0 disables it
};
```

### What the module generates

ser2net 4.x is configured with YAML (`ser2net.yaml(5)`); the generated
file for the entry above is:

```yaml
connection: &con20108
  accepter: telnet(rfc2217),tcp,127.0.0.1,20108
  connector: serialdev,/dev/ttyACM0,115200N81
  timeout: 0
  enable: on
```

- `accepter` — the listening side. `telnet(rfc2217)` lets clients
  renegotiate baud/parity remotely (needed by some flashers);
  set `rfc2217 = false` for a plain `tcp` accepter when the client
  interprets telnet escapes (rare for embedded tooling).
- `connector` — the serial device plus gensio serialoptions
  (`<speed><parity><databits><stopbits>`, see `gensio(5)`).
- serial devices are opened lazily on the first TCP connection, so
  the service starts even when the device is not yet plugged in.

### Multiple targets

Add one entry per serial port; name the ports by hardware:

```nix
myconfig.host.thing.ser2net.ports = [
  { port = 20108; device = "/dev/ttyACM0"; } # nRF52840-DK
  { port = 20109; device = "/dev/ttyACM1"; } # ESP32-C3 devkit
  { port = 20110; device = "/dev/ttyACM2"; } # STM32 Nucleo
];
```

### Systemd service

The module's unit starts ser2net with `-n` (foreground) and `-u`
(disable UUCP locking, which is pointless on modern udev-managed
devices). Verify it is listening:

```bash
ss -tlnp | grep ser2net
# tcp  LISTEN  0  128  127.0.0.1:20108  users:(("ser2net",pid=...,fd=...))
```

### Per-port convenience

The ports are defined per host; see the `ports` list above.

## Connecting from the sandbox

### Telnet

nixpkgs has no standalone `telnet` package — the client comes from
`pkgs.inetutils`. Host `thing` puts `inetutils` and `socat` into the
mysbx `extraTools`, so every sandbox has both on its dev-tool PATH.
The simplest way to observe serial output:

```bash
# Inside mysbx — any backend
telnet 127.0.0.1 20108
```

This drops you into a raw terminal on the serial port. Use it to
observe Zephyr console output, interact with a shell on the target,
or send manual commands.

### socat (for tooling that needs a PTY)

Some tools (minicom, picocom, screen) expect a real TTY device.
socat creates a PTY on the sandbox side and bridges it to TCP:

```bash
# Inside mysbx
socat readline,echo=0 /dev/pts/2,raw,echo=0 127.0.0.1:20108,telnet-option=NA
```

Or use the helper mysbx ships (if available):

```bash
# If socat is in extraTools
socat -d -d PTY,link=/tmp/serial0,raw,echo=0 TCP:127.0.0.1:20108
minicom -D /tmp/serial0
```

### Zephyr / west flash

Zephyr's `west flash` tool calls `nrfjprog`, `openocd`, or
`esptool.py` internally. These tools talk to a **debug probe**
(J-Link, stlink, DAP-Link) — not directly to the serial port.
The serial port is only for console output (logging), not for
flashing.

Flashing itself uses SWD/JTAG over the probe, which is a separate
hardware path. ser2net forwards the **UART console** that the chip
outputs over the same USB-serial chip that `west build` and `west
flash` use for logging:

```bash
# Inside mysbx — observe console output in real time
telnet 127.0.0.1 20108

# In another pane — flash the target (probe is separate hardware)
west flash --runner nrfjprog --board nrf52840dk_nrf52840
```

The flash tool does not touch the serial port at all. ser2net only
gives you the console stream.

### OpenOCD over ser2net (for UART-based debug)

Some targets expose a debug shell over UART (OpenOCD's `uart`
adapter type). This is less common than SWD/JTAG but supported:

```bash
# Inside mysbx — OpenOCD speaks the UART through ser2net
openocd -f interface/remote_bitbang.cfg \
        -c "set BITBANG_URL tcp:127.0.0.1:20108" \
        -f target/nrf52.cfg
```

Note: most Zephyr targets use SWD/JTAG probes, not UART debug.
This section is for the edge case where a target speaks a debug
protocol over its UART lines.

## Network path per backend

Every mysbx backend can reach `127.0.0.1:<port>` on the host, but
the mechanism differs:

| Backend | How the sandbox reaches host loopback |
|---|---|
| `bubblewrap` | Shares the host network namespace — `127.0.0.1` is the host's loopback |
| `nono` | Same network namespace as bubblewrap; nono's policy governs egress |
| `podman-gvisor` | Pasta maps the guest's `127.0.0.1` to the host's global address; ser2net binds `127.0.0.1` so it **is** reachable (pasta forwards to host loopback) |
| `podman-krun` | libkrun TSI dials from the container netns — `127.0.0.1` in the guest is the container's loopback, which pasta maps to host loopback |
| `krun` (direct) | libkrun TSI dials from the launcher's netns, which shares the host namespace — `127.0.0.1` is the host's loopback |
| `microvm-qemu` | SLiRP NAT with gateway `10.0.2.2`; the guest must dial `10.0.2.2:20108` (SLiRP forwards to host loopback) |
| `microvm-ch` | passt over vhost-user, SLiRP-compatible address plan; same `10.0.2.2:20108` address |

For the VM backends (`microvm-*`), the gateway address `10.0.2.2`
replaces `127.0.0.1`. This is a known constant, baked into the
guest's NIC config by QEMU/passt. Adjust the connection target:

```bash
# On bubblewrap / nono / krun
telnet 127.0.0.1 20108

# On microvm-qemu / microvm-ch
telnet 10.0.2.2 20108
```

The `10.0.2.2` constant is the same one used for the LiteLLM
endpoint pin (config.md D20, backends.md D9/D11).

## Security considerations

### Binding address

The generated accepter binds `127.0.0.1` — it is only reachable from
the host. This is the correct default for mysbx: the sandbox is the
only consumer of the forwarded serial port. (To widen it, change the
accepter in the module — e.g. `tcp,0.0.0.0,20108`.)

This widens the attack surface. The serial device becomes reachable
from any network interface the host has. Only do this when
justified.

### Network policy

- **bubblewrap / nono**: shared host network, no restriction needed.
- **nono with allowlist**: `connect-ports = [ 20108 ]` if the
  allowlist is active and you want to restrict which ports the
  sandbox can dial.
- **podman-gvisor / podman-krun**: pasta/TSI forwards all egress.
  No per-port restriction mechanism exists yet (config.md D21
  refuses allowlists on these backends).
- **krun (direct)**: TSI forwards all egress. Same as Podman.
- **microvm-qemu / microvm-ch**: SLiRP/passt forwards all egress.
  Same as Podman.

### Connection lifetime

ser2net's `timeout` parameter closes idle connections. Set it long
enough for interactive use (300–600 seconds). A closed connection
causes the sandbox client to hang; the client must re-dial.

For tools that expect a persistent connection (minicom, screen),
configure a keepalive or reconnect on disconnect.

## Comparison: direct bind vs. ser2net

| Aspect | Direct `/dev/tty*` bind | ser2net |
|---|---|---|
| Backend support | bubblewrap, nono only | **all seven** backends |
| Host privilege | User must be in `dialout`/`uucp` group | Only host-side ser2net needs it |
| Machine coupling | Sandbox must run on the host with the device | Any machine with TCP reach can connect |
| Multiple sandboxes | One bind per sandbox (port conflict on /dev) | Each sandbox is a separate TCP connection |
| Console observation | Same device, one reader | Multiple telnet clients can connect simultaneously |
| Remote lab access | Not possible | Built in (bind `0.0.0.0`) |
| Complexity | `--bind` flag in mysbx | NixOS service config + port number |
| Latency | Zero (same kernel) | Negligible (loopback TCP) |

## Configuration example — full host setup

```nix
# hosts/host.thing/default.nix (excerpt)
{
  imports = [ ./services.ser2net.nix ];

  myconfig.host.thing.ser2net = {
    enable = true;
    ports = [ { port = 20108; device = "/dev/ttyACM0"; } ];
  };

  # socat for PTY bridging inside the sandbox:
  # myconfig.ai.dev.mysbx.extraTools = [ pkgs.socat ];

  # If using microvm backends, the 10.0.2.2 gateway is reachable
  # by default — no firewall config needed (SLiRP/passt handle it).
  # If binding ser2net to 0.0.0.0, add firewall rules:
  # networking.firewall.allowedUDPPorts = [ 20108 ];
}
```

## See also

- [backends.md](./backends.md) — backend decisions, network models
- [config.md](./config.md) — configuration schema, mount options
- [backend-comparison.md](../backend-comparison.md) — which backend
  supports what
