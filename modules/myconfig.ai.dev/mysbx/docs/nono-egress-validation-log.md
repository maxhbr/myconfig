# mysbx nono proxy-only live validation

Evidence for `network = "proxy-only"` on the layered nono backend
(bd myconfig-vpi9.1): the mapping of the D20 profile onto nono's own
network flags. Bubblewrap's half of the profile is the channel from bd
myconfig-mo3.2.6 and is unchanged — nono runs on top of the same bwrap
view, so what needs evidence here is the SECOND layer.

## The chain

`bwrap … -- <relay> --listen 127.0.0.1:<port> --connect <socket> -- <nono> run … -- <env> <payload>`

The relay sits INSIDE bwrap's empty netns but OUTSIDE nono
(`src/bwrap.rs`, the `--` separator before both the relay and
`Params::inner`). So the relay's own listener never crosses nono's
Landlock filter, and nono only has to allow the PAYLOAD to CONNECT to
it.

## The decision: `--block-net` + one `--allow-connect-port`

The bead left two options open. Both were measured against the pinned
nono (0.81.0 at the time of writing; `nono run --help` is the
authority for the flag set).

| argv tail | payload reaches the relay | payload can bind the port | UDP / raw sockets |
| --- | --- | --- | --- |
| no flag (nono's default) | yes | yes | allowed |
| `--allow-connect-port <port>` only | yes | no | allowed |
| `--block-net` only | **no** — EPERM | no | denied |
| `--block-net --allow-connect-port <port>` | **yes** | no | **denied** |
| `--block-net --open-port <port>` | yes | **yes** | denied |

Mapping taken: `--block-net` plus ONE `--allow-connect-port <port>`.

- `--block-net` is required. Without it nono's default is "outbound
  allowed", so the profile would rest on bwrap's empty netns alone and
  a payload could still open UDP and raw sockets — a grant-free hole
  the bubblewrap backend does not have.
- `--block-net` alone is not enough: it also denies the loopback
  connect to the relay, so the sandbox could not reach its model
  endpoint at all.
- `--open-port` was rejected: it grants connect AND listen, and the
  payload has no reason to bind the relay's port. Listen is the
  relay's job, and the relay is not inside this layer.
- The grant names a PORT, not an address. Landlock grants ports, so
  this is not an address restriction — harmless here because the
  namespace carries nothing but `lo`.

## Probe transcripts

Payload under `--block-net --allow-connect-port <port>`, with a unix
listener standing in for the forwarder and an outer process standing in
for the relay:

```
PAYLOAD->RELAY OK
hello-from-litellm
UDP DENIED PermissionError
RELAY BIND OK
```

Payload attempting to BIND a listener, per flag set (the relay's own
bind happens outside this layer, so the payload has no reason to have
it):
```
--allow-connect-port 18501  -> BIND DENIED   PermissionError
--open-port          18501  -> BIND OK       (rejected for this reason)
```

Un-granted TCP the payload tries directly (no relay in the way):

```
tcp 127.0.0.1:18092  PermissionError    # ungranted loopback port
tcp 10.0.2.3:53      PermissionError    # off-box
```

Raw sockets and datagrams under `--block-net`:

```
socket(AF_INET, SOCK_DGRAM)  -> PermissionError: [Errno 1] Operation not permitted
```

## Not covered here

- A host run with a real LiteLLM forwarder socket and the pinned
  wrapper (`MYSBX_EGRESS_SOCKET`/`_PORT`/`_RELAY`/`_ETC`). The probes
  above drive the argv the builder emits against stand-in peers; the
  end-to-end run is the same one bd myconfig-mo3.2.8 recorded for
  bubblewrap, and every extra process it needs is on the bwrap side.
- Kernels below Landlock ABI 4. `--allow-connect-port` is a Landlock
  TCP rule; `mysbx doctor` fails such a kernel rather than letting a
  proxy-only run start ungranted.
