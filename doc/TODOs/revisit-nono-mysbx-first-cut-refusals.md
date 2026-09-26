# Revisit the nono backend's first-cut refusals

The mysbx nono backend (`modules/myconfig.ai.dev/mysbx/mysbx-rs/src/nono.rs`)
is the LAYERED backend of `docs/design/backends.md` D1 (bd
myconfig-6di.4.2): bubblewrap builds the filesystem view, `nono run`
wraps the payload inside it. The first cut (bd myconfig-6di.2,
introduced by commits `7d1e097928` and `5a02f7a3bb` on branch
`6di2-nono`) ran nono directly on the host tree and refused five
mysbx features outright; the two refusals bubblewrap now covers are
LIFTED, the three network/display/multiplexer ones remain and are
tracked by their own beads.

## The refusals

### Lifted (bd myconfig-6di.4.2, backends.md D1)

- **Clone sessions** (`--session NAME`) — LIFTED: `Error::CloneUnsupported`
  and the `lib.rs` step 4c refusal are deleted; bwrap binds the clone AT
  the repo's own path (`docs/design/workspace.md` D3), and the repo-root
  grant covers it.
- **Mount `dest` remap** (a `[[mounts]]` entry whose `dest` differs from
  its `path`) — LIFTED: `Error::RemapUnsupported` is deleted; bwrap binds
  the dest, and the grant lands at the in-sandbox dest with the effective
  mode (last bind wins).

### Lifted (bd myconfig-6di.4.5, and the casing of bd myconfig-7hh)

- ~~**Multiplexer sessions**~~ — LIFTED: the socket dir gets nono's
  `--allow-unix-socket-dir-bind` when a session starts, and the mysbx
  profile ships `linux.af_unix_mediation = "pathname"` (the seccomp
  filter rejects pathname bind/connect without an explicit
  unix-socket grant — the pin that makes the D16/D17 claim hold under
  nono too). The mux-entry pin requirement is the bwrap layout's and
  stays. Live probed with a static bind/connect probe binary (bind
  and connect succeed under the grant, both fail without it).
- **Sidecar temp isolation** (bd myconfig-7hh) — verified structurally:
  `/tmp` is bwrap's private tmpfs; the sidecar policy files are NOT
  bound under it. nono 0.74.0 resolves an UNSET TMPDIR to `/tmp` and
  its `system_write_linux` group grants `$TMPDIR` — the mysbx profile
  writes `$TMPDIR` and the infra env pins `TMPDIR=/mysbx-nono/tmp`
  (the nono state tmpfs), so the grant covers only sandbox-private
  content; the payload env unsets TMPDIR (tools fall back to
  in-sandbox `/tmp`). Host-side live check of a `/tmp`-located repo
  stays with bd myconfig-27o.

### Lifted (bd myconfig-6di.4.4, backends.md D1's network table)

- **Shared network on nono** (`network = true`, the default, with an EMPTY
  allowlist) — LIFTED: `Error::NetworkSharedUnsupported` is deleted; the
  premise was wrong (nono allows outbound traffic by default), so the
  layered backend is bubblewrap parity: bwrap's `--share-net`, resolver
  binds and the ro `/nix/var/nix` bind, and nono adds no egress flag at
  all. The daemon socket is bound and granted only in THIS case (bd
  myconfig-nj9 pairs it): under an allowlist neither layer exposes
  `/nix/var/nix`, and a mount sourcing it is refused by the layout.
  Two allowlist honesty refusals came with it: `listen-ports` alone
  (nono would report "outbound allowed" — bd myconfig-a14) and
  URL-form `allow-domains` entries (no TLS interception on this
  backend) are refused with `Error::ListenPortsOnly` /
  `Error::DomainUrlForm`.

### Still refused

- ~~**Waypipe display**~~ — LIFTED (bd myconfig-6di.4.6): audited
  against nono 0.74.0's filter tables — AF_UNIX socket/socketpair
  pass the static baselines, the mediation filter continues
  `sendmsg` with a NULL `msg_name` (fd passing), `memfd_create` is
  never trapped; live-probed end-to-end (waypipe server inside a
  bwrap view under `--block-net`). The grants:
  `--allow-unix-socket-dir-bind /mysbx-home/wayland-0` and on the
  per-run socket dir; the guest-binary pin requirement is the bwrap
  layout's (unchanged).
## Related follow-up beads

- `bd myconfig-xob`: module options for the allowlist keys, so the
  NixOS module can seed `allow-domains`/`connect-ports`/`listen-ports`
  host-wide (the generated user layer does not carry them yet).
- `bd myconfig-27o`: live validation of the nono backend on a host
  (argv-mapped and cargo-tested only; no host has run it).

## How to verify

- `backend = "nono"` with each refused feature present exits `70` with the
  refusal message, also under `--dry-run` (the refusals sit before the
  dry-run early return).
- A lifted refusal must keep the corresponding cargo golden argv tests and
  the `config.md`/`docs/design/` decision text in step.
