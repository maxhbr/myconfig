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

### Still refused

- **Waypipe display** (`display = "waypipe"`) — refused with
  `Error::DisplayUnavailable` (`nono.rs`). waypipe's syscall set (memfd,
  `SCM_RIGHTS` on the guest-side socket) must be audited under nono's
  seccomp filter (the `config.md` D18 "The other backends" note anticipated
  exactly this). To lift: the audit must come out clean, then wire the
  channel like bwrap does (the `lib.rs` waypipe arm + display handling in
  `nono.rs`). Tracked by bd myconfig-6di.4.6.
- **Multiplexer sessions** (`multiplexer` = `tmux`/`workmux`/`aoe`/`herdr`/
  `orca`) — refused with `Error::MultiplexerUnavailable` (`nono.rs`). The
  private socket directory lives in the sandbox home tmpfs again, but
  nono's unix-socket grants for the multiplexer payload are not audited
  yet. To lift: audit the socket path under nono's filters. Tracked by
  bd myconfig-6di.4.5.
- **Shared network on nono** (`network = true`, the default, with an EMPTY
  allowlist) — refused with `Error::NetworkSharedUnsupported` (`nono.rs`).
  nono mediates per connection (seccomp baseline; only the
  `allow-domains`/`connect-ports`/`listen-ports` entries of
  `config.md` D21 pass), so "share the host network" is inexpressible and
  silently granting nothing would be a silent downgrade. To lift: a future
  nono flag or profile that grants unmediated shared networking — map
  `network = true` onto it. Tracked by bd myconfig-6di.4.4.

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
