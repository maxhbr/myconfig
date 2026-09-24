# Revisit the nono backend's first-cut refusals

The mysbx nono backend (`modules/myconfig.ai.dev/mysbx/mysbx-rs/src/nono.rs`,
dispatched in `src/lib.rs` step 5; its refusals live in `src/lib.rs`
steps 4b/4c and in the `nono.rs` argv builder) maps the merged config
onto a `nono run` under Landlock + seccomp (bd myconfig-6di.2). Its
Landlock model has no path remap and no bind machinery, so five mysbx
features are refused outright on this backend. Each refusal below is
correct today; revisit each once nono's flag surface is confirmed on a
real host.

Introduced by commits `7d1e097928` and `5a02f7a3bb` on branch
`6di2-nono`.

## The refusals

- **Clone sessions** (`--session NAME`) — refused with
  `Error::CloneUnsupported` (`nono.rs`) and in `lib.rs` step 4c, before the
  clone would be created. A clone run is a path remap (the clone bound AT
  the repo's own path, `docs/design/workspace.md` D3); Landlock grants
  access AT a path, it cannot move one. To lift: a Landlock-safe way to
  present the clone at the repo's path must exist in nono (none does
  today), or the workspace semantic must change (a non-remapped clone
  path).
- **Mount `dest` remap** (a `[[mounts]]` entry whose `dest` differs
  from its `path`) — refused with `Error::RemapUnsupported` (`nono.rs`,
  argv section 5). Landlock grants access AT a path, it cannot move
  one, so a bind at a different destination is inexpressible. To lift:
  nono must gain a remap mechanism, or the operator keeps
  `dest` == `path` for every mount under this backend.
- **Waypipe display** (`display = "waypipe"`) — refused with
  `Error::DisplayUnavailable` (`nono.rs`). waypipe's syscall set (memfd,
  `SCM_RIGHTS` on the guest-side socket) must be audited under nono's
  seccomp filter (the `config.md` D18 "The other backends" note anticipated
  exactly this). To lift: the audit must come out clean, then wire the
  channel like bwrap does (the `lib.rs` waypipe arm + display handling in
  `nono.rs`).
- **Multiplexer sessions** (`multiplexer` = `tmux`/`workmux`/`aoe`/`herdr`/
  `orca`) — refused with `Error::MultiplexerUnavailable` (`nono.rs`). The
  socket isolation of `config.md` D16/D17 (a private
  `/mysbx-home/.mysbx-tmux` inside the tmpfs home) has no Landlock
  equivalent: nono has no sandbox home, and the host tmux socket dir
  `/tmp/tmux-<uid>` is writable under nono's default profile. To lift: a
  Landlock-equivalent private socket location is needed (a socket-bind
  grant could serve, if nono grows one — the verified 0.74.0 surface
  has `--allow-unix-socket`, no bind family).
- **Shared network on nono** (`network = true`, the default, with an EMPTY
  allowlist) — refused with `Error::NetworkSharedUnsupported` (`nono.rs`).
  nono mediates per connection (seccomp baseline; only the
  `allow-domains`/`connect-ports`/`listen-ports` entries of
  `config.md` D21 pass), so "share the host network" is inexpressible and
  silently granting nothing would be a silent downgrade. To lift: a future
  nono flag or profile that grants unmediated shared networking — map
  `network = true` onto it.

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
