# Validate `--nix` agent-gvisor sessions on a real host before enabling them

`myconfig.ai.gvisor-agent-sandbox.nix.enable` is still **off on every host**
because the core mechanism of `--nix` sessions — Podman's copy-up into a
named volume mounted at `/nix/store`, owned by the `keep-id`-mapped user,
under the `runsc` runtime — has never been exercised on real hardware. Only
static checks exist (`nix/checks.nix`: argv tests, the init-wrapper harness,
module eval).

Introduced by `9b7e777e82` ("agent-gvisor: `--nix` sessions — a writable Nix
store inside the sandbox"); the fail-closed preflight was added in the commit
that carries this note.

## Relevant code

- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix` —
  option `nix.enable` (plus `nix.package`, `nix.substituters`,
  `nix.trustedPublicKeys`, `nix.extraConfig`).
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/nix/agent-gvisor-init.sh`
  — the in-container preflight (`AGENT_GVISOR_NIX` branch): creates the Nix
  state dirs and probes `NIX_STORE_DIR` for writability, aborting the
  session with exit 1 otherwise.
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/rust/src/podman.rs`
  — the `--mount type=volume,src=<container>-nix,dst=/nix/store,U` argv
  (the `U` chowns the copy-up to the session user; without it the volume
  lands root-owned and unwritable under `keep-id` — the failure the first
  real-host run hit) and the `NIX_*` env for the session.
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/docs/nix-in-sandbox.md`
  §7 — the host verification checklist V1–V6.

## What to do

1. On a host with rootless Podman + runsc and the loaded sandbox image, set
   `nix.enable = true` **temporarily** (not committed) and run the V1–V6
   checklist of `docs/nix-in-sandbox.md` §7, in order.
2. If V1 fails, `start --nix` now aborts with
   `agent-gvisor-init: error: /nix/store is not writable in this sandbox`.
   In that case switch the host to the documented writable-rootfs fallback
   (§2 "Drop `--read-only`") or drop `--nix` on that host — do not paper
   over the preflight.
   (History: the first real-host run of V1 failed because the volume mount
   lacked the `U` option — the copy-up landed root-owned and unwritable
   under `keep-id`. Fixed since; only a `U`-chown failure under a specific
   Podman/runsc combination still justifies the fallback.)
3. Record the outcome (host, podman/runsc versions, what worked) in
   `docs/nix-in-sandbox.md` §7, then enable `nix.enable` for that host in
   `hosts/host.<name>/ai.<name>.nix`.
4. Delete this note once at least one host has a validated, committed
   `nix.enable = true`.
