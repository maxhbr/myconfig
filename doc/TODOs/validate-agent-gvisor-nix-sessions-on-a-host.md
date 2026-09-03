# Validate `--nix` agent-gvisor sessions on a real host

`myconfig.ai.gvisor-agent-sandbox.nix.enable` has been committed `true` on
f13 since `65920e6dd7` (2026-09-01) — but the core mechanism of `--nix`
sessions — Podman's copy-up into a named volume mounted at `/nix/store`,
chowned by the `U` mount option to the `keep-id`-mapped user, under the
`runsc` runtime — has never been *validated* on real hardware: no outcome
is recorded in `docs/nix-in-sandbox.md` §7, and §7 still tells every
reader to keep `nix.enable` off until V1–V5 have been run. Only static
checks exist (`modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/nix/
checks.nix`: argv tests, the cli/init-wrapper harnesses, completions;
plus per-host eval in `flake.nix`).

Introduced by `9b7e777e82` ("agent-gvisor: `--nix` sessions — a writable
Nix store inside the sandbox"); the fail-closed preflight was added by
`bf932b61e8` (the commit that introduced this note); the first real-host
V1 failure (root-owned, unwritable store volume) was fixed in
`62e5490dac` ("agent-gvisor: chown the --nix store volume to the session
user (U)").

## Triage 2026-09-03

Verified from inside a bwrap-jail agent session (no podman/runsc
available there — the checklist itself could not be run):

- All referenced code parts exist and match the note: the `nix.*` options
  in `default.nix`, the `AGENT_GVISOR_NIX` preflight branch in
  `nix/agent-gvisor-init.sh` (fails closed, error prefix
  `agent-gvisor-init: error:`), the
  `type=volume,src=<container>-nix,dst=/nix/store,U` argv and `NIX_*`
  env block in `rust/src/podman.rs`, and the V1–V6 checklist in
  `docs/nix-in-sandbox.md` §7.
- Referenced commits all resolve: `9b7e777e82`, `bf932b61e8`,
  `62e5490dac`.
- The original premise "off on every host" is stale: f13 has
  `nix.enable = true` committed (`65920e6dd7`, 2026-09-01). That commit
  motivated the flip with doctor/session changes assuming nix in-sandbox,
  **not** with a recorded §7 outcome.
- Open question: if V1–V5 were in fact run on f13 on 2026-09-01 before
  that flip, record the outcome in §7 and delete this note. Otherwise run
  the checklist below.

## Relevant code

- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix` —
  option `nix.enable` (plus `nix.package`, `nix.substituters`,
  `nix.trustedPublicKeys`, `nix.extraConfig`).
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/nix/agent-gvisor-init.sh`
  — the in-container preflight (`AGENT_GVISOR_NIX` branch): creates the
  Nix state dirs and probes `NIX_STORE_DIR` for writability, aborting the
  session with exit 1 otherwise.
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/rust/src/podman.rs`
  — the `--mount type=volume,src=<container>-nix,dst=/nix/store,U` argv
  (the `U` chowns the copy-up to the session user; without it the volume
  lands root-owned and unwritable under `keep-id` — the failure the first
  real-host run hit) and the `NIX_*` env for the session.
- `modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/docs/nix-in-sandbox.md`
  §7 — the host verification checklist V1–V6.

## What to do

1. On f13 (rootless Podman + runsc + the loaded sandbox image;
   `nix.enable = true` is already committed), run the V1–V6 checklist of
   `docs/nix-in-sandbox.md` §7, in order. V1 is self-verifying: a broken
   store volume makes `start --nix` abort with
   `agent-gvisor-init: error: /nix/store is not writable in this
   sandbox`.
2. If V1 fails, do not paper over the preflight: switch the host to the
   documented writable-rootfs fallback (§2 "Drop `--read-only`") or drop
   `--nix` on that host — and revert the f13 `nix.enable`. A `U`-chown
   failure under a specific Podman/runsc combination is the only case
   that still justifies the fallback.
3. Record the outcome (host, podman/runsc versions, what worked) in
   `docs/nix-in-sandbox.md` §7. This is the missing artifact: without
   it, §7 contradicts f13's committed `nix.enable = true`.
4. Delete this note once the outcome is recorded and V1–V5 pass.
