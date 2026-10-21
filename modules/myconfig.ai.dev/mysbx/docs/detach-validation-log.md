# mysbx detach live-validation log

One block per host run of the detached-run matrix (detach.md B9):
host, date, PASS/FAIL per scenario. A FAIL invalidates the decision
row — a bead goes against the decision document, not this log.

The matrix is `--detach`/`--detachable`/`attach` across the backends
(bubblewrap, nono, krun, podman-gvisor, podman-krun) × multiplexers
(none, tmux, workmux, herdr, aoe), with the scenarios below.

## Scenarios (detach.md B9)

1. `--detach` + `attach` — start a detached run, reattach from
   another terminal, the terminal relays.
2. close the starting terminal — the run survives.
3. `^\` detach — the dtach client detaches, the run survives.
4. inner multiplexer detach + reattach — `prefix d` (tmux/workmux),
   quitting the aoe TUI, leaving the herdr client: the B7 prompt
   prints, `Enter` reattaches, `q` ends the sandbox, the run ends
   when the multiplexer has no session left.
5. a parallel foreground run in the same checkout — refused (one
   detached run per workspace, B3).
6. `--session NAME` detached — a named workspace, `attach --session
   NAME` reattaches.
7. `kill -9` of the dtach master — the record goes `lost`; `attach`
   reports it and removes the record (B5).

## Status

### Not yet run

No host run has been recorded. The orchestrator that implemented
beads `.1`–`.7` could not run the matrix: uid 0 without a passwd
entry (the mysbx home guard refuses, `whoami`/`ssh-keygen` fail), no
controlling tty (`--detach`/`--detachable`/`attach` all need a
terminal), no `dtach` on PATH, and the backends are not exercisable
from that sandbox (bubblewrap needs unprivileged user namespaces or
setuid, krun needs `/dev/kvm`, podman needs a container runtime).

The podman backends stay refused for `--detach` until
`myconfig-cad` (unique per-run container names) lands — the refusal
is enforced in `src/lib.rs` and names the bead.

All code-level validation is green across `.1`–`.7`: the cargo suite
(26 pre-existing failures, no new), clippy (byte-identical baseline),
the `mysbx-completions` and `mysbx-tests` nix checks, the forced
ssh-keygen-gated lifecycle tests, and `nix eval` of the `test-f13`
toplevel.

To run the matrix: from an interactive multi-backend host with `dtach`
on PATH and a real `$XDG_RUNTIME_DIR`, exercise each scenario above
and append a block per host run (host, date, kernel, PASS/FAIL per
scenario). A backend that fails stays refused with a named reason in
`src/lib.rs` and a row here.
