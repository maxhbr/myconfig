# mysbx detach live-validation log

One block per host run of the detached-run matrix (detach.md B9):
host, date, PASS/FAIL per scenario. A FAIL invalidates the decision
row — a bead goes against the decision document, not this log.

The matrix is `--detach`/`--detachable`/`attach` across the backends
(bubblewrap, nono, krun, microvm-qemu, podman-gvisor, podman-krun) ×
multiplexers (none, tmux, workmux, herdr, aoe), with the scenarios below.
Orca is a bubblewrap-only server payload and has no inner client-detach
step. See the [current support matrix](backend-comparison.md).
A refused backend is not a completed live validation.

## Scenarios (detach.md B9)

1. `--detach` + `attach` — start a detached run, reattach from
   another terminal, the terminal relays.
2. close the starting terminal — the run survives.
3. `^\` detach — the dtach client detaches, the run survives.
4. inner multiplexer detach + reattach — `prefix d` (tmux/workmux),
   quitting the aoe TUI, leaving the herdr client: the B7 prompt
   prints, `Enter` reattaches, `q` ends the sandbox, the run ends
   when the multiplexer has no session left.
5. a parallel foreground run in the same checkout — allowed and must
   not terminate the detached run (B3). A second **detached** run in the
   same workspace is refused; these are separate cases.
6. `--session NAME` detached — a named workspace, `attach --session
   NAME` reattaches.
7. `kill -9` of the dtach master — the record goes `lost`; `attach`
   reports it and removes the record (B5).

## Status

### Not yet run

No host run has been recorded. The orchestrator that implemented
beads `.1`–`.7` could not run the matrix: uid 0 without a passwd
entry (the mysbx home guard refuses, `whoami`/`ssh-keygen` fail), no
controlling tty (`--detachable` and `attach` need a terminal;
`--detach` itself does not), no `dtach` on PATH, and the backends are not exercisable
from that sandbox (bubblewrap needs unprivileged user namespaces or
setuid, krun needs `/dev/kvm`, podman needs a container runtime).

Both Podman backends still refuse `--detach` and `--detachable` in
`mysbx-rs/src/lib.rs`. Unique per-run container names already exist;
the old `myconfig-cad` naming explanation is stale, not evidence that
detach is now safe to enable.

Bubblewrap, nono, direct krun and microvm-qemu currently accept these
modes despite the absence of the live matrix required by design B9.
Their lifecycle and parallel-run acceptance are therefore unverified.

The implementation-time report for `.1`–`.7` recorded the cargo suite
with 26 pre-existing failures and no new failures, clippy with a
byte-identical baseline, the `mysbx-completions` and `mysbx-tests` Nix
checks, forced ssh-keygen-gated lifecycle tests, and `nix eval` of the
`test-f13` toplevel. This is historical code-level evidence, not a clean
full-suite result, a current rerun, or a live detach test.

To run the matrix: from an interactive multi-backend host with `dtach`
on PATH and a real `$XDG_RUNTIME_DIR`, exercise each scenario above
and append a block per host run (host, date, kernel, PASS/FAIL per
scenario). Record refusals and failures separately. Design B9 requires
failed combinations to remain unavailable; the currently enabled paths
must not be described as validated before their results are recorded.
