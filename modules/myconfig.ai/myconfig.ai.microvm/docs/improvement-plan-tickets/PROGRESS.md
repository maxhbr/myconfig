# `myconfig.ai.microvm` improvement tickets — progress

Tracking file for the ticket series in [`README.md`](./README.md). One commit
per ticket (or per reviewable sub-step); this file is updated in the same
commit as the work it describes.

| # | Ticket | Status | Commit |
|---|--------|--------|--------|
| 1 | `01-agent-registry-refactor.md` | DONE | see `git log --oneline -- modules/myconfig.ai/myconfig.ai.microvm/agents.nix` |
| 2 | `02-add-hermes-support.md` | DONE | see `git log --oneline -- modules/myconfig.ai/myconfig.ai.microvm/agents.nix` |
| 3 | `03-network-and-control-channel-hardening.md` | IN PROGRESS (A, B done) | — |
| 4 | `04-batch-execution-and-lifecycle.md` | TODO | — |
| 5 | `05-resource-classes-and-state-management.md` | TODO | — |
| 6 | `06-runtime-validation-and-documentation.md` | TODO | — |

Status values: `TODO`, `IN PROGRESS`, `DONE`, `BLOCKED`.

## Notes

### Ticket 1 — agent registry refactor

- New `modules/myconfig.ai/myconfig.ai.microvm/agents.nix` is the authoritative
  registry (`package` / `executable` / `workmuxType` / `interactiveArgs`, with
  `workmuxName = "microvm-<name>"` derived).
- Consumers generated from it: guest packages + `agent-run` dispatch
  (`guest.nix`), `--agent` validation + help (`launcher.nix`), workmux agents
  (`workmux.nix`), well-formedness assertions (`default.nix`), and the
  shellcheck-gate list in `tests/microvm.nix`.
- New check `microvm-agent-registry` proves the registry really is the single
  source of truth (eval: workmux keys / guest closure / workmux types; build:
  greps the built launcher + `agent-run` for every registry agent).
- Verified behaviour-preserving: guest `systemPackages` names/paths and the
  workmux agent `{type, command}` attrs are byte-identical to before; only the
  `agent-run` / `agent-microvm` script texts changed (generated dispatch,
  generated validation set + help listing).

### Ticket 2 — Hermes support

- Registry gained `guestEnvironment` + `persistentState` fields and is now
  instantiated ONCE in `default.nix` (with context: `litellmPort`,
  `hermesModel`) and shared via `_module.args.agentRegistry`.
- `hermes` entry uses `inputs.hermes-agent.packages.<system>.default` (the same
  package the host `myconfig.ai.hermes` backends use), `executable = "hermes"`,
  `interactiveArgs = [ "--model" <myconfig.ai.hermes.model.default> ]` and
  `guestEnvironment.OPENROUTER_BASE_URL = http://127.0.0.1:<litellmPort>/v1`.
- Verified against the pinned hermes source: endpoint resolution order
  (config.yaml → `CUSTOM_BASE_URL` → `OPENROUTER_BASE_URL` → openrouter.ai),
  api-key pick for non-OpenRouter base URLs (`OPENAI_API_KEY`), the first-run
  provider guard (satisfied by the placeholder key, so no wizard), and the
  state root `$HERMES_HOME` (default `~/.hermes`) — recorded as
  `persistentState.directories = [ ".hermes" ]`, `enabledByDefault = false`
  (guest home stays disposable; opt-in persistence is ticket 5).
- workmux has no `hermes` profile → falls back to its default profile; that is
  a graceful degradation, not an error (`resolve_profile_with_type`).
- New check `microvm-agent-executables` BUILDS every registry agent package
  and asserts `bin/<executable>` exists (the `command -v hermes` criterion).
- Manual interactive smoke test documented in `docs/agent-microvm.md`.

### Ticket 3 — network + control-channel hardening

Split into three commits (A: L2 isolation, B: SSH host keys + VSOCK,
C: network profiles).

- **Part A (done).** `agent-microvm-attach-<slot>` now also runs
  `bridge link set dev <tap> isolated on`, so the bridge itself drops
  guest↔guest frames for every EtherType (ARP / IPv6 ND included) — the IPv4
  `FORWARD` inter-VM DROP becomes the second line of defence. Host-facing
  bridge side deliberately not isolated. Locked down per slot in
  `microvm-eval-enabled`; resolves open item A1 in
  `docs/agent-microvm-remaining.md`. Runtime proof (`bridge link show`,
  guest→guest ping/ARP) remains part of the B4 runtime tier.
- **Part B (done).** Authenticated control channel + reserved VSOCK identity:
  - new `hostkeys.nix` provisions ONE stable ed25519 host key per slot at
    runtime under `${runtimeRoot}/hostkeys/<slot>` (root:root 0400, never in
    the Nix store, not agenix — host-local regenerable identities) and
    rebuilds `${runtimeRoot}/known_hosts` (0444, public keys only) atomically.
  - delivery to the guest is a SECOND virtiofs share, per slot, READ-ONLY,
    mounted at `/var/lib/agent-hostkey`; virtiofsd passes 0400 root:root
    through, so the untrusted `agent` user cannot read it and the guest cannot
    rewrite its identity. Documented amendment to plan §10's "exactly one
    share" (the `microvm-eval-workspace-share` check now pins exactly two).
    `microvm.credentialFiles` was NOT usable: cloud-hypervisor throws on it.
  - guest sshd uses only that key (`generateHostKeys = false`).
  - launcher: single `SSH_VERIFY_OPTS` array with
    `StrictHostKeyChecking=yes` + `UserKnownHostsFile=<known_hosts>`, a
    fail-closed `require_known_hosts`, and `run` starts the (idempotent)
    provisioning unit before booting a slot. No `StrictHostKeyChecking=no`
    remains — asserted by the new `microvm-host-identity` check.
  - slots.nix gained a deterministic `cid = 8300 + index` (unique, avoids
    reserved 0/1/2 and VMADDR_CID_ANY, asserted in default.nix + the slot-pool
    check, shown by `status`). Deliberately NOT yet passed to
    `microvm.vsock.cid`: that flips `microvm@<slot>` to `Type=notify` (socat
    ↔ vsock notify bridge), a startup change only verifiable by booting on
    KVM — so it is activated together with the ticket-4 control channel.
