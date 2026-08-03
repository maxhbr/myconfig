# `myconfig.ai.microvm` improvement tickets — progress

Tracking file for the ticket series in [`README.md`](./README.md). One commit
per ticket (or per reviewable sub-step); this file is updated in the same
commit as the work it describes.

| # | Ticket | Status | Commit |
|---|--------|--------|--------|
| 1 | `01-agent-registry-refactor.md` | DONE | see `git log --oneline -- modules/myconfig.ai/myconfig.ai.microvm/agents.nix` |
| 2 | `02-add-hermes-support.md` | DONE | see `git log --oneline -- modules/myconfig.ai/myconfig.ai.microvm/agents.nix` |
| 3 | `03-network-and-control-channel-hardening.md` | DONE | A, B, C (3 commits) |
| 4 | `04-batch-execution-and-lifecycle.md` | DONE | A, B (2 commits) |
| 5 | `05-resource-classes-and-state-management.md` | DONE | A, B, C+D (3 commits) |
| 6 | `06-runtime-validation-and-documentation.md` | DONE | A, B, C+D (3 commits) |

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
- **Part C (done).** Network booleans replaced by named profiles:
  - new `network-profiles.nix` is the authoritative capability table
    (`litellm` / `packageProxy` / `dns` / `internetEgress` / `nat` /
    `logDrops`) for `offline` / `proxy-only` / `package-access` / `internet`.
  - resolved ONCE in `default.nix` → `_module.args.agentNetwork`, consumed by
    `network.nix` (firewall + NAT chain) and `guest.nix` (loopback LiteLLM
    forwarder gating, `http_proxy` for package-access, resolvers for
    internet), so host policy and guest config cannot disagree.
  - `internet` is real egress: dedicated `AGENT_MICROVM_NAT` POSTROUTING chain
    with MASQUERADE (symmetric teardown), DNS restricted to `dnsServers` with
    every other port-53 destination dropped, rate-limited drop logging.
    `ip_forward` is already 1 repo-wide (`modules/nixos.networking`).
  - unconditional in every profile: metadata drop, guest↔guest drop,
    private-range drop, terminal DROPs, host→guest control traffic.
  - migration: `allowPublicInternet = true` translates to `internet` (with a
    warning) only when `networkProfile` is not explicitly defined, otherwise it
    is rejected as ambiguous; `allowPrivateNetworks` / `allowInterVmTraffic`
    are rejected outright (no profile grants them; L2 isolation is
    unconditional). Any *defined* legacy boolean warns.
  - NOTE for future work: "was this option explicitly set?" must use
    `opt.highestPrio < 1500`, NOT `opt.isDefined` — current nixpkgs implements
    `mkOption { default = …; }` as a priority-1500 definition, so `isDefined`
    is true for every option with a default.
  - `hosts/host.f13/ai.f13.nix` migrated to `networkProfile = "proxy-only"`.
  - new check `microvm-network-profiles` (43 assertions) renders all four
    profiles and asserts both what each allows and what it must NOT allow;
    `microvm-eval-rejects-invalid` grew the migration/ambiguity cases.

### Ticket 4 — batch execution + lifecycle

Split into two commits (A: job format + guest runner, B: host submit/cancel/
recover + allocation tokens).

- **Part A (done).** New `job.nix` owns the versioned job format, the per-slot
  host job directories (tmpfiles-created, because virtiofsd needs its share
  source to exist), the `myconfig.ai.microvm.job.*` timeout options and the
  guest-side `agent-job` runner + hardened oneshot.
  - THIRD virtiofs share per slot at `/run/agent-job`: read-write (the guest
    writes `out/result.json`) but spec+prompt are root-owned 0444 inside a
    root-owned 0755 dir, so the untrusted agent can only read them. Prompts
    never become process arguments and never enter the Nix store.
  - registry gained `batchArgs` (with a `%PROMPT%` placeholder) + `batchStdin`,
    taken from each pinned build's own `--help`; `batchDispatchCases` generates
    the guest runner's case table, so batch mode cannot drift from the registry.
  - timeout enforced twice in the guest (per-job `timeout(1)` + static
    `RuntimeMaxSec` ceiling); the host adds a third in part B.
  - deliberately NO guest power-off: `microvm@<slot>` has `Restart=always`, so
    a self-powering-off guest would boot-loop. The host stops the VM.
  - VERIFIED BY RUNNING the built runner under bwrap with a stubbed agent and
    faked `/run/agent-job` + `/workspace`: success, agent failure (exit code
    propagated), timeout (`state=timed-out`, `timedOut=true`), stdin-mode agent
    (codex), inert-without-spec, and every rejection path (bad version,
    smuggled `command`, traversal `promptFile`, excessive timeout, non-batch
    agent).
  - new check `microvm-batch-jobs` (13 eval assertions + dispatch/rejection
    greps of the BUILT runner); shares check now pins exactly three shares.
- **Part B (done).** Host lifecycle in `launcher.nix`:
  - `submit` (validate → allocate → clone → job dir → mount → start → wait for
    the structured result with `timeout + job.gracePeriodSeconds`, breaking
    early if the VM dies → archive result → stop/unmount/clear → release), exit
    codes 0/1/124/70.
  - `cancel <task>` records a `cancelled` result and tears the slot down only
    while the allocation TOKEN still matches (`cleanup_slot_owned`).
  - `recover [--dry-run]` classifies every slot (stale marker, orphaned unit,
    orphaned attached/batch run with a dead launcher, stale mount, stale job
    data), prints every action and always keeps clones. A `detached` slot with
    a dead launcher is explicitly NOT recovered — that is normal.
  - allocation markers now carry token, mode, unit, launcher pid AND that pid's
    /proc start time (`owner_alive` compares both, so a recycled pid cannot
    impersonate the owner); allocation itself was factored into
    `allocate_slot` + `write_session_marker`, shared by `run` and `submit`.
  - `status` additionally shows mode, job state (live result, else the archived
    one) and the job timeout; results are archived OUTSIDE every guest share.
  - VERIFIED BY RUNNING the built launcher against stubbed
    systemctl/mount/findmnt (with the stub simulating a guest that writes
    result.json): validation rejections, completed/failed/timed-out/no-result
    runs incl. exit codes, teardown state (marker+mount+job cleared, clone
    kept), token-guarded cancel (valid + refused-on-mismatch), and every
    `recover` branch in both dry-run and real mode. The HOST-generated
    `spec.json` was then fed to the real GUEST runner to prove the format
    contract end-to-end.

### Ticket 5 — resource classes + state management

Split into commits (A: resource classes, B: task-scoped agent state, C: limits
+ usage reporting + workspace-safety review).

- **Part A (done).** `resourceClasses` (attrsOf { count; vcpu; memoryMiB; })
  replaces `slotCount`/`defaultVcpu`/`defaultMemoryMiB` (deprecated, still
  honoured as a synthesized single `normal` class; setting both spellings is
  rejected as ambiguous, and using the legacy ones warns).
  - `slots.nix` now takes the class table and assigns a per-class `classIndex`
    (→ name `agent-<class>-<i>`, tap `vm-<class>-<i>`) plus a pool-wide
    `globalIndex` (→ MAC/IPv4/CID). New assertions: non-empty pool, total ≤
    maxSlotCount, class-name charset, TAP ≤ 15 chars (IFNAMSIZ), unique
    names/taps.
  - the effective class table is resolved ONCE in default.nix and shared via
    `_module.args.agentResourceClasses`, so guest/network/hostkeys/job/launcher
    all build the SAME pool; guests are sized from their slot's class.
  - launcher: `--resource-class` (validated, generated from the options) and
    `--wait <sec>` (bounded); the allocator filters by class and NEVER
    substitutes another one; `status`/`list`/`--help` show classes and sizing.
  - f13 migrated to an explicit `mkForce { small; normal; }` pool.
  - VERIFIED BY RUNNING the built launcher (stubbed systemctl/mount): unknown
    class rejected, per-class allocation, "class full" refusal WITHOUT
    substitution, bounded `--wait` (measured 5s), class-aware status/list.
  - new check `microvm-resource-classes` (11 eval assertions + launcher greps);
    the slot-pool check now exercises six class tables (66 assertions).
- **Part B (done).** Opt-in, task-scoped agent state:
  - new `state.nix` owns the paths (`state/tasks/<task>/<agent>/<dir>` kept,
    `state/slots/<slot>` = the share source), the registry-driven guest linker
    (`agent-state-link.service`, ordered before `agent-job`) and the tmpfiles
    rules (virtiofsd needs the per-slot source to exist).
  - FOURTH virtiofs share at `/var/lib/agent-state`; the launcher `mount --bind`s
    the per-TASK dir onto the per-slot source only for `--persist-agent-state`
    runs and otherwise leaves it EMPTY, so the default stays disposable and no
    state leaks between tasks on the same slot.
  - only registry-DECLARED dirs are created/exposed; requesting persistence for
    an agent that declares none is an error, not a silent no-op; the linker
    refuses to replace a non-empty home directory (never clobbers dotfiles).
  - new options `guestAgentUid`/`guestAgentGid` (default 1000, asserted
    unprivileged) replace the hardcoded 1000 in guest.nix/launcher.nix/job.nix
    (ticket 5's "make the UID configurable or assert it").
  - assertion that declared state dirs stay disjoint from
    `guestDotfiles.homeFilePrefixes` (HM activation vs. linker).
  - VERIFIED BY RUNNING: launcher (stubbed mounts) — refusal without `--agent`,
    refusal for an agent without declared dirs (and no dirs created), per-task
    dir + bind for hermes, task isolation across two tasks, disposable runs
    clearing a dirty slot share, `status` reporting; guest linker under bwrap —
    empty share (no links), link creation, idempotent rerun, empty-dir
    replacement, non-empty-dir preservation.
  - new check `microvm-agent-state`; the shares check now pins exactly four.
- **Parts C+D (done).** Limits, usage reporting and the workspace-safety review:
  - guest `agent-job` limits are now derived from the slot's CLASS
    (`job.nix`'s guestModule became `mkGuestModule slot`): `CPUQuota =
    vcpu*100%`, `MemoryMax = classRAM - job.guestMemoryHeadroomMiB` (never below
    half), `TasksMax = job.tasksMax`, on top of the existing `RuntimeMaxSec`.
  - host-side drop-ins on `microvm@<slot>`: `MemoryMax = classRAM +
    hypervisorMemoryOverheadMiB` (asserted NEVER below guest RAM + overhead),
    `TasksMax`, and relative `CPUWeight`/`IOWeight` = 50 so sandboxes yield to
    interactive host work without a hard quota wasting idle capacity.
  - new `agent-microvm usage`: retained disk usage per task (workspace clone +
    task-scoped agent state), the runtime roots, and the pruning command;
    `workspace-remove` now also drops the task's agent state and archived result.
  - ticket 5 D re-verified in the BUILT launcher by
    `microvm-limits-and-workspace-safety`: git-dir/common-dir escape checks,
    realpath canonicalisation, scoped removal, `--no-local` clones, and a
    forbidden-tooling grep proving the host never evaluates repo-provided nix /
    direnv / npm / cargo / make / git hooks.

### Ticket 6 — runtime validation, observability, documentation

Split into commits (B: observability, A: real-KVM validation suite, C+D: doc
sync + backward-compat confirmation).

- **Part B (done).** Structured lifecycle events: `emit_event` writes ONE JSON
  record per transition to stderr, to the journal (`journalctl -t agent-microvm`)
  and to a BOUNDED per-task log (`<runtimeRoot>/logs/<task>.jsonl`, one rotated
  generation at `taskLogMaxBytes`). Records carry ts/event/task/slot/agent/
  resource_class/mode/state/exit_code; the guest runner emits its own
  agent-started/agent-finished/timeout records to the console (captured by
  `microvm@<slot>`), so host and guest streams correlate. Prompt CONTENT, keys,
  credentials and env vars are never logged (only the prompt file's path+size).
  VERIFIED BY RUNNING: full batch stream (submitted→allocated→created→start→
  finished→stopped→cleanup), the timeout path, cancellation, recovery actions,
  and a grep proving no secrets/prompt text reach the task log. New check
  `microvm-observability`.
- **Part A (defined, NOT executed).** New `runtime-validation.sh` (shellcheck +
  shfmt clean) is a repeatable, root+KVM-only suite with six sections — boot/
  filesystem, proxy-only network matrix, two-guest layer 2, credential boundary,
  forced lifecycle failures, hostile repository fixture — printing PASS/FAIL/SKIP
  per check and exiting non-zero on any failure. `SKIP` is honest (e.g. "fewer
  than two slots"), never a disguised pass. Deliberately kept OUT of
  `nix flake check` (no KVM in CI, it boots real VMs).
  New `docs/agent-microvm-runtime-validation.md` is the test guide: prerequisites,
  per-section expected outcomes, manual extras (tcpdump, cgroup inspection,
  journal streams) and how to interpret failures. Both state clearly that the
  suite has NOT been run from the development environment (no /dev/kvm, no root),
  so nothing in them may be read as runtime evidence.
- **Parts C+D (done).** Documentation synchronised with the implementation and
  backward compatibility confirmed:
  - new `docs/agent-microvm-architecture.md` (module map, prebuilt pool,
    workspace indirection, self-contained guest store, network path, credential
    boundary, interactive vs batch path, state lifetimes, classes + limits,
    control-channel identities),
    `docs/agent-microvm-operator-guide.md` (ten exact procedures: start, submit,
    status, attach, cancel, collect, remove, recover, logs, triage) and
    `docs/agent-microvm-security-model.md` (trusted/untrusted, what the VM
    boundary protects, what virtiofs still shares — per share, with its risk —
    what LiteLLM protects, mitigated attacks, residual risks, and an explicit
    "why this is not a hardened multi-tenant cloud sandbox").
  - `agent-microvm.md`: status block rewritten (eval/build + stubbed-run
    verified; real KVM NOT executed), activation example migrated to
    resourceClasses/networkProfile, the stale "no passwordless sudoers" and
    "test-f13 builds with the feature disabled" claims fixed, a new
    "Backward compatibility & migration" section (unchanged commands + the four
    deprecated options with translate/reject semantics + the slot-rename note),
    and a companion-document index.
  - `agent-microvm-validation.md` gained an addendum listing the grown check
    suite, the slot rename, the four shares, the run-verified shell components,
    and a restatement that the runtime tier is still unexecuted.
    `agent-microvm-remaining.md` and `agent-microvm-review.md` carry
    "superseded/historical" notes (A1 + A2 resolved, M3 addressed).

## Definition of done (ticket 6)

| Criterion | State |
|---|---|
| Claude, Codex, Pi, OpenCode, Hermes from ONE registry | done (`agents.nix`) |
| Interactive and batch modes both work | done (batch verified against stubs; real KVM pending) |
| Batch: structured status, cancellation, recovery, hard timeouts | done |
| All TAP ports use layer-2 isolation | done (unconditional) |
| SSH host identity verified / VSOCK for batch control | SSH done; VSOCK CIDs reserved, transport deferred (documented) |
| Network behaviour via explicit profiles | done (4 profiles) |
| Upstream provider credentials never enter guests | done (placeholders only) |
| Guest root and home disposable by default | done |
| Agent persistence task-scoped and explicit | done |
| Fixed resource classes, no per-job Nix evaluation | done |
| Real-KVM filesystem/network/lifecycle/malicious-repo tests pass | **NOT executed** (suite + guide exist) |
| Documentation matches the implementation | done |
| Residual risks documented rather than implied solved | done (security model) |
