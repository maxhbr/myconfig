<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — Merge-Readiness Code Review (branch `improve-sandboxing`)

Companion to [`agent-microvm.md`](./agent-microvm.md) (user/operator docs),
[`agent-microvm-validation.md`](./agent-microvm-validation.md) (eval/build
results + §45 checklist) and
[`agent-microvm-remaining.md`](./agent-microvm-remaining.md) (remaining
work / DoD). This file records the findings of a full code review of the
branch (diff base `master...HEAD`, 8 commits) covering **bugs/logic errors**,
**security issues** and **error-handling gaps** in
`modules/myconfig.ai/myconfig.ai.microvm/{default,slots,guest,network,launcher,workmux}.nix`,
`tests/microvm.nix` and the f13 enablement.

Scope note: the deferred runtime/KVM validation (plan §40–§44) and the
documented open items A1/A2/A3 were **not** counted against merge-readiness;
only code that is itself wrong or contradicts its own documentation is
flagged.

## 1. Verdict — **NOT READY** (one root cause, two HIGH findings; fix is small and localized)

The implementation is otherwise unusually careful: strict quoting throughout,
shellcheck-gated `writeShellApplication` launchers, a genuinely
single-source-of-truth slot table, a fail-closed firewall, and non-vacuous
eval tests with a positive control. But git's `safe.directory`
dubious-ownership check — a failure mode this repo has already worked around
elsewhere — breaks the primary `run` flow and silently voids the
`workspace-remove` data-loss guard. Once H1/H2 (and ideally M1/M2) are fixed,
the branch is READY WITH NITS.

## 2. Root cause shared by H1/H2

The launcher requires root (`require_root`, `launcher.nix:393`) but operates
on git repositories owned by uid 1000:

- the source repo the user points `--repository` at (workmux flow:
  `exec sudo agent-microvm run …`, `workmux.nix:134`), and
- the workspace clone, which `create_clone` chowns to `1000:1000`
  (`launcher.nix:263`, plan §11).

The pinned nixpkgs ships **git 2.55.0**, whose ownership check
(CVE-2022-24765 mitigation, git ≥ 2.35.2) refuses to operate on a repository
owned by a different user — **root is not exempt**. Every root-run `git`
invocation against a uid-1000-owned tree dies with
`fatal: detected dubious ownership in repository`, and the launcher suppresses
exactly that stderr. The repo is aware of this hazard in other modules
(`modules/myconfig.ai/myconfig.ai.workmux/sandbox.nix:118`,
`modules/myconfig.ai/programs.pi-coding-agent/default.nix:761`), but the
microvm launcher does not handle it.

Suggested fix: scoped `git -c safe.directory="$path" …` on the specific
invocations (**never** `safe.directory='*'`), or drop privileges
(`runuser -u`) for the git operations. Additionally make the
`workspace-remove` guards fail **closed** on git error (see H2).

## 3. Findings

> Note: `launcher.nix:NNN` line references below are **pre-fix** — they
> describe the code as reviewed and no longer point at the same lines
> after the H1/H2 resolutions were applied.

### HIGH

#### H1 — `run` is broken for user-owned repos (dubious-ownership) — `launcher.nix:178`, `launcher.nix:240`

- `git -C "$real" rev-parse --show-toplevel 2>/dev/null`
  (`launcher.nix:178`) fails the ownership check for any uid-1000-owned
  repository when run as root; the fatal message is discarded and the user
  sees the misleading `die "not a git repository: $real"`.
- Even if validation passed, `git clone --no-local -- "$repo" "$clone"`
  (`launcher.nix:240`) opens the same user-owned source repo as root and
  fails the same check.

Effect: the **entire intended flow** — `workmux add --agent microvm-*` →
`sudo agent-microvm run --attach --repository <user repo>` — cannot work as
shipped. It fails *closed* (no security hole), which is consistent with the
"config-complete but runtime-unproven" state, but the code is wrong, not
merely unproven.

**Resolution (FIXED):** Both git invocations now carry a scoped override:
`git -c safe.directory="$real" -C "$real" rev-parse --show-toplevel` in
`validate_repository`, and `git -c safe.directory="$repo" clone --no-local
-- "$repo" "$clone"` in `create_clone` (never `safe.directory='*'`). The
remaining git calls in `create_clone`/`verify_clone` (rev-parse,
`checkout -b`) operate on the freshly created root-owned clone *before* the
`chown -R 1000:1000`, so they need no override (verified ordering).

#### H2 — `workspace-remove` uncommitted/unpushed guards are silently vacuous → data loss — `launcher.nix:641`, `launcher.nix:645`

The clone is owned by 1000:1000; `workspace-remove` runs as root:

- `git -C "$clone" status --porcelain 2>/dev/null` fails the ownership
  check, its error is discarded, output is empty → treated as **clean**.
- `git log --branches --not --remotes --oneline 2>/dev/null || true` fails
  the same way → treated as **no unexported commits**.

The §35 guard the help text and operator docs advertise ("refuses on
uncommitted changes without `--force`") therefore never fires; `rm -rf`
proceeds unguarded. Two fixes required:

1. same `safe.directory` scoping as H1, and
2. **fail closed**: a git error must mean "cannot determine → refuse", not
   "safe to delete" (drop the blanket `2>/dev/null` / `|| true` swallowing).

**Resolution (FIXED):** Both guard invocations now use
`git -c safe.directory="$clone" -C "$clone" …` and capture the exit status
explicitly (no `2>/dev/null`, no `|| true`). If `git status --porcelain` or
`git log --branches --not --remotes` fails, the command `die`s with a clear
"cannot determine …; refusing (use --force)" message. Semantics preserved:
clean+pushed → proceed; dirty or unpushed → refuse without `--force`; git
error → refuse.

### MEDIUM

#### M1 — Guest IP addressing gated on `enableSsh` — `guest.nix:239`

`systemd.network = lib.mkIf cfg.enableSsh { … }` means that with
`enableSsh = false` the guest gets **no address and no default route**, so it
cannot reach the LiteLLM forwarder either — contradicting §17/§31 ("guest
reaches the model API via `<gateway>:<port>`"), which is independent of SSH.
Latent on f13 (ssh enabled), but the gate is wrong: addressing should be
unconditional.

#### M2 — `AGENT_MICROVM_SSH_KEY` does not survive `sudo` — `launcher.nix:327`, `launcher.nix:487`, `workmux.nix:134`

`run --attach` (the workmux path) runs under `sudo` with `env_reset`, so the
documented `AGENT_MICROVM_SSH_KEY=… agent-microvm …` mechanism silently does
not apply to it; readiness/attach then depends on root's default `~/.ssh`
keys matching the dedicated pubkey — an undocumented assumption. Otherwise
`wait_ready` times out after 90 s and tears the slot down. Fix options:
`sudo --preserve-env=AGENT_MICROVM_SSH_KEY` in `workmux.nix`, a fixed
key-path module option, or explicit operator docs. (Related to open item A2.)

#### M3 — `StrictHostKeyChecking=no` rationale overstates the trust boundary — `launcher.nix:607–612` vs open item A1

The comment claims "the trust boundary is the bridge/firewall, not SSH host
identity", but iptables/br_netfilter does **not** filter ARP: absent the
per-TAP `isolated` flag (documented open item A1,
`agent-microvm-remaining.md`), a hostile co-resident guest can ARP-spoof the
gateway or another slot's IP and MITM `ssh` / `--attach` sessions (agent
prompts/commands — no secrets, per §17). A1 is honestly documented; this is
flagged only because the launcher comment contradicts it. Reword the comment,
and note this strengthens the case for resolving A1 before relying on
multi-slot concurrency.

### LOW

| # | Finding | Location |
|---|---------|----------|
| L1 | `cmd_destroy` is byte-identical to `cmd_stop`, though help text/docs claim destroy does "plus clear ephemeral slot runtime". Make them differ or document them as aliases. | `launcher.nix:515–532` |
| L2 | `validate_repository` rejects `$STATE_ROOT/*` but not `$top == $STATE_ROOT` exactly, unlike the RUNTIME_ROOT / WORKSPACE_ROOT checks. Cosmetic inconsistency. | `launcher.nix:190` |
| L3 | `allowPublicInternet = true` is non-functional: the FORWARD ACCEPT exists but no NAT/MASQUERADE is configured, so RFC1918-sourced guests cannot actually reach the internet. Fine for the secure default; add a comment so a future relaxation doesn't "pass" the assertion while doing nothing. | `network.nix:143` |
| L4 | `net.bridge.bridge-nf-call-iptables=1` + `br_netfilter` is host-global and affects **all** bridges (docker/libvirt/NM), potentially changing behavior of unrelated bridged workloads on f13. Worth a comment; `-m physdev` scoping is possible if it ever bites. | `network.nix` (kernel/sysctl block) |
| L5 | `prefixLength` parsing assumes `subnet` contains `/`; no assertion on CIDR format nor that `gatewayAddress` ∈ `subnet`. Malformed values fail with an opaque `lib.toInt` eval error instead of a module assertion. | `network.nix` / `guest.nix` prefix derivation, `default.nix` assertions |
| L6 | `cmd_status` with no argument probes SSH for every slot with a 3 s connect timeout each — slow for stopped slots. UX only. | `launcher.nix` (`cmd_status`) |

## 4. Reviewed and found sound

- **`slots.nix`** — hex/bounds math correct (`maxSlotCount = 200` well
  inside both the MAC ≤ 239 and IP ≤ 244 ceilings; `toHexString (16 + i)` is
  always 2 hex digits; TAP names ≤ 15 chars); the uniqueness assertions in
  `default.nix` guard the exact generator the guests are built from.
- **Launcher flock protocol** — allocator lock held across free-check +
  marker write; per-slot lock; the cleanup trap is armed *after* the
  `flock -n 8` failure path, so a failed lock acquisition can never tear
  down another launcher's slot; cleanup keeps the clone in **all** paths;
  `''${SLOTS_DIR:?}` guards the `rm -rf`; the documented stale-slot
  trade-off (no auto-reclaim) is race-correct.
- **Input validation** — task/branch/agent validation is strict and applied
  before use; no `eval`; `chown -R` defaults to `-P` (no symlink
  traversal); the clone-escape checks (`git-dir` / `git-common-dir` inside
  the workspace) run before the chown, while the clone is still root-owned.
- **`guest.nix` share wiring** — share source
  `${cfg.stateRoot}/${slot.name}/workspace` exactly matches the launcher's
  `mount_point()`; exactly one share; locked down by
  `microvm-eval-workspace-share`.
- **Firewall** — metadata DROP first in both chains, ESTABLISHED after it,
  terminal DROP fail-closed in both FORWARD and INPUT; teardown mirrors
  setup; the LiteLLM socket is bridge-bound with `BindToDevice` (never
  0.0.0.0); the `systemd-socket-proxyd` service runs hardened
  (`DynamicUser`, `ProtectSystem=strict`). Rule ordering is correct.
- **Tests** — non-vacuous (positive control `baselineClean`, needle-pinned
  negative assertions) and honestly labeled eval-only.

## 5. Path to merge

1. Fix **H1 + H2** (scoped `safe.directory` on the specific git invocations
   or privilege drop; fail-closed guard semantics in `workspace-remove`).
   **Done** — see the Resolution notes under H1/H2.
2. Fix **M1** (unconditional guest addressing) and **M2**
   (`--preserve-env` or key-path option) — both one-liners.
3. Reword the **M3** comment (or fold into the A1 decision).
4. L1–L6 at leisure; none block merge.
5. Re-run `./nixfmtall.sh --check`, `nix flake check`, and the H1/H2 paths
   once runtime KVM validation (§40–§44) becomes possible — the fixed flows
   are exactly the ones the deferred runtime tier would have caught.
