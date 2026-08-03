<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — Remaining Work / Definition-of-Done

Companion to [`agent-microvm.md`](./agent-microvm.md) (user/operator docs) and
[`agent-microvm-validation.md`](./agent-microvm-validation.md) (recorded
eval/build results + §45 checklist). This file answers **"are we there yet?"**
against the 8-phase plan and the goal spec
[`microvm-sandbox-plan.md`](./microvm-sandbox-plan.md), and lists the exact
remaining work.

## 1. Verdict — split

**Config + eval/build tier: REACHED.** All 8 phases are implemented and wired.
The six module files build and evaluate; all 7 `microvm-*` checks pass under
`nix flake check`; `./nixfmtall.sh --check` is clean; the virtiofs `/workspace`
share is now populated (`source=/var/lib/microvms/agent-0/workspace`, `virtiofs`,
`readOnly=false`, `tag=workspace`) and §11 ownership is handled via
`launcher.nix` `chown -R 1000:1000` on the clone. 40 of 42 §45 criteria are
satisfied at config/eval level.

**Runtime / KVM tier: NOT REACHED.** Plan §40–§44 and §45 criteria 41
("≥1 VM tested on real KVM") and 42 ("firewall tested from guest") have **not
been executed** — the CI/dev environment has no `/dev/kvm`, no `agentbr0`, no
live guest. The eval suite encodes the *shape* of the controls (firewall DROP
rules, slot uniqueness, workspace share, shellcheck-clean launcher) but shape is
not a packet-path / boot proof.

> Per plan §46 (last clause) and §48: **do not claim build success proves the
> runtime firewall is secure.** The goal is therefore **config-complete but not
> yet fully reached** until the runtime tier below is executed on real f13
> hardware and recorded.

## 2. Per-phase status

| Phase | Status | Remaining |
| --- | --- | --- |
| 0 — skeleton + options + assertions + registration (§1, §37) | **DONE** | — |
| 1 — host microvm.nix import + fixed slot pool (§2, §4) | **DONE** | — |
| 2 — minimal CH guest: user/pkgs/virtiofs/SSH/agent-run/proxy env (§5–11, §17–19) | **DONE (eval)** | Runtime boot + `/workspace` write/ownership (§41/§42) |
| 3 — bridge + AGENT_MICROVM_* firewall + IPv6 off + socket-proxyd (§12–16) | **DONE (eval)** | Packet-path proof from guest (§44); guest-to-guest decision (A1) |
| 4 — launcher + flock + clone + bind-mount + validation (§20–28, §33–35) | **DONE (eval)** | Behavioral negative-launcher tests (§43) |
| 5 — Workmux registrations (§29–31) | **DONE** | Optional sudoers UX (A2) |
| 6 — f13 enablement + dedicated SSH key (§3) | **DONE** | Real private-`f13` toplevel build (A3) |
| 7 — docs + assertions + tests + validation (§36–39, §45–48) | **DONE (eval tier)** | Fill §40–44 runtime results; tick §45 crit 41–42; finalise §48 report |

Phases 0, 1, 5, 6 fully DONE. Phases 2, 3, 4, 7 are DONE at eval/build tier with
a runtime-pending tail — none is MISSING or PARTIAL in code, only in *executed
validation evidence*.

## 3. Gap-closure plan

### (A) CI-doable (no KVM) — can be done now

| # | Item | Plan | File(s) | Acceptance | Effort/Risk |
| --- | --- | --- | --- | --- | --- |
| A1 | ~~Guest-to-guest isolation: blocked only by terminal `FORWARD -j DROP`; no per-TAP L2 `isolated` flag.~~ **RESOLVED (improvement ticket 3 A):** each slot's `agent-microvm-attach-<slot>` oneshot now runs `bridge link set dev <tap> isolated on` after enslaving the TAP, so the bridge drops guest↔guest frames for EVERY EtherType (ARP / IPv6 ND included), independently of iptables. Locked down per slot by `microvm-eval-enabled`. Runtime guest→guest DROP proof is still B4. | §14, §44 | `network.nix` | `isolated` set on each TAP ✅ | Low / Med (defense-in-depth) |
| A2 | Passwordless sudoers scoped to `agent-microvm` so `workmux add --agent microvm-*` doesn't prompt. Currently deferred. | §29 | new sudoers snippet + `workmux.nix` | Launch without password prompt; rule tightly scoped to the launcher only. | Low / Low-Med (privilege surface) |
| A3 | Build the real private `f13` toplevel (not just `test-f13`). §44 explicitly requires the real host config. | §44, §45 | `hosts/host.f13` | `nix build .#nixosConfigurations.f13...toplevel` succeeds with `../priv` present. | Low / Low (needs private inputs) |

Constraint-safe: none add a new microvm.nix input, none add KVM checks to
`nix flake check`, none weaken defaults, none commit private keys.

### (B) Runtime KVM/network/negative tier (§40–44) — execute on real f13

Run on f13 with `/dev/kvm`; capture stdout/exit-codes into
`agent-microvm-validation.md`. **Checklist, not code.**

| # | Plan | Run | Evidence |
| --- | --- | --- | --- |
| B1 Host | §40 / §45-41 | `test -r /dev/kvm`; `ip addr show agentbr0`; `ss -ltnp \| grep 192.168.83.1:4000`; `systemctl status microvm@agent-0`; `findmnt /var/lib/microvms/agent-0/workspace`; TAP ∈ bridge; no unexpected forwarding | KVM readable; bridge up; forwarder on bridge addr only; unit active; workspace mounted; TAP enslaved |
| B2 Launch+guest | §41 / §45-41 | `sudo agent-microvm run --name t --repository <repo> --attach --agent pi`; in guest: `id; hostname; findmnt /workspace; test -w /workspace; ip addr; ip route` | agent uid 1000 unprivileged; `/workspace` virtiofs mounted writable, correct ownership; static addr/route |
| B3 Proxy reach | §41 | from guest reach `http://192.168.83.1:4000/v1` | end-to-end HTTP to bridge endpoint works |
| B4 Network-negative | §44 / §45-42 | from guest confirm **failure** to `169.254.169.254`, `10.0.0.1`, `172.16.0.1`, `192.168.0.1`, other slot IPs, host SSH, host Nix daemon, host dev services, WireGuard peers, internet, and `192.168.83.1` on ports ≠ 4000. **Use real f13 config, not test-f13.** | all DROP except `192.168.83.1:4000` |
| B5 Persistence | §42 | create files in `/workspace`, `/tmp`, `/home/agent`; stop; destroy; restart same workspace | `/workspace` persists; `/tmp` + `/home/agent` gone |
| B6 Negative-launcher | §43 | reject `--repository /`, `--repository $HOME`, `--name ../bad`, `--name /tmp/bad`, dup name; concurrent dup → single winner; symlink cannot escape roots; interrupted launch cleans VM/bind-mount/lock/metadata/TAP but keeps clone | each rejected/handled; cleanup preserves clone |

Effort ≈ 1 focused f13 session. Risk Med-High: first real boot can surface
virtiofsd ownership/ACL surprises, iptables rule-ordering gaps, or
socket-proxyd binding issues not caught by eval.

### (C) Documentation / security-assessment finalisation

| # | Plan | File | Acceptance |
| --- | --- | --- | --- |
| C1 | §45 | `agent-microvm-validation.md` | Tick criteria 41 & 42 once B1–B6 pass; keep all 42 items accurate |
| C2 | §40–44 | `agent-microvm-validation.md` | Paste actual executed outputs; keep "not executed" for anything still skipped |
| C3 | §48 | validation doc | Finalise report: Files changed / Architecture / Workmux / Security boundary / Validation (separate eval vs runtime, claim only executed) / Remaining limitations |
| C4 | §46 | docs | Re-affirm no build-only runtime-security claim; record A1 decision + A2 resolution |

## 4. Sequencing & dependencies

**Now, in CI (no ordering blockers):** A1 decision (+optional `isolated`), A2
sudoers, A3 private-f13 build, C1–C4 scaffolding (fill after B).

**Hardware-gated (real f13 + `/dev/kvm`):** B1 → B2 → B3 → B4 → B5 → B6.

**Recommended order:**
1. A1, A2 (config decisions) — land before booting so runtime tests validate the final config.
2. A3 — build real f13 toplevel (catches private-input divergence pre-boot).
3. Deploy to f13; run B1 (gates all) → B2 → B3 → B4 (highest-value security proof) → B5 → B6.
4. C1–C4 — record results, finalise §48 report.

Hard chain: B2⇐B1; B3/B4/B5⇐B2; C1/C2⇐B1–B6; A1/A2 precede B (else re-run B).

## 5. Definition of Done

### Eval / build DoD — ✅ MET
- [x] All 6 module files build & evaluate.
- [x] All 7 `tests/microvm.nix` checks pass under `nix flake check`.
- [x] `./nixfmtall.sh --check` exits 0.
- [x] `microvm.shares` populated (`/workspace` virtiofs, writable, source == launcher target).
- [x] §11 ownership wired (`launcher.nix` `chown 1000:1000`).
- [x] All assertions fire (`microvm-eval-rejects-invalid`).
- [x] Feature disabled ⇒ no VMs (`microvm-eval-disabled`).
- [x] Secure defaults intact; single microvm.nix input; no private key committed; no KVM test in `nix flake check`.
- [ ] (A3) Real private-`f13` toplevel builds (pending `../priv`).

### Runtime / validation DoD — ❌ NOT YET MET (blocks "goal reached")
- [ ] B1 §40 host checks pass.
- [ ] B2 §41 guest: unprivileged agent, `/workspace` writable + correct ownership, disposable `/tmp`+`/home`.
- [ ] B3 §41 guest reaches `http://192.168.83.1:4000/v1`.
- [ ] B4 §44 firewall proven from guest (DROP all but `192.168.83.1:4000`).
- [ ] B5 §42 persistence (only `/workspace` survives).
- [ ] B6 §43 negative-launcher cases handled.
- [ ] §45 crit 41 ("≥1 VM on real KVM") ticked.
- [ ] §45 crit 42 ("firewall tested from guest") ticked.
- [x] (A1) Guest-to-guest isolation implemented at L2 (`isolated` per TAP) —
      runtime verification (`bridge link show`, guest→guest ping/ARP) still
      pending as part of B4.

### Deferred / optional (not blocking)
- [ ] A2 passwordless sudoers UX.

## 6. Residual risks
- **First-boot ownership:** virtiofsd may enforce ownership/ACL differently than the eval-modeled `chown` assumes (B2).
- **Firewall ordering:** B4 is the single highest-risk gate — controls depend on `br_netfilter` + `bridge-nf-call-iptables` + exact iptables rule ordering, none proven at packet level.
- **Guest-to-guest:** now belt-and-suspenders — per-TAP L2 `isolated` (bridge-level, all EtherTypes) *plus* the IPv4 `FORWARD` DROP, so an iptables rule-ordering regression alone can no longer open it. Packet-level proof is still B4.
- **test-f13 vs private f13:** §44 requires the real host config; skipping A3 risks validating the wrong config.
- **Constraint adherence:** keep all runtime tests OUT of `nix flake check`; keep any sudoers rule tightly scoped.

**Bottom line:** implemented and fully eval-verified across all 8 phases;
the goal is **config-complete but not fully reached**. Remaining work is
exclusively the **§40–44 runtime/KVM tier on real f13** (plus §45 crit 41–42 and
the §48 final report), with the sudoers nicety (A2) as a minor open item
(A1's L2 isolation has since been implemented, see the (A) table).
