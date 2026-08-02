<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — Recorded Validation Results & Security Assessment

This document records the **actually executed** repository validation
(plan §39), a **§45 security-acceptance-criteria** checklist mapped to concrete
config/eval-test evidence, and a **§48-format final report**.

- **Date executed:** 2026-08-02 (UTC); **Phase 8 addendum** re-run for the
  guest `/workspace` share + §11 UID/GID ownership fix.
- **Repository:** `maxhbr/myconfig`, worktree `improve-sandboxing`
- **Commit under test:** phase 8 working tree on top of
  `2a26b0fc51db2a08edcc3394187d2916749a29cd`
  (`feat(ai.microvm): enable on f13 and add eval/build test suite (phases 5-6)`).
  Phase 8 wires the guest virtiofs `/workspace` share (`guest.nix`) and the
  `chown -R 1000:1000` ownership strategy (`launcher.nix`), adds the
  `microvm-eval-workspace-share` eval check, and closes criterion 12.
- **Executed on:** the CI/dev host running this worktree — **no `/dev/kvm`**
  (`test -r /dev/kvm` → absent) and **no `agentbr0` bridge**
  (`ip link show agentbr0` → does not exist). This is an **eval/build-only**
  environment.

> [!IMPORTANT]
> **Honesty boundary.** Everything under *"Executed — EVAL/BUILD"* was run in
> this environment and its real output/exit status is recorded verbatim.
> Everything under *"NOT executed — requires real KVM/network"* (plan
> §40–§44) **was not run** because this host has no KVM device and no agent
> bridge. **No runtime, KVM-boot, guest-network, firewall-from-guest, or
> negative-launcher success is claimed here.** Those remain
> **NOT YET EXECUTED** and require the real f13 host with KVM hardware.

---

## 1. Executed — EVAL / BUILD validation (plan §39)

All commands were run from the repository root. Store paths are recorded so the
results are reproducible/auditable.

### 1.1 Formatting — `./nixfmtall.sh --check`

```
$ ./nixfmtall.sh --check
traversed 815 files
emitted 436 files for processing
formatted 0 files (0 changed) in 11ms
```

- **Result:** ✅ PASS (exit 0, `0 changed`).
- **Note:** re-run after this doc was added; the `traversed` count (815) and
  the `formatted N files` figure are transient nixfmt reporting artifacts (an
  earlier run showed `814 files` / `formatted 2 files`). The load-bearing
  signal — `0 changed`, exit 0 — is stable across runs (nixfmt is idempotent
  here).

### 1.2 `nix flake check`

```
$ nix flake check
...
checking derivation checks.x86_64-linux.microvm-eval-disabled...
checking derivation checks.x86_64-linux.microvm-eval-enabled...
checking derivation checks.x86_64-linux.microvm-slot-uniqueness...
checking derivation checks.x86_64-linux.microvm-eval-rejects-invalid...
checking derivation checks.x86_64-linux.microvm-eval-workspace-share...
checking derivation checks.x86_64-linux.microvm-guest-evaluates...
checking derivation checks.x86_64-linux.microvm-launcher-shellcheck...
...
running 0 flake checks...
all checks passed!
warning: The check omitted these incompatible systems: aarch64-linux
```

- **Result:** ✅ PASS (exit 0). All seven `microvm-*` checks (six original +
  the new `microvm-eval-workspace-share`) are present and evaluate cleanly.
- **Caveat (recorded honestly):** `nix flake check` reported
  `running 0 flake checks` — i.e. it *evaluated* the check derivations but the
  build products were already realisable/cached, so it built nothing new in
  that invocation. To prove the checks actually **build** (and, for the
  shellcheck/guest checks, actually **run** their build-time gate), each was
  built explicitly below (§1.3).

### 1.3 Explicit build of each of the 7 `microvm-*` checks

```
$ nix build .#checks.x86_64-linux.<name> --no-link --print-out-paths
```

| Check | Exit | Out path |
| --- | --- | --- |
| `microvm-eval-disabled` | 0 | `/nix/store/p0f803cv128sxl48lz7i5cpbxhdnf83n-microvm-eval-disabled` |
| `microvm-eval-enabled` | 0 | `/nix/store/7jwqds3g86r90lw4x9wkd774jvq743vn-microvm-eval-enabled` |
| `microvm-slot-uniqueness` | 0 | `/nix/store/rs8a3zdw8w4d1pikf3ly17gs5j4ham08-microvm-slot-uniqueness` |
| `microvm-eval-rejects-invalid` | 0 | `/nix/store/1qi2d2qi07brszalsnh9pi49qj9l9p1a-microvm-eval-rejects-invalid` |
| `microvm-eval-workspace-share` | 0 | `/nix/store/hbjk2ai4p69v7k99sc4fdsri2x4crsbv-microvm-eval-workspace-share` |
| `microvm-guest-evaluates` | 0 | `/nix/store/cagfnx6g6j5m3lvnm7jwcg4y8d7qa7l6-microvm-guest-evaluates` |
| `microvm-launcher-shellcheck` | 0 | `/nix/store/0lnpfdha4p6d46w2wfm5wdfvl1n8wl08-microvm-launcher-shellcheck` |

- **Result:** ✅ ALL PASS (exit 0). Note `microvm-launcher-shellcheck` is a
  real build — the `writeShellApplication` `shellcheck` gate actually executes
  for the host launcher, the guest `agent-run`, and the four workmux launchers.

### 1.4 `test-f13` system toplevel

```
$ nix build .#nixosConfigurations.test-f13.config.system.build.toplevel \
      --no-link --print-out-paths
```

- **Result:** ✅ PASS (exit 0).
- **Out path (phase 8):** `/nix/store/pfdih0f2dk00h1kyvll72j8hw2vgsd0i-nixos-system-f13-niri-26.11.20260730.1559d3d`
  (changed from the phase-6 `3j118nza…` path because `guest.nix` +
  `launcher.nix` changed).
- **Caveat — self-referential hash:** the `nixos-system` toplevel embeds the
  flake `self` narHash (the whole working tree), so editing *any* tracked file
  — **including this validation document** — shifts this out-path *without
  changing system behaviour*. (Verified: reverting only the doc nit-fixes
  yields the earlier `nlwrqszb…` path; the guest runner and the
  `microvm-*` check derivations, which depend on specific module files rather
  than `self`, are unaffected.) The value above is the build observed during
  validation of the code-complete tree; to reproduce the exact hash for your
  current tree, re-run the command — a doc-only delta is expected and benign.
- Benign eval warnings only (deprecated `system`/`xorg.*` renames, a
  `getExe`/`meta.mainProgram` note on `workmux`, and microvm.nix's
  `microvm.vsock.cid` notice). No errors.

### 1.5 Guest closure — actually built (not capped)

Per §39 the guest closure was built **explicitly and completely** (a 1200 s
timeout was set as a safety cap but was **not** hit — the build finished well
inside it):

```
$ nix build \
    .#nixosConfigurations.test-f13.config.microvm.vms.agent-0.config.config.microvm.declaredRunner \
    --no-link --print-out-paths
```

- **Result:** ✅ PASS (exit 0) — the full Cloud Hypervisor guest runner
  (guest kernel + closure, now including the `/workspace` virtiofsd share
  program) realised.
- **Out path (phase 8):** `/nix/store/cyzr3vimaxsz9x91r1nl9nrwihnwlv0h-microvm-cloud-hypervisor-agent-0`
  (changed from phase-6 `6x75q9yy…` — the guest now declares the workspace
  virtiofs share).

> This is stronger than the in-CI `microvm-guest-evaluates` check (which only
> forces the guest to `.drvPath` to keep `nix flake check` affordable). Here
> the guest was realised for real. It still proves **build**, not **boot**.

### 1.6 Host launcher — actually built

```
$ nix build --impure --expr '<find agent-microvm in test-f13 systemPackages>' \
      --no-link --print-out-paths
```

- **Result:** ✅ PASS (exit 0).
- **Out path (phase 8):** `/nix/store/h177130vn60p6ih0wzvaj3fp6a2nfnyw-agent-microvm`
  (changed from phase-6 `h6yqzj02…` — `launcher.nix` now chowns the clone to
  `1000:1000`).

### 1.7 Private-input `f13` build — SKIPPED (unavailable)

- **Status:** ⏭️ SKIPPED. The private overlay repo `../priv` is **not present**
  in this environment, and `nixosConfigurations` exposes only `test-f13` (no
  private `f13`). Per §39 ("f13 build when private inputs available"), this is
  correctly **not run** here.
- `test-f13` is the same host config generated from
  `nixosConfigurationsGen.host-f13` with the microvm feature enabled via
  `hosts/host.f13/ai.f13.nix`; the private `f13` differs only in private
  secrets/inputs, not in the microvm module wiring.

### 1.8 Guest `/workspace` share — eval now non-empty (§10, crit. 12)

```
$ nix eval .#nixosConfigurations.test-f13.config.microvm.vms.agent-0.config.config.microvm.shares --json
[{"cache":"auto","extraArgs":[],"mountPoint":"/workspace","posixAcl":true,
  "proto":"virtiofs","readOnly":false,"securityModel":"none",
  "socket":"agent-0-virtiofs-workspace.sock",
  "source":"/var/lib/microvms/agent-0/workspace","tag":"workspace"}]
```

- **Result:** ✅ the guest now declares **exactly one** virtiofs share — the
  writable `/workspace`, `source=/var/lib/microvms/agent-0/workspace` (the
  launcher's `mount_point()` target). Previously this returned `[]` (crit. 12
  GAP). The `microvm-eval-workspace-share` check (§1.3) locks this down and was
  verified to FAIL if the share is removed/renamed/repointed.

---

## 2. NOT executed — RUNTIME KVM / NETWORK / NEGATIVE tests (plan §40–§44)

**None of the following were run.** This host has no `/dev/kvm` and no
`agentbr0`, so a guest cannot boot and no packet can traverse the bridge.
They are recorded here as **NOT YET EXECUTED — requires real KVM hardware on
f13** so that no reader mistakes build success for runtime proof.

| Plan | Test class | Status |
| --- | --- | --- |
| §40 | Runtime host validation (`/dev/kvm` readable, CH opens KVM, `agentbr0` up, forwarder `ss -ltnp` on `192.168.83.1:4000`, `microvm@agent-0.service` running, workspace `findmnt`, TAP-in-bridge, no unexpected forwarding) | ❌ NOT YET EXECUTED |
| §41 | Guest validation (`id`/`hostname`/`findmnt /workspace`/`test -w`, reach `http://192.168.83.1:4000/v1`, **cannot** reach other ports/slots/LAN/WireGuard/router/`169.254.169.254`/internet/host sockets/host home/original repo; workspace host ownership) | ❌ NOT YET EXECUTED |
| §42 | Persistence test (only `/workspace` survives stop→destroy→restart; `/tmp` and `/home/agent` gone) | ❌ NOT YET EXECUTED |
| §43 | Negative launcher tests (reject `--repository /`, `--repository $HOME`, `--name ../bad`, `--name /tmp/bad`, duplicate name; concurrent duplicate → one wins; symlink escape; interrupted launch cleanup) | ❌ NOT YET EXECUTED |
| §44 | Network negative tests from guest (drops to `169.254.169.254`, `10.0.0.1`, `172.16.0.1`, `192.168.0.1`, other slot IPs, host SSH, host Nix daemon, WireGuard peers, internet) against the **real f13** config | ❌ NOT YET EXECUTED |

> The eval/build suite deliberately encodes the *shape* of some of these
> controls (terminal `FORWARD -j DROP`, the `169.254.169.254` drop, the
> bridge-only `ListenStream`, slot IP/MAC uniqueness, launcher validation via
> shellcheck) so regressions are caught at `nix flake check` time — but a
> passing eval check is **not** runtime proof that a packet is dropped or a
> guest is confined. Those require §40–§44 on real hardware.

---

## 3. Security acceptance criteria (plan §45)

Each criterion is marked as one of:

- **config** — satisfied by module configuration (file/line evidence).
- **eval-test** — additionally locked down by an executed eval/build check
  (§1.3).
- **runtime** — cannot be proven here; **requires §40–§44 on real KVM**.

Evidence paths are relative to
`modules/myconfig.ai/myconfig.ai.microvm/` unless noted.

| # | Criterion | Status | Evidence |
| --- | --- | --- | --- |
| 1 | Reuse existing `microvm.nix` input (no new/replaced input) | config | `flake.nix:52-53` (`microvm.url`); `guest.nix` imports `inputs.microvm.nixosModules.host`; no input added by this branch |
| 2 | Import host module only when enabled | config + eval-test | `guest.nix` imports host module unconditionally but neutralises with `microvm.host.enable = lib.mkDefault false`, flipped to `true` only under `lib.mkIf cfg.enable`; check `microvm-eval-disabled` asserts `microvm.host.enable == false` when disabled |
| 3 | Enable only on f13 | config | `hosts/host.f13/ai.f13.nix:30` `enable = true;` (inside the `microvm = {` block opened at line 29); NOT under `myconfig.ai.enable` (comment lines 22-28); default `enable=false` (`default.nix` `mkEnableOption`) |
| 4 | Cloud Hypervisor hypervisor | config + eval-test | `guest.nix` `microvm.hypervisor = "cloud-hypervisor"`; guest runner built as `microvm-cloud-hypervisor-agent-0` (§1.5) |
| 5 | Guest has its own kernel (host store not shared) | config | microvm.nix builds a self-contained guest store disk; guest closure realised independently (§1.5) |
| 6 | Fixed, bounded slot pool | config + eval-test | `slots.nix` `mkSlots`; `default.nix` `slotCount` `ints.positive`, asserted `<= maxSlotCount`; `microvm-eval-enabled` asserts exactly `slotCount` VMs; `microvm-slot-uniqueness` exercises pools 1..maxSlotCount |
| 7 | Unique, deterministic MAC/IP per slot | config + eval-test | `slots.nix` derives MAC/IP from index; `default.nix` uniqueness assertions; `microvm-slot-uniqueness` + `microvm-eval-rejects-invalid` |
| 8 | Disposable root/home; only `/workspace` persists | config + eval-test | guest root/home are ephemeral microvm.nix state; the host-side clone under `$WORKSPACE_ROOT/<task>` persists across stop/destroy (`launcher.nix` `cleanup_slot` never deletes the clone). `/workspace` is now surfaced into the guest via the virtiofs share (crit. 12), asserted by `microvm-eval-workspace-share`. **Persistence behaviour itself (only `/workspace` survives stop→destroy→restart) is runtime (§42).** |
| 9 | Standalone clone (`git clone --no-local`) | config | `launcher.nix:233` `git clone --no-local`; §24 |
| 10 | Original repo hidden from guest | config | only the standalone clone (never the origin) is bind-mounted host-side onto the slot's `/workspace` source, then surfaced into the guest by the single virtiofs share (crit. 12); `launcher.nix` repo validation refuses runtime/workspace/state roots (lines 176-184). **Guest-side confirmation is runtime (§41)** |
| 11 | No shared git common dir | config | `launcher.nix:214-222` verifies `git-dir`/`git-common-dir` resolve *inside* the clone; `--no-local` forbids alternates |
| 12 | Virtiofs workspace | config + eval-test | ✅ **WIRED (phase 8).** `guest.nix` declares EXACTLY ONE `microvm.shares` entry per slot: `{ proto = "virtiofs"; tag = "workspace"; source = "${cfg.stateRoot}/<slot>/workspace"; mountPoint = "/workspace"; }` (read-write). The `source` is the SAME host path the launcher bind-mounts the clone onto (`mount_point()` = `$STATE_ROOT/<slot>/workspace`), so the standalone clone is surfaced into the guest as `/workspace`. microvm.nix keeps `/nix/store` on its own storeDisk (no store share), so the guest has exactly this one share. **§11 ownership:** `launcher.nix` `chown -R 1000:1000` the clone so it appears owned by the guest `agent` user (uid 1000) — virtiofsd passes ownership through — making `agent-run`'s `test -w /workspace` pass. Locked down by `microvm-eval-workspace-share` (asserts one virtiofs `/workspace` share, correct tag/source, read-write; fails if removed). Eval `microvm.vms.agent-0.….microvm.shares` now returns the workspace share, not `[]` (§1.8). **End-to-end BOOT/write on live KVM is still runtime §41/§42, NOT executed here.** |
| 13 | Bind mount, not symlink | config | `launcher.nix:258` `mount --bind`; `findmnt` verify line 259 |
| 14 | Non-root guest agent | config + eval-test | `guest.nix` `users.users.agent` `isNormalUser`, `uid 1000`, `hashedPassword="!"`, `extraGroups=[]`; `agent-run` refuses root (`guest.nix`); `microvm-launcher-shellcheck` builds `agent-run` |
| 15 | Root SSH disabled | config | `guest.nix` `services.openssh.settings.PermitRootLogin = "no"` |
| 16 | Password SSH disabled | config | `guest.nix` `PasswordAuthentication = false`, `KbdInteractiveAuthentication = false` |
| 17 | Agent forwarding disabled | config | `guest.nix` `AllowAgentForwarding = "no"` (also `AllowTcpForwarding="no"`, `PermitTunnel="no"`, `X11Forwarding=false`) |
| 18 | No host home mount | config | `launcher.nix` mounts only the clone; repo validation rejects `$HOME` (line 177-178); **runtime confirm §41** |
| 19 | No host credentials in guest | config | `guest.nix` sets only `OPENAI_BASE_URL` + placeholder `OPENAI_API_KEY="not-needed"`; no `SSH_AUTH_SOCK`/`AWS_*`/`GITHUB_TOKEN`/... forwarded |
| 20 | No Nix daemon socket in guest | config | immutable guest, no host daemon socket mounted (guest.nix §8) |
| 21 | No Docker/Podman sockets in guest | config | no container runtime in guest package set; `extraGroups=[]` |
| 22 | Nix store not writable by guest | config | guest uses its own store disk; host store not shared (§5) |
| 23 | LiteLLM stays loopback-only | config | `network.nix` forwarder `ExecStart … 127.0.0.1:<port>`; main proxy untouched |
| 24 | Bridge-only forwarding endpoint | config + eval-test | `network.nix` socket `ListenStream="${gateway}:${port}"`, `BindToDevice=bridge`; `microvm-eval-enabled` asserts `ListenStream == 192.168.83.1:4000`, never `0.0.0.0`, and `BindToDevice == agentbr0` |
| 25 | No upstream API key in guest | config | `guest.nix` `OPENAI_API_KEY="not-needed"` placeholder; no real key in Nix store/argv |
| 26 | No public internet by default | config + eval-test | `default.nix` `allowPublicInternet=false` default + `acknowledgeInsecureNetwork` gate assertion; `network.nix` `internetVerdict=DROP` by default; `microvm-eval-rejects-invalid` proves the ack-gate fires; **packet drop is runtime (§44)** |
| 27 | No host LAN reachability | config | `network.nix` `privateRanges` DROP (incl. `192.168/16`, `10/8`, `172.16/12`); terminal `FORWARD -j DROP`; **runtime §44** |
| 28 | No WireGuard peer reachability | config | covered by RFC1918/terminal DROP in `network.nix`; **runtime §44** |
| 29 | No cloud metadata (`169.254.169.254`) | config + eval-test | `network.nix` unconditional `-d 169.254.169.254 -j DROP` in INPUT and FORWARD (first rule); `microvm-eval-enabled` asserts the `169.254.169.254` drop is present; **runtime §44** |
| 30 | No guest-to-guest traffic | config | `network.nix` `interVmVerdict=DROP` (subnet→subnet), `br_netfilter` + `bridge-nf-call-iptables=1`; NM leaves TAPs unmanaged; **runtime §44** |
| 31 | Bounded CPU/memory | config | `guest.nix` `vcpu=cfg.defaultVcpu`, `mem=cfg.defaultMemoryMiB`; `default.nix` positive-int assertions |
| 32 | Bounded VM count | config + eval-test | fixed slot pool; `slotCount` bounded (crit. 6) |
| 33 | Lock-protected allocation | config | `launcher.nix` global `allocator.lock` `flock 9` (`exec 9>`/`flock 9` lines 406-407) + per-slot `flock 8` (lines 416-417); global lock released at `flock -u 9` line 437 (block ~406-438); §21 |
| 34 | Stop/destroy keep the workspace | config | `launcher.nix` `cmd_destroy` "workspace kept" (line 513); `workspace-remove` is a separate guarded op (`cmd_workspace_remove`) refusing uncommitted/unexported work; guards use scoped `safe.directory` and fail closed on git error (review H2 resolution) |
| 35 | Workmux stays the UI/frontend | config | `workmux.nix` registers agents into existing `myconfig.ai.workmux.agents`; launcher is backend only; user flow `workmux add --agent microvm-claude …` |
| 36 | Existing jails still work | config | this branch adds a parallel tier; no change to existing jail/sandboxed-pi modules |
| 37 | Formatting passes | eval-test (executed) | §1.1 `./nixfmtall.sh --check` exit 0 |
| 38 | `nix flake check` passes | eval-test (executed) | §1.2 exit 0, all 7 microvm checks present (incl. microvm-eval-workspace-share) |
| 39 | `test-f13` builds | eval-test (executed) | §1.4 toplevel built |
| 40 | Guest config builds | eval-test (executed) | §1.5 guest runner built in full |
| 41 | At least one VM tested on real KVM | **runtime** | ❌ NOT YET EXECUTED — no `/dev/kvm` here (§40–§42) |
| 42 | Firewall tested from guest | **runtime** | ❌ NOT YET EXECUTED — no bridge/guest here (§44) |

**Summary:** criteria **1–40** are satisfied by config and/or the executed
eval/build suite — including **criterion 12 (Virtiofs workspace), now WIRED
and eval-tested (phase 8)**: the guest declares exactly one virtiofs
`/workspace` share whose source matches the launcher bind-mount target, and
the launcher chowns the clone to uid/gid 1000 (§11) so the guest `agent` user
can rw it. The two explicitly runtime-only items — **41 (a VM tested on real
KVM)** and **42 (firewall tested from the guest)** — remain **NOT YET
EXECUTED** and require the real f13 host with KVM. The interactive
`run --attach` flow is now **wired end-to-end in config**, but actually
**booting** a guest and writing to `/workspace` remains a runtime step
(§41/§42) not executed here.

---

## 4. Final report (plan §48 format)

### Files changed (feature as a whole, phases 0–7)

- `modules/myconfig.ai/myconfig.ai.microvm/default.nix` — option namespace,
  assertions, module registration.
- `modules/myconfig.ai/myconfig.ai.microvm/slots.nix` — deterministic slot
  table (name/MAC/IP/TAP from index).
- `modules/myconfig.ai/myconfig.ai.microvm/guest.nix` — host microvm.nix
  integration, fixed slot pool, minimal Cloud Hypervisor guest, `agent-run`.
- `modules/myconfig.ai/myconfig.ai.microvm/network.nix` — private bridge,
  proxy-only firewall, bridge-only LiteLLM forwarder.
- `modules/myconfig.ai/myconfig.ai.microvm/launcher.nix` — host `agent-microvm`
  launcher (run/stop/status/ssh/console/destroy/list/workspace-remove),
  validation, lock-based slot allocation, bind-mount lifecycle.
- `modules/myconfig.ai/myconfig.ai.microvm/workmux.nix` — Workmux agent
  registrations (`microvm-claude/-pi/-codex/-opencode`).
- `modules/myconfig.ai/default.nix` — imports the new module.
- `hosts/host.f13/ai.f13.nix` + `hosts/host.f13/dedicated-agent-vm-key.pub` —
  enables the feature on f13 with the secure default profile.
- `tests/microvm.nix` + `flake.nix` — eval/build test suite wired into
  `nix flake check`.
- `modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm.md` — user/operator documentation.
- `modules/myconfig.ai/myconfig.ai.microvm/docs/agent-microvm-validation.md` — **this document** (phase 7).

### Architecture

Host f13 imports `inputs.microvm.nixosModules.host` (only when
`myconfig.ai.microvm.enable`). A fixed pool of slots `agent-0..agent-3` is
declared as microvm.nix Cloud Hypervisor VMs, each with a deterministic
MAC/IPv4/TAP. A NetworkManager-compatible bridge `agentbr0`
(`192.168.83.1/24`) carries the TAPs; a dedicated iptables chain set
(`AGENT_MICROVM_INPUT/FORWARD/OUTPUT`) enforces a **proxy-only, fail-closed**
policy. The guest is a minimal NixOS with its own kernel, an unprivileged
`agent` user, a single writable virtiofs `/workspace` (a `--no-local`
standalone clone, chowned to uid/gid 1000 so the guest `agent` user can rw it,
§11), and model-API access via the bridge-only LiteLLM forwarder → loopback
`127.0.0.1:4000`.

> [!NOTE]
> **Phase 8 update:** the guest-side virtiofs `/workspace` share is now
> declared (crit. 12), and the launcher chowns the clone to uid/gid 1000
> (§11) so the guest `agent` user can rw it. The `run --attach` workspace flow
> is **wired end-to-end in config** and eval-tested. Actually booting a guest
> and confirming the write path is still a runtime step (§41/§42) NOT executed
> here — no `/dev/kvm` in this environment.

### Workmux integration

Workmux remains the frontend. Four agents (`microvm-{claude,pi,codex,opencode}`)
are registered into the existing `myconfig.ai.workmux.agents` registry; each
agent's `command` is a thin launcher that resolves the linked main repo,
derives a launcher-safe task name from the branch, and execs
`sudo agent-microvm run --attach --name <task> --repository <repo> --agent <bin>`
with **no** network-relaxation flags (secure proxy-only profile). User workflow
stays `workmux add --agent microvm-claude feature-name`.

### Security boundary

The guest is treated as hostile. Confinement rests on: a separate guest kernel
(Cloud Hypervisor/KVM), no shared host store or Nix daemon, no host home/creds
mounted, exactly one writable host path (`/workspace`, a standalone clone with the
origin hidden — now wired into the guest via a single virtiofs share, crit.
12, owned by uid/gid 1000 so the guest `agent` user can rw it while no guest
id maps to a privileged host id, §11), a non-root agent user with
hardened/optional SSH, and a
fail-closed proxy-only firewall whose only permitted egress is
`192.168.83.1:4000`. The cloud-metadata IP is dropped unconditionally and
first. Insecure relaxations are off by default and gated behind an explicit
`acknowledgeInsecureNetwork`.

### Validation performed

- **Executed (eval/build):** `./nixfmtall.sh --check` ✅; `nix flake check` ✅
  (7 microvm checks); explicit build of all 7 checks ✅; `test-f13` toplevel ✅;
  full guest runner ✅; host launcher ✅. See §1 for commands + store paths.
- **NOT executed (runtime KVM/network/negative, §40–§44):** ❌ NOT YET
  EXECUTED — this environment has no `/dev/kvm` and no `agentbr0`. No guest was
  booted; no firewall/network/persistence/negative-launcher behaviour was
  exercised on real hardware. See §2.
- **Skipped:** private-input `f13` build — `../priv` unavailable (§1.7).

### Remaining limitations

- **CH/KVM/guest-kernel/virtiofs attack surface:** a guest kernel + hypervisor
  is trusted at the CH/virtiofsd boundary; a CH or virtiofsd escape would
  breach the sandbox. Untested at runtime here.
- **Guest `/workspace` is wired (phase 8) but not KVM-verified:** `guest.nix`
  now declares the single virtiofs `/workspace` share (source =
  `$STATE_ROOT/<slot>/workspace`, the launcher's bind-mount target), and
  `launcher.nix` chowns the clone to uid/gid 1000 (§11) so the guest `agent`
  user (uid 1000) sees it as agent-owned and writable (virtiofsd passes
  ownership through). This closes the previous crit. 12 gap and the
  known-broken `run --attach` writability failure **in config**. Confirming
  the mount + write path on a booted guest is runtime (§41/§42), NOT executed
  here.
- **Writable-workspace exposure:** `/workspace` is host-backed and writable;
  the standalone clone's contents (owned by uid/gid 1000 on the host, i.e. the
  primary unprivileged interactive user) are exposed to the (hostile) guest
  agent. Runtime host-ownership confirmation is §41.
- **Prompt/source disclosure:** the agent sends workspace source + prompts to
  the model API via the host LiteLLM proxy — an intentional data-egress path.
- **Resource exhaustion / host-proxy DoS:** vCPU/mem/slot-count are bounded,
  but a guest can still saturate its slot and hammer the shared LiteLLM proxy.
- **Future public-internet risk:** relaxations exist behind an explicit
  acknowledgement; enabling them widens egress substantially.
- **Firewall-ordering-dependent controls:** the proxy-only policy relies on
  the dedicated chains being installed and `bridge-nf-call-iptables=1`;
  ordering/teardown correctness is asserted structurally but **not** verified
  by packet path here.
- **IPv6:** disabled on the bridge (MVP limitation, §15); no equivalent IPv6
  policy exists yet.
- **Untested controls:** every §40–§44 runtime control (KVM boot, guest
  confinement, firewall drops, persistence, negative-launcher rejection) is
  **NOT YET EXECUTED**.
- **test-f13 vs private f13:** validation ran against `test-f13`; the private
  `f13` differs only in private secrets/inputs, not in the microvm wiring, and
  was not built (no `../priv`).
