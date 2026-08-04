<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — security model

What this tier does and does not protect, stated so it can be argued with.
Mechanisms are described in the
[architecture document](./agent-microvm-architecture.md).

**Verification status:** every property below is covered by the eval/build tier
(`nix flake check`), i.e. *designed and eval-asserted*. Runtime properties are
only *measured* once the [runtime suite](./agent-microvm-runtime-validation.md)
is run on real KVM. The batch result channel additionally has *executed* checks
that are not runtime proofs: `microvm-batch-result-integrity` really runs the
host result verifier and the guest-side permission assertions against forged,
stale and malformed fixtures; `microvm-batch-controller-smoke` really runs the
guest controller (with `systemctl` stubbed) and feeds its output to the host
verifier; and `microvm-batch-launcher-submit` really runs `agent-microvm submit`
against a stubbed guest that plants forged, stale and malformed results. All
three run inside a build sandbox, so they validate the layout, the validators and
the host/guest protocol — not the guest kernel's enforcement.

## Trusted vs untrusted

**Trusted (the TCB — a bug here defeats the tier):**

- the host kernel, KVM, Cloud Hypervisor, virtiofsd, the guest kernel;
- microvm.nix and this module's own Nix + shell code;
- the host LiteLLM proxy (it holds the only real upstream credential);
- the host operator (already a full sudoer) and systemd on the host;
- the agent packages as *builds* (their content is baked in at build time).

- the guest-side **batch job controller** (`agent-job-controller.service`, guest
  root): it validates the job, starts the worker under another uid, enforces the
  deadline and writes the authoritative result. A bug here is a bug in the TCB.

**Untrusted (assumed hostile):**

- everything the agent does inside the guest, including the agent binary's
  runtime behaviour and any model output it acts on;
- the repository content that becomes `/workspace` (hooks, `flake.nix`, `.envrc`,
  MCP config, scripts, symlinks);
- prompts and any data the agent fetches through LiteLLM;
- **everything the batch worker produces** (`agent-job-worker@<agent>.service`,
  guest `agent`): the CONTENT of its stdout/stderr logs, its artifacts, any file
  it writes into `/workspace` or `/run/agent-job/worker/`, and any JSON that
  *looks* like a result. None of it is evidence of anything;
- other guests.

## The batch result channel

The host acts on the batch result: it decides success/failure, stops the VM and
reports an exit code. So the result must be attributable, not merely readable.

- **Two guest identities.** The trusted controller (root) and the untrusted
  worker (`agent`, uid `guestAgentUid`) are separate units. The controller never
  executes a repository- or spec-provided command; the executable and argv come
  from the build-time registry (`agents.nix`), keyed by the worker unit's
  instance name.
- **The result directory is not reachable by the worker.**
  `/run/agent-job/controller/` is `root:root 0700` (virtiofsd passes ownership
  through, so that is the *effective* permission inside the guest) and the worker
  unit additionally masks it with `InaccessiblePaths`. The worker cannot write,
  read, delete or list it, and cannot rename or shadow it either, because
  `/run/agent-job` is root-owned `0755`.
- **The spec is root-only (`0400`).** It carries the allocation token, so the
  worker cannot read the value it would need to mint a plausible result. The
  prompt is `0444`: readable by the worker, writable by nobody.
- **The token never travels in an argument vector.** `/proc/<pid>/cmdline` is
  world-readable (`0444`) and the worker shares the guest PID namespace, so a
  helper invoked as `jq --arg allocationToken <token>` would hand the token to
  the worker (and, on the host, to every local user) for as long as it lived.
  Both sides therefore pass it in the ENVIRONMENT (`/proc/<pid>/environ` is
  `0400`), and the host result verifier reads the expected token from
  `AGENT_JOB_EXPECTED_TOKEN` and *refuses* a `--token` argument. The worker unit
  additionally sets `ProtectProc=invisible`, so processes it does not own — i.e.
  every process of the trusted controller — have no visible `/proc` entry at all.
  This is checked BY EXECUTION: `microvm-batch-controller-smoke` and
  `microvm-batch-launcher-submit` bind an argv recorder over the exact `jq` the
  controller/launcher resolve and require that the active token never appears in
  any recorded argv (while also requiring that the recorder saw invocations and
  that the token *is* in the resulting documents, so the check cannot pass
  vacuously).
- **The worker's log files are root-owned, outside its own directory.** systemd
  (PID 1, root) opens `worker-logs/stdout.log`/`stderr.log` with `append:` and
  follows symlinks, so they must not sit in a directory the worker uid can write:
  `worker/` is agent-owned, so anything running as uid 1000 could otherwise
  rename it and plant a symlink between the controller's check and the worker
  start, redirecting a root-opened append fd. `worker-logs/` is `root:root 0755`
  directly under the root-owned `0755` share root, and the files are
  `root:root 0644` — the worker can read its own logs but cannot truncate,
  replace or redirect them. Their CONTENT is untrusted all the same.
- **Allocation tokens.** Each allocation gets 256 bits from `/dev/urandom`,
  recorded in the host session marker, in the guest's immutable input and in the
  result. The host rejects a result unless schema version, controller version,
  task id, allocation token, slot and agent all match the ACTIVE allocation, so a
  stale or cross-allocation result cannot terminate a job — and a stale
  cancellation request cannot stop a newly allocated one.
- **Atomic rename gives consistency, not authenticity.** Writing a temp file and
  renaming it means the host never reads a half-written document. It says nothing
  about *who* wrote it. Authenticity comes from ownership plus the token.
- **Worker output is never authoritative.** The controller derives the outcome
  only from what it observed itself: the worker's exit status (from systemd), its
  own timeout, its own cancellation decision, or an infrastructure error it hit.
  Never from agent output text, a workspace file, a worker-written JSON document
  or a "completion marker".
- **The host treats the result as untrusted input anyway.** One verifier
  (`agent-job-verify-result`) requires a regular, non-symlink, root-owned,
  size-bounded file in a root-owned, non-group/other-writable directory, parses
  it strictly, rejects unknown fields, and checks the identity, the terminal-state
  enum, the exit-code range and the timestamps. A malformed or foreign document
  becomes an INFRASTRUCTURE ERROR — never a success. The archived result is the
  validated document (tagged `source: "controller"`) or an explicitly
  host-generated record (`source: "host"`), never a raw guest file.
- **virtiofs permissions are part of the trusted configuration.** The modes in
  `job.nix`/tmpfiles *are* the boundary. The guest controller therefore asserts
  them at startup (`agent-job-assert-paths`) — including every parent of the
  result directory — and refuses to run if they are wrong, because mode `0600` on
  a file is worthless if a parent directory can be replaced.
- **Future direction.** VSOCK may replace the filesystem result channel (a
  per-slot CID is already reserved). That would remove the shared-directory
  question entirely; it does not change the trust split described here.

## What the VM boundary protects

- **Kernel/syscall isolation.** A guest compromise is confined to its own kernel;
  escaping requires a hypervisor/virtiofsd/KVM bug, not a container escape.
- **Disposable state.** Guest root and `/home/agent` are tmpfs, rebuilt every
  boot. An agent cannot leave persistent implants in its own sandbox.
- **No host filesystem.** The guest sees only the four declared virtiofs shares
  (see below) — no host home, no `/nix/store`, no sockets.
- **No build/fetch capability.** No Nix daemon socket, no store write access; the
  guest can only run what was baked in.
- **Bounded resources.** Per-class `CPUQuota`/`MemoryMax`/`TasksMax` inside the
  guest, plus host-side `MemoryMax`/`TasksMax`/`CPUWeight`/`IOWeight` on the
  hypervisor unit; three independent timeout layers for batch jobs.
- **Non-root agent.** uid `guestAgentUid` (1000, asserted unprivileged), locked
  password, no extra groups, `NoNewPrivileges`, `PrivateDevices`,
  `ProtectKernel*`, `RestrictSUIDSGID` for batch jobs.

## What is still shared through virtiofs

Exactly four shares, all per-slot:

| Guest path | Mode | Content | Risk |
| --- | --- | --- | --- |
| `/workspace` | rw | the task's standalone clone | the agent can write anything into your task's clone; you review it before importing |
| `/var/lib/agent-hostkey` | **ro** | that slot's SSH host key, `root:root 0400` | unreadable by the guest agent user; the guest cannot change its own identity |
| `/run/agent-job` | rw | `input/` (`spec.json` `root:root 0400`, `prompt.md` `0444`), `controller/` (`root:root 0700`, the authoritative result), `worker/` (agent-owned), `worker-logs/` (`root:root 0755`, the log files systemd opens as root) | the agent can write junk into `worker/`; it cannot read or write `controller/`, cannot read the token-bearing spec, cannot alter its own spec/prompt, and cannot replace or redirect its own log files |
| `/var/lib/agent-state` | rw | only when `--persist-agent-state`: the task's declared agent-state dirs | the agent can poison ITS OWN task's future state; other tasks are unreachable |

virtiofsd is in the TCB for all four. Ownership is passed through unchanged, so
host-side modes are what the guest sees.

## What LiteLLM protects

- The **upstream credential** never leaves the host: the guest holds placeholders
  and endpoint URLs only, so a compromised guest cannot exfiltrate your API key.
- It is the **only** reachable egress in the default profile, so model traffic is
  the only traffic — the proxy is a single, inspectable choke point.
- It does **not** protect the *content* of what you send: prompts, source and
  agent output are disclosed to whatever the proxy forwards to. That is inherent
  to using a model.

## Mitigated attacks

| Attack | Mitigation |
| --- | --- |
| exfiltrating host API keys | keys exist only in the host proxy; guest has placeholders |
| stealing host SSH/GPG/cloud credentials | no host home, no agent sockets, no credential files in the guest |
| reaching the host LAN / VPN / router | private-range DROP in every profile |
| cloud-metadata SSRF (`169.254.169.254`) | dropped first, unconditionally, in INPUT and FORWARD |
| arbitrary internet egress / data smuggling | denied in `offline`/`proxy-only`; `package-access` allows one host proxy port; only `internet` routes, and then with NAT + a DNS allowlist + drop logging |
| DNS tunnelling | port 53 only to configured resolvers, everything else dropped |
| guest → guest attacks, ARP spoofing, MITM of host↔guest | per-TAP `isolated` bridge ports (all EtherTypes) + IPv4 inter-VM DROP + strict SSH host-key verification |
| impersonating a slot to the operator | per-slot host keys, delivered read-only and root-only, pinned in `known_hosts` |
| persistence across sessions | disposable tmpfs root/home; persistence is opt-in, declared-paths-only and task-scoped |
| cross-task contamination | per-task clone + per-task state; the slot's share sources are cleared/rebound per run |
| host code execution from a hostile repo | the launcher never evaluates repo-provided nix/direnv/hooks/npm/make/MCP (asserted by a check); clones use `--no-local`; git-dir/common-dir escapes rejected; all paths canonicalised |
| runaway resource use | per-class cgroup limits on both sides; three timeout layers; `usage` makes retained disk visible |
| stale/lost slots after crashes | allocation tokens + pid/pid-start ownership, `recover` (with `--dry-run`) |
| **forging a batch result** (report success, hide a failure, fake an exit code, stop the VM early) | the result is written only by the trusted guest controller into a `root:root 0700` directory the worker cannot write, read, rename or shadow; the host verifies ownership, schema, task, slot, agent and allocation token |
| **replaying a result / cancellation from an earlier allocation** | 256-bit per-allocation token in marker + guest input + result; a mismatch is an infrastructure error, and cancellation requests are token-bound |
| **misreporting via malformed output** | one strict verifier; unknown fields, wrong types, non-terminal states, bad exit codes and unparsable JSON all become infrastructure errors, never success |
| **a repository process outliving its job** | the worker is a separate systemd unit killed as a `control-group`, so double-forked descendants die with it |
| prompt/secret leakage into logs | structured events log identities only — never prompt content, keys or env vars |

## Residual risks

- **Hypervisor/virtiofsd/KVM escape.** Not mitigated; it is the boundary.
- **Runtime properties are not yet measured.** The firewall, L2 isolation,
  credential absence and limit containment have not been observed on real KVM
  from a live guest. Run the [runtime suite](./agent-microvm-runtime-validation.md).
- **`/workspace` is writable by a hostile agent.** Review diffs before importing;
  a malicious commit is a *your-review* problem, not a sandbox problem.
- **Disclosure through the model.** Prompts and source go wherever LiteLLM
  forwards them.
- **IPv6 is disabled rather than policed.** No IPv6 firewall policy exists; the
  bridge simply has IPv6 off. A future IPv6 need means writing that policy.
- **Guest ARP/L2 relies on a kernel bridge flag.** `isolated on` is set by a
  per-slot oneshot; if that unit fails, only the IPv4 FORWARD DROP remains
  (which does not cover ARP). The `recover`/status path does not currently
  re-check the flag — `bridge link show` does.
- **VSOCK is reserved, not used.** Batch control still rides SSH-less file
  exchange through the job share; the CID exists but no vsock transport is wired.
  The result channel's authenticity therefore rests on virtiofs passing ownership
  through unchanged — a property of virtiofsd, which is in the TCB.
- **The guest batch controller runs as guest root.** It is a small, fixed script
  that never executes anything the repository or the spec supplied, and it is
  hardened (`NoNewPrivileges`, `PrivateDevices`, `ProtectHome`, `ProtectKernel*`,
  `ProtectControlGroups`, `RestrictSUIDSGID`, read-only input, write access only
  to its own directory). It deliberately does *not* set `ProtectSystem=`, because
  the writable-path set it would need (D-Bus, systemd runtime) cannot be validated
  without booting on real KVM.
- **The result channel's kernel-level enforcement is not yet measured.** The
  layout, the validators, the controller's own logic and the unit properties are
  covered by `nix flake check` (`microvm-batch-result-integrity`, 63 executed
  fixtures; `microvm-batch-controller-smoke`, 39 executed controller assertions;
  `microvm-batch-launcher-submit`, 39 executed host-submit assertions), but
  "a uid-1000 worker really cannot write `controller/result.json`" is only
  observable in a booted guest — see the `forgery` section of the
  [runtime suite](./agent-microvm-runtime-validation.md), currently recorded as
  NOT EXECUTED. The same applies to "the timeout stopped the worker's whole
  cgroup": the checks prove the controller *issues* the cgroup-wide stop
  (`systemctl kill --kill-whom=all`, observed in a stub's call log), not that
  systemd reaped a double-forked descendant.
- **An archived result still contains its allocation token.** It is kept for
  forensic correlation with the session marker, so both the archive directory and
  the files in it are root-only (`0700`/`0600`). A local user who can read root's
  files can read a *finished* run's token; that token no longer authorises
  anything, since the allocation is gone.
- **Operator convenience trades some strictness.** `passwordlessControl` grants a
  scoped `NOPASSWD`+`SETENV` sudo rule and authorises the operator's own public
  keys on the guest agent user. The guest cannot reach host sudo, but the blast
  radius of a compromised *operator account* is larger with it enabled.
- **Agent packages are trusted as builds.** A malicious upstream release would be
  baked into the guest closure; the sandbox limits what it can reach, not whether
  it runs.
- **Single host, shared kernel version.** All slots run the same host kernel and
  hypervisor build; one bad CVE affects every slot at once.

## Why this is not a hardened multi-tenant cloud sandbox

- **One tenant.** The threat model is "my agent may be hostile", not "mutually
  hostile strangers". There is no per-tenant key management, quota accounting,
  audit retention or abuse response.
- **The operator is trusted.** The launcher runs as root on a laptop the operator
  owns; there is no privilege separation between "the platform" and "the user".
- **No defence in depth against hypervisor escape.** A cloud sandbox adds
  microVM + seccomp-bpf'd device model + separate physical/kernel domains +
  network segmentation between *hosts*. Here, an escape lands directly on the
  operator's workstation.
- **Availability is not a goal.** A guest can make the host slower (weights, not
  hard quotas, on CPU/IO) and can fill the disk with retained clones until the
  operator prunes them.
- **The network policy is host-local.** It relies on this host's iptables and
  bridge behaviour; there is no external egress filter, no TLS interception, no
  DLP on what the model receives.
- **Not audited.** No external review, no fuzzing of the launcher, no formal
  verification of the firewall rules.
