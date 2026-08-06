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
  It exists only on a host that selects the `batch` capability; an
  interactive-only guest has no batch trust split to get wrong, because it has
  neither identity.

**Untrusted (assumed hostile):**

- everything the agent does inside the guest, including the agent binary's
  runtime behaviour and any model output it acts on;
- the repository content that becomes `/workspace` (hooks, `flake.nix`, `.envrc`,
  MCP config, scripts, symlinks);
- prompts and any data the agent fetches through LiteLLM;
- **everything the batch worker produces** (`agent-job-worker@<agent>.service`,
  guest `agent`): the CONTENT of its stdout/stderr logs, its artifacts, any file
  it writes into `/workspace` or `/run/agent-session/worker/`, and any JSON that
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
  `/run/agent-session/controller/` is `root:root 0700` (virtiofsd passes
  ownership through, so that is the *effective* permission inside the guest) and
  the worker unit additionally masks it with `InaccessiblePaths`. The worker
  cannot write, read, delete or list it, and cannot rename or shadow it either,
  because the session root `/run/agent-session` is root-owned `0755`.
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
- **No host filesystem.** The guest sees only the two declared virtiofs shares
  — no host home, no `/nix/store`, no sockets.
- **No build/fetch capability.** No Nix daemon socket, no store write access; the
  guest can only run what was baked in.
- **Bounded resources.** Per-class `CPUQuota`/`MemoryMax`/`TasksMax` inside the
  guest, plus host-side `MemoryMax`/`TasksMax`/`CPUWeight`/`IOWeight` on the
  hypervisor unit; three independent timeout layers for batch jobs.
- **Non-root agent.** uid `guestAgentUid` (1000, asserted unprivileged), locked
  password, no extra groups, `NoNewPrivileges`, `PrivateDevices`,
  `ProtectKernel*`, `RestrictSUIDSGID` for batch jobs.

## What is still shared through virtiofs

Exactly **two** shares, both per-slot: ONE writable per-session share and ONE
read-only share (lightweight plan phase 4; the historical four/five separate
shares are gone). A host that narrows `capabilities` (lightweight plan phase 5)
gets the same two shares with FEWER subdirectories — an interactive-only guest
has no `input/`, `controller/`, `worker/` or `worker-logs/` (there is no batch
protocol to protect, because there is no controller and no worker), and a
batch-only guest has no `hostkeys/` in the read-only tree (there is no sshd, so
the slot has no SSH identity to keep from the agent). A batch+vsock guest
(lightweight plan phase 6) KEEPS `hostkeys/` — the VSOCK `sshd-vsock@` control
channel reuses the per-slot host identity — while still having no `sshd.service`
(TCP) listener. No owner or mode changes,
and no invariant is relaxed: a narrowing only removes attack surface. The trust
POLICY is asserted against the FULL layout table rather than the selected slice,
so weakening an entry a host happens not to create still fails the build, and the
pre-launch verifier additionally refuses any top-level entry the (narrowed) table
does not declare — the case that matters is a stale `input/` or `worker-logs/`
left behind by a generation that did select `batch`.

Also worth being precise about: a batch-only guest has no SSH *daemon* — no
`sshd`/`sshd@`/`sshd-vsock@` unit, `services.openssh.enable = false`, no host
identity, no authorized key — but the openssh *binaries* are still in its
closure, because NixOS' `environment.requiredPackages` (which also brings
coreutils-full, curl, …) provides `scp`/`ssh` and is load-bearing for a bootable
system. The criterion "a batch-mode guest has no SSH daemon" is a unit/config
claim, not a closure-size claim.

**Phase 6 refines this for a `batch`+`vsock` host.** Such a guest DOES run an
sshd — but ONLY the VSOCK `sshd-vsock@` (vsock::22), reachable solely from the
host (CID 2) over AF_VSOCK, never from the TAP. The TCP `sshd.service` is
suppressed (`systemd.services.sshd.enable = false`), so no network SSH listener
exists. The invariant above is therefore scoped to **"no TCP/network SSH
daemon"**: a VSOCK-only inetd sshd is the host↔guest *control channel* (the
enabler that lets `runtime-validation.sh` reach a batch-only guest at all), not
an interactive login service exposed to the guest's network. It reuses the
per-slot host identity + a `known_hosts` entry keyed by the VSOCK mux path, so
the channel is host-key-verified exactly like the TAP one. See the plan's
phase-6 recorded deviations for the full rationale.

The consolidation moved the paths into the two trees WITHOUT
changing a single owner or mode — the trust boundary was never the share split,
it is ownership and modes, which virtiofsd passes through unchanged:

| Historical guest path | Guest path today |
| --- | --- |
| `/workspace` | `/workspace` (a bind mount of `/run/agent-session/workspace`) |
| `/run/agent-job/...` | `/run/agent-session/...` (`input/`, `controller/`, `worker/`, `worker-logs/`) |
| `/var/lib/agent-state` | `/run/agent-session/state` |
| `/var/lib/agent-hostkey` | `/run/agent-session-ro/hostkeys` |
| `/run/agent-config-seed` | `/run/agent-session-ro/config-seed` |

The writable session share is `<runtimeRoot>/sessions/<slot>/` (root-owned
`0755`), the read-only one `<runtimeRoot>/sessions-ro/<slot>/` (root-owned
`0700`, mounted with virtiofsd `--readonly`). The SSH private host key and the
staged configuration are deliberately NOT in the writable tree; the launcher
runs `agent-microvm-verify-session` before every launch, which re-derives every
expected owner/mode from the ONE layout table (`session.nix`), refuses a
symlinked or non-root-owned component, and refuses to launch at all if host-key
material is found in the writable tree. Teardown removes the COMPLETE per-session
tree (after proving both bind mounts are gone, and that no *other* mount survives
anywhere underneath it — `rm -rf` would otherwise descend through a bind and
delete the user's clone) and recreates the empty skeleton.

### The one genuine security delta of consolidation

Owners and modes are unchanged, but the *mount* boundary is not: `workspace/`,
`worker/` and `state/` sit in the **same filesystem** as the root-owned
`input/`, `controller/` and `worker-logs/`, whereas in the historical layout each
was its own virtiofs mount. Operations that a kernel refuses **across**
filesystems — `link(2)` and `rename(2)` — are therefore newly possible *in
principle* between the agent-writable and the root-owned parts of the tree.

The practical impact is nil, and for a structural reason: both operations need
write permission on the **destination directory**, and the guest `agent` user has
write access to no root-owned directory in either tree. It cannot rename
anything *into* `input/`, `controller/` or `worker-logs/`, and it cannot create a
hardlink there either. In the other direction — hardlinking a root-owned *file*
into its own `worker/` — the only root-owned files the agent can even open are
its own prompt (`0444`) and its own worker logs (`0644`); a hardlink to them
grants exactly the read access it already has, and Linux'
`fs.protected_hardlinks` (default `1`) additionally refuses hardlinks to files
the user neither owns nor can write. The token-bearing `spec.json` is `0400`
root-owned and unreadable to it, and `controller/` is `0700` root-only, so
neither can be reached, linked or renamed.

Consolidation is also a *strengthening* in one respect that is easy to miss: in
the historical four-share layout the `workspace`, `state` and job directories were the
mount points themselves, so the directory *entries* lived in a tree the agent
could not touch — but each share was created and torn down separately. Now the
whole tree hangs below a root-owned `0755` session root, so the agent can
neither create, rename nor shadow **anything** at the top level of its own
session (it cannot, for example, replace `worker-logs/` with a symlink or move
`controller/` aside), and a single teardown provably removes all of it.

| Guest path | Mode | Content | Risk |
| --- | --- | --- | --- |
| `/workspace` | rw | the task's standalone clone | the agent can write anything into your task's clone; you review it before importing |
| `/run/agent-session-ro/hostkeys` | **ro** | that slot's SSH host key, `root:root 0400` | unreadable by the guest agent user; the guest cannot change its own identity |
| `/run/agent-session` | rw | `input/` (`spec.json` `root:root 0400`, `prompt.md` `0444`), `controller/` (`root:root 0700`, the authoritative result), `worker/` (agent-owned), `worker-logs/` (`root:root 0755`, the log files systemd opens as root) | the agent can write junk into `worker/`; it cannot read or write `controller/`, cannot read the token-bearing spec, cannot alter its own spec/prompt, and cannot replace or redirect its own log files |
| `/run/agent-session/state` | rw | only when `--persist-agent-state`: the task's declared agent-state dirs | the agent can poison ITS OWN task's future state; other tasks are unreachable |
| `/run/agent-session-ro/config-seed` | **ro** | the launch-time staged copy of the **allowlisted** host agent configuration, `root:root` `0500`/`0400`. The read-only share source is the slot's read-only tree; the staging `manifest.json` lives in the host-only `<runtimeRoot>/config-seed/<slot>/` and is **not** in any share (it names the host home and every skipped, credential-shaped host file name) | the guest *root* seeder copies the tree into the disposable home; the unprivileged agent can neither read the staged original nor modify it, and cannot reach anything else in the host home (subject to the host-home write assumption below) |

virtiofsd is in the TCB for all of them. Ownership is passed through unchanged,
so host-side modes are what the guest sees.

The config seed does **not** widen the trust boundary towards the host home: it
is a per-slot *copy*, not a mount, produced from a positive allowlist of exact
files/directories (the selected agents' `configPaths` + `configSeed.extraPaths`)
with a credential denylist on top — applied to the path's own name *and to its
resolved target*, so a benignly named symlink (`.codex/config.toml` →
`.codex/auth.json`, `.agents/skills/x` → `~/.ssh`) cannot smuggle a credential —
with symlink escapes rejected, `/nix/store` symlinks dereferenced (any home
symlink into the world-readable store is followed, including the host's rendered
dotfiles), sockets/devices/FIFOs/setuid files excluded, and the destination
cleaned before every launch. It moves the *risk of over-sharing* from "whatever
the guest image baked in" to "whatever the allowlist names", and makes that
decision auditable per session through the manifest. See [agent-microvm.md](./agent-microvm.md#runtime-configuration-staging).

**Residual risk, explicitly not covered:** every control above is *name-based*
and assumes the host home is **trusted**. An attacker who already has write
access inside it can

- **hardlink** a credential into an allowlisted path (a hardlink has no target
  name, so the denylist cannot see it), or
- swap a resolved path for a symlink **between** the stager's `realpath` check
  and its `install` (TOCTOU), which root then follows.

Neither is defended against, because an attacker with that access can simply
edit an allowlisted configuration file instead — a strictly easier attack with
the same reach. The staging boundary protects the guest→host direction and
constrains what the *host operator's own* configuration exposes; it is not a
boundary against a compromised host home.

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
| stealing host SSH/GPG/cloud credentials | no host home, no agent sockets, no credential files in the guest; runtime config staging is allowlist-only with a credential denylist, so `~/.ssh`, `auth.json`, `*.pem`, `.netrc`, … are rejected at evaluation time and again at copy time |
| tampering with the configuration the host staged | the staged tree is root-owned `0500`/`0400` on a **read-only** share, and the guest seeding oneshot refuses a tree that is not root-owned or is group/other-writable |
| reaching the host LAN / VPN / router | private-range DROP in every profile |
| cloud-metadata SSRF (`169.254.169.254`) | dropped first, unconditionally, in INPUT and FORWARD |
| arbitrary internet egress / data smuggling | denied in `offline`/`proxy-only`; `package-access` allows one host proxy port; only `internet` routes, and then with NAT + a DNS allowlist + drop logging |
| DNS tunnelling | port 53 only to configured resolvers, everything else dropped |
| guest → guest attacks, ARP spoofing, MITM of host↔guest | per-TAP `isolated` bridge ports (all EtherTypes) + IPv4 inter-VM DROP + strict SSH host-key verification |
| impersonating a slot to the operator | per-slot host keys, delivered read-only and root-only, pinned in `known_hosts` |
| persistence across sessions | disposable tmpfs root/home; persistence is opt-in, declared-paths-only and task-scoped |
| cross-task contamination | per-task clone + per-task state; the slot's share sources are cleared/rebound per run |
| host code execution from a hostile repo | the launcher never evaluates repo-provided nix/direnv/hooks/npm/make/MCP (asserted by a check); clones copy the object store (`--local --no-hardlinks`, never `--shared`/`--reference`) and are verified free of object alternates; git-dir/common-dir escapes rejected; all paths canonicalised |
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
  re-check the flag — `bridge -d link show` does (the plain `bridge link show`
  prints no port flags at all, which is why the runtime-validation suite's first
  real run reported a false negative here).
- **VSOCK is now wired (phase 6), as a CONTROL channel.** A host that
  selects the `vsock` capability gives its guests a VSOCK `sshd-vsock@`
  (vsock::22, reused from upstream) reachable ONLY from the host (CID 2) over
  AF_VSOCK — never from the TAP. The TCP `sshd.service` is suppressed for a
  batch+vsock guest, so the channel is host-only. What crosses it both ways is
  the SAME as the existing SSH control channel — host→guest: the operator's
  commands (`agent-microvm ssh` / the runtime-validation suite); guest→host:
  command stdout/exit — just over a different L2. No host home, no credential,
  no host socket, no batch result, and no second writable share cross it (the
  VSOCK channel is not a virtiofs share); the invariants are unchanged. The
  batch result channel itself still rides the SSH-less file exchange through the
  job subtree of the ONE writable session share (VSOCK is the *control*
  transport, not the *result* transport), so its authenticity still rests on
  virtiofs passing ownership through unchanged — a property of virtiofsd, which
  is in the TCB. The plan's literal phase-6 VSOCK *proxy* forwarder (carrying
  the model API over VSOCK and eliminating the TAP) is NOT implemented — it
  would fork the one forwarder shape; see the plan's phase-6 deviations.
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
