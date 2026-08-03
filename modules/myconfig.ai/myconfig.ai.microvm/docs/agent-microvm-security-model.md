<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — security model

What this tier does and does not protect, stated so it can be argued with.
Mechanisms are described in the
[architecture document](./agent-microvm-architecture.md).

**Verification status:** every property below is implemented and covered by the
eval/build tier, and the shell components were exercised against stubbed
systemd/mounts. The **real-KVM measurements have not been run**
([guide](./agent-microvm-runtime-validation.md)), so runtime properties are
*designed and eval-asserted*, not *measured*. Nothing here should be read as
"tested in production".

## Trusted vs untrusted

**Trusted (the TCB — a bug here defeats the tier):**

- the host kernel, KVM, Cloud Hypervisor, virtiofsd, the guest kernel;
- microvm.nix and this module's own Nix + shell code;
- the host LiteLLM proxy (it holds the only real upstream credential);
- the host operator (already a full sudoer) and systemd on the host;
- the agent packages as *builds* (their content is baked in at build time).

**Untrusted (assumed hostile):**

- everything the agent does inside the guest, including the agent binary's
  runtime behaviour and any model output it acts on;
- the repository content that becomes `/workspace` (hooks, `flake.nix`, `.envrc`,
  MCP config, scripts, symlinks);
- prompts and any data the agent fetches through LiteLLM;
- other guests.

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
| `/run/agent-job` | rw | `spec.json` + `prompt.md` (`root:root 0444`) and a guest-writable `out/` | the agent can write junk into `out/`; it cannot alter its own spec/prompt |
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
