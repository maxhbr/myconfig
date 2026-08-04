<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — real-KVM validation guide

This guide describes how to run [`../runtime-validation.sh`](../runtime-validation.sh)
on a host with real KVM. Nothing in this file is evidence that the runtime
properties hold — only running the suite is.

## Why a separate tier

`nix flake check` proves that the module evaluates, that the slot pool is
well-formed, that every generated artefact comes from the single authoritative
registry, and that all shell code passes `shellcheck`. It **cannot** boot a
guest, move a packet, or observe a cgroup limit taking effect. Those properties
need real KVM, so they live here — deliberately outside CI.

## Prerequisites

```bash
# on the host under test (f13):
ls -l /dev/kvm                       # must exist
command -v agent-microvm             # feature enabled for this host
sudo agent-microvm list              # the prebuilt slot pool
sudo systemctl status agent-microvm-hostkeys.service   # per-slot host keys provisioned
ip -br link show agentbr0            # the private bridge exists
```

A **throwaway** git repository to use as the source for clones (its content is
irrelevant; the suite clones it repeatedly):

```bash
git init /tmp/rtv-src && (cd /tmp/rtv-src && echo hi > a && git add . &&
  git -c user.email=t@t -c user.name=t commit -qm init)
```

## Running the suite

```bash
sudo ./modules/myconfig.ai/myconfig.ai.microvm/runtime-validation.sh \
     --repository /tmp/rtv-src | tee /tmp/rtv-$(date +%F).log
```

Or one section at a time (recommended the first time):

```bash
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section boot
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section net
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section l2
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section creds
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section lifecycle
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section malrepo
sudo ./…/runtime-validation.sh --repository /tmp/rtv-src --section forgery
```

## Execution status

| Section | Last executed on real KVM |
| --- | --- |
| `boot`, `net`, `l2`, `creds`, `lifecycle`, `malrepo` | **NOT EXECUTED** (no KVM host was available when they were written) |
| `forgery` | **NOT EXECUTED** — written together with the controller/worker split; the environment it was written in had no `/dev/kvm` and no root. What *has* been executed for that section's properties are the two eval/build checks `microvm-batch-result-integrity` and `microvm-batch-controller-smoke` (see below). |

This table is deliberately pessimistic: update it only with a pasted log.

Every check prints one line — `PASS`, `FAIL` or `SKIP` — and the script exits
non-zero if anything failed. `SKIP` is honest (e.g. "fewer than two slots in the
pool"), never a disguised pass.

## What each section asserts

### `boot` — boot, filesystem, persistence, isolation

| Expected outcome |
| --- |
| every resource class boots and becomes SSH-ready |
| `/workspace` is a mount point and is writable by the guest `agent` user |
| the host `/nix/store` is **not** shared into the guest |
| exactly four virtiofs shares: `/workspace`, `/var/lib/agent-hostkey`, `/run/agent-job`, `/var/lib/agent-state` |
| workspace changes survive shutdown (they are in the standalone clone) |
| guest root and guest home changes do **not** survive a restart |
| `--persist-agent-state` persists **only** declared paths (`~/.hermes`), never undeclared ones |
| a task cannot see another task's workspace, nor the host workspace root |

### `net` — proxy-only allow/deny matrix

Allowed: `127.0.0.1:4000` (guest loopback forwarder) and
`192.168.83.1:4000` (bridge endpoint) — i.e. model access **only** through
LiteLLM.

Denied (each success here is a security failure):
`169.254.169.254`, host SSH on the gateway, an arbitrary host port,
`10.0.0.0/8`, `172.16.0.0/12`, `192.168.0.0/16` outside the agent subnet, a
public IP, a public DNS server, public name resolution, and any IPv6 default
route that could bypass the IPv4 policy.

### `l2` — two-guest layer-2 isolation

| Expected outcome |
| --- |
| `bridge link show` reports `isolated on` for **every** guest TAP |
| guest A cannot ping or TCP-connect to guest B |
| guest A cannot reach guest B over IPv6 link-local / multicast |
| guest A cannot even learn guest B's MAC (ARP is dropped in the bridge) |
| while A adds B's IP to its own interface, the **host** still reaches the real B |

### `creds` — credential boundary

Only names and paths are printed, never values. Asserts that
`OPENAI_API_KEY`/`ANTHROPIC_API_KEY` are the placeholders, that no
`OPENROUTER_API_KEY`/`GITHUB_TOKEN`/`GH_TOKEN`/`GITLAB_TOKEN`/`AWS_*`/`GOOGLE_*`/
`AZURE_*`/`KUBECONFIG`/`SSH_AUTH_SOCK`/`GPG_AGENT_INFO` exist, and that none of
`~/.ssh/id_*`, `~/.aws`, `~/.config/gcloud`, `~/.kube`, `~/.password-store`,
`~/.gnupg`, a Docker/Podman socket, the Nix daemon socket or the system D-Bus
socket is reachable — plus no git credential helper and no other user's home.

### `lifecycle` — forced failures

Failures are forced at clone creation, repository validation, launcher
termination (`SIGKILL` mid-run) and guest crash (`systemctl kill` the VM). After
each one the suite asserts: no slot stays falsely allocated, no stale workspace
bind mount remains, no stale job spec remains, the workspace clone is preserved,
and a slot can still be allocated afterwards. `recover --dry-run` is run before
`recover` so its output can be compared.

### `forgery` — the batch result channel (ticket 7)

The authoritative batch result (`/run/agent-job/controller/result.json`) is
written by the TRUSTED guest controller (root) and consumed by the host. The
coding agent and every repository process run as the unprivileged guest `agent`
user. This section measures that separation on a *booted* guest, because it is
the guest kernel that enforces it over virtiofs.

| Expected outcome |
| --- |
| the guest agent cannot write, read, delete or list `controller/` (root:root `0700`) |
| the guest agent cannot read `input/spec.json` (`0400` — it carries the allocation token) |
| the guest agent cannot modify `input/spec.json` / `input/prompt.md`, nor forge `input/cancel.json` |
| the guest agent cannot rename, remove or symlink-shadow `controller/`, and cannot create anything in the share root |
| a worker-written `worker/result.json` and `/workspace/result.json` are possible but **ignored**: the host keeps waiting and the archived result carries `source:"controller"` |
| a stale result from an earlier allocation (valid schema, old allocation token) is **rejected** and cannot terminate the new job |
| a malformed controller result yields an **infrastructure error** (`submit` exit `70`), never success |
| a job that exceeds its deadline is `timed-out` (exit `124`) with the verdict coming from the controller, and the whole worker cgroup (including double-forked descendants) is gone |
| `cancel` records `cancelled`; replaying that cancellation request against a **new** allocation of the same slot does nothing (token mismatch) |

What the eval/build tier already executes for the same properties (run by
`nix flake check`, no KVM needed):

- `microvm-batch-result-integrity` runs the real host verifier and the real
  guest-side permission assertions against 56 forged / stale / malformed /
  symlinked / world-writable fixtures under `fakeroot`;
- `microvm-batch-controller-smoke` runs the real, unmodified guest **controller**
  (33 assertions) inside `bwrap` with a stubbed `systemctl`: healthy job, failing
  agent, deadline, token-bound cancellation, stale cancellation, six rejected
  specs, and a broken trust boundary — then feeds the documents it produced to
  the host verifier, which must accept them for this allocation and reject them
  for another.

Both prove the *validators, the protocol and the layout*, not the guest kernel's
enforcement or systemd's cgroup kill — which is exactly what this section adds.

### `malrepo` — hostile repository fixture

The fixture contains a git `post-checkout` hook, a `flake.nix` that throws when
evaluated, an `.envrc`, an `.mcp.json` that would run `touch`, symlinks to
`/etc/shadow` and `/`, a nested repository, and a `.git` **file** pointing at
`/etc`. Expected: the host never ran the hook, never evaluated the flake, never
ran the MCP command; the guest cannot read `/etc/shadow` through the symlink; a
fork bomb and a disk-filling attempt leave both the guest and the host healthy;
the guest cannot enumerate host block devices.

## Manual extras (not automated)

- `journalctl -t agent-microvm -f` during a run: the structured lifecycle
  stream (`task-submitted` → … → `cleanup-completed`).
- `journalctl -u microvm@<slot>` : the guest console, including the guest's own
  structured events.
- `tcpdump -ni agentbr0` / `tcpdump -ni vm-<class>-<i>` while the `net`/`l2`
  sections run, to *see* the drops rather than infer them.
- `systemd-cgtop` / `systemctl show microvm@<slot> -p MemoryMax,TasksMax` to
  confirm the host-side limits, and, inside the guest,
  `systemctl show agent-job-worker@<agent> -p MemoryMax,CPUQuota,TasksMax` for
  the per-class guest limits (they live on the WORKER unit) plus
  `systemctl status agent-job-controller` for the trusted half.
- inside the guest: `sudo -u agent ls -ld /run/agent-job/*` — `input/` and
  `controller/` must be root-owned (the latter `0700`), only `worker/` may be
  agent-owned.
- `iptables-save | grep AGENT_MICROVM` to review the rendered ruleset for the
  active network profile.

## Interpreting failures

- A `FAIL` in `net`, `l2` or `creds` is a **security** finding: stop and fix
  before using the tier for anything hostile.
- A `FAIL` in `lifecycle` usually leaves recoverable state; run
  `sudo agent-microvm recover` and re-run the section.
- A `SKIP` in `l2` normally means the pool has fewer than two slots — increase a
  class's `count` on the host under test and retry.
