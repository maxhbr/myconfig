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

### Host-side model-endpoint preflight

Before `net`, `forgery`, or `all` boot any VMs, the suite probes the SAME bridge
endpoint a guest would use (`http://192.168.83.1:4000/v1/models`, forwarded by
the `agent-litellm-proxy` socket to the loopback LiteLLM). If it is not
reachable the suite **aborts** those sections with a precise reason instead
of running them into the ground (a batch worker that dies in seconds fails
every forgery subtest, and the `net` endpoint checks fail for the same root
cause). `boot`/`l2`/`creds`/`lifecycle`/`malrepo` are still meaningful without
the endpoint and are not aborted. The preflight retries up to 3 times (3 s
each, 2 s apart) so a cold LiteLLM is not mistaken for a dead one; if it
fails, run `sudo agent-microvm doctor` on the host and re-run the section.

## Execution status

| Section | Last executed on real KVM |
| --- | --- |
| `boot`, `net`, `l2`, `creds`, `lifecycle`, `malrepo` | **NOT EXECUTED in the present form.** They were run once on f13 (root + `/dev/kvm`) as originally written. That run produced 20 `FAIL`s, of which all but two turned out to be defects of the HARNESS — and the same defects made roughly 25 of the reported `PASS`es vacuous. The harness has since been repaired (see “Harness validity invariants” below); the repaired suite has **not** been run yet. |
| `forgery` | **NOT EXECUTED** — written together with the controller/worker split; the environment it was written in had no `/dev/kvm` and no root. What *has* been executed for that section's properties are the three eval/build checks `microvm-batch-result-integrity`, `microvm-batch-controller-smoke` and `microvm-batch-launcher-submit` (see below). |

This table is deliberately pessimistic: update it only with a pasted log. The
repairs listed under “Harness validity invariants” (in particular the guest
command transport, invariant 2b) were made in an environment **without**
`/dev/kvm` and without root; what *was* executed there is the CI tier, including
the new `microvm-rtv-transport` check, which runs the suite's own transport
block against a stub that reproduces OpenSSH's argv flattening plus the guest's
fish login shell — with a negative control that fails for the previous,
unquoted transport.

Every check prints one line — `PASS`, `FAIL` or `SKIP` — and the script exits
non-zero if anything failed. `SKIP` is honest (e.g. "fewer than two slots in the
pool"), never a disguised pass.

## Harness validity invariants

The first real run showed that a validation suite has its own failure modes, and
that the dangerous one is not a false `FAIL` — it is a false `PASS`. These rules
now hold throughout `runtime-validation.sh`; break one and the suite starts
lying:

1. **Wait for the control channel, never sleep.** A detached
   `agent-microvm run` does *not* wait for guest readiness (`launcher.nix` calls
   its own `wait_ready` only under `--attach`), so every start path in the suite
   goes through the single `wait_guest_ready` helper, which polls
   `agent-microvm ssh <slot> -- true` until it succeeds (120 s by default,
   `AGENT_RTV_READY_TIMEOUT`). A start that never becomes reachable is reported
   by `report_start_failure`, not silently used.
2. **Every guest-side denial uses `check_denied`, never `check_fails`.**
   `check_denied` re-proves the SSH channel *after* the attempt failed and
   reports `SKIP` when the channel is dead, so “the attack failed” and “the
   connection failed” cannot look the same. `check_fails` is reserved for
   commands that run on the **host** (`test -e /tmp/rtv-HOOK-RAN`, a launcher
   invocation that must be rejected).
2b. **The command must reach the guest as written — and that is *measured*.**
   `agent-microvm ssh <slot> -- <argv…>` cannot preserve argument boundaries:
   OpenSSH joins the remaining argv with single spaces and the guest's **login
   shell** re-parses the string — and that shell is `fish` (`guest.nix`:
   `users.users.agent.shell = pkgs.fish`). A payload such as
   `sh -c "timeout 5 sh -c '</dev/tcp/GW/22'"` therefore used to arrive as
   `sh -c timeout 5 sh -c '…'`, i.e. `sh -c` received only the word `timeout`:
   the command failed for a **quoting** reason and the denial passed
   *vacuously*. `${VAR:-}` was worse — `${` is a fish syntax error, so the whole
   credential-environment block could never be evaluated.
   The suite now sends every payload as ONE token escaped for the guest's login
   shell (`guest_sh`/`guest`), **detects** the quoting dialect per slot, and
   gates every guest-side assertion on a transport probe that only succeeds when
   word boundaries, embedded quotes, `${VAR:-}` expansion *and* the exit code
   survive the round trip. Each started slot reports the verdict as its own
   check line (`assert_transport`); a broken transport is a `FAIL` plus `SKIP`s,
   never a green run. The probe and its payload classes are executed in CI by
   the `microvm-rtv-transport` check (`tests/microvm-rtv-transport.sh`), which
   contains a negative control: the previous, unquoted transport must fail it.
2c. **A probe mechanism needs its own positive control.** Every
   `</dev/tcp/host/port>` denial is preceded by `tcp_probe_works`, which
   requires the same mechanism to SUCCEED against the one endpoint the policy
   allows (LiteLLM on the gateway). Otherwise a guest shell without `/dev/tcp`
   support would “prove” the whole firewall matrix.
3. **Every block that asserts an absence carries a positive control, and the
   control must be able to distinguish which half answered.** The environment
   block asserts `OPENAI_BASE_URL` *is* set to the expected value before
   concluding anything from the absence of other variables; the hostile
   repository block asserts the fixture's symlink *is* in the guest workspace
   before concluding that it cannot be followed; the batch-environment block
   reads the systemd **manager** environment and the **unit's** `Environment=`
   *separately* and requires `PATH=` in **each** (concatenating them was a
   vacuity hole: `show-environment` alone always contains `PATH`, so an
   unreadable unit still satisfied the control). The ephemerality markers are
   asserted to EXIST in the first run before their absence in the second means
   anything, and “task A cannot see task B's workspace” is only asserted once
   that workspace really exists on the host. If a control fails the block
   `SKIP`s.
3b. **An attack that could not be mounted is `SKIP`, not `PASS`.** The
   impersonation check asserts that guest A really took B's address
   (`ip -o addr show dev eth0`) before concluding anything from the host still
   reaching B — `ip addr add` needs `CAP_NET_ADMIN`, which the unprivileged
   guest `agent` user does not have, so the attack normally never happens.
4. **Environment assertions run in a LOGIN shell.** `environment.variables`
   reaches a process only through `/etc/profile`, and
   `agent-microvm ssh <slot> -- <cmd>` is neither a login nor an interactive
   shell. `guest_login` (`sh -lc`) exists for exactly this.
5. **Introspection commands must actually print what is asserted.**
   `bridge link show` does not print port flags; only `bridge -d link show`
   does.
6. **Host-side channels use the launcher's own `ssh` subcommand**, so they get
   the same pinned host key *and the same identity* the operator would use. A
   raw `ssh -o BatchMode=yes` with no `-i` fails for authentication reasons
   whatever the property under test does.
7. **A subtest that could not set its fixture up must `SKIP`**, loudly, rather
   than assert against a situation it never created.

## What each section asserts

### `boot` — boot, filesystem, persistence, isolation

| Expected outcome |
| --- |
| every resource class boots and becomes SSH-ready (polled, see invariant 1) |
| `/workspace` is a mount point and is writable by the guest `agent` user |
| the host `/nix/store` is **not** shared into the guest |
| exactly four virtiofs shares: `/workspace`, `/var/lib/agent-hostkey`, `/run/agent-job`, `/var/lib/agent-state` (an empty enumeration is a `FAIL`, not a pass) |
| workspace changes survive shutdown (they are in the standalone clone) |
| guest home and guest `/tmp` changes do **not** survive a restart — asserted only after the markers were proved to have been CREATED in the first run (the suite used to write `/root-marker`, which the unprivileged agent cannot create at all, and then “prove” it had not persisted) |
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
| `bridge -d link show` reports `isolated on` for the TAP of **every running** slot, and every running slot has its TAP (the TAP name is `vm-<class>-<i>` for slot `agent-<class>-<i>`) |
| guest A cannot ping or TCP-connect to guest B |
| guest A cannot reach guest B over IPv6 link-local / multicast |
| guest A cannot even learn guest B's MAC (ARP is dropped in the bridge) |
| while A adds B's IP to its own interface, the **host** still reaches the real B — through `agent-microvm ssh`, after a positive control proved the host could reach B *before* the impersonation, and only if A's `eth0` really carries B's address afterwards (otherwise: `SKIP`, because the attack could not be attempted) |

### `creds` — credential boundary

Only names and paths are printed, never values. Asserts that
`OPENAI_API_KEY`/`ANTHROPIC_API_KEY` are the placeholders, that no
`OPENROUTER_API_KEY`/`GITHUB_TOKEN`/`GH_TOKEN`/`GITLAB_TOKEN`/`AWS_*`/`GOOGLE_*`/
`AZURE_*`/`KUBECONFIG`/`SSH_AUTH_SOCK`/`GPG_AGENT_INFO` exist, and that none of
`~/.ssh/id_*`, `~/.aws`, `~/.config/gcloud`, `~/.kube`, `~/.password-store`,
`~/.gnupg`, a Docker/Podman socket or the Nix daemon socket is reachable — plus
no git credential helper and no other user's home.

Two scoping notes that cost a round of false results:

- The environment facts are asserted **twice**: once in a login shell (the
  interactive path, where `/etc/profile` exports them) and once against what the
  batch **worker unit** would inherit (`systemctl show-environment` plus the
  unit's `Environment=`), because the batch path never sees a profile. Both
  blocks have a positive control.
- `/run/dbus/system_bus_socket` is **not** asserted absent. The guest runs its
  own systemd and therefore its own system bus; the socket existing is expected
  and is not a leak. The property that matters — no *host* bus is shared in — is
  covered by the share-set assertion in `boot`. What is asserted here is the
  cheap positive fact that the socket, if present, is on a guest-local
  filesystem rather than on a virtiofs share.

### `lifecycle` — forced failures

Failures are forced at clone creation, repository validation, launcher
termination (`SIGKILL` mid-run) and guest crash (`systemctl kill` the VM). After
each one the suite asserts: no slot stays falsely allocated, no stale workspace
bind mount remains, no stale job spec remains, the workspace clone is preserved,
and a slot can still be allocated afterwards. `recover --dry-run` is run before
`recover` so its output can be compared.

The killed-launcher subtest submits a **real, non-empty** prompt file (the
launcher rejects `--prompt-file /dev/null` twice over — not a regular file, and
empty — so a submit started that way dies in milliseconds and killing it later
tests nothing) and waits until the submit has really allocated a slot *and*
created the clone before the `kill -9`. If it never gets that far the subtest
`SKIP`s with that reason instead of asserting against a situation it never
created.

Residue is checked with the **same scope the launcher uses**: `list`, `status`
and `recover` iterate the *current* slot pool only. Per-slot state under a slot
name from an earlier generation's naming (e.g. `slots/agent-0/` from before the
`agent-<class>-<i>` rename) is therefore reported as a separate, clearly
labelled `FOREIGN SLOT STATE` diagnostic, never counted as residue of the run
under test — and when such state exists the suite additionally asserts that
`agent-microvm recover --dry-run` **reports** it (as a `foreign:` finding).
State the launcher cannot see is state nobody will clean up, so its absence from
`recover` is itself a `FAIL`.

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
| the guest agent cannot write, symlink-replace or rename its own `worker-logs/` files or directory (root-owned; systemd opens them as root), and the host still sees `root:root 0755` afterwards |
| no 64-hex allocation token is visible in any `/proc/*/cmdline` the guest agent can read, and the controller's processes are not visible to it at all (`ProtectProc=invisible`) |
| a worker-written `worker/result.json` and `/workspace/result.json` are possible but **ignored**: the host keeps waiting and the archived result carries `source:"controller"` |
| a result with a foreign allocation token, planted as **root** in `controller/` *after* the slot was allocated, is **rejected by name** (`allocation token does not belong`) and yields exit `70` |
| a malformed controller result yields an **infrastructure error** (`submit` exit `70`), never success |
| a job that exceeds its deadline is `timed-out` (exit `124`) with the verdict coming from the controller, a double-forked descendant planted in the guest is proved to have EXISTED while the job ran, and the slot is released with its VM stopped afterwards. Whether the **worker cgroup specifically** was killed is reported as an explicit `SKIP`: the host stops the whole VM on the timeout path, so from outside the guest that is indistinguishable from the VM teardown |
| `cancel` records `cancelled`; replaying that cancellation request against a **new** allocation of the same slot does nothing (token mismatch) |

Every "the guest agent cannot ..." check in this section uses `check_denied`,
which re-proves the SSH channel *after* the attempt failed: a dead channel is
reported as `SKIP` (undecidable), never as a pass, so a transient connection
failure cannot make the whole block pass vacuously. This section was the first
to use it; invariant 2 above now extends the same rule to every other section
(`check_fails` gives no such guard — any non-zero exit counts — so it is used
only for host-side commands).

What the eval/build tier already executes for the same properties (run by
`nix flake check`, no KVM needed):

- `microvm-batch-result-integrity` runs the real host verifier and the real
  guest-side permission assertions against 63 forged / stale / malformed /
  symlinked / world-writable fixtures under `fakeroot` — including a
  worker-owned, world-writable or symlink-shadowed `worker-logs/`;
- `microvm-batch-controller-smoke` runs the real, unmodified guest **controller**
  (39 assertions) inside `bwrap` with a stubbed `systemctl`: healthy job, failing
  agent, deadline, token-bound cancellation, stale cancellation, six rejected
  specs, and a broken trust boundary — then feeds the documents it produced to
  the host verifier, which must accept them for this allocation and reject them
  for another. It also binds an **argv recorder** over the exact `jq` the
  controller resolves and requires that the allocation token never appears in any
  recorded argv (`/proc/<pid>/cmdline` is world-readable `0444`), while requiring
  that the recorder actually observed invocations and that the token *is* in the
  documents produced — so the check cannot pass vacuously;
- `microvm-batch-launcher-submit` runs the real **host** `agent-microvm submit`
  (39 assertions) with `systemctl`/`mount`/`umount`/`findmnt` stubbed, where the
  `systemctl start microvm@<slot>` stub plays the guest: it records the effective
  ownership/modes of the job share the launcher created (input `0755`, spec
  `0400`, controller `0700`, worker agent-owned, worker-logs root-owned) and
  plants a genuine, a foreign-token, a foreign-slot, a v1, a malformed or a
  worker-only "result". Only the genuine one may yield exit 0; everything else
  must be exit 70, the clone must survive every case, the archived result must be
  `0600` in a `0700` directory, and the same argv recorder must not see the token.

What is deliberately NOT established by that tier: that the guest KERNEL denies a
uid-1000 worker access to `controller/` or `worker-logs/` (fakeroot fakes
metadata, not enforcement), and that systemd really reaps a double-forked
descendant on the cgroup-wide stop — the checks only prove the controller ISSUES
`systemctl kill --kill-whom=all`. Both need this suite on real KVM.

Both prove the *validators, the protocol and the layout*, not the guest kernel's
enforcement or systemd's cgroup kill — which is exactly what this section adds.

### `malrepo` — hostile repository fixture

The fixture contains a git `post-checkout` hook, a `flake.nix` that throws when
evaluated, an `.envrc`, an `.mcp.json` that would run `touch`, symlinks to
`/etc/shadow` and `/`, a nested repository, and a `.git` **file** pointing at
`/etc`. Expected: the host never ran the hook, never sourced the `.envrc`, never
ran the MCP command; the guest cannot read `/etc/shadow` through the symlink; a
fork bomb and a bounded disk-filling attempt leave both the guest and the host
healthy; the guest cannot enumerate host block devices.

Two honesty notes:

- **flake evaluation is not asserted.** A Nix evaluation cannot create a marker
  file, so there is no observable to test; the check that used to carry that
  name actually tested the *direnv* marker and is now named after what it
  measures. The throwing `flake.nix` stays in the fixture: an evaluation would
  be loud in the launcher output.
- **the disk filler is bounded by the host's free space** (at most 2 GiB, and
  only when ≥ 4 GiB would remain). `/workspace` is a bind mount of a HOST
  directory, so an unbounded 20 GiB write could wedge the very host whose health
  the next check asserts.

The fixture is the **source repository of the guest under test** (it used to be
built and then not used, so the in-guest assertions ran against a workspace that
never contained any of it). A positive control asserts that
`/workspace/escape-shadow` really is a symlink in the guest before the escape
check concludes anything.

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
