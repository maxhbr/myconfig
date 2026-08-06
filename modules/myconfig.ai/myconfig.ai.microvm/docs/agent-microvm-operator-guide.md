<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.microvm` — operator guide

Exact procedures. Concepts are in the
[architecture document](./agent-microvm-architecture.md), options in
[`agent-microvm.md`](./agent-microvm.md), the threat model in the
[security model](./agent-microvm-security-model.md).

All commands require root and are invoked via `sudo`
(`passwordlessControl = true` on `f13`, so no prompt). Nothing here ever deletes
a workspace clone unless you explicitly ask for it.

## 0. Orientation

```bash
sudo agent-microvm --help          # commands, supported agents, resource classes
sudo agent-microvm list            # slot | class | state | ip | task
sudo agent-microvm status          # detailed, all slots
sudo agent-microvm usage           # retained disk usage per task
agent-microvm capabilities         # what this host's guests can do (no root needed)
```

> **Capabilities.** A host selects which execution paths its guests carry
> (`myconfig.ai.microvm.capabilities`, default both). On a host that selects only
> one of them the launcher refuses the other's subcommands with a message naming
> the option — `run`/`ssh` need `interactive`, `submit`/`cancel` need `batch`:
>
> ```console
> agent-microvm: 'submit' needs the 'batch' capability, which this host does not
> select (myconfig.ai.microvm.capabilities = [ interactive ]); add "batch" to
> that list and rebuild to enable it
> ```
>
> That is a configuration answer, not a fault. Ask the host directly instead of
> inferring it from an error:
>
> ```console
> $ agent-microvm capabilities
> capabilities: interactive
> declared: interactive batch
> ```
>
> `console` is never refused (it reads the host's `microvm@<slot>` journal, so it
> works even for a guest without sshd). See
> [Capabilities](./agent-microvm.md#capabilities).

## 1. Start an interactive task

```bash
# through workmux (the normal path — it owns the worktree/pane/cleanup):
workmux add --agent microvm-pi my-feature
workmux add --agent microvm-hermes my-feature

# or directly, attached to this terminal:
sudo agent-microvm run --attach \
  --name my-feature --repository ~/src/my-repo \
  --agent pi --resource-class normal
```

The session ends when the agent exits; the VM is then stopped and the clone kept
at `/var/lib/agent-microvms/workspaces/my-feature`.

Detached variant (VM keeps running, connect later):

```bash
sudo agent-microvm run --name my-feature --repository ~/src/my-repo --agent pi
sudo agent-microvm ssh my-feature
sudo agent-microvm stop my-feature
```

Options worth knowing:

```text
--resource-class small|normal|…   allocate from that class only (never substituted)
--wait <sec>                      bounded wait for a free slot in that class
--branch <name>                   default: agent/<task>
--persist-agent-state             keep the agent's DECLARED state for this task
--no-preflight                    skip the model-endpoint preflight (interactive
                                  `run` only; `submit` has no such flag — batch
                                  runs stay fail-closed)
```

## 2. Submit a batch task

```bash
printf 'Fix the parser bug in src/parse.rs and add a regression test.\n' > /tmp/p.md

sudo agent-microvm submit \
  --name fix-parser --repository ~/src/my-repo \
  --agent opencode --prompt-file /tmp/p.md \
  --timeout 3600 --resource-class normal
echo "exit=$?"     # 0 completed | 1 agent failed | 124 timed out | 70 infra error
```

The command blocks until the guest **controller** writes a terminal result (or the
host deadline `timeout + job.gracePeriodSeconds` expires), prints the result JSON,
stops the VM and keeps the clone.

### Model-endpoint preflight

Before booting a VM, `run` and `submit` open a bounded HTTP connection to the
**same** bridge endpoint a guest would use
(`http://192.168.83.1:4000/v1/models`, forwarded to the loopback LiteLLM). A
successful 2xx means the host backend, the bridge address, the bridge-only
socket and the loopback LiteLLM are all wired up. A failure **aborts before any
VM is booted** and prints the most likely component to check:

```text
agent-microvm: PREFLIGHT FAILED: the model endpoint a guest must reach is not
  reachable at http://192.168.83.1:4000/v1/models (bounded to 3s).
  …
  or run: sudo agent-microvm doctor   (full diagnosis)
```

This makes the "worker dies after 2 s because it cannot reach LiteLLM" failure
FAST and LOUD instead of a 95-minute investigation. The preflight retries up to
3 times (3 s each, 2 s apart) so a cold LiteLLM (DB init on first post-boot
request) is not mistaken for a dead one. It is skipped under the `offline`
profile (no endpoint to probe). The `AGENT_MICROVM_SKIP_PREFLIGHT=1` escape
hatch is **test-only** — production callers must never set it.

Interactive `run` accepts `--no-preflight` to skip the probe deliberately (a
human is present, and a shell in the workspace is still useful for debugging
when the endpoint is down); it prints a `WARNING` pointing at `doctor`. Batch
`submit` has **no** such flag, so an unattended run can never boot a doomed VM
silently.

### Failed-job stderr surfacing

A batch job that exits `failed` (exit 1) surfaces a **bounded** tail (last
8192 bytes) of the worker's own stderr to the operator's terminal, labelled
`UNTRUSTED WORKER STDERR`. The worker is untrusted, so its stderr is run through
`cat -v` (which renders terminal-control bytes as visible `^X`/`M-X`) — a
hostile worker cannot inject ANSI/OSC escapes into the operator's TTY. The tail
is written **only** to stderr, never into the authoritative result JSON or the
structured event stream.

Where things live while a batch task runs (`<jobs> =
/var/lib/agent-microvms/sessions/<slot>`, the ONE writable virtiofs share):

| Path | Owner | Meaning |
| --- | --- | --- |
| `<jobs>/input/spec.json` | `root:root 0400` | the job. Root-only because it carries the allocation token |
| `<jobs>/input/prompt.md` | `root:root 0444` | the prompt |
| `<jobs>/input/cancel.json` | `root:root 0400` | a token-bound cancellation request |
| `<jobs>/controller/state.json` | `root:root 0600` | trusted **progress** (`validating` → `starting-worker` → `running` → `timing-out`/`cancelling` → `finished`). Not an outcome |
| `<jobs>/controller/result.json` | `root:root 0600` | the **AUTHORITATIVE** result. Written only by the guest controller; the guest agent cannot read or write this directory |
| `<jobs>/worker/artifacts/` | guest `agent` | **UNTRUSTED** agent output. Useful for debugging, never evidence |
| `<jobs>/worker-logs/stdout.log`, `stderr.log` | `root:root 0644` | the worker's stdout/stderr. systemd opens them as root, so the agent can read but not rewrite them; the CONTENT is **UNTRUSTED** all the same |
| `/var/lib/agent-microvms/results/<task>.json` | `root:root 0600` (dir `0700`) | the archived, *validated* result (`source: "controller"`) or an explicitly host-generated record (`source: "host"`). Root-only because it still carries the run's allocation token |

A "result" the agent writes anywhere else — `worker/result.json`,
`/workspace/result.json`, a completion marker — is ignored by design.

## 3. Inspect status

```bash
sudo agent-microvm list                 # one line per slot
sudo agent-microvm status               # all slots, full detail
sudo agent-microvm status fix-parser    # resolve a task to its slot
```

Reported: slot, class + sizing, service state, IP, MAC, VSOCK CID, task,
workspace, bind-mount state, agent, mode, job state, timeout, agent-state mode,
start time, SSH readiness, session state, `stale` flag, lock owner. Never
secrets.

`status` works without root, but the LIVE job state does not: the controller
channel is root-only `0700`, so a non-root caller cannot distinguish "no result
yet" from "permission denied" and the `job:` field says
`unreadable (run as root)` instead of implying the job has no state. Two other
`job:` values are worth knowing: `rejected (protocol error)` means the guest
wrote a document that does not belong to the active allocation, while
`unverifiable (host-side verifier error)` means the LAUNCHER called the verifier
wrongly (or it is missing) — a host-side bug, not a guest incident.

## 4. Attach for debugging

```bash
sudo agent-microvm ssh fix-parser              # shell as the guest `agent` user
sudo agent-microvm ssh fix-parser -- systemctl status agent-job-controller
sudo agent-microvm ssh fix-parser -- journalctl -u agent-job-controller -n 50
sudo agent-microvm ssh fix-parser -- systemctl status 'agent-job-worker@*'
sudo agent-microvm console agent-normal-0      # serial console (journal)

# untrusted worker output (host side, no SSH needed):
sudo tail -f /var/lib/agent-microvms/sessions/<slot>/worker-logs/stdout.log
```

The two guest units are deliberately separate: `agent-job-controller` is the
trusted half (it validates the job and writes the result),
`agent-job-worker@<agent>` is the untrusted half that runs the coding agent.

Host-key verification is **strict**; if it fails, the slot's identity is not what
the host expects — investigate rather than bypass. If `known_hosts` is missing:

```bash
sudo systemctl start agent-microvm-hostkeys.service
```

## 5. Cancel a task

```bash
sudo agent-microvm cancel fix-parser
```

Writes a token-bound cancellation request into the job's immutable input, waits up
to 20 s for the guest controller to stop the worker's whole cgroup and record
`cancelled`, then requests a clean shutdown, force-stops if needed, unmounts and
drops the slot's runtime data — **only** while the slot still carries that task's
allocation token. The clone is kept.

Because the request carries the allocation token, a cancellation can never affect
a slot that has meanwhile been re-allocated to another task, and a leftover
request file cannot stop a newly allocated job. If the controller does not
confirm in time, the archived record says so explicitly (`source: "host"`).

## 6. Collect results

```bash
# batch result (also printed by submit) — the VALIDATED controller document:
cat /var/lib/agent-microvms/results/fix-parser.json
jq -r '.state, .exitCode, .source' /var/lib/agent-microvms/results/fix-parser.json

# what the agent changed:
git -C /var/lib/agent-microvms/workspaces/fix-parser diff
git -C /var/lib/agent-microvms/workspaces/fix-parser log --oneline origin/HEAD..

# import into your own checkout:
git -C ~/src/my-repo fetch /var/lib/agent-microvms/workspaces/fix-parser agent/fix-parser
git -C ~/src/my-repo log --oneline FETCH_HEAD
# or as patches:
git -C /var/lib/agent-microvms/workspaces/fix-parser format-patch origin/HEAD..agent/fix-parser -o /tmp/patches
```

## 7. Stop, destroy, remove

These are distinct; **only `workspace-remove` deletes your clone**:

| Command | VM | bind mount | slot runtime state | workspace / git |
| --- | --- | --- | --- | --- |
| `stop <slot\|task>` | stopped | unmounted | removed | **kept** |
| `destroy <slot\|task>` | stopped | unmounted | removed | **kept** |
| `workspace-remove <task> [--force]` | (must already be stopped) | — | — | **deleted** |

`--attach` sessions tear the VM down on exit via a cleanup trap, always keeping
the clone; interrupted launches clean up VM / mount / lock / TAP and keep it too.
A slot with a session marker but an inactive unit is reported `stale: yes` (e.g.
after a hard kill) — reclaim it with `destroy` or `recover`.

## 8. Remove a retained workspace

```bash
sudo agent-microvm usage                        # what is retained, and how big
sudo agent-microvm workspace-remove fix-parser  # guarded
sudo agent-microvm workspace-remove fix-parser --force
```

Without `--force` it refuses on uncommitted changes, unexported commits, or while
a slot still holds the clone (it tells you which). It also removes that task's
persisted agent state and archived result, so pruning frees everything the task
retained.

## 9. Recover stale slots

```bash
sudo agent-microvm recover --dry-run    # prints, changes nothing
sudo agent-microvm recover
```

Handles: stale markers (unit inactive), orphaned units (no marker), orphaned
`attached`/`batch` runs whose launcher died, stale bind mounts and stale job
data. A `detached` slot whose launcher exited is normal and left alone. Clones
are always kept.

An unmount is **verified**: `recover` stops the slot's
`microvm-virtiofsd@<slot>.service` if the share is still held (which is what a
SIGKILLed guest leaves behind), then re-checks with `findmnt`. It never falls
back to a lazy unmount, because that leaves the mount in `findmnt` while
reporting success. A mount it cannot release is printed as
`FAILED to unmount …`, emitted as a `mount-leak` lifecycle event, and makes
`recover` exit non-zero.

### Foreign per-slot state

Every command iterates the slot pool of the **current** generation, so per-slot
state under a name this generation does not define — e.g. `slots/agent-0/` from
before the `agent-<class>-<i>` rename — is invisible to `list` and `status`.
`recover` reports it separately:

```text
foreign: 5 per-slot path(s) whose slot name is NOT in the current pool
foreign:   /var/lib/agent-microvms/slots/agent-0 (slot name agent-0)
foreign:     left alone; remove it with: agent-microvm recover --prune-foreign
```

It is **never** removed implicitly. `--prune-foreign` removes it (and unmounts a
stale foreign bind through the same verified path); combine it with `--dry-run`
to see what that would do first. Reporting foreign state alone is a finding, not
an error: `recover` still exits 0.

## 10. Inspect logs

```bash
journalctl -t agent-microvm -f                      # structured lifecycle events
journalctl -t agent-microvm | jq -c 'select(.task=="fix-parser")'
cat /var/lib/agent-microvms/logs/fix-parser.jsonl    # per-task, bounded

journalctl -u microvm@agent-normal-0            # guest console (incl. guest events)
journalctl -u agent-litellm-proxy               # bridge-only LiteLLM forwarder
journalctl -u agent-microvm-attach-agent-normal-0   # TAP enslave + L2 isolation
journalctl -u agent-microvm-hostkeys            # per-slot SSH host keys
```

One JSON record per transition, on the operator's stderr, in the journal under
the `agent-microvm` tag, and in the per-task log (rotated once at
`taskLogMaxBytes`, default 1 MiB). Events: `task-submitted`, `slot-allocated`,
`workspace-created`, `vm-start-requested`, `vm-ready`, `agent-started`,
`agent-finished`, `timeout`, `cancellation`, `vm-stopped`, `cleanup-completed`,
`recovery-action`, `result-rejected`, `mount-leak`; each carries `ts`, `event`, `task`, `slot`,
`agent`, `resource_class`, `mode` and, where applicable, `state` / `exit_code`.
The guest **controller** emits its own `agent-started` / `agent-finished` /
`timeout` / `cancellation` records to the console, so host and guest transitions
can be correlated. The untrusted worker deliberately emits no lifecycle events.

**Never logged:** API keys, prompt *content* (only path and byte size),
repository credentials, secret environment variables, private key material, or
allocation tokens.

## 11. When something looks wrong

```bash
sudo agent-microvm status                 # any slot marked stale: yes ?
sudo agent-microvm recover --dry-run      # what recovery WOULD do
sudo agent-microvm doctor                 # diagnose the model-API path
bridge -d link show | grep -A2 vm-        # every guest TAP must say "isolated on"
                                          # (-d is REQUIRED: plain `bridge link
                                          #  show` prints no port flags)
sudo iptables -S | grep AGENT_MICROVM     # the rendered profile ruleset
ss -ltnp | grep 4000                      # forwarder on the BRIDGE address only
```

### `agent-microvm doctor`

When `run`/`submit` fails the endpoint preflight (or a guest mysteriously
cannot reach the model API), `doctor` is the deep, read-only host-side
diagnosis. It checks every component the model-API path depends on and exits
non-zero if any check fails, so it is scriptable:

```bash
sudo agent-microvm doctor
```

It reports, section by section:

- **host LiteLLM backend** — is `litellm.service` active, and does
  `127.0.0.1:<litellmPort>/v1/models` answer?
- **bridge-only forwarder socket** — is `agent-litellm-proxy.socket` active,
  and is it ordered after `<bridge>-netdev.service` (so `SO_BINDTODEVICE`
  succeeds at boot)?
- **private bridge + gateway address** — does the bridge interface exist and
carry the gateway address?
- **firewall** — are the `AGENT_MICROVM_INPUT` / `AGENT_MICROVM_FORWARD` chains
installed, and does the INPUT chain ACCEPT the LiteLLM endpoint?
- **per-slot SSH host keys** — does every slot have a host-key directory?

Each line is prefixed `OK` or `FAIL` with a concrete remediation hint. Run it
whenever the preflight fires, before investigating anything else.

### Infrastructure / protocol errors

`submit` exits **70** and the archived state is `infrastructure-error` whenever
the job never really ran *or* its result could not be trusted:

```bash
# what the host decided, and why:
journalctl -t agent-microvm | jq -c 'select(.task=="fix-parser")'
# a rejected result shows up as:
#   {"event":"result-rejected","state":"infrastructure-error","message":"…reject: …"}
cat /var/lib/agent-microvms/logs/fix-parser.jsonl
```

`status <task>` prints `job: rejected (protocol error)` while such a document is
still present. Common reasons and what they mean:

| Rejection reason | Meaning |
| --- | --- |
| `allocation token does not belong to the active allocation` | a **stale** result from an earlier allocation of that slot (or a forgery attempt). The job data of a previous run was not cleared — `recover` fixes it |
| `task id` / `slot` / `agent does not belong …` | the document belongs to a different job; never accepted |
| `schema version mismatch` / `controller version mismatch` | host and guest closure disagree — the running VM predates the current host generation. Stop the slot and re-`submit` (the launcher refreshes the runner on start) |
| `not by the guest controller (uid 0)` / `group/other-writable` / `is a symlink` | the result was **not** written by the trusted controller. Treat as a security finding |
| `not valid JSON` / `unknown field` / `not a terminal state` / `exitCode …` | a malformed or non-terminal document; the guest controller crashed mid-write or a version drifted |
| `the VM stopped without a valid controller result` | the guest died before the controller could write. Check `journalctl -u microvm@<slot>` |

To look at the trusted state by hand (root only, and read-only — never edit it):

```bash
sudo cat /var/lib/agent-microvms/sessions/<slot>/controller/state.json | jq .
sudo ls -ld /var/lib/agent-microvms/sessions/<slot>/{input,controller,worker}
#   input/       root root 0755   (spec.json 0400, prompt.md 0444)
#   controller/  root root 0700
#   worker/      1000 1000 0755
```

If those owners/modes are not exactly that, the guest controller refuses to run
at all (`agent-job-assert-paths` fails) — that is intentional: the modes *are* the
trust boundary.

For a full runtime re-validation (real KVM, boots VMs, sends packets) see the
[runtime validation guide](./agent-microvm-runtime-validation.md).
