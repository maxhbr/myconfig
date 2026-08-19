<!--
Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# How to let an agent work on your repo — one full journey

A short, linear walkthrough: from *"I want an agent to work on my repo"* to
*"I have reviewed and imported its changes, and cleaned up"*. Everything below
assumes the host **`f13`** with the shipped configuration: `networkProfile =
"proxy-only"`, `passwordlessControl = true`, and the two resource classes

| class | slots | vcpu | memory |
| --- | --- | --- | --- |
| `small` | `agent-small-0` | 2 | 4 GiB |
| `normal` | `agent-normal-0` | 4 | 8 GiB |

`normal` is the **default** class when you pass no `--resource-class`.

Two things to internalise before you start:

- **Every `agent-microvm` command needs root** — always prefix `sudo`. On `f13`
  this does not prompt (`passwordlessControl = true`), so the workmux panes work
  unattended too.
- **The agent never touches your checkout.** It works in a *standalone git
  clone* at `/var/lib/agent-microvms/workspaces/<repoSlug>__agent-microvm/<task>`,
  on branch `agent/<task>`. You pull the result back yourself (step 5).
  Nothing deletes that clone until you say so (step 6).

For anything not covered here, follow the links in
[Where to go next](#where-to-go-next) — this document deliberately does not
repeat the reference material.

---

## 0. Check the host is healthy (do this first)

```bash
sudo agent-microvm doctor
```

`doctor` is read-only and exits `0` when every component of the model-API path
is fine (host LiteLLM, plus — depending on the resolved model transport, which it
prints — either the bridge-only forwarder socket, the private bridge and its
gateway address and the firewall chains, or the per-VM AF_VSOCK forwarder
sockets; and the per-slot SSH host keys). Each line is `OK` or `FAIL` with a
remediation hint.

Fix any `FAIL` before booting a VM. Skipping this step is the single most common
way to waste an hour on "the agent died after two seconds": both `run` and
`submit` preflight the endpoint, but `doctor` is what tells you *which* piece is
broken.

Then get your bearings:

```bash
sudo agent-microvm list     # slot | class | state | ip | task
sudo agent-microvm --help   # commands, supported agents, resource classes
```

Supported agents: `claude`, `codex`, `pi`, `opencode`, `hermes`.

This journey assumes a host with BOTH execution capabilities (the default). If a
step is refused with `'submit' needs the 'batch' capability` (or
`'run' needs the 'interactive' capability`), the host deliberately selects only
one of them — see [Capabilities](./agent-microvm.md#capabilities). A host that
also selects the `vsock` capability (plan phase 6) gets a VSOCK control channel
that lets `agent-microvm ssh` reach a guest even without a TCP sshd — and with
`networkProfile = "proxy-only"` its guests have no network interface at all (the
model API goes over AF_VSOCK; see
[VSOCK versus TAP transport](./agent-microvm.md#vsock-versus-tap-transport)).
Nothing in this journey changes: the guest endpoint, the commands and the outputs
are identical under both transports.

## 1. Interactive: hand a task to an agent

The normal route is **workmux**, which owns the worktree, the tmux pane and its
cleanup; `agent-microvm` is only the backend:

```bash
cd ~/src/my-repo
workmux add --agent microvm-pi fix-parser
```

That derives the task name from the workmux branch, clones your repo into a
fresh workspace, boots a microVM from the default (`normal`) class, and runs the
agent attached to the pane.

The same thing without workmux, attached to your current terminal:

```bash
sudo agent-microvm run --attach \
  --name fix-parser --repository ~/src/my-repo \
  --agent pi --resource-class normal
```

Both `--name` and `--repository` are optional: when omitted, `--repository`
defaults to the current directory and `--name` to its basename. So a bare
invocation from inside a repo checkout launches a slot for that repo:

```bash
sudo agent-microvm run --attach --agent pi
```

When the agent exits, the VM is stopped and torn down and **the clone is kept**.

Only one slot per class exists on `f13`, so a second task in the same class
fails fast with `no free slot in resource class 'normal'`. Either use
`--resource-class small`, or wait for a slot:

```bash
sudo agent-microvm run --attach --name fix-parser --repository ~/src/my-repo \
  --agent pi --wait 600
```

## 2. Batch: submit an unattended task

Same allocation, clone and retention semantics — but no human in the pane, a
hard timeout, and a machine-readable result:

```bash
printf 'Fix the parser bug in src/parse.rs and add a regression test.\n' \
  > /tmp/prompt.md

sudo agent-microvm submit \
  --name fix-parser-batch --repository ~/src/my-repo \
  --agent opencode --prompt-file /tmp/prompt.md \
  --timeout 3600 --resource-class normal
echo "exit=$?"
```

`submit` blocks until the guest's trusted *controller* writes a terminal result,
prints that result JSON, stops the VM, and keeps the clone.

**Exit codes — the whole contract:**

| code | meaning |
| --- | --- |
| `0` | `completed` — the agent finished successfully |
| `1` | `failed` — the agent ran and exited non-zero (a bounded, `cat -v`-sanitised tail of the *untrusted* worker stderr is printed to help you triage) |
| `124` | `timed-out` — the `--timeout` budget was exhausted; the worker's whole cgroup was killed |
| `130` | `cancelled` — someone ran `agent-microvm cancel` |
| `70` | `infrastructure-error` — the job never really ran, or its result could not be trusted (see [troubleshooting](#7-when-something-looks-wrong)) |

There is deliberately **no `--no-preflight` for `submit`**: an unattended run
must never boot a doomed VM. (Interactive `run` does accept `--no-preflight`,
because a human is present and a shell in the workspace is still useful when the
endpoint is down.)

## 3. Watch it while it runs

From another terminal:

```bash
sudo agent-microvm status fix-parser-batch   # slot, job state, timeout, IP, …
journalctl -t agent-microvm -f               # structured lifecycle events

# untrusted worker output, no SSH needed:
sudo tail -f /var/lib/agent-microvms/sessions/agent-normal-0/worker-logs/stdout.log

# or get a shell in the guest as the `agent` user:
sudo agent-microvm ssh fix-parser-batch
```

Changed your mind?

```bash
sudo agent-microvm cancel fix-parser-batch    # keeps the clone; submit exits 130
```

## 4. Read the result

```bash
sudo cat /var/lib/agent-microvms/results/fix-parser-batch.json   # root-only: it
                                                                 # still carries
                                                                 # the allocation
                                                                 # token
sudo jq -r '.state, .exitCode, .source' \
  /var/lib/agent-microvms/results/fix-parser-batch.json
```

`source: "controller"` means the guest's trusted controller wrote it and the
host validated it; `source: "host"` means the host had to record the outcome
itself (e.g. the VM died, or a cancellation the controller never confirmed).
Anything the *agent* wrote — `worker/result.json`, `/workspace/result.json`, a
completion marker — is ignored by design.

Interactive sessions produce no result JSON; the branch in the clone *is* the
result.

## 5. Review and import the work

The clone is a normal git repository, so just use git:

```bash
WS=/var/lib/agent-microvms/workspaces/my-repo__agent-microvm/fix-parser

sudo git -C "$WS" log --oneline origin/HEAD..agent/fix-parser
sudo git -C "$WS" diff origin/HEAD..agent/fix-parser
```

Then bring it into your own checkout — fetch the branch:

```bash
git -C ~/src/my-repo fetch "$WS" agent/fix-parser
git -C ~/src/my-repo log --oneline FETCH_HEAD
git -C ~/src/my-repo switch -c review/fix-parser FETCH_HEAD    # or cherry-pick
```

…or take it as patches, if you would rather read them before they touch your
object store:

```bash
sudo git -C "$WS" format-patch origin/HEAD..agent/fix-parser -o /tmp/patches
git -C ~/src/my-repo am /tmp/patches/*.patch
```

Review the diff as you would any untrusted contribution — the agent ran with
full control of that clone.

## 6. Clean up

`stop` and `destroy` release the VM and the slot but **keep** your clone; only
`workspace-remove` deletes it:

```bash
sudo agent-microvm usage                             # what is retained, and how big
sudo agent-microvm stop fix-parser-batch             # if a VM is still running
sudo agent-microvm workspace-remove fix-parser-batch
```

`workspace-remove` refuses (without `--force`) while there are uncommitted
changes, unexported commits, or a slot still holding the clone — it tells you
which. It also drops that task's persisted agent state and archived result, so
pruning frees everything the task retained.

## 7. When something looks wrong

In this order:

```bash
sudo agent-microvm doctor                # model-API path, read-only, scriptable
sudo agent-microvm status                # any slot marked `stale: yes` ?
sudo agent-microvm recover --dry-run     # what recovery WOULD do; changes nothing
sudo agent-microvm recover               # reclaim stale slots (clones always kept)
journalctl -t agent-microvm | jq -c 'select(.task=="fix-parser-batch")'
sudo cat /var/lib/agent-microvms/logs/fix-parser-batch.jsonl   # per-task event log
```

Quick mapping of the usual first-timer surprises:

| symptom | first thing to do |
| --- | --- |
| `PREFLIGHT FAILED: … model endpoint … not reachable` | `sudo agent-microvm doctor` — do not boot anything else yet |
| `no free slot in resource class 'normal'` | `sudo agent-microvm list`; use `--resource-class small` or `--wait <sec>` |
| a command says "permission denied" / `job: unreadable (run as root)` | you forgot `sudo`; the controller channel is root-only `0700` |
| `submit` exited `70` | `journalctl -t agent-microvm \| jq -c 'select(.task=="…")'` and look for `result-rejected` |
| `status` says `stale: yes` after a hard kill | `sudo agent-microvm recover --dry-run`, then `recover` |
| the workspace disappeared | it did not — nothing removes it except `workspace-remove` |

The [operator guide](./agent-microvm-operator-guide.md) has the full rejection
table for exit `70`, plus the exact ownership/mode expectations of the job
directories (those modes *are* the trust boundary).

## What is and is not runtime-verified

Honest status, matching the rest of this doc set:

- The **batch trust boundary** (controller validates and owns the result, worker
  cannot forge it) has been exercised on real KVM on `f13` via
  `runtime-validation.sh --section forgery`, and by the eval/build checks
  `microvm-batch-result-integrity`, `microvm-batch-controller-smoke` and
  `microvm-batch-launcher-submit`. The happy path in this document is
  trustworthy.
- The remaining runtime sections (`boot`, `net`, `l2`, `creds`, `lifecycle`,
  `malrepo`) are **NOT EXECUTED** in their current, repaired form. Firewall
  enforcement, layer-2 isolation, credential absence in the guest and cgroup
  limits are designed and eval-asserted, but not currently *measured*. See the
  [runtime validation guide](./agent-microvm-runtime-validation.md) before
  relying on them as security guarantees.
- The step-by-step sequence above has not been captured as an automated
  end-to-end test; treat it as a documented procedure, not a verified one.

## Where to go next

| Document | When |
| --- | --- |
| [`agent-microvm.md`](./agent-microvm.md) | options, activation, agent registry, network profiles, batch job format, limitations |
| [Operator guide](./agent-microvm-operator-guide.md) | every command in detail, all rejection reasons, log inventory |
| [Architecture](./agent-microvm-architecture.md) | slot pool, workspace indirection, network path, execution paths, state lifetimes |
| [Security model](./agent-microvm-security-model.md) | trusted vs untrusted, mitigated attacks, residual risks |
| [Runtime validation](./agent-microvm-runtime-validation.md) | how to re-measure the runtime properties on real KVM |
