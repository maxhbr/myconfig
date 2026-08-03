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
```

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

The command blocks until the guest writes a terminal result (or the host deadline
`timeout + job.gracePeriodSeconds` expires), prints the result JSON, stops the VM
and keeps the clone.

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

## 4. Attach for debugging

```bash
sudo agent-microvm ssh fix-parser              # shell as the guest `agent` user
sudo agent-microvm ssh fix-parser -- systemctl status agent-job
sudo agent-microvm ssh fix-parser -- journalctl -u agent-job -n 50
sudo agent-microvm console agent-normal-0      # serial console (journal)
```

Host-key verification is **strict**; if it fails, the slot's identity is not what
the host expects — investigate rather than bypass. If `known_hosts` is missing:

```bash
sudo systemctl start agent-microvm-hostkeys.service
```

## 5. Cancel a task

```bash
sudo agent-microvm cancel fix-parser
```

Records a `cancelled` result, requests a clean shutdown, force-stops if needed,
unmounts and drops the slot's runtime data — **only** while the slot still
carries that task's allocation token. The clone is kept.

## 6. Collect results

```bash
# batch result (also printed by submit):
cat /var/lib/agent-microvms/results/fix-parser.json

# what the agent changed:
git -C /var/lib/agent-microvms/workspaces/fix-parser diff
git -C /var/lib/agent-microvms/workspaces/fix-parser log --oneline origin/HEAD..

# import into your own checkout:
git -C ~/src/my-repo fetch /var/lib/agent-microvms/workspaces/fix-parser agent/fix-parser
git -C ~/src/my-repo log --oneline FETCH_HEAD
# or as patches:
git -C /var/lib/agent-microvms/workspaces/fix-parser format-patch origin/HEAD..agent/fix-parser -o /tmp/patches
```

## 7. Remove a retained workspace

```bash
sudo agent-microvm usage                        # what is retained, and how big
sudo agent-microvm workspace-remove fix-parser  # guarded
sudo agent-microvm workspace-remove fix-parser --force
```

Without `--force` it refuses on uncommitted changes, unexported commits, or while
a slot still holds the clone (it tells you which). It also removes that task's
persisted agent state and archived result, so pruning frees everything the task
retained.

## 8. Recover stale slots

```bash
sudo agent-microvm recover --dry-run    # prints, changes nothing
sudo agent-microvm recover
```

Handles: stale markers (unit inactive), orphaned units (no marker), orphaned
`attached`/`batch` runs whose launcher died, stale bind mounts and stale job
data. A `detached` slot whose launcher exited is normal and left alone. Clones
are always kept.

## 9. Inspect logs

```bash
journalctl -t agent-microvm -f                      # structured lifecycle events
journalctl -t agent-microvm | jq -c 'select(.task=="fix-parser")'
cat /var/lib/agent-microvms/logs/fix-parser.jsonl    # per-task, bounded

journalctl -u microvm@agent-normal-0            # guest console (incl. guest events)
journalctl -u agent-litellm-proxy               # bridge-only LiteLLM forwarder
journalctl -u agent-microvm-attach-agent-normal-0   # TAP enslave + L2 isolation
journalctl -u agent-microvm-hostkeys            # per-slot SSH host keys
```

## 10. When something looks wrong

```bash
sudo agent-microvm status                 # any slot marked stale: yes ?
sudo agent-microvm recover --dry-run      # what recovery WOULD do
bridge link show | grep vm-               # every guest TAP must say "isolated on"
sudo iptables -S | grep AGENT_MICROVM     # the rendered profile ruleset
ss -ltnp | grep 4000                      # forwarder on the BRIDGE address only
```

For a full runtime re-validation (real KVM, boots VMs, sends packets) see the
[runtime validation guide](./agent-microvm-runtime-validation.md).
