# Ticket 4: Add Unattended Batch Execution and Safe Lifecycle Management

## Goal

Preserve the existing interactive SSH workflow and add a complete unattended batch mode with structured job specifications, hard timeouts, status reporting, cancellation, and recovery.

## Prerequisites

- Tickets 1 through 3 are complete.
- The supported-agent registry exists.
- Slots have unique VSOCK CIDs or another trustworthy control channel.

## Existing behavior to preserve

```bash
agent-microvm run --attach --agent pi ...
agent-microvm run --attach --agent opencode ...
agent-microvm run --attach --agent hermes ...
```

Interactive mode must continue to:

1. Allocate a slot.
2. Prepare a standalone workspace clone.
3. Start the VM.
4. Wait for readiness.
5. Attach an interactive terminal.
6. Stop the VM when the session exits.
7. Preserve or remove the workspace according to current policy.

## New CLI surface

Add a batch submission command following repository conventions, for example:

```bash
agent-microvm submit \
  --name fix-parser \
  --repository /path/to/repository \
  --agent opencode \
  --prompt-file /path/to/prompt.md \
  --timeout 3600 \
  --resource-class normal
```

Add:

```bash
agent-microvm status
agent-microvm status <task-id>
agent-microvm cancel <task-id>
agent-microvm recover
agent-microvm recover --dry-run
```

## Versioned job format

Do not pass prompts through process arguments.

Write a runtime-only job directory such as:

```text
/var/lib/microvms/agent-N/job/
├── spec.json
├── prompt.md
└── secrets/
```

Suggested `spec.json`:

```json
{
  "version": 1,
  "taskId": "fix-parser",
  "agent": "opencode",
  "workspace": "/workspace",
  "promptFile": "/run/agent-job/prompt.md",
  "timeoutSeconds": 3600,
  "cleanup": {
    "removeWorkspaceOnSuccess": false,
    "removeWorkspaceOnFailure": false
  }
}
```

Validate on both host and guest.

Reject:

- Unknown schema versions.
- Unsupported agents.
- Relative workspace paths.
- Prompt paths outside the job directory.
- Invalid or excessive timeouts.
- Arbitrary executable paths.

Do not put prompts, job specifications, or secrets in the Nix store.

## Guest-side service

Create `agent-job.service` or an equivalent oneshot unit.

It must:

1. Remain inert when no batch job is present.
2. Wait for workspace and job mounts.
3. Validate `spec.json`.
4. Resolve the agent through generated registry metadata.
5. Run as the unprivileged `agent` user.
6. Use `/workspace` as the working directory.
7. Apply a hard runtime limit.
8. Record structured state transitions.
9. Never receive upstream provider credentials.
10. Optionally power off after completion.

Apply appropriate systemd hardening:

```nix
serviceConfig = {
  User = "agent";
  Group = "users";
  WorkingDirectory = "/workspace";
  NoNewPrivileges = true;
  PrivateDevices = true;
  ProtectKernelTunables = true;
  ProtectKernelModules = true;
  ProtectControlGroups = true;
  RestrictSUIDSGID = true;
  RuntimeMaxSec = "...";
};
```

Test hardening against compilers, test runners, package tooling, local subprocesses, and temporary files.

## Structured result

Write `result.json` atomically to a host-visible runtime location.

Example:

```json
{
  "version": 1,
  "taskId": "fix-parser",
  "agent": "opencode",
  "state": "completed",
  "exitCode": 0,
  "startedAt": "2026-08-03T18:10:00Z",
  "finishedAt": "2026-08-03T18:24:13Z",
  "timedOut": false
}
```

Supported states:

```text
starting
running
completed
failed
timed-out
cancelled
infrastructure-error
```

Use temporary-file-plus-rename semantics for atomic updates.

## Host lifecycle

Implement:

```text
validate request
→ allocate slot
→ prepare standalone clone
→ prepare state directories
→ prepare job specification
→ mount workspace and job data
→ start VM
→ wait for readiness
→ wait for structured result
→ enforce host timeout
→ stop VM
→ collect result
→ unmount
→ remove runtime secrets
→ retain or delete workspace
→ release slot
```

## Timeout and cancellation

Enforce timeouts in both guest and host.

The host timeout should allow a small grace period for guest cleanup and result writing.

On timeout or cancellation:

1. Verify task ownership through an allocation token.
2. Request clean shutdown.
3. Wait for a bounded interval.
4. Force-stop if required.
5. Unmount all runtime mounts.
6. Remove runtime secrets.
7. Preserve workspace unless explicitly configured otherwise.
8. Release the slot.

## Allocation safety

Allocation markers must contain at least:

```text
task ID
launcher PID
launcher process start time
slot name
workspace path
VM unit name
random allocation token
```

Cleanup and cancellation must compare the token, not just the slot name.

Protect against PID reuse by comparing both PID and process start time where practical.

## Recovery

`agent-microvm recover` must:

1. Scan every slot.
2. Compare allocation markers with systemd unit state.
3. Detect stale locks and stale mounts.
4. Stop orphaned units when safe.
5. Unmount orphaned workspace and job mounts.
6. Remove stale runtime secret directories.
7. Preserve task clones by default.
8. Print every action.

`--dry-run` must make no changes.

## Acceptance criteria

- Existing interactive commands remain functional.
- Batch jobs run unattended.
- Batch jobs produce structured results.
- Host and guest both enforce timeouts.
- Status reports task, slot, agent, VM state, job state, workspace, start time, and timeout.
- Cancellation cannot stop a slot belonging to another task.
- Recovery safely handles stale units, mounts, and markers.
- Prompts and secrets never enter the Nix store or process command line.

## Validation

Test:

```bash
agent-microvm submit --agent opencode ...
agent-microvm status
agent-microvm status <task-id>
agent-microvm cancel <task-id>
agent-microvm recover --dry-run
```

Force failure at clone, mount, VM startup, readiness, agent startup, timeout, guest crash, launcher termination, and host reboot stages.
