# Ticket 5: Add Fixed Resource Classes and Explicit Agent State Management

## Goal

Support multiple VM sizes and optional task-scoped agent persistence without introducing per-job Nix evaluation or exposing host user state.

## Prerequisites

- Tickets 1 through 4 are complete.

## Part A: Fixed resource classes

Introduce fixed, prebuilt resource classes such as:

```nix
resourceClasses = {
  small = {
    count = 2;
    vcpu = 2;
    memoryMiB = 4096;
  };

  normal = {
    count = 4;
    vcpu = 4;
    memoryMiB = 8192;
  };

  large = {
    count = 1;
    vcpu = 8;
    memoryMiB = 16384;
  };
};
```

Generate fixed slots:

```text
agent-small-0
agent-small-1
agent-normal-0
agent-normal-1
agent-large-0
```

Every slot must have unique:

- TAP interface.
- MAC address.
- IP or DHCP identity.
- VSOCK CID.
- State directory.
- SSH host identity, if SSH is enabled.

Add launcher support:

```bash
--resource-class small
--resource-class normal
--resource-class large
```

The allocator must search only the requested class.

Do not silently substitute a smaller resource class.

If no matching slot is free, return a clear error or support a bounded wait option.

## Part B: Agent-state policy

Extend each `agentSpecs` entry with an explicit persistence declaration, for example:

```nix
persistentState = {
  enabledByDefault = false;
  directories = [ ];
};
```

For Hermes, populate the actual verified directories used by the installed version.

Do not guess state paths.

## Storage model

Use task-scoped state under:

```text
/var/lib/agent-microvms/state/<task-id>/<agent>/
```

Expose only declared subdirectories.

Do not expose:

```text
host home directory
~/.ssh
SSH agent socket
Docker or Podman sockets
Nix daemon socket
host-wide agent configuration
state from unrelated tasks
```

Add an explicit opt-in flag such as:

```bash
--persist-agent-state
```

Persistence must not be silently enabled for every task.

## Ownership

Ensure state directories are usable by the guest `agent` UID/GID without recursively changing ownership of unrelated host files.

Document any fixed UID assumptions.

If the implementation relies on UID 1000, add an assertion or make the UID configurable.

## Part C: Resource and abuse limits

Apply guest-side limits appropriate to the selected class:

```nix
MemoryMax = "...";
TasksMax = "...";
CPUQuota = "...";
RuntimeMaxSec = "...";
```

Also review host-side limits on the hypervisor unit:

```text
MemoryMax
TasksMax
CPUWeight
IOWeight
```

Do not set host memory limits below guest configured memory plus hypervisor overhead.

Bound:

- Guest writable root.
- Guest temporary filesystems.
- Agent-state storage.
- Runtime job directories.
- Retained logs.
- Workspace retention growth where practical.

Add a command that reports retained workspace and state sizes.

## Part D: Workspace safety review

Preserve standalone clones.

Retain or add checks that:

1. `.git` resolves inside the workspace.
2. Git common directory resolves inside the workspace.
3. Bind sources cannot escape through symlinks.
4. Repository paths are canonicalized.
5. Cleanup targets remain under the configured workspace root.
6. Clones are created with `git clone --no-local` or an equivalent isolation-safe method.
7. Host launch code never evaluates repository-provided Nix, direnv, hooks, scripts, MCP configuration, or binaries.

## Acceptance criteria

- Resource classes are prebuilt and require no per-job Nix evaluation.
- The launcher allocates only from the requested class.
- Every slot has unique network and control identities.
- Guest home remains disposable by default.
- Hermes or another declared agent can persist only its explicit task-scoped state.
- State from task A is not visible to task B.
- No host home or credentials are exposed.
- Process, memory, runtime, and disk-growth risks are bounded.
- Retained workspace and state usage can be inspected.

## Validation

Build at least one guest runner per class.

Run jobs in each class and confirm reported vCPU and memory.

Verify persistence behavior:

1. Run without `--persist-agent-state`; confirm state is lost after shutdown.
2. Run with persistence; confirm only declared paths survive.
3. Start another task; confirm it cannot read the first task’s state.
4. Confirm host home and credentials remain absent.
