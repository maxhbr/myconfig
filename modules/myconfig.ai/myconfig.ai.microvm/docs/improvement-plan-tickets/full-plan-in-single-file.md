
# Improve the `myconfig.ai.microvm` Agent Sandbox

## Objective

Improve the existing NixOS MicroVM-based coding-agent sandbox located at:

```text
modules/myconfig.ai/myconfig.ai.microvm
```

The current implementation already has a strong architecture:

* A fixed pool of prebuilt MicroVM slots.
* Cloud Hypervisor guests.
* Dynamic allocation through a launcher.
* Standalone Git clones as workspaces.
* Virtiofs workspace attachment.
* Self-contained guest Nix stores.
* Host-side LiteLLM proxying.
* No upstream API credentials inside guests.
* Interactive access through SSH and Workmux.
* VM lifecycle managed through systemd.
* Atomic allocation through `flock`.

Preserve this architecture. Do not replace it with dynamically generated per-job NixOS configurations, OCI containers, shared host Nix stores, or mutable long-running guests.

Implement the improvements below incrementally, keeping the secure defaults and current interactive workflow working throughout.

---

# 1. Inspect and document the current implementation

Before modifying code, inspect all files under:

```text
modules/myconfig.ai/myconfig.ai.microvm
```

At minimum, identify the responsibilities of:

```text
default.nix
guest.nix
guest-home.nix
launcher.nix
network.nix
workmux.nix
docs/
```

Confirm the current flow:

```text
Workmux or CLI
    → agent-microvm launcher
    → allocate an agent-N slot
    → create or select a standalone Git clone
    → bind-mount it into the slot workspace directory
    → start microvm@agent-N.service
    → wait for SSH
    → run the selected agent
    → stop the VM
    → unmount the workspace
    → retain or remove the clone according to policy
```

Record any discrepancy between this description and the code before making changes.

Do not weaken any existing cleanup traps, lock handling, Git path validation, or credential isolation.

---

# 2. Create one declarative agent registry

## Problem

The supported agent list is currently duplicated across several places, such as:

* Guest package installation.
* Launcher argument validation.
* Launcher help text.
* Workmux agent definitions.
* Agent command dispatch.

This makes it easy for the lists to drift.

## Required change

Define one Nix attribute set that is the authoritative registry for all supported agents.

The registry must include at least:

```text
claude
codex
pi
opencode
hermes
```

Use a structure similar to:

```nix
agentSpecs = {
  claude = {
    package = ...;
    executable = "claude";
    workmuxName = "microvm-claude";
    interactiveArgs = [ ];
    batchArgs = promptFile: [ ... ];
  };

  codex = {
    package = ...;
    executable = "codex";
    workmuxName = "microvm-codex";
    interactiveArgs = [ ];
    batchArgs = promptFile: [ ... ];
  };

  pi = {
    package = ...;
    executable = "pi";
    workmuxName = "microvm-pi";
    interactiveArgs = [ ];
    batchArgs = promptFile: [ ... ];
  };

  opencode = {
    package = ...;
    executable = "opencode";
    workmuxName = "microvm-opencode";
    interactiveArgs = [ ];
    batchArgs = promptFile: [ ... ];
  };

  hermes = {
    package = ...;
    executable = "hermes";
    workmuxName = "microvm-hermes";
    interactiveArgs = [ ];
    batchArgs = promptFile: [ ... ];
  };
};
```

Adapt the exact fields to the repository’s Nix style.

## Generate from the registry

Use this registry to generate:

1. The guest’s installed agent packages.
2. The list of valid launcher agent names.
3. Launcher help text.
4. Workmux agent entries.
5. The guest-side command dispatch.
6. Any assertions about supported agents.

Do not maintain independent hard-coded lists after this change.

## Hermes packaging

Locate the project’s existing Hermes package or module, if present.

If Hermes is not currently packaged in the active package set:

1. Add or import an appropriate package derivation.
2. Ensure the executable name is correct.
3. Ensure the guest can evaluate and build without fetching mutable dependencies at runtime.
4. Do not install Hermes through `pip`, `npm`, or another package manager during guest boot.

## Acceptance criteria

The following must work from the same registry:

```bash
agent-microvm run --agent pi
agent-microvm run --agent opencode
agent-microvm run --agent hermes
```

The generated help output must list all supported agents.

The guest configuration must contain all corresponding packages.

Workmux must expose all corresponding `microvm-*` agents.

---

# 3. Define explicit persistent-state policy per agent

## Problem

The VM root filesystem and guest home are intended to be disposable, but some agents may normally store useful state under their home or configuration directories.

Hermes in particular may expect persistent memory, skills, sessions, or configuration.

Persistence must be explicit rather than accidental.

## Required change

Extend each registry entry with a persistence declaration, for example:

```nix
persistentState = {
  enabled = false;
  directories = [ ];
};
```

Hermes may use something resembling:

```nix
persistentState = {
  enabled = true;
  directories = [
    ".config/hermes"
    ".local/share/hermes"
  ];
};
```

Determine the actual paths used by the installed Hermes version. Do not guess silently.

## Storage model

Use one of these policies:

### Default policy

For Claude, Codex, Pi, and OpenCode:

* Guest home remains disposable.
* No state is retained unless explicitly enabled.
* Authentication must still be provided through the host proxy or runtime-only files.

### Optional per-task state

For agents that need persistence:

```text
/var/lib/agent-microvms/state/<task>/<agent>/
```

Expose only the explicitly declared subdirectories.

Do not expose:

```text
the host user’s home
~/.ssh
the SSH agent socket
the Nix daemon socket
container runtime sockets
host-wide agent configuration
unrelated task state
```

State must be owned by the expected guest UID/GID or mounted through a mechanism that gives the guest the required ownership.

## CLI behavior

Add an explicit launcher option such as:

```bash
--persist-agent-state
```

or a configuration-level default.

Persistence must not be silently enabled for all jobs.

## Acceptance criteria

* A default Pi or OpenCode session starts with a clean guest home.
* A Hermes session can optionally persist only its declared directories.
* State from task A is not visible to task B.
* No host home-directory files are visible inside the guest.

---

# 4. Keep interactive mode and add batch mode

## Existing behavior to preserve

The current interactive workflow must continue to work:

```bash
agent-microvm run \
  --attach \
  --agent pi \
  ...
```

It should still:

1. Allocate a slot.
2. Start the VM.
3. Wait for SSH readiness.
4. Attach an interactive terminal.
5. Stop and clean up after the session exits.

## New behavior

Add an unattended batch mode.

A target interface could be:

```bash
agent-microvm submit \
  --name fix-parser \
  --repository /path/to/repository \
  --agent opencode \
  --prompt-file /path/to/prompt.md \
  --timeout 3600
```

The exact CLI shape may follow the project’s existing conventions, but it must support:

* Task name.
* Repository path.
* Agent selection.
* Prompt file or job specification.
* Hard timeout.
* Optional resource class.
* Optional persistent agent state.
* Optional workspace cleanup policy.

## Do not pass prompts through process arguments

Do not execute this pattern:

```bash
agent "$PROMPT"
```

when the prompt may be large or sensitive.

Process arguments may be visible in process listings and logs.

Instead, write a job specification into a runtime-only directory.

Suggested structure:

```text
/var/lib/microvms/agent-N/job/
├── spec.json
├── prompt.md
└── secrets/
```

The guest should receive this directory through one of:

1. A small read-only virtiofs share.
2. A VSOCK-based transfer.
3. Another explicit runtime-only control mechanism.

Do not store job prompts or secrets in the Nix store.

## Suggested job specification

Use a versioned format:

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

Validate the specification on both the host and guest.

Reject:

* Unknown versions.
* Unknown agent names.
* Relative workspace paths.
* Prompt paths outside the job directory.
* Negative or excessive timeouts.
* Arbitrary executable paths.

---

# 5. Add a guest-side batch job service

Create a guest systemd service, such as:

```text
agent-job.service
```

It should be disabled or inert when no batch job specification is mounted.

## Required service behavior

The service must:

1. Wait for the required workspace and job mounts.
2. Validate the job specification.
3. Resolve the selected agent through the generated registry.
4. Run as the unprivileged `agent` user.
5. Set the working directory to `/workspace`.
6. Apply a hard runtime limit.
7. Record structured status.
8. Never expose host provider credentials.
9. Optionally power off the VM after job completion.

## Security hardening

Apply appropriate service hardening without blocking normal coding tools:

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

Review each hardening setting against tools that agents may need, such as:

* Git.
* Compilers.
* Language runtimes.
* Test runners.
* Local subprocesses.
* Unix sockets within the guest.
* Temporary directories.

Do not add restrictions without testing their impact.

## Result format

Write a machine-readable result to a host-visible runtime location:

```text
result.json
```

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

Valid states should include:

```text
starting
running
completed
failed
timed-out
cancelled
infrastructure-error
```

Write updates atomically:

1. Write to a temporary file.
2. `fsync` if appropriate.
3. Rename over the final file.

Do not treat the presence of an arbitrary workspace file as sufficient proof of completion.

---

# 6. Add host-side batch lifecycle management

Extend the launcher to support the full unattended lifecycle:

```text
validate request
→ allocate slot
→ prepare standalone clone
→ prepare state directories
→ prepare job specification
→ bind workspace
→ start VM
→ wait for readiness
→ wait for structured result
→ enforce host-side timeout
→ stop VM
→ collect result
→ unmount
→ clean runtime secrets
→ retain or delete workspace
→ release slot
```

## Timeout handling

Enforce the timeout in two places:

1. In the guest systemd service.
2. In the host launcher.

The host timeout should be slightly longer to allow guest-side cleanup and result writing.

On timeout:

1. Mark the task as timed out.
2. Request a clean VM stop.
3. Wait for a bounded shutdown period.
4. Force-stop the unit if required.
5. Unmount workspace and job shares.
6. Remove runtime secrets.
7. Preserve the workspace unless configured otherwise.
8. Release the slot.

## Cancellation

Add a command such as:

```bash
agent-microvm cancel <task-or-session-id>
```

Cancellation must validate that the task owns the selected slot before stopping anything.

Do not allow a stale task marker to terminate a newly allocated VM.

## Status

Add a command such as:

```bash
agent-microvm status
agent-microvm status <task-id>
```

It should report:

* Task ID.
* Slot.
* Agent.
* VM unit state.
* Job state.
* Workspace path.
* Start time.
* Timeout.
* Whether the task appears stale.

---

# 7. Preserve and strengthen allocation safety

Keep the existing global allocator lock, per-slot lock, and session marker model.

Review it for these race conditions:

1. Two processes selecting the same free slot.
2. A process dying between marker creation and VM startup.
3. A VM stopping while the launcher remains alive.
4. A stale marker surviving a host reboot.
5. A new task reusing a slot before old cleanup completes.
6. A cancellation command stopping a slot belonging to another task.
7. A bind mount remaining after a failed launch.
8. A stale `microvm@agent-N.service` remaining active.

## Required safeguards

Each allocation marker should include:

```text
task ID
launcher PID
launcher start time
slot name
workspace path
VM unit name
random allocation token
```

Cleanup and cancellation must compare the allocation token, not merely the slot name.

Where practical, verify launcher process identity through both PID and process start time to avoid PID reuse errors.

## Recovery command

Add:

```bash
agent-microvm recover
```

It should:

1. Scan all slots.
2. Compare markers with systemd unit state.
3. Detect stale locks and stale mounts.
4. Stop orphaned VM units when safe.
5. Unmount orphaned workspace/job mounts.
6. Remove stale runtime secret directories.
7. Preserve task clones by default.
8. Print every action it takes.

The command must have a dry-run mode:

```bash
agent-microvm recover --dry-run
```

---

# 8. Add per-TAP Layer 2 isolation

## Problem

IP firewalling through `br_netfilter` does not by itself prevent Layer 2 attacks such as ARP spoofing between guests on the same bridge.

This is particularly relevant because SSH host keys are ephemeral or not pinned.

## Required change

When attaching each TAP interface to the agent bridge, enable bridge-port isolation.

The resulting host logic should include the equivalent of:

```bash
ip link set "$tap" master agentbr0
bridge link set dev "$tap" isolated on
```

Use the correct bridge and TAP names from the current module.

Apply isolation to every guest-facing TAP interface.

Do not apply it blindly to the bridge’s host-facing interface.

## Verify kernel and bridge behavior

Ensure the host has the required tooling and kernel support.

Validate with:

```bash
bridge link show
```

Each active guest TAP should report isolation enabled.

## Required tests

With two VMs running concurrently:

1. VM A must not reach VM B by IPv4.
2. VM A must not reach VM B by IPv6.
3. VM A must not resolve or poison VM B through ARP.
4. VM A must not intercept host-to-VM B traffic.
5. Both VMs must still reach the explicitly allowed LiteLLM endpoint.

Include packet captures where useful:

```bash
tcpdump -ni agentbr0
tcpdump -ni <tap-interface>
```

---

# 9. Improve SSH trust or move control traffic to VSOCK

## Current limitation

Interactive attachment currently depends on SSH across the isolated bridge. If host keys are regenerated for each guest and not pinned, the connection is vulnerable if the network boundary is compromised.

Per-TAP isolation reduces this risk but does not make host-key verification unnecessary.

## Required approach

Implement at least one of these:

### Option A: Deterministic per-slot SSH host keys

Generate or provision one SSH host key per slot on the host.

Expose only the corresponding private host key to the appropriate guest.

Store the public key in a host-side known-hosts file.

Use strict host-key checking:

```bash
ssh \
  -o StrictHostKeyChecking=yes \
  -o UserKnownHostsFile=/var/lib/agent-microvms/known_hosts \
  ...
```

Private host keys must not be shared across slots.

### Option B: VSOCK control channel

Assign each slot a unique VSOCK CID.

Use VSOCK for:

* Guest readiness.
* Batch job submission.
* Status.
* Cancellation.
* Result delivery.

Keep SSH only for explicitly requested interactive terminals.

VSOCK CIDs must be unique among concurrently running VMs.

Do not use reserved CIDs.

## Recommended outcome

Use VSOCK for batch lifecycle and deterministic SSH host keys for interactive access.

---

# 10. Replace network booleans with explicit profiles

## Problem

Independent network booleans can produce unclear or incomplete combinations. In particular, allowing firewall forwarding does not automatically provide working internet access without routing, DNS, and NAT.

## Required change

Replace or deprecate ambiguous booleans with a closed profile option:

```nix
myconfig.ai.microvm.networkProfile = "proxy-only";
```

Support these profiles:

```text
offline
proxy-only
package-access
internet
```

## Profile behavior

### `offline`

Allow only traffic required for host control, if any.

Block:

* LiteLLM.
* Public internet.
* Private networks.
* Metadata endpoints.
* Other guests.

### `proxy-only`

Allow:

* The host-side LiteLLM forwarding endpoint.
* Required DHCP traffic.
* Required host control traffic.

Block:

* DNS to arbitrary servers.
* Public internet.
* Private networks.
* Metadata endpoints.
* Other guests.
* All unrelated host services.

This should remain the default.

### `package-access`

Allow:

* LiteLLM.
* Controlled package access through an explicit proxy or egress gateway.
* Only approved package registries and source hosts.

Do not implement this as unrestricted public internet.

Prefer a host-side HTTP CONNECT proxy or another auditable egress service.

### `internet`

Allow controlled general egress.

This profile must include the complete implementation:

* IP forwarding.
* NAT or masquerading.
* DNS policy.
* Firewall rules.
* Metadata blocking.
* Private-network blocking unless explicitly allowed.
* Guest-to-guest isolation.
* Logging policy.

Do not expose an `internet` profile that merely changes an iptables verdict without creating functional routing.

## Migration

If existing booleans are public configuration options:

1. Keep them temporarily with deprecation warnings.
2. Translate safe combinations to profiles.
3. Reject ambiguous or unsafe combinations.
4. Document the migration.
5. Remove them only in a later compatibility-breaking change.

---

# 11. Add resource classes without per-job Nix evaluation

## Goal

Support different CPU and memory requirements while retaining prebuilt slots.

Do not generate and evaluate a unique guest NixOS configuration for each job.

## Required design

Introduce fixed resource classes:

```text
small
normal
large
```

Example:

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

Generate slot names such as:

```text
agent-small-0
agent-small-1
agent-normal-0
agent-normal-1
agent-large-0
```

Each slot must still have unique:

* TAP interface.
* MAC address.
* IP assignment.
* VSOCK CID.
* State directory.

## Launcher support

Add:

```bash
--resource-class small
--resource-class normal
--resource-class large
```

Default to `normal` or the current equivalent.

The allocator should search only slots from the requested class.

When no matching slot is available, return a clear error or optionally wait with a bounded timeout.

Do not silently allocate a smaller class.

---

# 12. Review workspace and Git isolation

Preserve the standalone-clone design rather than switching to linked worktrees that expose the original repository’s Git metadata.

## Required checks

Retain or add validation that:

1. The workspace is a Git repository.
2. `.git` resolves inside the workspace.
3. The Git common directory resolves inside the workspace.
4. No bind source escapes through symlinks.
5. The repository path is canonicalized before use.
6. The workspace destination is not `/`, `/home`, `/var/lib`, or another dangerous parent.
7. Cleanup only removes directories beneath the configured workspace root.
8. The clone is not created with local hardlinks to the source object database.

Use:

```bash
git clone --no-local
```

or an equivalent isolation-safe method.

## Repository-local execution risk

Treat these as hostile:

```text
Git hooks
direnv files
flake configuration
MCP server configuration
agent instruction files
package-manager scripts
build scripts
test scripts
editor tasks
repository-local binaries
```

The host launcher must never evaluate or execute repository-provided code.

All such execution belongs inside the guest.

---

# 13. Keep credentials out of guests

Preserve the host LiteLLM model:

```text
guest
    → constrained bridge endpoint
    → host LiteLLM
    → upstream providers
```

Upstream provider credentials must remain on the host.

Guests may receive only non-sensitive placeholder variables when an agent refuses to start without a key-shaped value.

## Verify no credential leakage

Inside a guest, confirm the absence of:

```text
host API keys
~/.ssh
SSH agent sockets
host Git credential helpers
host browser credentials
host cloud credentials
Docker or Podman sockets
Nix daemon sockets
host-wide agent configuration
```

Search the guest environment and mounted files during testing.

Do not print secrets in logs or test output.

## Git authentication

When private Git access is required, use task-scoped credentials.

Do not mount the host user’s SSH directory.

Possible mechanisms:

* A short-lived deploy token.
* A task-specific SSH key.
* A controlled host Git proxy.
* A pre-created standalone clone that requires no guest-side remote authentication.

Destroy task-scoped credentials during cleanup.

---

# 14. Add comprehensive real-KVM integration tests

Evaluation tests and Nix assertions are not sufficient for this security boundary.

Create a repeatable live test procedure that boots actual MicroVMs under KVM.

Automate it where practical.

## Boot and filesystem tests

Verify:

1. A slot boots successfully.
2. SSH or VSOCK readiness works.
3. `/workspace` is mounted.
4. `/workspace` is writable.
5. Changes persist in the standalone clone after VM shutdown.
6. Guest root changes do not persist between runs.
7. Guest home changes do not persist unless explicitly configured.
8. A task cannot see another task’s workspace.
9. No host Nix store is exposed as a shared directory.
10. No unexpected host directories are mounted.

## Network tests

From the guest, verify:

### Allowed

* DHCP succeeds.
* The configured LiteLLM endpoint is reachable in `proxy-only`.
* Agent requests can successfully reach LiteLLM.
* Required host control traffic works.

### Blocked

* Host SSH port unless explicitly required.
* Arbitrary host ports.
* The host LAN.
* RFC1918 networks.
* Link-local networks.
* Cloud metadata addresses.
* Public DNS servers.
* Public internet in `offline` and `proxy-only`.
* Other guest IP addresses.
* IPv6 paths that bypass IPv4 filtering.

Test at least:

```text
169.254.169.254
10.0.0.0/8
172.16.0.0/12
192.168.0.0/16
the host LAN gateway
another running guest
a public IP
a public DNS server
```

Adapt exact addresses to avoid disrupting the host environment.

## Layer 2 tests

With two guests running:

* Attempt ARP spoofing.
* Attempt unsolicited ARP replies.
* Attempt direct Ethernet communication.
* Verify bridge isolation remains enabled throughout the run.
* Verify one VM cannot impersonate another slot’s IP.

## Lifecycle failure tests

Force failure at each stage:

1. Clone creation.
2. Workspace validation.
3. Bind mount.
4. VM start.
5. SSH readiness.
6. Agent startup.
7. Agent timeout.
8. Guest crash.
9. Launcher termination.
10. Host reboot.

After each failure, verify:

* No slot remains falsely allocated.
* No unintended VM remains active.
* No stale bind mount remains.
* No secret directory remains.
* The workspace is preserved unless deletion was explicitly requested.
* A subsequent task can reuse the slot safely.

## Malicious repository tests

Create a test repository containing:

* Symlinks pointing outside the workspace.
* A malicious Git hook.
* A malicious `flake.nix`.
* A malicious `direnv` configuration.
* Nested Git repositories.
* A `.git` file pointing outside the workspace.
* Repository-local MCP configuration.
* Scripts that attempt to access host services.
* Fork bombs or process explosions.
* Disk-filling behavior.
* Attempts to inspect mounted devices.
* Attempts to reach metadata addresses.

Confirm the host launcher never executes repository code and the guest remains constrained.

---

# 15. Add limits and abuse resistance

Apply limits at both the guest and host layers.

## Guest limits

Use systemd resource controls for batch jobs:

```nix
MemoryMax = "...";
TasksMax = "...";
CPUQuota = "...";
RuntimeMaxSec = "...";
```

Adapt values by resource class.

Ensure that an agent or repository cannot indefinitely:

* Fork processes.
* Consume all guest memory.
* Fill guest temporary filesystems.
* Run after task cancellation.
* Keep the VM alive beyond the configured timeout.

## Host limits

Ensure the total number of concurrently running guests cannot exceed the configured pool size.

Consider host-side limits for the hypervisor service:

```nix
MemoryMax
TasksMax
CPUWeight
IOWeight
```

Do not set a host `MemoryMax` below the guest’s configured memory plus hypervisor overhead.

## Disk limits

Bound:

* Workspace clone size where practical.
* Agent-state size.
* Runtime job directory size.
* Guest writable root disk.
* Temporary filesystem size.
* Log retention.

Do not allow abandoned task clones to grow without visibility.

Add a command to list retained workspace sizes.

---

# 16. Improve observability without leaking secrets

Add structured logs for:

```text
task submitted
slot allocated
workspace created
VM start requested
VM ready
agent started
agent finished
timeout
cancellation
VM stopped
cleanup completed
recovery action
```

Each log record should include:

```text
task ID
slot
agent
resource class
state transition
timestamp
exit code where applicable
```

Do not log:

```text
API keys
complete prompts by default
private repository credentials
secret environment variables
private key material
```

Make the journal useful through:

```bash
journalctl -u agent-microvm
journalctl -u microvm@agent-normal-0
```

For batch jobs, retain a bounded task log under the task directory or another explicit results directory.

---

# 17. Synchronize documentation with code

Review all documentation under:

```text
modules/myconfig.ai/myconfig.ai.microvm/docs
```

Remove claims that are no longer true.

Specifically verify documentation around:

* Passwordless control.
* Sudo policy.
* SSH trust.
* ARP and Layer 2 isolation.
* Public internet support.
* KVM runtime validation.
* Hermes support.
* Batch mode.
* Resource classes.
* Persistent agent state.
* Recovery behavior.

## Required documents

Provide or update:

### Architecture document

Explain:

```text
slot pool
workspace indirection
self-contained guest store
network path
credential boundary
interactive path
batch path
state persistence
```

### Operator guide

Include:

```bash
start an interactive task
submit a batch task
inspect status
attach for debugging
cancel a task
collect results
remove a workspace
recover stale slots
inspect logs
```

### Security model

Clearly state:

* What is trusted.
* What is untrusted.
* What the VM boundary protects.
* What remains shared through virtiofs.
* What LiteLLM protects.
* Which attacks are mitigated.
* Which risks remain.
* Why the system is not equivalent to a remote multi-tenant cloud sandbox unless further controls are added.

### Test guide

Include exact commands and expected results for real-KVM validation.

---

# 18. Preserve backward compatibility

Existing configurations and workflows should continue to work where possible.

At minimum, preserve:

```bash
agent-microvm run --attach --agent pi
agent-microvm run --attach --agent opencode
```

Preserve current option names unless replacing them is necessary.

For deprecated options:

1. Emit a clear Nix warning.
2. Document the replacement.
3. Translate safe configurations automatically.
4. Reject unsafe ambiguous combinations.

Do not silently reinterpret a previously secure configuration as a less restrictive one.

---

# 19. Implementation sequence

Implement in this order.

## Phase 1: refactor without behavior changes

1. Inspect current modules.
2. Add the single agent registry.
3. Generate existing supported agents from the registry.
4. Confirm evaluation and builds are unchanged.
5. Update unit tests or assertions.

## Phase 2: add Hermes

1. Package or import Hermes.
2. Add it to the registry.
3. Generate guest installation.
4. Generate launcher support.
5. Generate Workmux support.
6. Add interactive smoke tests.
7. Document its state paths.

## Phase 3: network hardening

1. Add bridge-port isolation.
2. Add deterministic SSH trust or VSOCK.
3. Replace network booleans with profiles.
4. Ensure `internet` includes complete NAT and DNS behavior.
5. Add network integration tests.

## Phase 4: batch execution

1. Define the versioned job format.
2. Add the runtime job directory.
3. Add the guest `agent-job.service`.
4. Add structured results.
5. Add host submit, status, cancel, and recovery commands.
6. Add host and guest timeouts.

## Phase 5: resource and state management

1. Add fixed resource classes.
2. Add optional per-task agent state.
3. Add disk and process limits.
4. Add retained-workspace reporting.

## Phase 6: validation and documentation

1. Run real-KVM boot tests.
2. Run multi-guest network tests.
3. Run malicious-repository tests.
4. Run failure-recovery tests.
5. Update all documentation.
6. Record known residual risks.

Do not combine all phases into one unreviewable change. Produce focused commits.

---

# 20. Testing commands

Use the repository’s existing formatting, evaluation, and test tooling.

At minimum, run the relevant equivalents of:

```bash
nix flake check
nix eval
nix build
nixos-rebuild build
```

Build the host configuration containing the MicroVM module.

Build at least one guest runner for every resource class.

Verify all agents are present in the guest closure.

Boot real guests and test:

```bash
sudo systemctl start microvm@agent-normal-0
sudo systemctl status microvm@agent-normal-0
sudo journalctl -u microvm@agent-normal-0
```

Test the launcher:

```bash
agent-microvm run --attach --agent pi ...
agent-microvm run --attach --agent hermes ...
agent-microvm submit --agent opencode ...
agent-microvm status
agent-microvm cancel ...
agent-microvm recover --dry-run
```

Run shell validation where applicable:

```bash
shellcheck
```

Format Nix files using the formatter already used by the repository.

---

# 21. Definition of done

The work is complete when all of the following are true:

* The fixed prebuilt slot-pool architecture remains intact.
* No per-job Nix evaluation is required for normal launches.
* Claude, Codex, Pi, OpenCode, and Hermes come from one registry.
* Workmux entries and launcher validation are generated from that registry.
* Interactive workflows still work.
* Batch tasks can execute unattended.
* Batch tasks produce structured results.
* Batch tasks have hard host and guest timeouts.
* Tasks can be queried and cancelled safely.
* Stale allocations can be recovered.
* Every TAP interface uses Layer 2 isolation.
* SSH host identity is verified or control traffic uses VSOCK.
* Network behavior is represented by explicit profiles.
* The default network profile exposes only LiteLLM and required control traffic.
* The internet profile includes complete routing, DNS, NAT, and filtering.
* Upstream provider credentials never enter guests.
* Guest root and home remain disposable by default.
* Hermes state can be persisted explicitly and only per task.
* Multiple fixed resource classes are supported.
* Workspaces remain standalone clones.
* Git metadata cannot escape the workspace.
* Real-KVM tests validate filesystem, network, lifecycle, and cleanup behavior.
* Multi-guest tests validate IPv4, IPv6, ARP, and bridge isolation.
* Documentation matches the resulting implementation.
* Known residual risks are documented rather than implied to be solved.

---

# 22. Constraints

Do not:

* Replace the MicroVM pool with containers.
* Generate a unique NixOS system for every normal task.
* Share the host `/nix/store` into the guest.
* Share the host home directory.
* Share SSH-agent, Docker, Podman, or Nix daemon sockets.
* Put secrets, prompts, or private keys into the Nix store.
* Run repository-provided code on the host.
* Use linked Git worktrees that expose metadata outside the workspace.
* Enable unrestricted networking by default.
* Trust SSH connections with host-key verification disabled after the hardening work.
* Delete retained task workspaces during automatic recovery.
* Duplicate the supported-agent list across modules.
* Claim hostile-workload isolation before the real-KVM tests pass.

Prefer small, auditable changes that preserve the existing secure architecture.
