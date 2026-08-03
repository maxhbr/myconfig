# Ticket 6: Complete Real-KVM Validation, Observability, and Documentation

## Goal

Demonstrate that the final sandbox behaves as designed under real KVM execution, document the verified security model, and provide operators with accurate procedures for normal use and failure recovery.

## Prerequisites

- Tickets 1 through 5 are complete.

## Part A: Real-KVM integration test suite

Evaluation-only tests and Nix assertions are not sufficient.

Create a repeatable test procedure that boots actual MicroVMs under KVM.

Automate it where practical.

### Boot and filesystem tests

Verify:

1. Every resource class can boot.
2. SSH and/or VSOCK readiness works.
3. `/workspace` is mounted and writable.
4. Workspace changes persist in the standalone clone after shutdown.
5. Guest root changes do not persist between runs.
6. Guest home changes do not persist by default.
7. Explicit agent-state persistence works only for declared paths.
8. Task A cannot see task B’s workspace or agent state.
9. The host `/nix/store` is not shared into the guest.
10. No unexpected host directories are mounted.

### Network tests

In `proxy-only`, verify that the guest can reach:

- DHCP.
- Required host control traffic.
- The LiteLLM forwarding endpoint.
- Upstream model APIs only through LiteLLM.

Verify that the guest cannot reach:

- Arbitrary host ports.
- Host SSH unless explicitly required.
- Host LAN systems.
- RFC1918 networks.
- Link-local networks.
- Cloud metadata addresses.
- Public DNS servers.
- Public internet.
- Another guest.
- IPv6 routes that bypass IPv4 policy.

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

### Layer 2 tests

With two simultaneous guests:

- Attempt ARP spoofing.
- Send unsolicited ARP replies.
- Attempt direct Ethernet communication.
- Attempt to impersonate another slot’s IP.
- Verify bridge-port isolation remains enabled.
- Verify guest A cannot intercept host-to-guest-B traffic.

### Credential-leakage tests

Inside a guest, verify absence of:

```text
host API keys
host SSH keys
SSH agent sockets
host Git credential helpers
host browser credentials
host cloud credentials
Docker or Podman sockets
Nix daemon socket
host-wide agent configuration
```

Do not print actual secret values in logs or test output.

### Lifecycle failure tests

Force failure at:

1. Clone creation.
2. Workspace validation.
3. Bind mount.
4. VM startup.
5. Readiness detection.
6. Agent startup.
7. Agent timeout.
8. Guest crash.
9. Launcher termination.
10. Host reboot.

After each failure, verify:

- No slot remains falsely allocated.
- No unintended VM remains active.
- No stale bind mount remains.
- No runtime secret directory remains.
- Workspace is preserved unless deletion was explicitly requested.
- The slot can be reused safely.

### Malicious repository tests

Create a repository containing:

- Symlinks outside the workspace.
- A malicious Git hook.
- A malicious `flake.nix`.
- A malicious direnv file.
- Nested repositories.
- A `.git` file pointing outside the workspace.
- Repository-local MCP configuration.
- Scripts that probe host services.
- Fork bombs or process explosions.
- Disk-filling behavior.
- Device enumeration attempts.
- Metadata endpoint access attempts.

Confirm that the host launcher never executes repository code and that guest limits contain the behavior.

## Part B: Observability

Add structured lifecycle logging for:

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

Each record should include:

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

Make logs discoverable through systemd journal units and retain bounded task logs for batch execution.

## Part C: Documentation synchronization

Review all files under:

```text
modules/myconfig.ai/myconfig.ai.microvm/docs
```

Remove stale claims and update documentation for:

- Passwordless control.
- Sudo policy.
- SSH trust.
- VSOCK behavior.
- Layer 2 isolation.
- Network profiles.
- Public internet support.
- Hermes support.
- Batch execution.
- Resource classes.
- Agent-state persistence.
- Recovery behavior.
- Real-KVM validation status.

## Required documentation

### Architecture document

Explain:

```text
prebuilt slot pool
workspace indirection
self-contained guest store
network path
credential boundary
interactive execution path
batch execution path
state persistence
resource classes
```

### Operator guide

Include exact procedures for:

```bash
start an interactive task
submit a batch task
inspect status
attach for debugging
cancel a task
collect results
remove a retained workspace
recover stale slots
inspect logs
```

### Security model

Clearly state:

- Trusted components.
- Untrusted components.
- What the VM boundary protects.
- What remains shared through virtiofs.
- What LiteLLM protects.
- Which attacks are mitigated.
- Which residual risks remain.
- Why the system is not automatically equivalent to a hardened remote multi-tenant cloud sandbox.

### Test guide

Include exact commands and expected outcomes for the real-KVM validation suite.

## Part D: Backward compatibility and migration

Confirm existing commands continue to work:

```bash
agent-microvm run --attach --agent pi ...
agent-microvm run --attach --agent opencode ...
```

For deprecated configuration options:

1. Emit clear Nix warnings.
2. Document replacements.
3. Translate safe configurations.
4. Reject unsafe ambiguity.
5. Never silently reduce isolation.

## Definition of done

The project is complete when:

- Claude, Codex, Pi, OpenCode, and Hermes come from one registry.
- Interactive and batch modes both work.
- Batch mode has structured status, cancellation, recovery, and hard timeouts.
- All TAP ports use Layer 2 isolation.
- SSH host identity is verified and/or batch control uses VSOCK.
- Network behavior is expressed through explicit profiles.
- Upstream provider credentials never enter guests.
- Guest root and home are disposable by default.
- Agent persistence is task-scoped and explicit.
- Fixed resource classes work without per-job Nix evaluation.
- Real-KVM filesystem, network, lifecycle, and malicious-repository tests pass.
- Documentation matches the implementation.
- Residual risks are documented rather than implied to be solved.

## Suggested final validation commands

Run relevant equivalents of:

```bash
nix flake check
nix eval
nix build
nixos-rebuild build --flake .#<host>
shellcheck <launcher-scripts>
```

Then perform real launches:

```bash
sudo systemctl start microvm@agent-normal-0
sudo systemctl status microvm@agent-normal-0
sudo journalctl -u microvm@agent-normal-0

agent-microvm run --attach --agent pi ...
agent-microvm run --attach --agent hermes ...
agent-microvm submit --agent opencode ...
agent-microvm status
agent-microvm cancel <task-id>
agent-microvm recover --dry-run
```
