# Ticket 2: Add Hermes to the MicroVM Agent Sandbox

## Goal

Add Hermes as a first-class supported coding agent using the declarative registry introduced in Ticket 1.

Hermes must work in the existing interactive MicroVM workflow without weakening guest isolation or installing mutable dependencies during boot.

## Prerequisites

- Ticket 1 is complete.
- A single authoritative `agentSpecs` registry exists.

## Scope

Update the agent registry, guest package closure, guest command dispatch, Workmux integration, state policy, and documentation.

## Implementation steps

1. Locate any existing Hermes package, overlay, flake input, or NixOS/Home Manager module in the repository.
2. Determine the exact package attribute and executable name.
3. Verify whether Hermes requires Python, Node.js, shell utilities, browsers, Git, or other runtime dependencies.
4. Add Hermes to `agentSpecs`.
5. Ensure Hermes is included in the immutable guest closure.
6. Do not install Hermes through `pip`, `npm`, `curl | sh`, or another mutable boot-time mechanism.
7. Generate the launcher, help output, Workmux entry, and guest command dispatch from the registry.
8. Determine Hermes’s actual configuration, memory, skills, session, and cache directories.
9. Document those paths in the registry or a dedicated state-policy structure.
10. Keep Hermes state disposable by default unless the current application requires otherwise.
11. If Hermes cannot operate usefully without persistence, add an explicit opt-in persistence declaration without exposing the host home directory.
12. Ensure Hermes uses the existing LiteLLM path and does not require upstream provider credentials in the guest.
13. Add an interactive smoke test.
14. Update the operator documentation with an example Hermes launch.

## Suggested registry entry

Adapt to the project’s actual package and CLI:

```nix
hermes = {
  package = pkgs.hermes-agent;
  executable = "hermes";
  workmuxName = "microvm-hermes";
  interactiveArgs = [ ];
  persistentState = {
    enabledByDefault = false;
    directories = [
      # Populate only after verifying actual Hermes paths.
    ];
  };
};
```

## Security requirements

Hermes must not receive:

```text
host home directory
host SSH configuration
host SSH agent socket
host Git credentials
host API credentials
Docker or Podman sockets
Nix daemon socket
unrelated task state
```

Any persistent Hermes state must be task-scoped and explicitly mounted.

## Acceptance criteria

The following works through the same generated registry:

```bash
agent-microvm run --attach --agent hermes ...
```

Also verify:

- Hermes appears in launcher help.
- Hermes appears as a Workmux agent.
- Hermes is installed in the guest closure.
- Hermes can reach the configured LiteLLM endpoint.
- Upstream provider credentials are absent from the guest.
- Hermes state paths are documented and not guessed.
- Default guest home remains disposable unless explicitly configured otherwise.

## Validation

Run:

```bash
nix flake check
nix build <guest-runner-or-host-target>
nixos-rebuild build --flake .#<host>
agent-microvm run --attach --agent hermes ...
```

Inside the guest, verify the executable and version:

```bash
command -v hermes
hermes --version
```

Inspect mounted paths and environment variables to confirm that no host credentials or home directories are exposed.
