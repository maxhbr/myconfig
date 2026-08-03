# Ticket 1: Refactor Supported Agents into One Declarative Registry

## Goal

Replace all duplicated supported-agent lists with a single Nix attribute set that becomes the authoritative source for guest packages, launcher validation, command dispatch, Workmux integration, and help text.

## Context

The current `myconfig.ai.microvm` implementation supports several coding agents, but agent metadata appears in multiple modules. This creates drift risk and makes adding new agents unnecessarily error-prone.

This ticket is a behavior-preserving refactor. Do not add Hermes yet; that belongs to Ticket 2.

## Scope

Inspect and update at least:

```text
modules/myconfig.ai/myconfig.ai.microvm/default.nix
modules/myconfig.ai/myconfig.ai.microvm/guest.nix
modules/myconfig.ai/myconfig.ai.microvm/guest-home.nix
modules/myconfig.ai/myconfig.ai.microvm/launcher.nix
modules/myconfig.ai/myconfig.ai.microvm/workmux.nix
```

## Required design

Create one authoritative registry, for example:

```nix
agentSpecs = {
  claude = {
    package = ...;
    executable = "claude";
    workmuxName = "microvm-claude";
    interactiveArgs = [ ];
  };

  codex = {
    package = ...;
    executable = "codex";
    workmuxName = "microvm-codex";
    interactiveArgs = [ ];
  };

  pi = {
    package = ...;
    executable = "pi";
    workmuxName = "microvm-pi";
    interactiveArgs = [ ];
  };

  opencode = {
    package = ...;
    executable = "opencode";
    workmuxName = "microvm-opencode";
    interactiveArgs = [ ];
  };
};
```

Adapt the exact schema and placement to the repository’s conventions. The registry may live in a dedicated file if that improves module boundaries.

## Implementation steps

1. Inspect every occurrence of supported agent names under `modules/myconfig.ai/myconfig.ai.microvm`.
2. Record which data is duplicated and where.
3. Introduce one authoritative `agentSpecs` attribute set.
4. Generate the guest package list from `agentSpecs`.
5. Generate launcher validation from `builtins.attrNames agentSpecs` or an equivalent expression.
6. Generate launcher help output from the same registry.
7. Generate Workmux agent entries from the same registry.
8. Generate guest-side agent dispatch metadata from the same registry.
9. Remove all independent hard-coded supported-agent lists.
10. Add assertions for malformed entries, including missing package, executable, or Workmux name.
11. Preserve existing command names and interactive behavior.
12. Update comments and module documentation to identify the registry as authoritative.

## Constraints

Do not:

- Change the VM pool architecture.
- Add per-job Nix evaluation.
- Change workspace handling.
- Change network policy.
- Add Hermes in this ticket.
- Break existing `agent-microvm run --attach` behavior.

## Acceptance criteria

- Exactly one authoritative supported-agent registry exists.
- Claude, Codex, Pi, and OpenCode are generated from it.
- Launcher validation is generated from the registry.
- Launcher help output is generated from the registry.
- Workmux entries are generated from the registry.
- Guest packages are generated from the registry.
- Existing commands continue to behave the same.
- No independent hard-coded list of supported agents remains in this module tree.

## Validation

Run the repository’s normal formatter and checks, including relevant equivalents of:

```bash
nix flake check
nix eval
nix build
nixos-rebuild build --flake .#<host>
shellcheck <generated-or-source-launcher-scripts>
```

Confirm that help output lists exactly the currently supported agents and that an unsupported name is rejected.
