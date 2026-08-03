# `myconfig.ai.microvm` Improvement Tickets

This package contains one implementation ticket per phase for improving the existing NixOS MicroVM-based coding-agent sandbox under:

```text
modules/myconfig.ai/myconfig.ai.microvm
```

## Ticket order

1. `01-agent-registry-refactor.md`
2. `02-add-hermes-support.md`
3. `03-network-and-control-channel-hardening.md`
4. `04-batch-execution-and-lifecycle.md`
5. `05-resource-classes-and-state-management.md`
6. `06-runtime-validation-and-documentation.md`

The tickets are intended to be implemented in order. Each ticket is independently reviewable and includes scope, constraints, implementation steps, acceptance criteria, and suggested validation commands.

## Architectural constraints

Preserve the current design:

- Fixed pool of prebuilt MicroVM slots.
- Cloud Hypervisor guests unless a ticket explicitly requires otherwise.
- Standalone Git clones as workspaces.
- Virtiofs workspace attachment.
- Self-contained guest Nix stores.
- Host-side LiteLLM proxying.
- No upstream API credentials inside guests.
- Systemd-managed VM lifecycle.
- Atomic allocation through `flock`.

Do not replace the system with dynamically generated per-job NixOS configurations or OCI containers.
