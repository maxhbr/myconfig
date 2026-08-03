# `myconfig.ai.microvm` improvement tickets — progress

Tracking file for the ticket series in [`README.md`](./README.md). One commit
per ticket (or per reviewable sub-step); this file is updated in the same
commit as the work it describes.

| # | Ticket | Status | Commit |
|---|--------|--------|--------|
| 1 | `01-agent-registry-refactor.md` | DONE | see `git log --oneline -- modules/myconfig.ai/myconfig.ai.microvm/agents.nix` |
| 2 | `02-add-hermes-support.md` | TODO | — |
| 3 | `03-network-and-control-channel-hardening.md` | TODO | — |
| 4 | `04-batch-execution-and-lifecycle.md` | TODO | — |
| 5 | `05-resource-classes-and-state-management.md` | TODO | — |
| 6 | `06-runtime-validation-and-documentation.md` | TODO | — |

Status values: `TODO`, `IN PROGRESS`, `DONE`, `BLOCKED`.

## Notes

### Ticket 1 — agent registry refactor

- New `modules/myconfig.ai/myconfig.ai.microvm/agents.nix` is the authoritative
  registry (`package` / `executable` / `workmuxType` / `interactiveArgs`, with
  `workmuxName = "microvm-<name>"` derived).
- Consumers generated from it: guest packages + `agent-run` dispatch
  (`guest.nix`), `--agent` validation + help (`launcher.nix`), workmux agents
  (`workmux.nix`), well-formedness assertions (`default.nix`), and the
  shellcheck-gate list in `tests/microvm.nix`.
- New check `microvm-agent-registry` proves the registry really is the single
  source of truth (eval: workmux keys / guest closure / workmux types; build:
  greps the built launcher + `agent-run` for every registry agent).
- Verified behaviour-preserving: guest `systemPackages` names/paths and the
  workmux agent `{type, command}` attrs are byte-identical to before; only the
  `agent-run` / `agent-microvm` script texts changed (generated dispatch,
  generated validation set + help listing).
