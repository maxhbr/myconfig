<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# `myconfig.ai.skills` — layout conventions

`modules/myconfig.ai/skills/` hosts the handcrafted skills, agents and prompt
templates this repo deploys to the enabled agent harnesses, next to the
`*.nix` files that register them (`skills/default.nix` is the module; each
`<name>.nix` registers one artefact in the central
`myconfig.ai.skills.handcrafted` / `handcraftedAgents` / `handcraftedPrompts`
registries and is deployed from there).

There is **no single on-disk skill format** — what a directory must contain
depends on the *kind* of artefact its `.nix` file registers:

| Directory | Registered as | Contents |
| --- | --- | --- |
| `commit/` | a skill **and** a pi prompt template | `SKILL.md` (the skill) + `prompt.md` (the pi `/commit` template) |
| `implement-and-review-and-commit/` | a skill | `SKILL.md` only |
| `implement-and-review-and-commit-until-done/` | a skill | `SKILL.md` only |
| `research/` | a pi **sub-agent** and a prompt template | `agent.md` (the sub-agent, deployed to `~/.pi/agent/agents/`) + `prompt.md` (the pi `/research` template) |
| `review/` | a pi prompt template only | `prompt.md` (spawns a code-review sub-agent via `pi --print`) |

Conventions:

- **One directory per artefact**, named exactly like the registering `.nix`
  file. The `.nix` file references the directory contents by explicit path
  (`./<name>/SKILL.md`, `./<name>/prompt.md`, `./<name>/agent.md`) — there is
  no auto-discovery.
- `SKILL.md` is the harness-agnostic skill body every enabled harness
  (opencode, claude-code, codex, pi) receives; `prompt.md` is a pi prompt
  template (`~/.pi/agent/prompts/`, reachable as `/<name>`); `agent.md` is a
  pi sub-agent definition (`~/.pi/agent/agents/`).
- A skill whose workflow is also useful as an interactive command ships both
  `SKILL.md` and `prompt.md` (`commit/`, `research/`); a pure skill ships
  `SKILL.md` only; a pure prompt ships `prompt.md` only.

Not everything under `myconfig.ai.skills` follows this layout: the option
namespace also covers skills whose *sources* come from elsewhere and only the
*nix glue* lives here —

- `grafana-core.nix`, `simple-english.nix`, `workmux.nix` — skills fetched
  from pinned external sources (nvfetcher / flake inputs) and registered
  from those trees, no local directory needed;
- `herdr` — skill generated at build time from the installed binary's
  `herdr --skill` output (a derivation, not a local directory); registered
  in `../programs.herdr.nix` and implicitly enabled by the herdr CLI, so
  there is no glue file under `skills/`;
- `playwright-cli.nix` — registers a package-provided skill;
- `default.nix` — the module itself: discovers skills from flake-pinned
  sources, syncs everything into the enabled harnesses via home-manager
  (vendored `agent-skills-nix` library).
