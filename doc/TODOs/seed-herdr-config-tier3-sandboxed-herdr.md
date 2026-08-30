# Seed the `herdr` config in the tier-3 `sandboxed-herdr` guest

## Context

Tier 4 seeds `herdr`'s rendered keybinding config, tier 3 does not — although
tier 3 is the tier whose *entry point* is `herdr`:

- **Tier 4** (`modules/myconfig.ai/myconfig.ai.microvm/agents.nix`, the
  `herdr` registry entry, `configPaths`) stages exactly
  `.config/herdr/config.toml` into its guests, so a tier-4 `herdr` guest gets
  the host's `ctrl+b` prefix / pane-focus bindings.
- **Tier 3** (`modules/myconfig.ai/fns/seed-agent-config.nix`,
  `agentConfigPaths`) has **no `herdr` entry**, so `sandboxed-herdr`
  (`modules/myconfig.ai/programs.herdr.nix`) drops the user into a `herdr`
  that starts with its **default** configuration. `fns/seed-agent-config.nix`'s
  header still claims the per-agent `configPaths` *mirror*
  `../myconfig.ai.microvm/agents.nix` — they no longer do.

The divergence was documented (not fixed) in
`modules/myconfig.ai/sandboxed-herdr.README.md` ("Agent-configuration seeding")
by the doc-review commit `6729a352d0` ("docs(ai): fix doc-vs-code drift and gaps found in the myconfig.ai review", branch `agent/gvisor-fun`, 2026-08).

## What to do

Decide between:

1. **Close the gap** (preferred): add a `herdr` entry to `agentConfigPaths` in
   `modules/myconfig.ai/fns/seed-agent-config.nix`, mirroring the tier-4 rule —
   the exact file `.config/herdr/config.toml`, never the `.config/herdr`
   directory (it must pass the credential denylist and the eval-time
   `validatePaths` assertions). Then update the README paragraph and the
   "mirrors" claims in both `fns/seed-agent-config.nix` and
   `doc/sandboxed-herdr-vs-agent-microvm-herdr.md` to say the sets match again.
2. **Accept the divergence**: make `fns/seed-agent-config.nix`'s header stop
   claiming an exact mirror and note the deliberate omission (e.g. tier-3
   `herdr` guests are disposable and a default config is acceptable).

## How to verify

- Option 1: run `sandboxed-herdr` in a project directory on a KVM host and
  check inside the guest that `~/.config/herdr/config.toml` matches the
  host's rendered file; `nix build` the affected hosts must still succeed
  (`nix eval --raw .#nixosConfigurations.f13.config.system.build.toplevel.drvPath`).
- Either way: the two path lists should either be identical again or the
  comments in both files should state the difference explicitly.
