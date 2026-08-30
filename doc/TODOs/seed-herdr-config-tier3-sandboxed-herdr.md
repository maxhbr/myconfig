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
  that starts with its **default** configuration. (The header's stale
  "mirror" claim was corrected — comments only — by the review-fixup commit
  on branch `agent/gvisor-fun`, 2026-08; this TODO tracks the remaining
  *decision* about the missing entry itself.)

The divergence was documented (not fixed) in
`modules/myconfig.ai/sandboxed-herdr.README.md` ("Agent-configuration seeding")
by the doc-review commit `04c9e05f0a` ("docs(ai): fix doc-vs-code drift and gaps found in the myconfig.ai review", branch `agent/gvisor-fun`, 2026-08).

## What to do

Decide between:

1. **Close the gap** (preferred): add a `herdr` entry to `agentConfigPaths` in
   `modules/myconfig.ai/fns/seed-agent-config.nix`, mirroring the tier-4 rule —
   the exact file `.config/herdr/config.toml`, never the `.config/herdr`
   directory (it must pass the credential denylist and the eval-time
   `validatePaths` assertions). Then update the README paragraph and the
   "mirrors" claims in both `fns/seed-agent-config.nix` and
   `doc/sandboxed-herdr-vs-agent-microvm-herdr.md` to say the sets match again.
2. **Accept the divergence**: the comment part is already done —
   `fns/seed-agent-config.nix`'s header now states the divergence and points
   back to this TODO. What remains is to *ratify* the omission (e.g. tier-3
   `herdr` guests are disposable and a default config is acceptable),
   optionally noting that rationale in the header, and to close this TODO.

## How to verify

- Option 1: run `sandboxed-herdr` in a project directory on a KVM host and
  check inside the guest that `~/.config/herdr/config.toml` matches the
  host's rendered file; `nix build` the affected hosts must still succeed
  (`nix eval --raw .#nixosConfigurations.f13.config.system.build.toplevel.drvPath`).
- Either way: the two path lists should either be identical again or the
  comments in both files should state the difference explicitly.
