# Upstream the agent-skills-nix takeover + ordering patches

The vendored `vendor/agent-skills-nix/` subtree carries two local changes
introduced in commit 5a6d7ca3 (bd myconfig-298). Remove them once upstream
https://github.com/Kyure-A/agent-skills-nix has equivalent behavior:

## Changes

1. `modules/home-manager/agent-skills.nix` — `home.activation.agent-skills`
   is ordered `entryAfter [ "writeBoundary" "linkGeneration" ]` (upstream:
   only `writeBoundary`). Without `linkGeneration`, the DAG order vs. HM's
   own file linking is underdetermined; if the sync runs first, it claims a
   destination that `cleanOldGen`/`linkNewGen` then populate with
   home-manager symlinks — the next sync's `rsync --delete` deletes them,
   and conversely a takeover of a home.file-populated dir is refused.

2. Same file — new `agent-skills-takeover` activation entry
   (`entryBetween [ "agent-skills" ] [ "linkGeneration" ]`): pre-marks a
   destination for the sync when every entry in it is a symlink into
   `/nix/store` (declaratively reproducible content, e.g. a home-manager
   plugin linked into the same skills dir). Destinations with regular files
   or foreign symlinks are left untouched so the sync still fails loudly
   (AGENT_SKILLS_FORCE hint). This removes the need for a manual one-time
   `AGENT_SKILLS_FORCE=1` on hosts that previously deployed skills via
   `home.file`/`programs.<harness>.skills` next to the framework.

## How to drop the patch

- `git subtree pull --prefix=vendor/agent-skills-nix
  https://github.com/Kyure-A/agent-skills-nix master` (or the
  `vendor/agent-skills-nix.upgrade.sh` helper).
- If conflicts appear in `modules/home-manager/agent-skills.nix`, prefer the
  upstream version provided it (a) keeps the activation after
  `linkGeneration` and (b) has its own takeover/marker bootstrap for
  store-symlink-populated destinations.
- Verify with a full f13 build +
  `nix build .#nixosConfigurations.test-f13.config.system.build.toplevel`
  and inspecting the generated `activate` script: `linkGeneration` must
  come before `agent-skills-takeover`/`agent-skills`.
