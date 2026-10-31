# Upstream the agent-skills-nix takeover + ordering patches

The vendored `vendor/agent-skills-nix/` subtree carries two local changes
introduced via bd myconfig-298 (merged as f60ba5fe6a; aafb3f5e4a added the
ordering + takeover, 8804a7d00c made the takeover check recursive). Remove
them once upstream https://github.com/Kyure-A/agent-skills-nix has equivalent
behavior:

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
   destination for the sync when everything in it is a symlink into
   `/nix/store`, or a directory tree of such symlinks (declaratively
   reproducible content, e.g. a home-manager plugin linked into the same
   skills dir — the recursive shape `programs.<harness>.skills` leaves).
   Destinations with regular files or foreign symlinks are left untouched
   so the sync still fails loudly (AGENT_SKILLS_FORCE hint). This removes
   the need for a manual one-time `AGENT_SKILLS_FORCE=1` on hosts that
   previously deployed skills via `home.file`/`programs.<harness>.skills`
   next to the framework.

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

## Triage 2026-10-31

BLOCKED — upstream still lacks both changes.

- Checked: fetched upstream master's
  `modules/home-manager/agent-skills.nix` — activation still
  `entryAfter [ "writeBoundary" ]` only, no `linkGeneration` ordering, no
  takeover entry. Whole-tree diff vs the vendored subtree shows the
  vendored copy = upstream master + exactly this patch (plus trivial
  README→docs comment drift from an upstream doc reorg). Last upstream
  commit touching the module: 19ece6ec, 2026-09-11. No open upstream
  PR/issue covers the takeover or the ordering (searched issues+PRs).
- Local relevance: still load-bearing. myconfig.ai.dev.skills routes
  handcrafted skills through the framework (aafb3f5e4a), and the recursive
  takeover (8804a7d00c) is what claims store-symlink trees left by
  `programs.<harness>.skills`; tests/agent-skills-activation.sh asserts it.
  Bead myconfig-298 is closed because the local patch fixed the symptom;
  upstreaming (this note) is the remaining follow-up.
- Commands run: curl raw upstream agent-skills.nix + GitHub commits/issues
  API (results above). `nix eval`/build verification not possible from this
  sandbox (read-only /nix, no daemon socket); nothing local changed that
  would require it — this triage only updates this note.
- Next check: re-fetch upstream master (or watch Kyure-A/agent-skills-nix
  commits on `modules/home-manager/agent-skills.nix`) and rerun the
  whole-tree diff; when upstream matches, run the drop steps above.
