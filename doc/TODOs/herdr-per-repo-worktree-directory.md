# Drop `herdr-worktree-sibling` once herdr supports a per-repo worktree directory

## What is missing upstream

herdr (checked against `herdr` 0.8.2) exposes exactly one worktree option:

```toml
[worktrees]
directory = "~/.herdr/worktrees"   # <root>/<repo-name>/<branch-slug>
```

It is a single global root, expanded once at server start
(`src/app/mod.rs` -> `worktree::expand_tilde_absolute_path`), and the checkout
path is `root.join(repo_name).join(branch_to_path_slug(branch))`
(`src/worktree.rs`, `default_checkout_path`). There are no placeholders
(`{repo_root}`, `{repo_parent}`, ...) and no per-repository override, so the
workmux layout `<parent-of-repo>/<repo-name>__worktrees/<handle>` — used by
`workmux` and by `git branch-to-worktree` in this repo — cannot be expressed
as a value of that option.

## Workaround in this repo

Introduced by commit `5724757a21` on branch `herdr-worktree-location` (see
`git log --follow modules/myconfig.ai.dev/programs/programs.herdr.nix`).

- `modules/myconfig.ai.dev/programs/programs.herdr.nix`
  - `herdr-worktree-sibling` (a `writeShellApplication`): resolves the main
    working tree of the focused pane's repo, slugifies the branch like
    workmux, and calls
    `herdr worktree create --cwd <repo> --branch <b> --path <sibling> --focus`.
  - generated `~/.config/herdr/config.toml`: `keys.new_worktree = ""` unbinds
    the built-in action, and a `[[keys.command]]` entry binds
    `prefix+shift+g` to the script as a popup.
  - `[worktrees] directory` is pinned to herdr's default and only acts as a
    fallback for flows that pass no explicit `--path`.
- `modules/myconfig.ai.dev/programs/programs.herdr.README.md` documents the resulting
  behaviour.

Sandboxed sessions do not need the script, because a jail has exactly one
repository and can therefore make the global option repository-local:

- `modules/myconfig.ai.dev/programs/programs.herdr.nix` — `agent-bubblewrap-herdr`:
  `worktreesSiblingPerm` creates `<parent-of-repo>/<repo>__worktrees` and binds
  it read-write at the identical path, and `herdr-jail-entry` writes a session
  `~/.config/herdr/config.toml` (from `herdrJailConfigCommonFile`) whose
  `[worktrees] directory` points at it, restores the built-in
  `keys.new_worktree`, and execs `herdr`. Checkouts land in
  `<parent-of-repo>/<repo>__worktrees/<repo>/<branch-slug>` — the extra
  `<repo>` level is herdr's fixed `<repo-name>/<branch-slug>` suffix.
- `modules/myconfig.ai.dev/programs/programs.pi-coding-agent/default.nix` —
  `worktreesSiblingPerm` `mkdir -p`s the sibling directory instead of skipping
  the bind when it does not exist yet.

## What to do once upstream supports it

Condition: a herdr release lets the worktree checkout path be derived from the
repository (e.g. placeholders in `worktrees.directory` such as
`{repo_parent}/{repo_name}__worktrees/{branch_slug}`, or a per-repo/relative
directory mode). Check with:

```sh
curl -fsSL 'https://raw.githubusercontent.com/herdrdev/herdr/v<VERSION>/docs/next/website/src/data/config-reference.json' \
  | jq '.sections[].keys[] | select(.key | startswith("worktrees"))'
```

Then:

1. Set `[worktrees] directory` (or the new option) to the workmux layout in
   `modules/myconfig.ai.dev/programs/programs.herdr.nix`.
2. Delete the `herdr-worktree-sibling` derivation, its entry in
   `home.packages`, and the `[[keys.command]]` block; restore
   `keys.new_worktree = "prefix+shift+g"` (or drop the key to take the
   default).
3. In `agent-bubblewrap-herdr`, drop the runtime-assembled session config
   (`herdr-jail-entry` / `herdrJailConfigCommonFile`) in favour of binding the
   ordinary host config, and fold `herdrConfigCommon` back into `herdrConfig`.
   Keep `worktreesSiblingPerm` — the directory still has to exist and be
   bound before bubblewrap starts.
4. Update `modules/myconfig.ai.dev/programs/programs.herdr.README.md` and delete this note.
5. Verify: `nix build .#nixosConfigurations.f13.config.system.build.toplevel`,
   then in a running herdr press `prefix+shift+g` inside a repository and
   check that the checkout appears at
   `<parent-of-repo>/<repo-name>__worktrees/<handle>`.

## Triage 2026-10-31

Condition still not met; blocked upstream.

- Latest herdr release: **v0.9.5** (2026-10-14), up from 0.8.2 when this
  note was written. Checked
  `https://raw.githubusercontent.com/herdrdev/herdr/v0.9.5/docs/next/website/src/data/config-reference.json`:
  `worktrees.directory` is still the *only* `worktrees.*` key ("Root
  directory under which Herdr creates <repo>/<branch-slug> checkouts.").
  No placeholders, no per-repo override. `src/worktree.rs` at v0.9.5 still
  has `default_checkout_path = root.join(repo_name).join(branch_to_path_slug(branch))`.
  The herdr `master` branch config-reference.json is identical.
- Release notes 0.9.0–0.9.5 mention many worktree fixes (removal,
  trust, remote, focus) but nothing about a configurable/templated worktree
  directory.
- Upstream tracking: issue herdrdev/herdr#261 ("Support placeholders in
  [worktrees].directory for sibling-folder layouts") was **closed as
  `not planned` on 2026-08-02**; the implementation PR #262 was closed
  unmerged on 2026-05-23. Users keep +1-ing in the thread and point to
  third-party workarounds (herdr-worktrunk, herdr-repo-worktrees plugins).
  Reopen unlikely; revisit only if upstream changes stance.
- Pinned nixpkgs `master` (flake.lock rev `3734c368e63f...`) also packages
  herdr **0.9.5**, so the `herdr = self.master.herdr` override in
  `flake.pkgs_overrides.nix` gains nothing here.
  (Verified via GitHub raw at that rev; `nix eval` is not runnable from the
  triaging sandbox — no nix daemon socket.)
- Repo state re-verified: `herdr-worktree-sibling`, the jail session-config
  assembly (`herdr-jail-entry` / `herdrJailConfigCommonFile`) and both
  `worktreesSiblingPerm` call sites are all still present and match this
  note's description.

Next check: after each new herdr release (>= 0.9.6), re-run the
config-reference.json query above; also glance at the #261 thread for any
upstream change of heart.
