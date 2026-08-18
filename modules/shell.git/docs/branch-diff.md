# "What does this branch add?" — evaluating the built-ins first

## The question

Show what the *current* branch adds on top of its base: the commits and the
diff relative to the merge-base (latest common commit) with the base branch.

## Built-ins already cover this

Git has everything needed as long as you know (or type) the base branch name:

```sh
git log <base>..HEAD                     # commits on this branch, not on <base>
git diff <base>...HEAD                   # file-level diff since the merge-base (triple-dot)
git diff --stat <base>...HEAD            # same, as a diffstat
git merge-base <base> HEAD               # the merge-base commit itself
git range-diff <base>...HEAD             # commit-by-commit comparison, e.g. after a rebase
git log --left-right --cherry <base>...HEAD   # what's on each side
```

`modules/shell.git/default.nix` already has thin wrappers around the
merge-base idiom (`dAncestor`, `dsAncestor`, `rAncestor`), so the pattern of
"diff/rebase against the merge-base" is well established here and the
built-ins are not the gap.

## The actual gap: base-branch auto-detection

Every existing merge-base alias hardcodes `master` as the base
(`dAncestor`, `dsAncestor`; `rAncestor` at least accepts an override
argument). That's a real ergonomic gap for the *current* task: figuring out
whether the base is `main`, `master`, or something else, and typing it every
time. A one-liner doesn't fix that — you still have to know/type the base
name.

## Decision

Add `git branch-summary` (short alias `git bs`): a thin wrapper that

1. auto-detects the base branch — tries origin's default branch
   (`git symbolic-ref refs/remotes/origin/HEAD`), then `main`, then
   `master`, resolving against `origin/<name>` first and falling back to
   the local branch of that name; an explicit `<base>` argument overrides
   detection entirely;
2. by default prints the commits (`log <base>..HEAD`) plus a combined
   diffstat (`diff --stat <base>...HEAD`) in one view;
3. supports `--log`, `--stat`, `--diff`/`--patch`, `--name-only`, and
   `--range` (which shells out to `git range-diff <base>...HEAD`) for the
   narrower built-in views when that's all you want.

Implementation: `pkgs.writeShellApplication` in
`modules/shell.git/default.nix` (installed via `home.packages`, so it's on
`PATH` as `git-branch-summary`, which `git branch-summary` finds
automatically), plus a short `bs = "!git-branch-summary"` alias for
discoverability alongside the other one-letter git aliases in this module.

## Usage

```sh
git branch-summary                # auto-detected base: commits + diffstat
git bs                            # same, short form
git bs --diff                     # full diff instead of the stat
git bs --log                      # commits only
git bs --name-only                # changed file names only
git bs --range                    # git range-diff <base>...HEAD
git bs origin/release-1.2         # explicit base override
```
