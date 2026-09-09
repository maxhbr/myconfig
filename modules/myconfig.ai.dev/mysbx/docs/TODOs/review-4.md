# Task: close the four remaining `mysbx` review findings

Repository: <https://github.com/maxhbr/myconfig>

Review baseline: `master` at `415c27495b8114b737575196dcaff39816ace404`.

Before editing, update to the latest `master`, record the exact base SHA, and verify that each finding still applies. Preserve all unrelated work that landed after this review, especially the recent state-directory and OpenCode changes. Follow the repository's `AGENTS.md`, work on a feature branch/worktree, and keep the changes reviewable—preferably one focused commit per item.

All four findings below still apply at the review baseline. Items 1 and 2 are security blockers. Item 4 is now a Nix-generation coverage and override-semantics issue; runtime ripgrep activation itself is already fixed.

## 1. P1 — Protect trusted policy pathnames, not only their resolved targets

Relevant code:

- `modules/myconfig.ai/mysbx/mysbx-rs/src/lib.rs`, where `policy_paths` is assembled and each path is fully canonicalized.
- `modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs`, where writable repo, Git-directory, and explicit-mount sources are compared with `policy_paths`.
- `modules/myconfig.ai/mysbx/default.nix`, where Home Manager generates `~/.config/mysbx/config.toml` as a symlink to a Nix-store file.

### Problem

The current check protects only the final target returned by `canonicalize()`. That protects the Nix-store target of a Home Manager symlink, but not the symlink or directory entry through which mysbx will find the policy on the next run.

Concrete exploit:

1. Home Manager creates `~/.config/mysbx/config.toml -> /nix/store/...-mysbx-config.toml`.
2. The trusted config grants a relocated RW mount that contains the lexical policy path, for example:

   ```toml
   [[mounts]]
   path = "~/.config/mysbx"
   dest = "/policy"
   mode = "rw"
   ```

3. The current comparison sees the mount source as `/home/alice/.config/mysbx` and the policy only as `/nix/store/...`; it therefore allows the run.
4. The untrusted payload removes the symlink at `/policy/config.toml` and replaces it with a malicious regular file.
5. That malicious policy removes the self-exposing mount and grants a different sensitive RW source.
6. The next run accepts the attacker-controlled policy because the original temporal check no longer sees the mount that enabled the mutation.

The same class of bug applies to sidecar policy paths and to symlinks in intermediate path components.

### Required behavior

Treat every policy file that contributed to the effective configuration as trusted cross-run state. Before launching the sandbox, reject any persistent writable host authority that can mutate either:

- the resolved policy file target; or
- the pathname/directory entries mysbx will traverse to find that policy on a later run, including replaceable final or intermediate symlinks.

This must cover every effective RW source: the implicit repository bind, approved Git-directory binds, and explicit RW mounts/aliases. Keep the resolved-target check as well; replacing it with a purely lexical comparison is insufficient.

Choose a representation that makes the security property explicit. One viable approach is to retain the original policy pathname, resolve and protect the parent/directory-entry chain without following away the final entry, and separately retain the fully resolved target. Do not depend on declaration order or on the sandbox destination spelling.

### Regression tests

Add execution-level tests, not just unit tests of path comparison:

- Model a Home Manager-style final symlink for the user config, grant an RW mount over its containing directory, and assert that mysbx refuses before the payload executes and that the symlink remains unchanged.
- Repeat for a sidecar policy.
- Cover a symlink in an intermediate directory component.
- Cover all relevant RW-source classes, or combine focused unit tests with at least one end-to-end test for the actual launch path.
- Confirm that an unrelated RW mount is still accepted.
- Confirm that an RO view of a policy path remains accepted.

## 2. P1 — Reject an implicit repository root that is an ancestor of `$HOME`

Relevant code:

- `modules/myconfig.ai/mysbx/mysbx-rs/src/repo.rs`, especially `guard_repo_root()`.
- `modules/myconfig.ai/mysbx/mysbx-rs/src/bwrap.rs`, where the discovered repository is bound RW.

### Problem

After canonicalization, `guard_repo_root()` rejects `/` and `root == home`, but it does not reject `root` when it is a strict ancestor of `home`.

Reproduction shape:

```text
HOME=/srv/tree/users/alice
repository marker or existing sidecar=/srv/tree/.git or /srv/tree.mysbx
cwd=/srv/tree/users/alice/project/subdir
discovered repo root=/srv/tree
```

The equality check passes, after which the implicit RW repository bind exposes the complete subtree, including the user's home and sensitive files such as `.ssh`.

### Required behavior

After canonicalizing both paths, reject a discovered repository root when it is equal to **or an ancestor of** the canonical home directory. Use component-aware path containment (`Path::starts_with` or an equivalent), not string-prefix logic. A normal repository below `$HOME` must remain allowed.

Apply this invariant consistently regardless of whether discovery selected the root from Git metadata or from an existing sidecar.

### Regression tests

- Git-marker discovery where the repository root is a strict ancestor of `$HOME`.
- Existing-sidecar discovery with the same relation.
- A symlinked/non-canonical spelling of `$HOME` to prove the comparison uses canonical paths.
- A normal repository strictly inside `$HOME` remains allowed.
- Preserve the existing rejection of `$HOME` itself and `/`.

## 3. P2 — Make `init --approve-git-dirs` perform a valid, table-aware TOML edit

Relevant code:

- `modules/myconfig.ai/mysbx/mysbx-rs/src/lib.rs`, especially the approval rewrite, `find_git_dirs_key()`, and `find_closing_bracket()`.
- `modules/myconfig.ai/mysbx/mysbx-rs/src/toml.rs`, whose parser correctly keeps the last table active.

### Problem

When no `git-dirs` key exists, the command appends it at EOF. TOML does not return to the root table after a table header, so a file ending in `[env]` interprets the new key as `env.git-dirs`, and a file ending in `[[mounts]]` interprets it as a mount field. The command can return success after writing a config that mysbx cannot parse.

The hand-written locator has two additional correctness bugs:

- `find_git_dirs_key()` recognizes only a raw line beginning with `git-dirs`, so it misses the equivalent quoted top-level key `"git-dirs"` and can confuse a same-named key in another table.
- `find_closing_bracket()` does not track TOML string/comment state, so `]` or `#` inside a quoted path can terminate or corrupt the edit.

### Required behavior

Use a table-aware and string-aware edit. A preservation-oriented TOML editor is acceptable; a custom span editor is also acceptable if it correctly implements the needed TOML lexical rules.

The command must:

- update only the top-level `git-dirs` key;
- recognize equivalent quoted-key syntax;
- insert a missing top-level key before the first active table header, or use another method that is unambiguously top-level;
- handle comments, escapes, quoted strings, and paths containing `]` or `#`;
- avoid duplicating approvals and be idempotent;
- preserve unrelated comments and formatting as far as reasonably possible;
- parse and validate the complete rewritten document with the real config parser before replacing the original; and
- leave the original untouched and return an error if generation or validation fails.

Use an atomic temp-file-plus-rename replacement where compatible with the existing policy-file rules, so interruption cannot leave a truncated policy.

### Regression tests

Cover at least:

- a valid config ending in `[env]`;
- a valid config ending in `[[mounts]]`;
- an existing quoted top-level `"git-dirs"` key;
- a non-top-level key with the same spelling;
- approved paths containing `]`, `#`, quotes, and backslashes as supported by the platform;
- preservation of unrelated comments/sections;
- running the approval twice produces no duplicate and no second semantic change;
- the rewritten file parses through `Config::parse`; and
- a subsequent `mysbx --dry-run` succeeds with the rewritten config.

## 4. P3 — Test the actual Nix-generated ripgrep configuration and make overrides real

Relevant code:

- `modules/myconfig.ai/mysbx/default.nix`, especially `baselineEnv`, `userConfigToml`, and the Home Manager `xdg.configFile."mysbx/config.toml"` source.
- `modules/myconfig.ai/mysbx/mysbx-rs/tests/argv.rs` and `tests/cli.rs`.

### Current status

The runtime behavior is fixed: the generated baseline mounts the ripgrep config below `/mysbx-home` and sets `RIPGREP_CONFIG_PATH`. Do not reopen that implementation unless a new test exposes a real failure.

However, the current Rust tests manually construct the mount and environment entry or manually write equivalent TOML. They do not evaluate `default.nix`, so they cannot detect a regression in the generator.

There is also a mismatch between the module comment and Nix semantics. The comment says a later same-key environment definition replaces the baseline, but `myconfig.ai.mysbx.config.env = baselineEnv` has normal priority. Two unequal same-priority definitions of `RIPGREP_CONFIG_PATH` generally conflict rather than override.

### Required behavior

- Add a Nix module-evaluation test that inspects the actual generated Home Manager `mysbx/config.toml`.
- When Home Manager ripgrep is enabled with non-empty arguments, assert that the generated file contains:
  - the RO mount from `~/.config/ripgrep` to `/mysbx-home/.config/ripgrep`; and
  - `RIPGREP_CONFIG_PATH = "/mysbx-home/.config/ripgrep/ripgreprc"` in `[env]`.
- When the ripgrep gate is false (disabled or no generated arguments), assert that the environment key is absent. The unconditional baseline mount may remain; this assertion is specifically about the environment entry.
- Give the baseline environment value default priority, or otherwise implement documented override semantics, so a host/per-agent explicit value for the same key wins without an evaluation conflict.
- Test both same-key override and merging of an unrelated environment key.
- Keep the existing execution-level Rust test as downstream coverage.
- Update the nearby comment so it precisely describes the implemented Nix priority/merge behavior.

Integrate the Nix test into the repository's normal check structure rather than leaving it as an ad-hoc command in documentation.

## Validation

Run the narrow tests during development and report every command and result. At minimum:

```sh
cd modules/myconfig.ai/mysbx/mysbx-rs
cargo test
```

From the repository root:

```sh
./nixfmtall.sh --check
git diff --check
```

Run the new Nix module test directly and ensure it is included in the applicable flake/check output. Because this changes a shared module, evaluate at least one host that enables mysbx (for example `f13`) as directed by `AGENTS.md`:

```sh
nix eval --raw .#nixosConfigurations.f13.config.system.build.toplevel.drvPath
```

Run `nix flake check` if practical for this cross-cutting shared-module change; if it is too expensive, say so explicitly and list the narrower checks that were run instead.

## Completion report

Return:

1. the exact base and final commit SHAs;
2. a concise summary of the fix for each numbered item;
3. the regression tests added for each item;
4. all validation commands with pass/fail/skip status;
5. any remaining security assumptions or known limitations; and
6. a short note if any item was already fixed differently on newer `master`, with evidence rather than duplicating or reverting that work.
