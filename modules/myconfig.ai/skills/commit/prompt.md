---
description: Commit current working-tree changes with a well-formed message derived from the diff
---
Commit the repository's current uncommitted changes with a clear, conventional
commit message derived from the diff and the repository's existing history.

This is the prompt-template form of the `commit` skill, exposed as `/commit`
so it can be invoked explicitly in the pi editor. It mirrors the skill's
workflow; the "do not commit unless asked" guard is dropped because typing
`/commit` is itself the explicit request.

## Workflow

1. **Survey the changes.** Run `git status` and `git diff` (and `git diff
   --staged` if some changes are already staged). Understand both *what*
   changed and *why*.

2. **Match the repository's conventions.** Run `git log --oneline -15` to learn
   the commit message style this repo already uses — scope prefixes, tense,
   length, whether bodies are used, whether issues are referenced. Match it; do
   not impose a foreign style.

3. **Run repo-specific pre-commit checks.** If this is the NixOS flake repo (a
   `flake.nix` and an `AGENTS.md` are present), follow its documented
   pre-commit workflow exactly:
   - Format Nix: `./nixfmtall.sh`
   - Validate: `nix flake check` (or `./nixfmtall.sh --check`)
   - `git add` every new file — Nix evaluates from the git tree, so untracked
     files are invisible to `nix` and produce misleading errors.
   - Review staged changes with `git diff --staged` before committing.

   For other repos, run whatever linters/formatters/tests the project documents
   (check for a `CONTRIBUTING.md`, `Makefile`, `package.json` scripts, or
   pre-commit hooks). If unsure which checks apply, ask.

4. **Stage intentionally.** `git add` only what belongs in this commit. Split
   into multiple commits if the changes cover unrelated concerns. Respect
   `.gitignore`; never `git add -A` blindly if it would sweep in unrelated
   build artifacts, editor backups, or `result*` symlinks. Never commit secrets
   — in the NixOS repo, secrets live in the separate `../priv/` repo.

5. **Write the message** in the repo's style. Default to conventional commits:

   ```
   <type>(<scope>): <imperative summary under ~72 chars>

   <body explaining *why*, not just what; wrap at ~72 chars>
   ```

   - The summary is imperative mood ("add …", "fix …"), not descriptive
     ("added").
   - The body explains the motivation and anything non-obvious in the diff.
   - Reference an issue/PR only if the change is genuinely related to one.

6. **Commit.** `git commit -m "<summary>" -m "<body>"` (or a heredoc for
   multi-paragraph bodies). If a pre-commit hook fails, read the failure, fix
   the root cause, and re-stage — do not bypass hooks with `--no-verify`. Then
   show the result with `git show --stat HEAD`.

## Pitfalls

- **`git add -A` with a dirty tree.** Stray files sneak in. Stage named paths
  or review `git status` first.
- **Secrets.** Keys, tokens, and `../priv/` content must never be committed
  here. If a staged file looks like a secret, stop and ask.
- **Bypassing hooks.** `--no-verify` hides problems. Fix the hook failure
  instead.
- **Vague messages.** "update", "fix", "wip", "changes" tell the reader
  nothing. The summary must convey what the commit does on its own.
