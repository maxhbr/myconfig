// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx-once` — the one-shot form (bd myconfig-7zwt, cli.md D22):
//! a sandbox run without any persistent per-repo setup.
//!
//! `mysbx-once` is the SAME binary as `mysbx`, dispatched on argv[0]
//! or on the `MYSBX_ONCE=1` marker the Nix package's second wrapped
//! program sets (argv[0] cannot carry the dispatch through a
//! makeBinaryWrapper chain — each `exec` resets it to the literal
//! program path). It starts `mysbx $ARGS` for the repo the cwd
//! resolves to (cli.md D1 — resolution is unchanged), but neither
//! reads nor writes the host's `<repo>.mysbx/` and
//! `<repo>__worktrees`: both are created in a private temporary
//! directory for one run and removed when it ends. An existing
//! host sidecar config is ignored, not loaded — the sidecar LAYER
//! is empty, so the effective configuration is built-in defaults
//! + user config + flags, exactly like a repo whose sidecar is not
//! inited.
//!
//! That is an explicit exception to cli.md D13 (a run never creates
//! the sidecar) which keeps D13's intent: nothing persistent is
//! created on the host. The one thing D13 lets an interactive run
//! create — the `<repo>__worktrees` sibling — is redirected the same
//! way: the run binds its TEMPORARY worktrees directory at the
//! sibling's path inside the sandbox (the clone-run precedent of
//! workspace.md D3a: bind a non-sibling source at the path the
//! payload computes), so `workmux add` / `git worktree add` work,
//! and the host path itself is never touched.
//!
//! Lifecycle (cli.md D22): mysbx-once stays the PARENT — no `exec`
//! — so it can tear the temp root down after the payload. It
//! forwards SIGINT/SIGTERM/SIGHUP to the child, waits, runs the
//! data-loss guard (a linked worktree under the temp root with
//! uncommitted changes keeps the root and names it), prunes the
//! host repo's stale worktree entries, removes the temp root, and
//! exits with the payload's own exit code (D8 unchanged; `70` when
//! the setup itself fails).

use crate::repo::Repo;
use std::path::{Path, PathBuf};

/// The marker the wrapper (or a test) can set instead of the
/// argv[0] spelling: `MYSBX_ONCE=1` selects once-mode for an
/// invocation of the plain binary. An empty value counts as unset,
/// the same treatment every `MYSBX_*` pin gets.
const ONCE_ENV: &str = "MYSBX_ONCE";

/// The basename of the symlink the Nix package installs beside
/// `mysbx` — the argv[0] spelling of once-mode.
const ONCE_ARGV0: &str = "mysbx-once";

/// Whether THIS invocation is a once-mode one: argv[0] is the
/// `mysbx-once` symlink, or `MYSBX_ONCE=1` is set. Called once at
/// the top of `run` (lib.rs), so every later decision — the verb
/// gate, the sidecar override, the wait — branches on one fact.
pub fn active() -> bool {
    if let Some(raw) = std::env::var_os(ONCE_ENV) {
        if !raw.is_empty() {
            return true;
        }
    }
    std::env::args_os()
        .next()
        .and_then(|a| Path::new(&a).file_name().map(|n| n == ONCE_ARGV0))
        .unwrap_or(false)
}

/// The verbs and flags once-mode refuses (cli.md D22): everything
/// that would need state the temp root does not carry, or that
/// would outlive it. `init`/`edit`/`status` write or report the
/// HOST sidecar; `run --result` writes result.json into the temp
/// sidecar (deleted with it — no consumable outcome); `--session`
/// creates the clone in the temp sidecar (the session registry is
/// the host's, the clone would vanish); the session/worktree noun
/// groups and the fetch/merge/push/diff handoff verbs are host-side
/// state machines over the sidecar; `attach` and `--detach`/
/// `--detachable` detach the run from the parent that owns the
/// teardown.
pub const REFUSED: &[&str] = &[
    "init",
    "edit",
    "status",
    "session",
    "worktree",
    "fetch",
    "merge",
    "push",
    "diff",
    "attach",
    "ssh-pubkey",
];

/// The refusal message of once-mode: `mysbx-once: <what>` on stderr,
/// exit `70` (cli.md D8 — the command line was fine, the form it
/// names does not exist here).
pub fn refuse(what: &str) -> i32 {
    eprintln!("mysbx-once: {what}");
    eprintln!(
        "  once-mode runs a sandbox and tears it down — use plain `mysbx` for the persistent forms"
    );
    crate::EXIT_INFRASTRUCTURE
}

/// The temporary root of one once-run: `<root>/sidecar/` (an EMPTY
/// `config.toml` — an empty layer, like a not-inited repo's would
/// be) and `<root>/worktrees/` (the directory bound at the host's
/// `<repo>__worktrees` path).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TempRoot {
    /// The private root, mode 0700, under
    /// `$XDG_RUNTIME_DIR/mysbx-once/` (fallback `${TMPDIR:-/tmp}`).
    pub root: PathBuf,
    /// `<root>/sidecar` — the ephemeral sidecar, `config.toml` an
    /// EMPTY file (a real, empty policy layer, not a missing one:
    /// the run report must show the sidecar config as existing and
    /// empty, the exact state `mysbx init` leaves in a repo without
    /// policy).
    pub sidecar: PathBuf,
    /// `<root>/worktrees` — the ephemeral worktrees sibling,
    /// bound rw at the host's `<repo>__worktrees` path.
    pub worktrees: PathBuf,
}

impl TempRoot {
    /// Create the temp root of one once-run (cli.md D22): under
    /// `$XDG_RUNTIME_DIR/mysbx-once/` (fallback `${TMPDIR:-/tmp}`),
    /// named by the pid — one root per process, never reused, and
    /// a crashed run leaves debris whose owner is identifiable
    /// (the same lifecycle the krun per-run dirs follow under the
    /// sidecar). Private, mode 0700, carrying `sidecar/config.toml`
    /// (empty) and `worktrees/`.
    pub fn create() -> Result<TempRoot, String> {
        let base = match std::env::var_os("XDG_RUNTIME_DIR") {
            Some(dir) if Path::new(&dir).is_absolute() => Path::new(&dir).join("mysbx-once"),
            _ => std::env::var_os("TMPDIR")
                .filter(|t| Path::new(t).is_absolute())
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from("/tmp"))
                .join("mysbx-once"),
        };
        Self::create_under(&base)
    }

    /// [`create`] at a caller-chosen base: the shape-building half,
    /// separated so the unit tests can give each invocation its own
    /// directory (the pid-named root is per PROCESS, and the tests
    /// share one — the same reason `repo::tests` hand-rolls its
    /// tmpdirs).
    pub fn create_under(base: &Path) -> Result<TempRoot, String> {
        use std::os::unix::fs::PermissionsExt;
        std::fs::create_dir_all(base).map_err(|e| {
            format!(
                "mysbx-once: cannot create the temp-root base {}: {e}",
                base.display()
            )
        })?;
        let root = base.join(std::process::id().to_string());
        std::fs::create_dir(&root).map_err(|e| {
            format!(
                "mysbx-once: cannot create the temp root {}: {e}",
                root.display()
            )
        })?;
        std::fs::set_permissions(&root, std::fs::Permissions::from_mode(0o700))
            .map_err(|e| format!("mysbx-once: cannot make {} private: {e}", root.display()))?;
        let sidecar = root.join("sidecar");
        std::fs::create_dir(&sidecar).map_err(|e| {
            format!(
                "mysbx-once: cannot create the ephemeral sidecar {}: {e}",
                sidecar.display()
            )
        })?;
        let worktrees = root.join("worktrees");
        std::fs::create_dir(&worktrees).map_err(|e| {
            format!(
                "mysbx-once: cannot create the ephemeral worktrees dir {}: {e}",
                worktrees.display()
            )
        })?;
        let config = sidecar.join("config.toml");
        std::fs::write(&config, "").map_err(|e| {
            format!(
                "mysbx-once: cannot create the ephemeral config {}: {e}",
                config.display()
            )
        })?;
        Ok(TempRoot {
            root,
            sidecar,
            worktrees,
        })
    }

    /// The placeholder of a once `--dry-run` (cli.md D22): the paths
    /// a real run's [`create`] would make, WITHOUT making them — a
    /// dry run creates nothing (D9) and prints the argv of the run
    /// that would happen, temp-root binds included. The per-run name
    /// is the literal `dry-run`, so two dry runs print the same argv
    /// (a pid would make them differ for no reason).
    pub fn placeholder() -> TempRoot {
        let base = match std::env::var_os("XDG_RUNTIME_DIR") {
            Some(dir) if Path::new(&dir).is_absolute() => Path::new(&dir).join("mysbx-once"),
            _ => std::env::var_os("TMPDIR")
                .filter(|t| Path::new(t).is_absolute())
                .map(PathBuf::from)
                .unwrap_or_else(|| PathBuf::from("/tmp"))
                .join("mysbx-once"),
        };
        let root = base.join("dry-run");
        TempRoot {
            sidecar: root.join("sidecar"),
            worktrees: root.join("worktrees"),
            root,
        }
    }

    /// Redirect the resolved repo's sidecar and worktrees at the
    /// temp root (the seam of bd myconfig-7zwt): the sidecar layer
    /// the run loads is the EMPTY one just created — an existing
    /// host `<repo>.mysbx/config.toml` is never read — and the
    /// worktrees sibling the run binds rw is the temp directory,
    /// which the argv binds at the host sibling's PATH inside the
    /// sandbox (workspace.md D3a precedent), so the host path
    /// itself is neither created nor touched.
    pub fn redirect(&self, repo: &mut Repo) {
        repo.sidecar = self.sidecar.clone();
        repo.worktrees = Some(self.worktrees.clone());
    }
}

/// The teardown of one once-run (cli.md D22): the data-loss guard
/// first — every linked worktree under `<root>/worktrees` is probed
/// with `git status --porcelain`; one with uncommitted changes keeps
/// the whole temp root, named on stdout so the operator knows where
/// the work still lives. Then the host repo's stale worktree
/// entries are pruned (`git worktree prune` removes exactly the
/// entries whose worktree directory is gone — the entries that
/// pointed into the removed temp root), and the temp root is
/// removed. Best-effort by stage: a prune that fails does not
/// block the removal, a removal that fails is reported.
///
/// Returns whether the temp root was KEPT (a dirty worktree), so
/// the caller can say so with the exit code unchanged — the run's
/// outcome is the payload's, not the teardown's.
pub fn teardown(root: &TempRoot, repo: &Repo) -> bool {
    // The guard: uncommitted changes in a linked worktree would be
    // DELETED with the root. Commits are safe — a worktree's branch
    // lives in the host repo's git dir — but the working tree is
    // only here. Dirty ⇒ keep the root, name it, skip removal.
    let mut keep = false;
    if let Some(entries) = read_worktree_dirs(&root.worktrees) {
        for wt in entries {
            if worktree_dirty(&wt) {
                println!(
                    "## mysbx-once: kept {}: uncommitted changes in {}",
                    root.root.display(),
                    wt.display()
                );
                keep = true;
            }
        }
    }
    // The host repo's worktree bookkeeping: `git worktree prune`
    // drops the entries whose worktree directories no longer exist
    // — so it must run AFTER the removal below to see them gone
    // (pruning before would be a no-op: the directories still
    // exist). Best-effort: a git that cannot run is debris of the
    // same class a crashed run leaves, reported but not fatal to
    // the teardown.
    if !keep {
        if let Err(e) = std::fs::remove_dir_all(&root.root) {
            eprintln!(
                "mysbx-once: cannot remove the temp root {}: {e}",
                root.root.display()
            );
        }
    }
    // Prune is best-effort and quiet: it talks to the HOST repo's
    // git, stderr included — a repo whose git cannot run (or is not
    // even a git checkout) is debris of the class a crashed run
    // leaves, never the run's own output.
    let _ = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo.root)
        .arg("worktree")
        .arg("prune")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status();
    keep
}

/// The linked worktrees under `<dir>`: every entry with a `.git`
/// FILE (a linked worktree's pointer; a directory of that name is a
/// plain repository someone put there, a missing one is debris —
/// both are removed with the root without a guard: neither carries
/// uncommitted worktree state git could still commit).
fn read_worktree_dirs(dir: &Path) -> Option<Vec<PathBuf>> {
    let mut out = Vec::new();
    for entry in std::fs::read_dir(dir).ok()? {
        let Ok(entry) = entry else { continue };
        let path = entry.path();
        if path.join(".git").is_file() {
            out.push(path);
        }
    }
    out.sort();
    Some(out)
}

/// Whether a worktree has uncommitted changes — `git status
/// --porcelain` with any output counts (the handoff verbs' own
/// probe, `handoff::host_tree_dirty`). Fails CLOSED: a `git` that
/// cannot run counts as dirty — the root must not be removed over
/// work whose state cannot be inspected.
fn worktree_dirty(worktree: &Path) -> bool {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(worktree)
        .args(["status", "--porcelain"])
        .stderr(std::process::Stdio::null())
        .output();
    match out {
        Ok(out) if out.status.success() => !out.stdout.is_empty(),
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A fresh temporary directory per test; hand-rolled, the crate
    /// has no dependencies.
    fn tmpdir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-once-test-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn create_makes_a_private_root_with_an_empty_sidecar_layer() {
        // The shape D22 requires: a 0700 root, an empty config.toml
        // (a real EMPTY policy layer — `require_initialized_sidecar`
        // reads existence, the merge reads emptiness), and the
        // worktrees dir the run will bind.
        let base = tmpdir("create");
        let root = TempRoot::create_under(&base).unwrap();
        let md = std::fs::symlink_metadata(&root.root).unwrap();
        use std::os::unix::fs::MetadataExt;
        assert_eq!(md.mode() & 0o777, 0o700);
        assert!(root.sidecar.join("config.toml").is_file());
        assert_eq!(
            std::fs::read_to_string(root.sidecar.join("config.toml")).unwrap(),
            ""
        );
        assert!(root.worktrees.is_dir());
        let _ = std::fs::remove_dir_all(&root.root);
    }

    #[test]
    fn create_refuses_a_second_root_for_the_same_pid() {
        // One process, one root: the second create finds the first
        // one's directory and refuses instead of reusing it.
        let base = tmpdir("create-twice");
        let root = TempRoot::create_under(&base).unwrap();
        assert!(TempRoot::create_under(&base).is_err());
        let _ = std::fs::remove_dir_all(&root.root);
    }

    #[test]
    fn redirect_replaces_sidecar_and_worktrees() {
        // The seam of bd myconfig-7zwt: whatever the host layout is
        // (here: an EXISTING sidecar and sibling, which must be
        // ignored), the redirect points the run at the temp root.
        let base = tmpdir("redirect");
        let repo_root = base.join("repo");
        std::fs::create_dir_all(&repo_root).unwrap();
        let host_sidecar = base.join("repo.mysbx");
        std::fs::create_dir_all(&host_sidecar).unwrap();
        std::fs::write(host_sidecar.join("config.toml"), "backend = \"krun\"\n").unwrap();
        let host_worktrees = base.join("repo__worktrees");
        std::fs::create_dir_all(&host_worktrees).unwrap();
        let mut repo = Repo {
            root: repo_root,
            sidecar: host_sidecar,
            git_dirs: Vec::new(),
            worktrees: Some(host_worktrees),
        };
        let temp = TempRoot {
            root: base.join("temp"),
            sidecar: base.join("temp/sidecar"),
            worktrees: base.join("temp/worktrees"),
        };
        temp.redirect(&mut repo);
        assert_eq!(repo.sidecar, temp.sidecar);
        assert_eq!(repo.worktrees, Some(temp.worktrees));
    }

    #[test]
    fn teardown_removes_a_clean_root_and_prunes_the_repo() {
        let base = tmpdir("teardown-clean");
        let repo_root = base.join("repo");
        std::fs::create_dir_all(&repo_root.join(".git")).unwrap();
        let temp = TempRoot {
            root: base.join("temp"),
            sidecar: base.join("temp/sidecar"),
            worktrees: base.join("temp/worktrees"),
        };
        std::fs::create_dir_all(&temp.sidecar).unwrap();
        std::fs::create_dir_all(&temp.worktrees).unwrap();
        let repo = Repo {
            root: repo_root,
            sidecar: base.join("repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        };
        assert!(!teardown(&temp, &repo), "a clean root is removed");
        assert!(!temp.root.exists());
    }

    #[test]
    fn teardown_keeps_a_root_with_a_dirty_worktree() {
        // The data-loss guard: an uncommitted worktree must keep the
        // root. No git fixture is needed for the GUARD's wiring — a
        // `.git` FILE entry whose `git status` cannot run fails
        // closed, i.e. counts as dirty.
        let base = tmpdir("teardown-dirty");
        let repo_root = base.join("repo");
        std::fs::create_dir_all(&repo_root.join(".git")).unwrap();
        let temp = TempRoot {
            root: base.join("temp"),
            sidecar: base.join("temp/sidecar"),
            worktrees: base.join("temp/worktrees"),
        };
        std::fs::create_dir_all(&temp.sidecar).unwrap();
        std::fs::create_dir_all(&temp.worktrees.join("wt")).unwrap();
        // A linked-worktree pointer at a NONEXISTENT gitdir: the
        // entry looks like a worktree, `git status` fails, the
        // guard fails closed → the root is kept.
        std::fs::write(temp.worktrees.join("wt/.git"), "gitdir: /nonexistent/x\n").unwrap();
        let repo = Repo {
            root: repo_root,
            sidecar: base.join("repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        };
        assert!(teardown(&temp, &repo), "a dirty worktree keeps the root");
        assert!(temp.root.exists());
    }
}
