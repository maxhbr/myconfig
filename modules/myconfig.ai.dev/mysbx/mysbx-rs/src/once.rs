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
/// the work still lives. The host repo's worktree BOOKKEEPING is
/// cleaned up surgically per worktree of THIS run — never with
/// `git worktree prune`, which is repo-global and would also delete
/// the registration of a KEPT worktree (its backlink names the
/// sandbox-only `<repo>__worktrees` spelling, absent on the host)
/// and of a CONCURRENT once-run (whose backlink names that same
/// absent spelling while its temp root still exists). A kept
/// worktree's backlink is REPAIRED to its real host path first, so
/// the operator's own later `git worktree prune` keeps finding it.
/// Best-effort by stage: a stage that fails does not block the
/// next, and only the removal failure is reported.
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
    let mut dirty: Vec<PathBuf> = Vec::new();
    if let Some(entries) = read_worktree_dirs(&root.worktrees) {
        for wt in entries {
            if worktree_dirty(&wt) {
                println!(
                    "## mysbx-once: kept {}: uncommitted changes in {}",
                    root.root.display(),
                    wt.display()
                );
                dirty.push(wt);
                keep = true;
            }
        }
    }
    if keep {
        // The kept worktrees' admin backlinks still name the SANDBOX
        // spelling (`<repo>__worktrees/...`, the path the payload's
        // git used) — a spelling that does not exist on the host, so
        // the operator's next `git worktree prune` would delete
        // their admin dirs (index, HEAD) and orphan the kept work.
        // `git worktree repair <path>` repoints the backlink at the
        // real kept path. Quiet and best-effort: a repair that fails
        // leaves debris the operator can see with `git worktree
        // list` — it does not lose work.
        for wt in &dirty {
            let _ = std::process::Command::new("git")
                .arg("-C")
                .arg(&repo.root)
                .args(["worktree", "repair"])
                .arg(wt)
                .stdout(std::process::Stdio::null())
                .stderr(std::process::Stdio::null())
                .status();
        }
        return keep;
    }
    // All clean: the admin dirs are collected BEFORE the removal
    // (they are read out of the worktrees' `.git` pointer files,
    // which die with the root), then the temp root goes, then THIS
    // run's registrations — surgically, one per worktree we found,
    // each only when its own backlink names a path of THIS run (a
    // concurrent once-run's backlink names ITS temp root, an
    // operator's names a real host path — neither is ours). This is
    // what `git worktree prune` would do to our entries, without its
    // repo-global reach.
    let admins = admin_dirs_of(&root.worktrees);
    if let Err(e) = std::fs::remove_dir_all(&root.root) {
        eprintln!(
            "mysbx-once: cannot remove the temp root {}: {e}",
            root.root.display()
        );
    }
    for admin in admins {
        let _ = remove_admin_if_ours(&admin, root, repo);
    }
    keep
}

/// The admin dirs (`<gitdir>/worktrees/<name>`) of every linked
/// worktree under `<dir>`, read from each worktree's `.git` pointer
/// file — the pointer IS the admin path (`gitdir:
/// <repo>/.git/worktrees/<name>`). Worktrees whose pointer cannot be
/// read are skipped: their admin dir is left for the operator's own
/// `git worktree prune` — a missed cleanup, never a lost worktree.
fn admin_dirs_of(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    if let Some(worktrees) = read_worktree_dirs(dir) {
        for wt in worktrees {
            if let Ok(pointer) = std::fs::read_to_string(wt.join(".git")) {
                // `gitdir: <path>` — git's own format, no quoting.
                if let Some(path) = pointer.strip_prefix("gitdir: ") {
                    let path = path.trim();
                    if !path.is_empty() {
                        out.push(PathBuf::from(path));
                    }
                }
            }
        }
    }
    out
}

/// Remove one admin registration when it belongs to THIS run: its
/// `gitdir` backlink file must name a worktree path that lived
/// under this run's temp root or at this run's sandbox spelling of
/// it. Anything else — a concurrent run's root, an operator's own
/// worktree — is left alone.
fn remove_admin_if_ours(admin: &Path, root: &TempRoot, repo: &Repo) -> std::io::Result<()> {
    let backlink = std::fs::read_to_string(admin.join("gitdir"))?;
    let backlink = backlink.trim();
    let ours = Path::new(backlink).starts_with(&root.worktrees)
        || crate::repo::sibling_worktrees(&repo.root)
            .is_some_and(|s| Path::new(backlink).starts_with(&s));
    if !ours {
        return Ok(());
    }
    std::fs::remove_dir_all(admin)
}

/// The linked worktrees under `<dir>` — at EVERY depth: a
/// workmux-style layout groups them (`<repo>__worktrees/<team>/<fix>`,
/// nested by topic), and `git worktree add` takes any path, so no
/// depth is more correct than another. The walk is cheap (the temp
/// root dies with the run, nobody else writes it) and it must not
/// MISS one: a missed worktree's uncommitted changes are deleted
/// with the root. An entry with a `.git` FILE is a linked worktree's
/// pointer; a `.git` DIRECTORY is a plain repository someone put
/// there, a missing one is debris — both are removed with the root
/// without a guard: neither carries uncommitted WORKTREE state git
/// could still commit.
fn read_worktree_dirs(dir: &Path) -> Option<Vec<PathBuf>> {
    let mut out = Vec::new();
    walk_worktree_dirs(dir, &mut out);
    out.sort();
    Some(out)
}

fn walk_worktree_dirs(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in std::fs::read_dir(dir).ok().into_iter().flatten() {
        let Ok(entry) = entry else { continue };
        let path = entry.path();
        let is_dir = path.is_dir();
        if path.join(".git").is_file() {
            out.push(path.clone());
        }
        // A linked worktree can hold nested ones in its own tree
        // (a repo checked out inside a worktree — git does not
        // care), and the plain grouping directories between them
        // are walked either way.
        if is_dir {
            walk_worktree_dirs(&path, out);
        }
    }
}

/// Whether a worktree has uncommitted changes — `git status
/// --porcelain` with any output counts (the handoff verbs' own
/// probe, `handoff::host_tree_dirty`) — with `--untracked-files=all`
/// FORCED on: this probe is a DELETION guard, and a repo (or the
/// operator's global config) with `status.showUntrackedFiles = no`
/// would otherwise make porcelain print nothing for a worktree
/// whose only work is new files — exactly the work that exists
/// nowhere else and would be deleted. Fails CLOSED: a `git` that
/// cannot run counts as dirty — the root must not be removed over
/// work whose state cannot be inspected.
fn worktree_dirty(worktree: &Path) -> bool {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(worktree)
        .args(["status", "--porcelain", "--untracked-files=all"])
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

    #[test]
    fn remove_admin_if_ours_never_touches_a_foreign_registration() {
        // The surgical cleanup's own rule: an admin dir whose
        // backlink names a path under THIS run's root (or this
        // run's sandbox spelling of it) is removed; one naming a
        // CONCURRENT run's root — or an operator's real worktree —
        // is left alone. The old repo-global `git worktree prune`
        // could not tell them apart.
        let base = tmpdir("admin-ours");
        let temp = TempRoot {
            root: base.join("temp"),
            sidecar: base.join("temp/sidecar"),
            worktrees: base.join("temp/worktrees"),
        };
        let repo = Repo {
            root: base.join("repo"),
            sidecar: base.join("repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        };
        // Ours: the backlink names a path under the temp root.
        let ours = base.join("git/worktrees/ours");
        std::fs::create_dir_all(&ours).unwrap();
        std::fs::write(
            ours.join("gitdir"),
            format!("{}", temp.worktrees.join("w").display()),
        )
        .unwrap();
        // Foreign: a concurrent run's root (a different pid dir).
        let theirs = base.join("git/worktrees/theirs");
        std::fs::create_dir_all(&theirs).unwrap();
        std::fs::write(
            theirs.join("gitdir"),
            format!("{}", base.join("other-root/worktrees/w").display()),
        )
        .unwrap();
        // Foreign: an operator's real worktree.
        let operator = base.join("git/worktrees/operator");
        std::fs::create_dir_all(&operator).unwrap();
        std::fs::write(
            operator.join("gitdir"),
            format!("{}", base.join("real-checkout").display()),
        )
        .unwrap();
        remove_admin_if_ours(&ours, &temp, &repo).unwrap();
        assert!(!ours.exists(), "our own registration is removed");
        let _ = remove_admin_if_ours(&theirs, &temp, &repo);
        assert!(theirs.exists(), "a concurrent run's registration is kept");
        let _ = remove_admin_if_ours(&operator, &temp, &repo);
        assert!(operator.exists(), "an operator's registration is kept");
    }

    #[test]
    fn read_worktree_dirs_finds_nested_worktrees() {
        // A workmux-style grouped layout: `<worktrees>/<team>/<fix>`
        // is a linked worktree two levels down, and a `.git`
        // DIRECTORY (a plain repo someone put there) is not one —
        // the guard must see the first and not the second.
        let base = tmpdir("nested");
        let deep = base.join("worktrees/team/fix");
        std::fs::create_dir_all(&deep).unwrap();
        std::fs::write(deep.join(".git"), "gitdir: somewhere\n").unwrap();
        let plain = base.join("worktrees/plain-repo");
        std::fs::create_dir_all(plain.join(".git")).unwrap();
        let found = read_worktree_dirs(&base.join("worktrees")).unwrap();
        assert_eq!(found, vec![deep], "only the linked worktree is found");
    }

    #[test]
    fn worktree_dirty_forces_untracked_files_all_over_the_config() {
        // A repo (or a global config) with `status.showUntrackedFiles
        // = no` must NOT hide a worktree whose only work is new
        // files — the guard would delete them. `--untracked-files=all`
        // on the command line overrides the config.
        let dir = tmpdir("untracked");
        let dirty = dir.join("dirty");
        std::fs::create_dir_all(&dirty).unwrap();
        let git = |args: &[&str]| {
            std::process::Command::new("git")
                .arg("-C")
                .arg(&dirty)
                .args(args)
                .env("GIT_CONFIG_GLOBAL", "/dev/null")
                .env("GIT_CONFIG_SYSTEM", "/dev/null")
                .env("GIT_AUTHOR_NAME", "mysbx-tests")
                .env("GIT_AUTHOR_EMAIL", "mysbx-tests@invalid")
                .env("GIT_COMMITTER_NAME", "mysbx-tests")
                .env("GIT_COMMITTER_EMAIL", "mysbx-tests@invalid")
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        };
        if !git(&["init", "-b", "main"]) {
            eprintln!("skipping: no git on PATH");
            return;
        }
        if !git(&["config", "status.showUntrackedFiles", "no"]) {
            return;
        }
        std::fs::write(dirty.join("uncommitted"), "work").unwrap();
        // The probe sees the untracked file despite the config.
        assert!(
            worktree_dirty(&dirty),
            "an untracked-only worktree counts as dirty despite status.showUntrackedFiles=no"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }
}
