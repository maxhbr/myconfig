// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Repository discovery (docs/TODOs/mvp-2-repo-discovery.md).
//!
//! From a starting directory the repository is resolved in three steps:
//!
//! 1. Walk up; the first ancestor `<dir>` with an **existing** `<dir>.mysbx`
//!    directory wins. An existing sidecar is a policy the user wrote, so it
//!    must not be shadowed by a `.git` closer to the start (nested repos,
//!    submodules).
//! 2. Otherwise the git work-tree root: the nearest ancestor containing
//!    `.git` — a file or a directory, so worktrees and submodules resolve.
//! 3. Otherwise the starting directory itself.
//!
//! The sidecar is always `<repo>.mysbx`: repo and sidecar determine each
//! other, and the repo itself is not expressible in the configuration
//! (docs/design/config.md D13).
//!
//! When the root carries a `.git` FILE (linked worktrees, submodules), the
//! git metadata lives OUTSIDE the root; `resolve` parses the file's
//! `gitdir:` pointer and the gitdir's `commondir` file and records both
//! as absolute paths in [`Repo::git_dirs`], so the sandbox can bind
//! them (review-1 finding 4). A `.git` DIRECTORY contributes nothing —
//! it is inside the root and mounted with it.
//!
//! The resolution is a pure function over the starting path and the home
//! directory, so the tests run against a temporary directory tree without
//! touching the real environment. `resolve_cwd` is the thin wrapper that
//! reads the real CWD and `$HOME`.

use std::fmt;
use std::path::{Path, PathBuf};

/// A resolved repository and its sidecar.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Repo {
    /// The repository root directory.
    pub root: PathBuf,
    /// The sidecar directory `<root>.mysbx`, always a sibling of `root`.
    pub sidecar: PathBuf,
    /// Absolute git metadata directories this repository needs outside
    /// its root — empty for a plain repository (`.git` is a directory
    /// inside the root, mounted with it), two entries at most for a
    /// linked worktree or submodule: the git dir the `.git` FILE points
    /// at and, when it differs, the common dir its `commondir` file names
    /// (refs, config, objects live there). Without these binds `git
    /// status` and friends fail inside the sandbox, because the `.git`
    /// file's pointer leaves the worktree (review-1 finding 4).
    pub git_dirs: Vec<PathBuf>,
}

/// Why a repository could not be resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The resolved repository is the user's home directory. Never
    /// overridable in the MVP: implicit init must not `rw`-bind the whole
    /// home under a policy nobody wrote.
    HomeDir(PathBuf),
    /// The resolved repository is the filesystem root.
    RootDir,
    /// The filesystem could not be queried.
    Io(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::HomeDir(p) => {
                write!(
                    f,
                    "refusing to use the home directory as a repo: {}",
                    p.display()
                )
            }
            Error::RootDir => f.write_str("refusing to use / as a repo"),
            Error::Io(m) => f.write_str(m),
        }
    }
}

impl std::error::Error for Error {}

/// Resolve the repository for the real current working directory and the
/// real `$HOME`.
pub fn resolve_cwd() -> Result<Repo, Error> {
    let cwd = std::env::current_dir()
        .map_err(|e| Error::Io(format!("cannot determine current directory: {e}")))?;
    // A missing `$HOME` is an error, not a silent guard bypass: every
    // other failure here is loud, and the home guard must never quietly
    // disappear (cron, systemd units, `su` shells).
    let home = std::env::var_os("HOME").ok_or_else(|| {
        Error::Io("$HOME is not set; refusing to run without the home guard".into())
    })?;
    resolve(&cwd, Some(home.as_ref()))
}

/// Resolve the repository for `start`, walking up the directory tree.
///
/// `home` is the user's home directory; both it and the resolved repo are
/// canonicalized before they are compared, so a symlinked home cannot slip
/// past the guard. `home = None` disables the home guard — only reachable
/// from tests; `resolve_cwd` treats a missing `$HOME` as an error so the
/// guard can never silently disappear.
pub fn resolve(start: &Path, home: Option<&Path>) -> Result<Repo, Error> {
    // Step 1: an existing sidecar wins, nearest first.
    let mut dir = Some(start);
    while let Some(d) = dir {
        let sidecar = sibling_sidecar(d);
        if sidecar.is_dir() {
            return guarded(
                repo_at(d, sidecar)?,
                home,
            );
        }
        dir = d.parent();
    }
    // Step 2: the nearest ancestor with a `.git` entry is the work-tree
    // root; `.git` may be a file (worktrees, submodules).
    let mut dir = Some(start);
    while let Some(d) = dir {
        if d.join(".git").exists() {
            let sidecar = sibling_sidecar(d);
            return guarded(
                repo_at(d, sidecar)?,
                home,
            );
        }
        dir = d.parent();
    }
    // Step 3: no sidecar, no git — the starting directory is the repo.
    let sidecar = sibling_sidecar(start);
    guarded(
        repo_at(start, sidecar)?,
        home,
    )
}

/// A `Repo` rooted at `dir`, resolving git metadata that lives OUTSIDE
/// the root: a `.git` FILE points at a gitdir elsewhere (linked
/// worktrees, submodules), whose `commondir` file in turn names the
/// shared metadata dir. Both are recorded as absolute paths so the
/// argv builder can bind them into the sandbox; a `.git` DIRECTORY
/// (plain repository) contributes nothing — it is inside the root and
/// mounted with it. Unparsable or dangling pointers are ignored: repo
/// resolution must stay advisory here, `git` inside the sandbox will
/// give the authoritative error when the metadata is really broken.
fn repo_at(dir: &Path, sidecar: PathBuf) -> Result<Repo, Error> {
    let mut git_dirs = Vec::new();
    let dot_git = dir.join(".git");
    if dot_git.is_file() {
        if let Some(gitdir) = parse_gitdir(&dot_git, dir) {
            if let Some(common) = parse_commondir(&gitdir) {
                if common != gitdir && !git_dirs.contains(&common) {
                    git_dirs.push(common);
                }
            }
            git_dirs.push(gitdir);
        }
    }
    Ok(Repo {
        root: dir.to_owned(),
        sidecar,
        git_dirs,
    })
}

/// Parse a `.git` file's `gitdir: <path>` line (the only line git
/// writes for linked worktrees and submodules). Relative paths are
/// resolved against the worktree root, like git does. Fails soft: an
/// unreadable file or missing pointer yields `None`.
fn parse_gitdir(dot_git: &Path, worktree_root: &Path) -> Option<PathBuf> {
    let text = std::fs::read_to_string(dot_git).ok()?;
    let line = text.lines().find(|l| l.starts_with("gitdir: "))?;
    let raw = line["gitdir: ".len()..].trim();
    if raw.is_empty() {
        return None;
    }
    let path = Path::new(raw);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else {
        worktree_root.join(path)
    };
    std::fs::canonicalize(&resolved).ok()
}

/// Parse a gitdir's `commondir` file: one relative-or-absolute path
/// naming the shared metadata dir (a linked worktree's refs/objects
/// live in the main repository). Fails soft, same as `parse_gitdir`.
fn parse_commondir(gitdir: &Path) -> Option<PathBuf> {
    let text = std::fs::read_to_string(gitdir.join("commondir")).ok()?;
    let raw = text.trim();
    if raw.is_empty() {
        return None;
    }
    let path = Path::new(raw);
    let resolved = if path.is_absolute() {
        path.to_path_buf()
    } else {
        gitdir.join(path)
    };
    std::fs::canonicalize(&resolved).ok()
}

/// `<dir>.mysbx` — the sidecar always sits next to the repo
/// (docs/design/config.md D13).
fn sibling_sidecar(dir: &Path) -> PathBuf {
    let mut name = dir.as_os_str().to_owned();
    name.push(".mysbx");
    PathBuf::from(name)
}

/// The guard: never operate on `$HOME` or `/`
/// (docs/TODOs/mvp-2-repo-discovery.md). Canonicalize before comparing,
/// otherwise a symlinked home slips past.
fn guarded(repo: Repo, home: Option<&Path>) -> Result<Repo, Error> {
    let root = std::fs::canonicalize(&repo.root)
        .map_err(|e| Error::Io(format!("cannot canonicalize {}: {e}", repo.root.display())))?;
    if root == Path::new("/") {
        return Err(Error::RootDir);
    }
    if let Some(home) = home {
        let home = std::fs::canonicalize(home)
            .map_err(|e| Error::Io(format!("cannot canonicalize home {}: {e}", home.display())))?;
        if root == home {
            return Err(Error::HomeDir(home));
        }
    }
    Ok(repo)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A fresh temporary directory per test; hand-rolled, the crate has no
    /// dependencies.
    fn tmpdir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-repo-test-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn touch_dir(path: &Path) {
        std::fs::create_dir_all(path).unwrap();
    }

    const FAKE_HOME_SUB: &str = "fake-home";

    /// A home directory that is not the real one, so tests never depend on
    /// the machine.
    fn fake_home(base: &Path) -> PathBuf {
        let home = base.join(FAKE_HOME_SUB);
        touch_dir(&home);
        home
    }

    #[test]
    fn sidecar_in_ancestor_wins() {
        let base = tmpdir("sidecar-ancestor");
        let repo = base.join("repo");
        touch_dir(&repo.join("deep").join("sub"));
        touch_dir(&base.join("repo.mysbx"));

        let start = repo.join("deep").join("sub");
        let r = resolve(&start, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, repo);
        assert_eq!(r.sidecar, base.join("repo.mysbx"));
    }

    #[test]
    fn git_root_two_levels_up() {
        let base = tmpdir("git-ancestor");
        let proj = base.join("proj");
        touch_dir(&proj.join("sub").join("deep"));
        touch_dir(&proj.join(".git"));

        let start = proj.join("sub").join("deep");
        let r = resolve(&start, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, proj);
        // The sidecar does not exist yet; it is where implicit init would
        // create it.
        assert_eq!(r.sidecar, base.join("proj.mysbx"));
    }

    #[test]
    fn neither_sidecar_nor_git_falls_back_to_start() {
        let base = tmpdir("fallback");
        let start = base.join("plain").join("sub");
        touch_dir(&start);

        let r = resolve(&start, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, start);
        assert_eq!(r.sidecar, base.join("plain").join("sub.mysbx"));
    }

    #[test]
    fn home_is_rejected() {
        let base = tmpdir("home-guard");
        let home = fake_home(&base);

        let e = resolve(&home, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::HomeDir(_)), "{e}");
    }

    #[test]
    fn symlinked_home_is_rejected() {
        let base = tmpdir("symlink-home-guard");
        let home = fake_home(&base);
        let link = base.join("link-to-home");
        std::os::unix::fs::symlink(&home, &link).unwrap();

        let e = resolve(&link, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::HomeDir(_)), "{e}");
    }

    #[test]
    fn root_is_rejected() {
        let base = tmpdir("root-guard");

        let e = resolve(Path::new("/"), Some(&fake_home(&base))).unwrap_err();
        assert_eq!(e, Error::RootDir);
    }

    #[test]
    fn git_file_worktree_resolves() {
        let base = tmpdir("git-file");
        let wt = base.join("wt");
        touch_dir(&wt.join("sub"));
        std::fs::write(wt.join(".git"), "gitdir: /somewhere/else\n").unwrap();

        let r = resolve(&wt.join("sub"), Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, wt);
        assert_eq!(r.sidecar, base.join("wt.mysbx"));
    }

    #[test]
    fn sidecar_beats_closer_git() {
        // docs/TODOs/mvp-2-repo-discovery.md "Watch out": step 1 beats step 2
        // on purpose — a nested repo or submodule must not shadow the
        // sidecar the user wrote next to the outer repo.
        let base = tmpdir("sidecar-beats-git");
        let outer = base.join("outer");
        touch_dir(&outer.join("inner").join("sub"));
        touch_dir(&outer.join("inner").join(".git"));
        touch_dir(&base.join("outer.mysbx"));

        let r = resolve(&outer.join("inner").join("sub"), Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, outer);
        assert_eq!(r.sidecar, base.join("outer.mysbx"));
    }

    #[test]
    fn symlinked_start_into_home_cannot_slip_past() {
        // The *start* is a symlink to the home itself: guarded() must
        // canonicalize the resolved root before comparing.
        let base = tmpdir("start-symlink");
        let home = fake_home(&base);
        let link = base.join("link-to-home");
        std::os::unix::fs::symlink(&home, &link).unwrap();

        let e = resolve(&link, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::HomeDir(_)), "{e}");
    }

    /// Build a linked-worktree layout: `main/.git` is a plain directory,
    /// `main/.git/worktrees/<name>` holds the per-worktree gitdir, and the
    /// worktree's `.git` is a FILE pointing at it, with a `commondir`
    /// file pointing back at `main/.git` (relative, like real git).
    fn make_worktree(base: &Path) -> (PathBuf, PathBuf) {
        let main = base.join("main");
        let gitdir = main.join(".git").join("worktrees").join("wt");
        touch_dir(&gitdir);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", gitdir.display()),
        )
        .unwrap();
        std::fs::write(gitdir.join("commondir"), "../..\n").unwrap();
        (main, worktree)
    }

    #[test]
    fn linked_worktree_resolves_git_dirs() {
        // Review-1 finding 4: a `.git` FILE must yield the gitdir AND the
        // commondir it names, both absolute, so the sandbox can bind
        // them.
        let base = tmpdir("worktree");
        let (main, worktree) = make_worktree(&base);

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, worktree);
        let main_git = main.join(".git");
        let expected = vec![
            std::fs::canonicalize(&main_git).unwrap(),
            std::fs::canonicalize(main_git.join("worktrees").join("wt"))
                .unwrap(),
        ];
        assert_eq!(r.git_dirs, expected);
    }

    #[test]
    fn sidecar_in_worktree_still_resolves_git_dirs() {
        // Step 1 (sidecar wins) must not lose the git metadata either.
        let base = tmpdir("worktree-sidecar");
        let (main, worktree) = make_worktree(&base);
        touch_dir(worktree.join("sibling.mysbx").parent().unwrap());
        let sidecar = base.join("wt.mysbx");
        touch_dir(&sidecar);

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, worktree);
        assert_eq!(r.sidecar, sidecar);
        assert_eq!(r.git_dirs.len(), 2);
        assert!(r.git_dirs.contains(&std::fs::canonicalize(main.join(".git"))
            .unwrap()));
    }

    #[test]
    fn plain_repository_has_no_external_git_dirs() {
        // A `.git` DIRECTORY is inside the root and mounted with it:
        // `git_dirs` must stay empty.
        let base = tmpdir("plain-git");
        let proj = base.join("proj");
        touch_dir(&proj.join(".git"));

        let r = resolve(&proj, Some(&fake_home(&base))).unwrap();
        assert!(r.git_dirs.is_empty());
    }

    #[test]
    fn dangling_gitdir_pointer_is_ignored() {
        // Fail-soft: a `.git` file pointing nowhere resolves the repo
        // (git itself gives the authoritative error later).
        let base = tmpdir("dangling-gitdir");
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(worktree.join(".git"), "gitdir: /nonexistent/does-not-exist\n")
            .unwrap();

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, worktree);
        assert!(r.git_dirs.is_empty());
    }

    #[test]
    fn git_file_without_gitdir_line_is_ignored() {
        let base = tmpdir("no-gitdir-line");
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(worktree.join(".git"), "not a gitdir pointer\n").unwrap();

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, worktree);
        assert!(r.git_dirs.is_empty());
    }

    #[test]
    fn crlf_git_file_is_parsed() {
        // A checkout created on Windows carries CRLF line endings; the
        // pointer must still resolve.
        let base = tmpdir("crlf-gitdir");
        let main = base.join("main");
        let gitdir = main.join(".git").join("worktrees").join("wt");
        touch_dir(&gitdir);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\r\n", gitdir.display()),
        )
        .unwrap();

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(
            r.git_dirs,
            vec![std::fs::canonicalize(&gitdir).unwrap()]
        );
    }

    #[test]
    fn relative_gitdir_resolves_against_the_worktree() {
        // Submodules use relative gitdir paths (`gitdir: ../.git/modules/x`)
        // and usually have NO commondir file — the gitdir IS the common
        // dir then, so exactly one entry must be recorded.
        let base = tmpdir("relative-gitdir");
        let superp = base.join("super");
        let gitdir = superp.join(".git").join("modules").join("sub");
        touch_dir(&gitdir);
        let sub = base.join("super").join("sub");
        touch_dir(&sub);
        std::fs::write(sub.join(".git"), "gitdir: ../.git/modules/sub\n").unwrap();

        let r = resolve(&sub, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, sub);
        assert_eq!(r.git_dirs, vec![std::fs::canonicalize(&gitdir).unwrap()]);
    }
}
