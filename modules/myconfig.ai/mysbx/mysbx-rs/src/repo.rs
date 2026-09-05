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
                Repo {
                    root: d.to_owned(),
                    sidecar,
                },
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
                Repo {
                    root: d.to_owned(),
                    sidecar,
                },
                home,
            );
        }
        dir = d.parent();
    }
    // Step 3: no sidecar, no git — the starting directory is the repo.
    let sidecar = sibling_sidecar(start);
    guarded(
        Repo {
            root: start.to_owned(),
            sidecar,
        },
        home,
    )
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
}
