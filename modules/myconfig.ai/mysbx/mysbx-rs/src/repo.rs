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
    /// overridable in the MVP: no run may `rw`-bind the whole home under
    /// a policy nobody wrote.
    HomeDir(PathBuf),
    /// The resolved repository CONTAINS the user's home directory
    /// (review-4 item 2): a `.git` marker or a stale sidecar high up
    /// the tree (`/srv/tree/.git` with `HOME=/srv/tree/users/alice`)
    /// would otherwise make the whole subtree the repo — and the repo
    /// is bound read-write, so the home, `.ssh` and every other
    /// user's files ride along. Exactly as unoverridable as
    /// [`Error::HomeDir`]: the equality check was only ever the
    /// degenerate case of this one.
    HomeAncestorDir {
        /// The discovered repository root, canonicalized.
        root: PathBuf,
        /// The home directory it contains, canonicalized.
        home: PathBuf,
    },
    /// The resolved repository is the filesystem root.
    RootDir,
    /// The filesystem could not be queried.
    Io(String),
    /// A `.git` FILE points at a directory that can never be approved as
    /// git metadata (review-2 item 1): the filesystem root, an
    /// ancestor of the home directory, an ancestor of (or equal to)
    /// the repo root, or the home directory itself. Unlike a merely
    /// unapproved target — which the argv builder refuses against the
    /// `git-dirs` approval list — these are refused at resolution
    /// time, hard: binding any of them would re-expose exactly what
    /// the base table and the home guard keep out, and no
    /// configuration can make them safe.
    GitDirForbidden { gitdir: PathBuf },
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
            Error::HomeAncestorDir { root, home } => write!(
                f,
                "refusing to use {} as a repo: it contains the home directory {} \
                 — the repo is bound read-write, so the whole home would be \
                 exposed to the sandbox; move the .git marker or the sidecar, \
                 or run from a repository below the home",
                root.display(),
                home.display()
            ),
            Error::RootDir => f.write_str("refusing to use / as a repo"),
            Error::Io(m) => f.write_str(m),
            Error::GitDirForbidden { gitdir } => write!(
                f,
                "refusing to bind git metadata {}: a .git file must never point at the filesystem root, the home directory, an ancestor of either, or a directory containing the repo (review-2 item 1)",
                gitdir.display()
            ),
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
            return guarded(repo_at(d, sidecar)?, home);
        }
        dir = d.parent();
    }
    // Step 2: the nearest ancestor with a `.git` entry is the work-tree
    // root; `.git` may be a file (worktrees, submodules).
    let mut dir = Some(start);
    while let Some(d) = dir {
        if d.join(".git").exists() {
            let sidecar = sibling_sidecar(d);
            return guarded(repo_at(d, sidecar)?, home);
        }
        dir = d.parent();
    }
    // Step 3: no sidecar, no git — the starting directory is the repo.
    let sidecar = sibling_sidecar(start);
    guarded(repo_at(start, sidecar)?, home)
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
/// unreadable file, missing pointer, or a target that does not look
/// like git metadata yields `None` — repo resolution stays advisory;
/// `git` inside the sandbox gives the authoritative error when the
/// metadata is really broken. The target must pass [`looks_like_git`],
/// so a hostile `.git` file cannot name an arbitrary host directory
/// and have it recorded (review-2 item 1); what gets BOUND is decided
/// by the approval list, not here.
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
    let canon = std::fs::canonicalize(&resolved).ok()?;
    if !looks_like_git(&canon) {
        return None;
    }
    Some(canon)
}

/// A cheap structural check that a directory is git metadata (review-2
/// item 1): git requires `HEAD` and `refs/` in every git dir and every
/// common dir. It is not a content check — an attacker who controls the
/// directory can fake it — but it stops an untrusted `.git` FILE from
/// naming an arbitrary host directory (`gitdir: /`, `gitdir: $HOME`)
/// and having it recorded as metadata. Whether the directory may be
/// BOUND is the argv builder's approval question, not this one.
fn looks_like_git(dir: &Path) -> bool {
    dir.join("HEAD").exists() && dir.join("refs").is_dir()
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
    let canon = std::fs::canonicalize(&resolved).ok()?;
    if !looks_like_git(&canon) {
        return None;
    }
    Some(canon)
}

/// `<dir>.mysbx` — the sidecar always sits next to the repo
/// (docs/design/config.md D13).
fn sibling_sidecar(dir: &Path) -> PathBuf {
    let mut name = dir.as_os_str().to_owned();
    name.push(".mysbx");
    PathBuf::from(name)
}

/// The guard: never operate on `$HOME`, on a directory CONTAINING
/// `$HOME` (review-4 item 2) or on `/`
/// (docs/TODOs/mvp-2-repo-discovery.md). Canonicalize before comparing,
/// otherwise a symlinked home slips past. Also guards the git metadata
/// targets a repo-writable `.git` FILE points at (review-2 item 1):
/// `/`, the home directory, and any ancestor of either — or of the repo
/// root — can never be approved, so they are refused here, hard,
/// instead of reaching the argv builder's approval list at all.
fn guarded(repo: Repo, home: Option<&Path>) -> Result<Repo, Error> {
    let root = std::fs::canonicalize(&repo.root)
        .map_err(|e| Error::Io(format!("cannot canonicalize {}: {e}", repo.root.display())))?;
    if root == Path::new("/") {
        return Err(Error::RootDir);
    }
    if let Some(home) = home {
        let home = std::fs::canonicalize(home)
            .map_err(|e| Error::Io(format!("cannot canonicalize home {}: {e}", home.display())))?;
        // Equal OR an ancestor (review-4 item 2). `Path::starts_with`
        // compares whole COMPONENTS on the two canonical paths, so
        // `/srv/tree` catches `/srv/tree/users/alice` while the
        // lookalike `/srv/treehouse` stays a perfectly ordinary repo
        // root — a string-prefix test would confuse the two. A repo
        // BELOW the home (the normal `~/src/project`) is untouched:
        // the home does not start with it.
        if home.starts_with(&root) {
            return Err(if root == home {
                Error::HomeDir(home)
            } else {
                Error::HomeAncestorDir { root, home }
            });
        }
        for gitdir in &repo.git_dirs {
            // The git dir AT or BELOW home is the normal approved
            // case (`~/src/repo/.git/worktrees/wt`); an ANCESTOR of
            // home (`gitdir: /home`) or home itself is never
            // approvable.
            if home.starts_with(gitdir) {
                return Err(Error::GitDirForbidden {
                    gitdir: gitdir.to_owned(),
                });
            }
        }
    }
    for gitdir in &repo.git_dirs {
        // `/` is an ancestor of every path; the explicit check keeps
        // the message honest for it. An ancestor of (or equal to) the
        // repo root would rw-bind a directory containing the worktree.
        if gitdir == Path::new("/") || root.starts_with(gitdir) {
            return Err(Error::GitDirForbidden {
                gitdir: gitdir.to_owned(),
            });
        }
        // The sidecar is mysbx's own trusted state — the file that
        // decides what may be bound at all (config.md D2). Binding it
        // (or anything containing it, or anything inside it) would let
        // the payload rewrite its own policy, so a `.git` pointer at
        // the sidecar is refused in EVERY direction, not approvable.
        if gitdir.starts_with(&repo.sidecar) || repo.sidecar.starts_with(gitdir) {
            return Err(Error::GitDirForbidden {
                gitdir: gitdir.to_owned(),
            });
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

    /// Give a directory the shape git metadata must have (review-2
    /// item 1): `HEAD` and `refs/`.
    fn git_shape(dir: &Path) {
        touch_dir(&dir.join("refs"));
        std::fs::write(dir.join("HEAD"), "ref: refs/heads/main\n").unwrap();
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
        // The sidecar does not exist yet; it is where `mysbx init` would
        // create it (a run there fails with the init hint, cli.md D13).
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

    // ---- a repo root ABOVE the home is refused (review-4 item 2) -------

    #[test]
    fn a_git_root_containing_the_home_is_rejected() {
        // The review's shape: `<base>/tree/.git` with
        // `HOME=<base>/tree/users/alice`. The equality check passed and
        // the implicit rw repo bind then exposed the entire subtree,
        // home and `.ssh` included.
        let base = tmpdir("git-root-above-home");
        let tree = base.join("tree");
        let home = tree.join("users").join("alice");
        touch_dir(&home);
        touch_dir(&tree.join(".git"));
        let start = home.join("project").join("subdir");
        touch_dir(&start);

        let e = resolve(&start, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::HomeAncestorDir { .. }), "{e}");
    }

    #[test]
    fn a_sidecar_root_containing_the_home_is_rejected() {
        // The same invariant for step 1: an EXISTING sidecar high up
        // the tree must not buy a repo root that contains the home.
        let base = tmpdir("sidecar-root-above-home");
        let tree = base.join("tree");
        let home = tree.join("users").join("alice");
        touch_dir(&home);
        touch_dir(&base.join("tree.mysbx"));
        let start = home.join("project").join("subdir");
        touch_dir(&start);

        let e = resolve(&start, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::HomeAncestorDir { .. }), "{e}");
    }

    #[test]
    fn a_symlinked_home_spelling_cannot_slip_past_the_ancestor_guard() {
        // The comparison is on CANONICAL paths: `$HOME` handed in
        // through a symlink must not make the containment invisible.
        let base = tmpdir("symlinked-home-above");
        let tree = base.join("tree");
        let home = tree.join("users").join("alice");
        touch_dir(&home);
        touch_dir(&tree.join(".git"));
        let link = base.join("link-to-home");
        std::os::unix::fs::symlink(&home, &link).unwrap();
        let start = home.join("project");
        touch_dir(&start);

        let e = resolve(&start, Some(&link)).unwrap_err();
        assert!(matches!(e, Error::HomeAncestorDir { .. }), "{e}");
    }

    #[test]
    fn a_repository_below_the_home_stays_allowed() {
        // The normal case must not become collateral damage: a repo
        // INSIDE the home is the everyday layout.
        let base = tmpdir("repo-below-home");
        let home = fake_home(&base);
        let proj = home.join("src").join("project");
        touch_dir(&proj.join(".git"));
        let start = proj.join("sub");
        touch_dir(&start);

        let r = resolve(&start, Some(&home)).unwrap();
        assert_eq!(r.root, proj);
    }

    #[test]
    fn a_lookalike_sibling_of_the_home_stays_allowed() {
        // Component-aware containment, not string prefixes:
        // `<base>/fake-home-2` merely SPELLS like a prefix of
        // `<base>/fake-home`, and is an ordinary repo root.
        let base = tmpdir("lookalike-home");
        let home = fake_home(&base);
        let sibling = base.join(format!("{FAKE_HOME_SUB}-2"));
        touch_dir(&sibling.join(".git"));

        let r = resolve(&sibling, Some(&home)).unwrap();
        assert_eq!(r.root, sibling);
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
        git_shape(&gitdir);
        // The common dir needs the shape too — commondir parsing runs
        // the same structural check.
        git_shape(&main.join(".git"));
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
            std::fs::canonicalize(main_git.join("worktrees").join("wt")).unwrap(),
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
        assert!(r
            .git_dirs
            .contains(&std::fs::canonicalize(main.join(".git")).unwrap()));
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
        std::fs::write(
            worktree.join(".git"),
            "gitdir: /nonexistent/does-not-exist\n",
        )
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
        git_shape(&gitdir);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\r\n", gitdir.display()),
        )
        .unwrap();

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.git_dirs, vec![std::fs::canonicalize(&gitdir).unwrap()]);
    }

    // ---- adversarial .git pointers (review-2 item 1) ---------------------

    #[test]
    fn gitdir_pointing_at_root_is_refused_by_the_builder() {
        // `gitdir: /` must never be bound. At resolution time the
        // guard cannot be exercised without writing a fake git shape
        // into the real `/` (parse_gitdir fails soft on a non-git
        // root, which is already the safe outcome: nothing is
        // recorded, nothing is bound); the HARD refusal is pinned at
        // the argv builder in tests/argv.rs
        // (root_git_dir_is_refused_even_if_listed), where a Repo
        // carrying `/` as a git dir is refused regardless of the
        // approval list.
        let base = tmpdir("gitdir-root");
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(worktree.join(".git"), "gitdir: /\n").unwrap();

        // Fail-soft here: the root does not look like git metadata,
        // so nothing is recorded and the run proceeds without the
        // bind.
        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert!(r.git_dirs.is_empty());
    }

    #[test]
    fn gitdir_ancestor_of_home_is_refused() {
        // `gitdir: <an ancestor of $HOME>` (here: the base dir that
        // contains the fake home) must never resolve.
        let base = tmpdir("gitdir-home-ancestor");
        let home = fake_home(&base);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        git_shape(&base);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", base.display()),
        )
        .unwrap();

        let e = resolve(&worktree, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::GitDirForbidden { .. }), "{e}");
    }

    #[test]
    fn gitdir_equal_to_home_is_refused() {
        let base = tmpdir("gitdir-home");
        let home = fake_home(&base);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        git_shape(&home);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", home.display()),
        )
        .unwrap();

        let e = resolve(&worktree, Some(&home)).unwrap_err();
        assert!(matches!(e, Error::GitDirForbidden { .. }), "{e}");
    }

    #[test]
    fn gitdir_containing_the_repo_is_refused() {
        // The worktree sits INSIDE the pointed-at directory: binding it
        // rw would expose a directory that contains the repo (and its
        // sidecar), not just git metadata.
        let base = tmpdir("gitdir-repo-ancestor");
        let outer = base.join("outer");
        let worktree = outer.join("wt");
        touch_dir(&worktree);
        git_shape(&outer);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", outer.display()),
        )
        .unwrap();

        let e = resolve(&worktree, Some(&fake_home(&base))).unwrap_err();
        assert!(matches!(e, Error::GitDirForbidden { .. }), "{e}");
    }

    #[test]
    fn gitdir_pointing_at_the_sidecar_is_refused() {
        // The sidecar is mysbx's own policy state: binding it rw would
        // let the payload approve whatever it likes on the next run.
        let base = tmpdir("gitdir-sidecar");
        let worktree = base.join("wt");
        touch_dir(&worktree);
        let sidecar = base.join("wt.mysbx");
        git_shape(&sidecar);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", sidecar.display()),
        )
        .unwrap();

        let e = resolve(&worktree, Some(&fake_home(&base))).unwrap_err();
        assert!(matches!(e, Error::GitDirForbidden { .. }), "{e}");
    }

    #[test]
    fn gitdir_inside_the_sidecar_is_refused() {
        let base = tmpdir("gitdir-in-sidecar");
        let worktree = base.join("wt");
        touch_dir(&worktree);
        let inside = base.join("wt.mysbx").join("state").join("git");
        git_shape(&inside);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", inside.display()),
        )
        .unwrap();

        let e = resolve(&worktree, Some(&fake_home(&base))).unwrap_err();
        assert!(matches!(e, Error::GitDirForbidden { .. }), "{e}");
    }

    #[test]
    fn gitdir_without_git_shape_is_ignored() {
        // A pointer at an ordinary directory (no HEAD, no refs/) is
        // dropped: repo resolution is advisory and the bind would be
        // refused by the approval list anyway, but the Repo must not
        // even carry it.
        let base = tmpdir("gitdir-plain-dir");
        let target = base.join("plain");
        touch_dir(&target);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", target.display()),
        )
        .unwrap();

        let r = resolve(&worktree, Some(&fake_home(&base))).unwrap();
        assert!(r.git_dirs.is_empty());
    }

    #[test]
    fn gitdir_inside_home_is_fine() {
        // The NORMAL layout: metadata under $HOME (a checkout in the
        // home directory). An at-or-below-home gitdir is the approved
        // case, not a violation.
        let base = tmpdir("gitdir-in-home");
        let home = fake_home(&base);
        let main = home.join("src").join("main");
        let gitdir = main.join(".git").join("worktrees").join("wt");
        git_shape(&gitdir);
        let worktree = base.join("wt");
        touch_dir(&worktree);
        std::fs::write(
            worktree.join(".git"),
            format!("gitdir: {}\n", gitdir.display()),
        )
        .unwrap();

        let r = resolve(&worktree, Some(&home)).unwrap();
        assert_eq!(r.git_dirs, vec![std::fs::canonicalize(&gitdir).unwrap()]);
    }

    #[test]
    fn relative_gitdir_resolves_against_the_worktree() {
        // Submodules use relative gitdir paths (`gitdir: ../.git/modules/x`)
        // and usually have NO commondir file — the gitdir IS the common
        // dir then, so exactly one entry must be recorded.
        let base = tmpdir("relative-gitdir");
        let superp = base.join("super");
        let gitdir = superp.join(".git").join("modules").join("sub");
        git_shape(&gitdir);
        let sub = base.join("super").join("sub");
        touch_dir(&sub);
        std::fs::write(sub.join(".git"), "gitdir: ../.git/modules/sub\n").unwrap();

        let r = resolve(&sub, Some(&fake_home(&base))).unwrap();
        assert_eq!(r.root, sub);
        assert_eq!(r.git_dirs, vec![std::fs::canonicalize(&gitdir).unwrap()]);
    }
}
