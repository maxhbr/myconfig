// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The named clone sessions of the workspace model
//! (docs/design/workspace.md, D1–D5).
//!
//! A **session** is a named clone run: `--session NAME` selects clone
//! mode (D1), the clone itself is a standalone git repository at
//! `<repo>.mysbx/clones/NAME` (D2), created by the FIRST `--session`
//! run from the host repo's current HEAD, and a run inside it binds
//! the clone rw at the repo's own path while nothing else of the host
//! repo is mounted (D3). This module owns the session's derived paths
//! and the clone-creation decision; the argv differences of a clone
//! run live in `bwrap.rs` ([`crate::bwrap::Workspace`]), the flag in
//! `lib.rs`, the per-session result file of D5 in
//! [`Session::result`].
//!
//! The creation decision is data where it can be: [`Session::plan`]
//! computes the exact `git` commands of a first run ([`CloneSteps`])
//! from the three facts the decision probes — does the clone exist
//! yet, does the host repo have commits, does it have a branch
//! literally named `agent` — so `--dry-run` prints the same commands a
//! real first run executes (cli.md D9: the argv block is a result,
//! and the clone-creation commands are part of what a dry run
//! audits).

use crate::repo::Repo;
use std::path::PathBuf;

/// The directory holding the sessions' clones, `<sidecar>/clones/`
/// (workspace.md D2): the filesystem layout IS the registry — a clone
/// directory exists ⇒ a session exists — like the sidecar itself.
pub const CLONES_DIR: &str = "clones";

/// The prefix of every session branch, `agent/mysbx/` (workspace.md
/// D2): reserved for sessions, so a host branch literally named
/// `agent` — which collides with the `refs/heads/agent/mysbx/*`
/// namespace as a ref-directory conflict — is refused up front.
pub const BRANCH_PREFIX: &str = "agent/mysbx/";

/// Validate a session NAME (workspace.md D2):
/// `[A-Za-z0-9][A-Za-z0-9._-]{0,63}` — no slashes, no leading dot, no
/// empty string. Anything else is a usage error at parse time
/// (cli.md D8), like every schema edge. Public so the parser's tests
/// assert against the same grammar the pipeline checks.
pub fn valid_name(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_alphanumeric() {
        return false;
    }
    name.len() <= 64 && chars.all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'))
}

/// One named clone session: the derived paths of D2/D5 and the branch
/// of D2. Plain data, built by [`Session::new`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Session {
    /// The validated NAME.
    pub name: String,
    /// The session's clone, `<sidecar>/clones/NAME` (D2): a
    /// standalone git repository, bound rw at the repo's own path in
    /// a clone run (D3).
    pub clone: PathBuf,
    /// The session branch, `agent/mysbx/NAME` (D2): the branch a
    /// session commits to, fetched/merged/pushed by the handoff verbs
    /// of D6.
    pub branch: String,
    /// The session's result file, `<sidecar>/clones/NAME.json` (D5):
    /// the per-session spelling of the D17 result contract, so
    /// parallel sessions cannot overwrite each other's outcomes.
    pub result: PathBuf,
}

impl Session {
    /// The session `name` belongs to, with all derived paths. The
    /// caller has validated the name ([`valid_name`]); an invalid one
    /// here would be a programming error, not operator input.
    pub fn new(repo: &Repo, name: &str) -> Self {
        Self {
            name: name.to_string(),
            clone: repo.sidecar.join(CLONES_DIR).join(name),
            branch: format!("{BRANCH_PREFIX}{name}"),
            result: repo.sidecar.join(CLONES_DIR).join(format!("{name}.json")),
        }
    }

    /// Whether this session's clone already exists: the registry of
    /// D2. A directory that exists without a `.git` is debris of an
    /// interrupted creation (`session destroy`, D7, is the verb that
    /// names it); for the run the distinction does not matter — only
    /// whether the creation is needed.
    pub fn clone_exists(&self) -> bool {
        self.clone.is_dir()
    }
}

/// The `git` commands of a first `--session` run (workspace.md D2),
/// in execution order: the vectors are the complete argv of each
/// invocation AFTER `git` — the same shape [`print_steps`] prints and
/// [`execute`] runs.
///
/// Both clone flags are required, per the gvisor precedent
/// (`myconfig.ai.gvisor-agent-sandbox/rust/src/session.rs`):
/// `--no-hardlinks` because a local clone hardlinks object FILES, and
/// a writable bind of the clone would then write through to the host
/// repo's objects; `--origin origin` pins the remote name, so a
/// user's `clone.defaultRemoteName` cannot move the exact-ref probes
/// of the handoff verbs (D6) out from under them.
///
/// The starting point is the host repo's CURRENT CHECKED-OUT HEAD
/// (D2): the committed state, never a dirty working tree — the
/// session is a commit-based handoff. The session branch continues at
/// `refs/remotes/origin/agent/mysbx/NAME` when an earlier session of
/// the same name left the branch in the host repo via `fetch` (D6) —
/// destroying and recreating a session must not silently fork its
/// own branch — and is new at HEAD otherwise.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CloneSteps {
    /// `git clone --origin origin --no-hardlinks <repo> <clone>` —
    /// always first.
    pub clone: Vec<String>,
    /// The branch checkout, second: `git -C <clone> checkout …` —
    /// continuing at the existing tip (`--no-track -b`, the
    /// remote-tracking ref spelled out) or creating the branch at
    /// HEAD.
    pub checkout: Vec<String>,
}

/// The outcome of the creation decision (workspace.md D2): nothing to
/// do (the clone exists — a session is a directory), the commands of
/// the creation, or the refusal naming the offending fact.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Plan {
    /// The clone exists already — the run proceeds into it. Nothing
    /// is created.
    Exists,
    /// The first run of this session: the commands of [`CloneSteps`],
    /// to execute (a real run) or print ([`print_steps`], `--dry-run`,
    /// cli.md D9 — a dry run creates nothing).
    Create(CloneSteps),
    /// The host repo has no commits — there is nothing to clone.
    EmptyHostRepo,
    /// The host repo has a branch literally named `agent`: it collides
    /// with the `refs/heads/agent/mysbx/*` namespace
    /// (ref-directory conflict), and refusing it up front beats
    /// failing inside a handoff verb later.
    AgentBranch,
}

impl Session {
    /// Decide what a run has to do about this session's clone
    /// (workspace.md D2): nothing (it exists), create it (the exact
    /// commands), or refuse. Pure apart from the three probes — the
    /// clone's existence, the host repo's HEAD and the `agent`
    /// branch — which are the facts the decision is about.
    pub fn plan(&self, repo: &Repo) -> Plan {
        if self.clone_exists() {
            return Plan::Exists;
        }
        // The starting point and the empty-repo refusal are the SAME
        // probe: a repo whose HEAD cannot be resolved has no commits,
        // and there is nothing to clone (D2).
        let Some(head) = host_head(repo) else {
            return Plan::EmptyHostRepo;
        };
        if branch_named_agent(repo) {
            return Plan::AgentBranch;
        }
        let clone = self.clone.to_string_lossy().into_owned();
        let checkout = if remote_session_ref_exists(repo, &self.branch) {
            // An earlier session of the same name left the branch in
            // the host repo via a `fetch` (D6): continue at that
            // existing tip. `--no-track`: the session owns the branch,
            // `origin/<branch>` is not its upstream (inside the
            // sandbox the origin path IS the clone bind, D3).
            vec![
                "-C".into(),
                clone.clone(),
                "checkout".into(),
                "--no-track".into(),
                "-b".into(),
                self.branch.clone(),
                format!("refs/remotes/origin/{}", self.branch),
            ]
        } else {
            vec![
                "-C".into(),
                clone.clone(),
                "checkout".into(),
                "-b".into(),
                self.branch.clone(),
                head,
            ]
        };
        Plan::Create(CloneSteps {
            clone: vec![
                "clone".into(),
                "--origin".into(),
                "origin".into(),
                "--no-hardlinks".into(),
                repo.root.to_string_lossy().into_owned(),
                clone.clone(),
            ],
            checkout,
        })
    }
}

/// Whether the host repo has a branch literally named `agent`
/// (workspace.md D2): it collides with the reserved
/// `refs/heads/agent/mysbx/*` namespace of the session branches, so
/// creating any session would fail inside a later handoff verb. The
/// exact-ref probe is immune to tags and remotes of the same spelling.
fn branch_named_agent(repo: &Repo) -> bool {
    git_ok(
        repo,
        &["show-ref", "--verify", "--quiet", "refs/heads/agent"],
    )
}

/// Whether `refs/remotes/origin/<branch>` exists in the host repo: an
/// earlier session of the same name left the branch there via a
/// `fetch` (workspace.md D6), so a recreated session continues at
/// that tip instead of forking (D2).
fn remote_session_ref_exists(repo: &Repo, branch: &str) -> bool {
    git_ok(
        repo,
        &[
            "show-ref",
            "--verify",
            "--quiet",
            &format!("refs/remotes/origin/{branch}"),
        ],
    )
}

/// Run one `git` probe in the host repo, `git -C <repo> …`, quiet and
/// null-stdio: `true` iff it exited `0`. A `git` that cannot run at
/// all counts as "no" everywhere it is probed — the caller's refusal
/// message names the fact the probe decided.
fn git_ok(repo: &Repo, args: &[&str]) -> bool {
    std::process::Command::new("git")
        .arg("-C")
        .arg(&repo.root)
        .args(args)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// The current checked-out HEAD commit of the host repo, or `None`
/// when it cannot be resolved (an empty repository — the clone
/// creation refuses that with its own message, D2). `rev-parse HEAD`
/// of the checked-out state is exactly the D2 starting point: the
/// committed state, never a dirty working tree.
pub fn host_head(repo: &Repo) -> Option<String> {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo.root)
        .args(["rev-parse", "HEAD"])
        .stderr(std::process::Stdio::null())
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let head = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if head.is_empty() {
        None
    } else {
        Some(head)
    }
}

/// The `--dry-run` spelling of the creation (cli.md D9): the commands
/// of a [`Plan::Create`], printed to stdout unprefixed, one argument
/// per line, the executable (`git`) first — the same format as the
/// bwrap argv block, so a dry run of a first `--session` run shows
/// everything that would happen, in execution order, and creates
/// nothing.
pub fn print_steps(steps: &CloneSteps) {
    for cmd in [&steps.clone, &steps.checkout] {
        println!("git");
        for arg in cmd {
            println!("{arg}");
        }
    }
}

/// Execute the creation commands of a [`Plan::Create`] and report
/// what happens like the creation lines of `init` (cli.md D9:
/// progress on stdout, `## `-prefixed — D2: the creation is
/// "reported before the run starts").
///
/// The clone directory's parent (`<sidecar>/clones/`) is created with
/// the same one-component-at-a-time discipline as the state tree
/// ([`crate::ensure_plain_dir`]): the sidecar is the one place beside
/// the repo this tool may write, but a symlink planted in it must not
/// redirect a clone out of the sidecar either. The clone itself is
/// left to `git clone` — it makes the directory and refuses an
/// existing one, which is exactly the first-run invariant.
pub fn execute(session: &Session, steps: &CloneSteps) -> Result<(), String> {
    let clones_dir = session
        .clone
        .parent()
        .ok_or_else(|| format!("the clone path {} has no parent", session.clone.display()))?
        .to_path_buf();
    crate::ensure_plain_dir(&clones_dir)?;
    println!(
        "## creating: {} (session {}, branch {})",
        session.clone.display(),
        session.name,
        session.branch
    );
    let run = |argv: &[String]| -> Result<(), String> {
        let mut cmd = std::process::Command::new("git");
        cmd.args(argv);
        let status = cmd.status().map_err(|e| format!("cannot run git: {e}"))?;
        if !status.success() {
            return Err(format!(
                "git {} failed — the session clone was not created",
                argv.first().map(String::as_str).unwrap_or("invocation")
            ));
        }
        Ok(())
    };
    run(&steps.clone)?;
    run(&steps.checkout)?;
    println!("## created: {}", session.clone.display());
    Ok(())
}

/// Whether a `git` usable by the tests is on PATH — the creation
/// tests drive the real one (the gvisor tier's precedent,
/// `branch_lifecycle.rs`); without it they skip, like the bwrap
/// execution tests do.
pub fn git_available() -> bool {
    std::process::Command::new("git")
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// A fresh temporary directory per test; hand-rolled, the crate
    /// has no dependencies.
    fn tmpdir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-session-test-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn synth_repo() -> Repo {
        Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        }
    }

    /// A real host repo with one commit on `main`, via the real git.
    /// Returns the repo and the HEAD sha.
    fn real_repo(base: &Path, name: &str) -> (Repo, String) {
        let root = base.join(name);
        std::fs::create_dir_all(&root).unwrap();
        assert!(git_available(), "git is required for this test");
        let git = |args: &[&str]| {
            let status = std::process::Command::new("git")
                .arg("-C")
                .arg(&root)
                .args(args)
                .env("GIT_CONFIG_GLOBAL", "/dev/null")
                .env("GIT_CONFIG_SYSTEM", "/dev/null")
                .env("GIT_AUTHOR_NAME", "mysbx-tests")
                .env("GIT_AUTHOR_EMAIL", "mysbx-tests@invalid")
                .env("GIT_COMMITTER_NAME", "mysbx-tests")
                .env("GIT_COMMITTER_EMAIL", "mysbx-tests@invalid")
                .status()
                .unwrap();
            assert!(status.success(), "git {} failed", args.join(" "));
        };
        git(&["init", "-b", "main"]);
        std::fs::write(root.join("file"), "content").unwrap();
        git(&["add", "file"]);
        git(&["commit", "-m", "initial"]);
        let head = host_head(&repo_of(&root)).expect("HEAD resolves");
        (repo_of(&root), head)
    }

    /// The `Repo` of a plain checkout: sidecar sibling, no git dirs.
    fn repo_of(root: &Path) -> Repo {
        Repo {
            root: root.to_path_buf(),
            sidecar: {
                let mut n = root.as_os_str().to_owned();
                n.push(".mysbx");
                PathBuf::from(n)
            },
            git_dirs: Vec::new(),
            worktrees: None,
        }
    }

    // ---- the NAME grammar (workspace.md D2) --------------------------------

    #[test]
    fn valid_names() {
        for name in ["a", "A", "0", "fix-1", "feature.x", "a_b", "a-b.c_d", "s3"] {
            assert!(valid_name(name), "`{name}` should be valid");
        }
    }

    #[test]
    fn invalid_names() {
        for name in ["", ".", "-x", "_x", "a/b", "a b", "..", "ä", "a:b", "a\\b"] {
            assert!(!valid_name(name), "`{name}` should be invalid");
        }
    }

    #[test]
    fn a_64_char_name_is_the_longest_valid_one() {
        assert!(valid_name(&"a".repeat(64)));
        assert!(!valid_name(&"a".repeat(65)));
    }

    #[test]
    fn the_derived_paths_follow_d2_and_d5() {
        let s = Session::new(&synth_repo(), "fix-1");
        assert_eq!(s.clone, PathBuf::from("/synth/repo.mysbx/clones/fix-1"));
        assert_eq!(s.branch, "agent/mysbx/fix-1");
        assert_eq!(
            s.result,
            PathBuf::from("/synth/repo.mysbx/clones/fix-1.json")
        );
    }

    // ---- the creation decision (workspace.md D2) --------------------------

    #[test]
    fn an_existing_clone_means_no_creation() {
        // The registry of D2: a clone directory exists ⇒ a session
        // exists ⇒ the run proceeds, nothing is created. No git
        // involved — the decision is the directory's existence.
        let base = tmpdir("plan-exists");
        let (repo, _) = real_repo(&base, "repo");
        let session = Session::new(&repo, "fix-1");
        std::fs::create_dir_all(&session.clone).unwrap();
        assert_eq!(session.plan(&repo), Plan::Exists);
    }

    #[test]
    fn a_first_run_clones_from_the_host_head() {
        let base = tmpdir("plan-create");
        let (repo, head) = real_repo(&base, "repo");
        let session = Session::new(&repo, "fix-1");
        match session.plan(&repo) {
            Plan::Create(steps) => {
                // D2, verbatim: `git clone --origin origin
                // --no-hardlinks <repo> <clone>` — both flags, the
                // host repo first, the derived clone path last.
                assert_eq!(
                    steps.clone,
                    vec![
                        "clone".to_string(),
                        "--origin".to_string(),
                        "origin".to_string(),
                        "--no-hardlinks".to_string(),
                        repo.root.to_string_lossy().into_owned(),
                        session.clone.to_string_lossy().into_owned(),
                    ]
                );
                // The new branch starts at the host's current HEAD.
                assert_eq!(
                    steps.checkout,
                    vec![
                        "-C".to_string(),
                        session.clone.to_string_lossy().into_owned(),
                        "checkout".to_string(),
                        "-b".to_string(),
                        "agent/mysbx/fix-1".to_string(),
                        head,
                    ]
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
    }

    #[test]
    fn an_empty_host_repo_is_refused() {
        let base = tmpdir("plan-empty");
        let root = base.join("repo");
        std::fs::create_dir_all(&root).unwrap();
        assert!(git_available(), "git is required for this test");
        let status = std::process::Command::new("git")
            .arg("-C")
            .arg(&root)
            .args(["init"])
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .status()
            .unwrap();
        assert!(status.success());
        let repo = repo_of(&root);
        let session = Session::new(&repo, "fix-1");
        assert_eq!(session.plan(&repo), Plan::EmptyHostRepo);
    }

    #[test]
    fn a_host_branch_named_agent_is_refused() {
        // The ref-directory conflict of D2: `refs/heads/agent`
        // collides with `refs/heads/agent/mysbx/*`.
        let base = tmpdir("plan-agent-branch");
        let (repo, _) = real_repo(&base, "repo");
        let status = std::process::Command::new("git")
            .arg("-C")
            .arg(&repo.root)
            .args(["branch", "agent"])
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .env("GIT_AUTHOR_NAME", "mysbx-tests")
            .env("GIT_AUTHOR_EMAIL", "mysbx-tests@invalid")
            .env("GIT_COMMITTER_NAME", "mysbx-tests")
            .env("GIT_COMMITTER_EMAIL", "mysbx-tests@invalid")
            .status()
            .unwrap();
        assert!(status.success());
        let session = Session::new(&repo, "fix-1");
        assert_eq!(session.plan(&repo), Plan::AgentBranch);
    }

    #[test]
    fn a_recreated_session_continues_at_the_existing_tip() {
        // D2: `refs/remotes/origin/agent/mysbx/NAME` in the host repo
        // (left by a `fetch`, D6) is the starting point — the
        // recreated session must not fork its own branch.
        let base = tmpdir("plan-continue");
        let (repo, _) = real_repo(&base, "repo");
        let status = std::process::Command::new("git")
            .arg("-C")
            .arg(&repo.root)
            .args([
                "update-ref",
                "refs/remotes/origin/agent/mysbx/fix-1",
                "HEAD",
            ])
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .status()
            .unwrap();
        assert!(status.success());
        let session = Session::new(&repo, "fix-1");
        match session.plan(&repo) {
            Plan::Create(steps) => {
                assert_eq!(
                    steps.checkout,
                    vec![
                        "-C".to_string(),
                        session.clone.to_string_lossy().into_owned(),
                        "checkout".to_string(),
                        "--no-track".to_string(),
                        "-b".to_string(),
                        "agent/mysbx/fix-1".to_string(),
                        "refs/remotes/origin/agent/mysbx/fix-1".to_string(),
                    ]
                );
            }
            other => panic!("expected Create, got {other:?}"),
        }
    }
}
