// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The host-side handoff verbs of the workspace model
//! (docs/design/workspace.md, D6): `fetch`, `merge`, `push`, `diff`.
//!
//! Four subcommands that do git plumbing between the host repo and a
//! session's clone — none of them starts a sandbox. Each takes the
//! session as its ONE positional argument; there is no `--session`
//! flag (the session is what the verb is about) and no `--repo` flag
//! (the host repo is the one the cwd resolves to, cli.md D1, like
//! every other verb).
//!
//! The mechanics adopt the gvisor spec verbatim
//! (`myconfig.ai.gvisor-agent-sandbox/rust/src/session.rs`,
//! `try_fetch_branch_from_worktree` & co):
//!
//! - **fetch** — the fast-forward-only refspec
//!   `git -C <repo> fetch --no-tags <clone>
//!   refs/heads/agent/mysbx/NAME:refs/heads/agent/mysbx/NAME`:
//!   both sides fully qualified (a short source ref is ambiguous
//!   against a same-named tag, and a branch literally named
//!   `+agent/…` would smuggle a force marker), no `+` force marker
//!   anywhere — Git creates an absent destination ref, advances one
//!   that is an ancestor of the session tip, and REJECTS a diverged
//!   or rewound one without touching it. The host repo learns
//!   nothing permanent about the clone: the fetch names the clone
//!   path once, no remote is configured.
//! - **merge** — refuse a detached HEAD and a dirty host tree
//!   first, `--no-ff` the default, then the fetch, then
//!   `git merge <args> refs/heads/agent/mysbx/NAME` — the EXACT
//!   fetched ref, never the bare name (git's DWIM order checks
//!   `refs/tags/` first, so a same-named tag could win). On success
//!   the fetched host-local ref is deleted — it was the ferry copy;
//!   on conflict it stays, with the recovery hint.
//! - **push** — the implicit `fetch` first (the pushed ref is
//!   current; a diverged host-local branch fails the push before
//!   anything is published), then
//!   `git -C <repo> push <remote> <refspec>` — explicit,
//!   non-forced, fully qualified. The default REMOTE is the host
//!   repo's own `origin`.
//! - **diff** — the implicit fetch (like `push`), then
//!   `git -C <repo> diff HEAD...refs/heads/agent/mysbx/NAME` — the
//!   THREE-DOT form: the changes on the session branch since it
//!   diverged from the host's HEAD, not the host's own drift.
//!
//! The argv vectors are pure functions ([`fetch_argv`],
//! [`merge_argv`], [`push_argv`], [`diff_argv`]) of the
//! [`Session`](crate::session::Session) — the same
//! data-where-it-can-be discipline as [`crate::session::Session::plan`]
//! — so `--dry-run` prints the exact commands a real invocation runs
//! (cli.md D9: the argv block is a result, one argument per line,
//! the executable first) and the tests assert against the same
//! builders the verbs use.

use crate::repo::Repo;
use crate::session::Session;

/// `git -C <repo> fetch --no-tags <clone> <refspec>` — the shared
/// step of `fetch` (all of it), `merge` (step 2) and the implicit
/// fetch of `push`/`diff` (D6). FAST-FORWARD-ONLY by construction:
/// the refspec has no leading `+`, so Git creates an absent
/// destination ref, advances one that is an ancestor of the session
/// tip, and rejects a diverged or rewound one WITHOUT touching it.
///
/// BOTH sides are FULLY QUALIFIED. A short source ref would be
/// ambiguous — a branch named `nested` could resolve to a same-named
/// tag — and worse, a branch literally named `+agent/…` would produce
/// `+agent/…:refs/heads/+agent/…`, where git parses the leading `+`
/// as a FORCE marker and fetches the wrong branch, bypassing the
/// fast-forward-only policy.
pub fn fetch_argv(repo: &Repo, session: &Session) -> Vec<String> {
    vec![
        "-C".into(),
        repo.root.to_string_lossy().into_owned(),
        "fetch".into(),
        "--no-tags".into(),
        session.clone.to_string_lossy().into_owned(),
        refspec(session),
    ]
}

/// `refs/heads/<branch>:refs/heads/<branch>` of the session — both
/// sides fully qualified, no `+` marker (see [`fetch_argv`]).
pub fn refspec(session: &Session) -> String {
    format!(
        "refs/heads/{}:refs/heads/{}",
        session.branch, session.branch
    )
}

/// Apply D6's merge-strategy default: `--no-ff` is prepended unless the
/// operator named a strategy themselves (`--ff`/`--squash`, or `--no-ff`
/// again). A merge commit by default keeps the session's work
/// traceable as one unit on the host branch.
pub fn apply_ff_default(mut merge_args: Vec<String>, ff_set: bool) -> Vec<String> {
    if !ff_set {
        merge_args.insert(0, "--no-ff".into());
    }
    merge_args
}

/// `git -C <repo> merge <merge-args…> refs/heads/<branch>` — the
/// EXACT fetched ref, never the bare name (git's DWIM order checks
/// `refs/tags/` first, so a same-named tag could "successfully" be
/// merged instead of the session branch). `merge_args` is the
/// operator's own choice, ff default already applied by
/// [`apply_ff_default`] — `--ff`/`--squash`/explicit git-merge args
/// passed through (D6).
pub fn merge_argv(repo: &Repo, session: &Session, merge_args: &[String]) -> Vec<String> {
    let mut argv = vec![
        "-C".to_string(),
        repo.root.to_string_lossy().into_owned(),
        "merge".to_string(),
    ];
    argv.extend(merge_args.iter().cloned());
    // The exact fetched ref — `refs/heads/<branch>`, not the bare
    // name, which a same-named tag could shadow.
    argv.push(format!("refs/heads/{}", session.branch));
    argv
}

/// `git -C <repo> branch -D <branch>` — the deletion of the ferry
/// copy after a successful merge (D6): the work is in the merge, and
/// a recreated session then correctly starts at HEAD again. `-D`,
/// not `-d`: the session branch is never merged INTO, so git's
/// "not fully merged" guard of `-d` would refuse exactly the
/// deletion the handoff just made possible.
pub fn delete_ref_argv(repo: &Repo, session: &Session) -> Vec<String> {
    vec![
        "-C".into(),
        repo.root.to_string_lossy().into_owned(),
        "branch".into(),
        "-D".into(),
        session.branch.clone(),
    ]
}

/// `git -C <repo> push <remote> <refspec>` — explicit, NON-FORCED,
/// both sides fully qualified: a bare `git push <remote> <branch>`
/// would parse a branch literally named `+topic` as a force marker
/// plus the branch `topic` and publish the wrong branch (the gvisor
/// spec, adopted verbatim). The remote is the host repo's own —
/// `origin` by default, an operator-named one explicitly — so
/// publishing goes through the host repo's configured remotes with
/// host-side credentials; the clone never talks to a network remote.
pub fn push_argv(repo: &Repo, session: &Session, remote: &str) -> Vec<String> {
    vec![
        "-C".into(),
        repo.root.to_string_lossy().into_owned(),
        "push".into(),
        remote.to_string(),
        refspec(session),
    ]
}

/// `git -C <repo> diff HEAD...refs/heads/<branch>` — the THREE-DOT
/// form (D6): the changes on the session branch since it diverged
/// from the host's HEAD, not the host's own drift. A two-dot diff
/// would report the host's unpushed commits as removals the session
/// "made".
pub fn diff_argv(repo: &Repo, session: &Session) -> Vec<String> {
    vec![
        "-C".into(),
        repo.root.to_string_lossy().into_owned(),
        "diff".into(),
        format!("HEAD...refs/heads/{}", session.branch),
    ]
}

/// Whether the host working tree is dirty — `git status --porcelain`
/// with any output counts. A merge (and only a merge, D6) refuses a
/// dirty tree first: the merge lands in the currently checked-out
/// branch, and a conflict must not leave a half-merged checkout the
/// operator cannot distinguish from their own uncommitted work.
///
/// The probe fails closed: a `git` that cannot run at all counts as
/// dirty — the merge must not run on a repo whose state cannot be
/// inspected.
pub fn host_tree_dirty(repo: &Repo) -> bool {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo.root)
        .args(["status", "--porcelain"])
        .stderr(std::process::Stdio::null())
        .output();
    match out {
        Ok(out) if out.status.success() => !out.stdout.is_empty(),
        _ => true,
    }
}

/// The currently checked-out branch of the host repo, or `None`
/// when HEAD is detached — `git symbolic-ref --short HEAD` fails
/// on a detached HEAD, which is exactly the refusal fact (D6: the
/// merge lands in the currently checked-out branch, and a detached
/// HEAD names none).
pub fn host_current_branch(repo: &Repo) -> Option<String> {
    let out = std::process::Command::new("git")
        .arg("-C")
        .arg(&repo.root)
        .args(["symbolic-ref", "--short", "HEAD"])
        .stderr(std::process::Stdio::null())
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let branch = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if branch.is_empty() {
        None
    } else {
        Some(branch)
    }
}

/// Run one `git` argv with inherited stdio — git's own diagnostics
/// and output (the diff's, for instance) pass through (D6).
///
/// Returns the process's exit code, or `None` when the command could
/// not be spawned at all.
pub fn run_git(argv: &[String]) -> Option<i32> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(argv);
    match cmd.status() {
        Ok(status) => status.code(),
        Err(_) => None,
    }
}

/// Which handoff verb the operator typed (workspace.md D6) — the
/// dispatcher's spelling of the four subcommands, deciding what
/// [`verb`] does around the shared argv builders.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Fetch,
    Merge,
    Push,
    Diff,
}

impl Kind {
    /// The verb's own usage line, printed by its usage errors (the
    /// same pairing rule usage.txt follows, cli.md D5).
    fn usage(self) -> String {
        match self {
            Kind::Fetch => "usage: mysbx fetch NAME".to_string(),
            Kind::Merge => {
                "usage: mysbx merge NAME [--no-ff|--ff|--squash] [-- GIT-MERGE-ARGS...]".to_string()
            }
            Kind::Push => "usage: mysbx push NAME [REMOTE]".to_string(),
            Kind::Diff => "usage: mysbx diff NAME".to_string(),
        }
    }

    /// The verb's name, for the `mysbx <verb>:` prefix of its usage
    /// errors — the position-typed prefix every subcommand parser
    /// uses (`run`, `gui`).
    fn name(self) -> &'static str {
        match self {
            Kind::Fetch => "fetch",
            Kind::Merge => "merge",
            Kind::Push => "push",
            Kind::Diff => "diff",
        }
    }
}

/// The arguments of [`Kind::Merge`]: the operator's git-merge
/// strategy (`--no-ff` the default) and any explicit git-merge
/// args, flags before `--`, everything after it verbatim (D6 — the
/// same `--` rule `run` gives its payload, cli.md D4).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct MergeArgs {
    /// `--no-ff`/`--ff`/`--squash` and any other flag/argument
    /// before `--`, in order, plus everything after `--` verbatim.
    args: Vec<String>,
    /// Whether one of `--no-ff`/`--ff`/`--squash` was named — the
    /// D6 default (`--no-ff`) is applied only when none was.
    ff_set: bool,
}

impl MergeArgs {
    /// Parse the `merge` argument tail after the NAME (D6). The
    /// strategy flags and any other token before `--` pass through in
    /// order; everything after `--` is git-merge's own, verbatim. A
    /// repeated strategy flag is a typo, not an intensifier — usage
    /// error, the same rule every mysbx flag follows (cli.md D5).
    fn parse(kind: Kind, args: &[String]) -> Result<Self, i32> {
        let mut out = Self::default();
        let mut i = 0;
        while i < args.len() {
            match args[i].as_str() {
                "--no-ff" | "--ff" | "--squash" => {
                    if out.ff_set {
                        eprintln!("mysbx merge: repeated merge-strategy flag: {}", args[i]);
                        eprintln!("{}", kind.usage());
                        return Err(2);
                    }
                    out.ff_set = true;
                    out.args.push(args[i].clone());
                    i += 1;
                }
                "--" => {
                    out.args.extend(args[i + 1..].iter().cloned());
                    break;
                }
                other => {
                    out.args.push(other.to_string());
                    i += 1;
                }
            }
        }
        Ok(out)
    }
}

/// The arguments of [`Kind::Push`]: the optional REMOTE (D6: the
/// host repo's own `origin` by default, an operator-named one
/// explicitly — publishing goes through the host repo's configured
/// remotes with host-side credentials).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct PushArgs {
    remote: Option<String>,
}

impl PushArgs {
    /// Parse the `push` argument tail after the NAME: at most one
    /// positional REMOTE, no flags and no `--` (D6 — the verb has no
    /// payload for one to separate). A second positional and any
    /// flag are usage errors naming the accepted shape.
    fn parse(kind: Kind, args: &[String]) -> Result<Self, i32> {
        let mut out = Self::default();
        for arg in args {
            if arg.starts_with('-') {
                eprintln!("mysbx push: unknown push option: {arg}");
                eprintln!("{}", kind.usage());
                return Err(2);
            }
            if out.remote.is_some() {
                eprintln!("mysbx push: push accepts at most one remote: {arg}");
                eprintln!("{}", kind.usage());
                return Err(2);
            }
            out.remote = Some(arg.clone());
        }
        Ok(out)
    }
}

/// One handoff verb, end to end (workspace.md D6): parse the
/// argument tail, resolve the host repo (the cwd's, cli.md D1 — the
/// D9 inside-clone refusal of `repo::resolve_cwd` fires here too),
/// require the session's clone to exist (the registry of D2: a
/// clone directory exists ⇒ a session exists), then run the verb's
/// git steps — or print them, with `--dry-run` (cli.md D9: the
/// commands are the result, one argument per line, the executable
/// first, and nothing runs).
///
/// Exit codes (cli.md D8): `2` for a wrong command line (bad NAME
/// grammar, wrong verb arguments), `70` for the operational
/// refusals — an unknown session, a detached HEAD, a dirty host
/// tree, a diverged or rewound host-local branch, a merge conflict —
/// the command line was fine but the world it named could not do
/// what it was asked to. The one exception is the FINAL git of `push`
/// and `diff`: its own exit code propagates (a remote refusing a
/// push, a `git diff` reporting differences with `1` — results the
/// operator asked for, not infrastructure failures), with git's own
/// diagnostics passing through untouched (D6).
pub fn verb(args: &[String], kind: Kind, dry_run: bool) -> i32 {
    // 1. the session NAME — the one positional of all four verbs
    // (D6). No `--session` flag: the session is what the verb is
    // about, and the dispatcher has already refused the flag for
    // these verbs. The grammar is enforced at parse time like every
    // schema edge (workspace.md D2, cli.md D8).
    let name = match args.first() {
        Some(n) if crate::session::valid_name(n) => n.clone(),
        Some(n) => {
            eprintln!("mysbx {}: invalid session name `{n}`", kind.name());
            eprintln!(
                "  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot"
            );
            eprintln!("try `mysbx --help`");
            return 2;
        }
        None => {
            eprintln!("mysbx {}: a session name is required", kind.name());
            eprintln!("try `mysbx --help`");
            return 2;
        }
    };
    // 2. the verb's own argument tail.
    let merge_args = if let Kind::Merge = kind {
        match MergeArgs::parse(kind, &args[1..]) {
            Ok(a) => Some(a),
            Err(code) => return code,
        }
    } else {
        None
    };
    let push_args = if let Kind::Push = kind {
        match PushArgs::parse(kind, &args[1..]) {
            Ok(a) => Some(a),
            Err(code) => return code,
        }
    } else {
        None
    };
    if let Kind::Fetch | Kind::Diff = kind {
        if let Some(arg) = args.get(1) {
            eprintln!("mysbx {}: unexpected argument: {arg}", kind.name());
            eprintln!("{}", kind.usage());
            return 2;
        }
    }

    // 3. the host repo (cli.md D1: the one the cwd resolves to,
    // like every other verb — there is no `--repo` flag). The D9
    // inside-clone refusal of the resolver fires here too: the
    // handoff verbs belong to the HOST side of a session, and a
    // start inside the clone is the error naming the owning repo.
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let session = crate::session::Session::new(&repo, &name);

    // 4. the session must exist — `clones/` is the registry (D2): a
    // clone directory exists ⇒ a session exists. A missing clone is
    // not a usage error (the NAME was fine) but the unknown-session
    // refusal, naming the session and how to start one.
    if !session.clone_exists() {
        eprintln!(
            "mysbx: unknown session: {name} — the clone {} does not exist",
            session.clone.display()
        );
        eprintln!("  start one with `mysbx run --session {name} -- CMD`");
        return crate::EXIT_INFRASTRUCTURE;
    }

    // 5. the verb's git steps, in D6's order — collected first so
    // `--dry-run` prints the complete plan before anything runs.
    //
    // The fetch: the shared step of `fetch` (all of it), `merge`
    // (after its refusals) and the implicit fetch of `push`/`diff`.
    let fetch = fetch_argv(&repo, &session);

    // The merge's own refusals run BEFORE any git does (D6: the
    // merge lands in the currently checked-out branch, and a
    // conflict must not leave a half-merged checkout the operator
    // cannot attribute). A detached HEAD names no branch to land
    // in; a dirty tree would mix the session's merge with
    // uncommitted operator work.
    let current_branch = if let Kind::Merge = kind {
        match host_current_branch(&repo) {
            Some(b) => Some(b),
            None => {
                eprintln!(
                    "mysbx: cannot merge into {} — it is in detached HEAD state; switch to the branch you want to merge into first (workspace.md D6)",
                    repo.root.display()
                );
                return crate::EXIT_INFRASTRUCTURE;
            }
        }
    } else {
        None
    };
    if let Kind::Merge = kind {
        if host_tree_dirty(&repo) {
            eprintln!(
                "mysbx: the working tree of {} is dirty — commit or stash before merging a session (workspace.md D6)",
                repo.root.display()
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
    }

    // 6. `--dry-run` (cli.md D9): the commands are the result —
    // printed unprefixed to stdout, one argument per line, the
    // executable (`git`) first, in execution order — and nothing
    // runs, nothing is refused that a real run would not also
    // refuse (the refusals above already ran).
    if dry_run {
        print_commands(&[&fetch]);
        match kind {
            Kind::Fetch => {}
            Kind::Merge => {
                let merge_args = merge_args.expect("parsed for Merge");
                print_commands(&[&merge_argv(
                    &repo,
                    &session,
                    &apply_ff_default(merge_args.args, merge_args.ff_set),
                )]);
                print_commands(&[&delete_ref_argv(&repo, &session)]);
            }
            Kind::Push => {
                let push_args = push_args.expect("parsed for Push");
                print_commands(&[&push_argv(
                    &repo,
                    &session,
                    push_args.remote.as_deref().unwrap_or("origin"),
                )]);
            }
            Kind::Diff => {
                print_commands(&[&diff_argv(&repo, &session)]);
            }
        }
        return 0;
    }

    // 7. the real run. Progress lines go to stderr like the
    // diagnostics (cli.md D9: stdout belongs to results — the
    // session verbs' results are git's own, e.g. the diff's).
    eprintln!(
        "mysbx: fetching branch {} from the session clone into {}",
        session.branch,
        repo.root.display()
    );
    match run_git(&fetch) {
        Some(0) => {}
        _ => {
            eprintln!(
                "mysbx: fetch from the session clone failed; the clone may be missing or broken, or the host branch {} may have diverged from the session branch (workspace.md D6)",
                session.branch
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
    }
    match kind {
        Kind::Fetch => {
            eprintln!(
                "mysbx: fetched {} into {}; merge it with `mysbx merge {name}`",
                session.branch,
                repo.root.display()
            );
            0
        }
        Kind::Merge => {
            let merge_args = merge_args.expect("parsed for Merge");
            let current_branch = current_branch.expect("probed for Merge");
            eprintln!(
                "mysbx: merging {} into {current_branch} of {}",
                session.branch,
                repo.root.display()
            );
            let merge = merge_argv(
                &repo,
                &session,
                &apply_ff_default(merge_args.args, merge_args.ff_set),
            );
            match run_git(&merge) {
                Some(0) => {
                    // The ferry copy is deleted on success (D6): the
                    // work is in the merge, and a recreated session
                    // then correctly starts at HEAD again. A failure
                    // to delete is reported but does not fail the
                    // merge — the work IS in.
                    let delete = delete_ref_argv(&repo, &session);
                    if run_git(&delete) != Some(0) {
                        eprintln!(
                            "mysbx: could not delete the fetched ref {} — remove it with `git -C {} branch -D {}`",
                            session.branch,
                            repo.root.display(),
                            session.branch
                        );
                    }
                    0
                }
                _ => {
                    // On conflict the fetched ref STAYS, with the
                    // recovery hint (D6): resolve, then delete the
                    // leftover ref by hand — destructive replacement
                    // is never automatic.
                    eprintln!(
                        "mysbx: merge failed — resolve the conflicts, then delete the leftover ref with `git -C {} branch -D {}` (workspace.md D6)",
                        repo.root.display(),
                        session.branch
                    );
                    crate::EXIT_INFRASTRUCTURE
                }
            }
        }
        Kind::Push => {
            let push_args = push_args.expect("parsed for Push");
            let remote = push_args.remote.unwrap_or_else(|| "origin".to_string());
            eprintln!(
                "mysbx: pushing {} to {remote} of {}",
                session.branch,
                repo.root.display()
            );
            // The final git's own exit code propagates (D6, the
            // gvisor set -e semantics): a remote refusing the push
            // is git's answer, with git's own diagnostics already on
            // stderr. `None` (git could not run) is the one case
            // that is mysbx's own failure.
            match run_git(&push_argv(&repo, &session, &remote)) {
                Some(code) => code,
                None => {
                    eprintln!("mysbx: cannot run git");
                    crate::EXIT_INFRASTRUCTURE
                }
            }
        }
        Kind::Diff => {
            // The three-dot diff's exit code propagates like
            // `push`'s: its output on stdout IS the answer the
            // operator asked for, so its own code is the result's.
            match run_git(&diff_argv(&repo, &session)) {
                Some(code) => code,
                None => {
                    eprintln!("mysbx: cannot run git");
                    crate::EXIT_INFRASTRUCTURE
                }
            }
        }
    }
}

/// The `--dry-run` spelling of a handoff verb (cli.md D9): each git
/// argv, printed to stdout unprefixed, one argument per line, the
/// executable (`git`) first — the same format as the bwrap argv
/// block and the clone-creation commands of
/// [`crate::session::print_steps`], so a dry run audits exactly the
/// commands a real run executes, in execution order, and runs
/// nothing.
fn print_commands(commands: &[&[String]]) {
    for argv in commands {
        println!("git");
        for arg in argv.iter() {
            println!("{arg}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    fn synth_repo() -> Repo {
        Repo {
            root: PathBuf::from("/synth/repo"),
            sidecar: PathBuf::from("/synth/repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        }
    }

    fn synth_session() -> Session {
        Session::new(&synth_repo(), "fix-1")
    }

    // ---- the exact argv of D6, verbatim ---------------------------------

    #[test]
    fn fetch_is_ff_only_fully_qualified_and_tagless() {
        // D6, byte for byte:
        // `git -C <repo> fetch --no-tags <clone>
        // refs/heads/agent/mysbx/NAME:refs/heads/agent/mysbx/NAME`.
        // Both sides fully qualified, no `+` force marker anywhere.
        assert_eq!(
            fetch_argv(&synth_repo(), &synth_session()),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "fetch".to_string(),
                "--no-tags".to_string(),
                "/synth/repo.mysbx/clones/fix-1".to_string(),
                "refs/heads/agent/mysbx/fix-1:refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn the_refspec_has_no_force_marker() {
        // The smuggled-force edge the full qualification exists for:
        // a session name cannot contain `+` (the grammar), and the
        // refspec spells `refs/heads/` on BOTH sides, so no branch
        // spelling can make git parse a leading `+`.
        let spec = refspec(&synth_session());
        assert!(!spec.starts_with('+'), "{spec}");
        assert!(spec.starts_with("refs/heads/"), "{spec}");
        assert!(spec.ends_with(":refs/heads/agent/mysbx/fix-1"), "{spec}");
    }

    #[test]
    fn merge_defaults_to_no_ff_and_uses_the_exact_ref() {
        // D6: `--no-ff` is the default, and the merge consumes the
        // EXACT fetched ref — `refs/heads/<branch>`, never the bare
        // name a same-named tag could shadow.
        assert_eq!(
            merge_argv(
                &synth_repo(),
                &synth_session(),
                &apply_ff_default(Vec::new(), false)
            ),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "merge".to_string(),
                "--no-ff".to_string(),
                "refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
        // The operator's own choice passes through, before the ref.
        assert_eq!(
            merge_argv(
                &synth_repo(),
                &synth_session(),
                &apply_ff_default(vec!["--squash".to_string()], true)
            ),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "merge".to_string(),
                "--squash".to_string(),
                "refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
        // Explicit git-merge args after `--` ride along verbatim.
        assert_eq!(
            merge_argv(
                &synth_repo(),
                &synth_session(),
                &apply_ff_default(
                    vec![
                        "--ff".to_string(),
                        "-m".to_string(),
                        "session work".to_string()
                    ],
                    true
                )
            ),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "merge".to_string(),
                "--ff".to_string(),
                "-m".to_string(),
                "session work".to_string(),
                "refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn push_is_explicit_non_forced_and_fully_qualified() {
        // D6: `git -C <repo> push <remote> <refspec>` — the default
        // remote is the host repo's own `origin`, an explicit one
        // passes through.
        assert_eq!(
            push_argv(&synth_repo(), &synth_session(), "origin"),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "push".to_string(),
                "origin".to_string(),
                "refs/heads/agent/mysbx/fix-1:refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
        assert_eq!(
            push_argv(&synth_repo(), &synth_session(), "upstream"),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "push".to_string(),
                "upstream".to_string(),
                "refs/heads/agent/mysbx/fix-1:refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn diff_is_the_three_dot_form() {
        // D6: the THREE-DOT diff — the changes on the session branch
        // since it diverged from the host's HEAD, not the host's own
        // drift.
        assert_eq!(
            diff_argv(&synth_repo(), &synth_session()),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "diff".to_string(),
                "HEAD...refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn the_ferry_ref_deletion_targets_the_branch() {
        // On a successful merge the fetched host-local ref is
        // deleted — the ferry copy. `-D` because the session branch
        // is never merged into, so `-d` would refuse.
        assert_eq!(
            delete_ref_argv(&synth_repo(), &synth_session()),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "branch".to_string(),
                "-D".to_string(),
                "agent/mysbx/fix-1".to_string(),
            ]
        );
    }
}
