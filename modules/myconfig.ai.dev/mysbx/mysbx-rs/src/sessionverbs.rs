// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The session noun group of the workspace model
//! (docs/design/workspace.md, D7): `session list` /
//! `session destroy` / `session hunk` — the ONE closed
//! nested-verb exception to cli.md D3's "single verbs, no nested
//! command trees" rule. Three verbs, not an open tree: a fourth is a
//! decision, not a given.
//!
//! Neither verb starts a sandbox. All operate on the repo the cwd
//! resolves to (cli.md D1 — there is no `--repo` flag, like every
//! other verb), and the D9 inside-clone refusal of
//! [`crate::repo::resolve_cwd`] fires for them too: the session verbs
//! belong to the HOST side of a session.
//!
//! - **`session list`** — `clones/` is the registry (D2): one line
//!   per entry with the name, the session branch, and the
//!   ahead-count — the commits in the session branch that the host
//!   repo does not have. An entry without `.git` is debris of an
//!   interrupted creation and is marked as such, per the gvisor
//!   incomplete-inventory precedent.
//! - **`session destroy NAME [--force]`** — guarded removal, per the
//!   microvm precedent (intrinsic properties, not location trust):
//!   the resolved path must be strictly inside
//!   `<repo>.mysbx/clones/`, its basename must equal `NAME`, and it
//!   must contain `.git` to be a live session (an entry without
//!   `.git` is debris — nothing to lose, so it is removed with the
//!   debris named); never the sidecar root, never `/`. It REFUSES
//!   while the session branch holds commits the host repo does not
//!   have (unmerged work, checked without mutating anything) unless
//!   `--force` is given. The clone is plain `rm -rf`'d — it is
//!   standalone, no worktree bookkeeping, the host repo is
//!   untouched. A host-local `agent/mysbx/NAME` branch left by a
//!   `fetch` (D6) is NOT deleted: it is the operator's imported copy,
//!   and deleting it silently would contradict the unmerged-work
//!   guard this verb exists to enforce.
//! - **`session hunk NAME`** — the diff of `mysbx diff NAME` (D6's
//!   implicit fetch, then the THREE-DOT range
//!   `HEAD...refs/heads/agent/mysbx/NAME`) in the interactive `hunk`
//!   viewer, exec'd like `$EDITOR` is for `mysbx edit` (cli.md D12,
//!   the `worktree hunk` precedent of docs/design/worktree.md W4):
//!   the tool replaces this process, owns the terminal, and its
//!   exit code propagates unchanged (cli.md D8).
//!
//! "The host repo does not have a commit" is answered by REACHABILITY
//! from the host's own refs — a commit the host fetched into a
//! ferry branch, or merged into its history, IS had, however the
//! operator filed it; a commit that exists only in the clone's object
//! store is not. [`host_contains`] is the single probe both verbs
//! use, so the ahead-count of `list` and the refusal of `destroy`
//! can never disagree about the same session.
//!
//! The argv vectors the `--dry-run` contract prints are pure
//! functions of the [`Session`](crate::session::Session)
//! ([`count_argv`], [`contains_argv`], [`remove_argv`],
//! [`hunk_argv`]) — the same
//! data-where-it-can-be discipline as
//! [`crate::session::Session::plan`] and the handoff builders of
//! [`crate::handoff`] — so the tests assert against the same builders
//! the verbs use, and a dry run audits exactly what a real
//! invocation would do (cli.md D9), running nothing.

use crate::repo::Repo;
use crate::session::Session;
use std::path::{Path, PathBuf};

/// The session group's own usage lines, printed by its usage errors
/// (the same pairing rule usage.txt follows, cli.md D5).
const USAGE_LIST: &str = "usage: mysbx session list";
const USAGE_DESTROY: &str = "usage: mysbx session destroy NAME [--force]";
const USAGE_HUNK: &str = "usage: mysbx session hunk NAME";

/// The column widths of `session list`'s table — the gvisor
/// precedent's shape (`agent-gvisor list`), narrowed to the three
/// columns D7 names. A name longer than [`NAME_WIDTH`] overflows the
/// column rather than truncating: a session name is data, and
/// truncating it would make two rows look like one session.
const NAME_WIDTH: usize = 24;
const BRANCH_WIDTH: usize = 28;

/// One row of `session list` — the facts of a `clones/` entry, with
/// the parts the probes could not answer as [`None`] (rendered `-`).
/// Plain data, so the list tests assert the rows before the printing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    /// The entry's name — a `clones/` directory name.
    pub name: String,
    /// The session branch, `agent/mysbx/NAME` (D2) — derived, always
    /// known.
    pub branch: String,
    /// The ahead-count: commits in the session branch that the host
    /// repo does not have. `None` when no honest number could be
    /// computed (a broken clone, a probe failure) — the column
    /// prints `-` instead of a guess.
    pub ahead: Option<u32>,
    /// The entry has no `.git`: debris of an interrupted creation
    /// (D2/D7). Such an entry has no session branch and no work; its
    /// row is the debris marker instead of a count.
    pub debris: bool,
}

/// The `session list` rows of `repo`: one per DIRECTORY in
/// `clones/` (a session is a directory, the registry of D2 — the
/// per-session `NAME.json` result files of D5 are files, not
/// entries), sorted by name, so the listing is deterministic.
///
/// Pure apart from the read_dir and the git probes — the facts the
/// listing is about. A `clones/` that does not exist is the empty
/// registry: no rows, not an error (a repo without sessions lists
/// nothing).
pub fn entries(repo: &Repo) -> Vec<Entry> {
    let clones = repo.sidecar.join(crate::session::CLONES_DIR);
    let mut names: Vec<String> = match std::fs::read_dir(&clones) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            // Only directories are registry entries; the per-session
            // result files (D5) and anything else that is not a
            // session live in the same directory and are skipped.
            .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .collect(),
        Err(_) => Vec::new(),
    };
    names.sort();
    names
        .into_iter()
        .map(|name| {
            let session = Session::new(repo, &name);
            // A session branch exists per NAME by derivation (D2),
            // whether or not this particular clone is healthy.
            let branch = session.branch.clone();
            let debris = !session.clone.join(".git").exists();
            let ahead = if debris {
                // Debris has no session branch and no work — nothing
                // to count. The row's ahead column IS the debris
                // marker.
                None
            } else {
                ahead_count(repo, &session)
            };
            Entry {
                name,
                branch,
                ahead,
                debris,
            }
        })
        .collect()
}

/// The ahead-count of a session (D7): the commits in the session
/// branch that the host repo does not have, as an honest number or
/// [`None`] when no honest number can be computed.
///
/// The case split (see the probes below):
///
/// - The host repo HAS the session tip — reachable from one of its
///   refs (a merge into its history, a ferry branch of a `fetch`,
///   D6): it has every commit of the branch, so the count is `0`.
/// - Otherwise the host lacks at least the tip, and the count is
///   taken in the CLONE, whose object store is the only one holding
///   the session's commits: every commit of the branch that is
///   reachable from neither the host's refs AS OF THE CLONE'S
///   CREATION (the clone's `origin` remote-tracking refs — the host
///   repo, snapshotted) nor the host's CURRENT session branch (the
///   prefix an earlier `fetch` imported). Anything else in the
///   branch is, in its entirety, work that exists only in the clone.
/// - A probe that cannot run or resolve (a broken clone, a rewound
///   host branch the clone cannot relate to) is [`None`] — the
///   listing prints `-` rather than a guess, and `destroy`'s guard
///   treats it as unmerged (fail-safe).
pub fn ahead_count(repo: &Repo, session: &Session) -> Option<u32> {
    let tip = tip_of(session)?;
    if host_contains(repo, &tip) {
        return Some(0);
    }
    // The host's current session branch, when one exists (a fetch
    // left it, D6): the prefix of the session's work the host
    // already has. Its tip lives in the clone's object store — the
    // commits came from there.
    let ferry = git_output(
        &[
            "-C",
            &repo.root.to_string_lossy(),
            "rev-parse",
            "--verify",
            "--quiet",
            &format!("refs/heads/{}", session.branch),
        ],
        false,
    );
    let tip = tip_of(session)?;
    let argv = count_argv(session, &tip, ferry.as_deref());
    let out = git_output(
        &argv.iter().map(String::as_str).collect::<Vec<&str>>(),
        true,
    )?;
    out.trim().parse().ok()
}

/// The session branch's tip, from the clone's own object store —
/// `git -C <clone> rev-parse --verify --quiet refs/heads/<branch>`
/// (exact ref, immune to tag DWIM). `None` when the clone cannot
/// answer (a broken or hand-mangled clone, a checkout that never
/// finished creating the branch).
fn tip_of(session: &Session) -> Option<String> {
    // The trailing newline of `rev-parse`'s output is TRIMMED here,
    // once: every later argv embeds the tip — the probes of
    // [`host_contains`], the count of [`count_argv`] — and a sha
    // with a newline glued on is not a sha to any of them (the
    // probes would fail closed, and a healthy session would list
    // `-` and refuse to be destroyed).
    git_output(
        &[
            "-C",
            &session.clone.to_string_lossy(),
            "rev-parse",
            "--verify",
            "--quiet",
            &format!("refs/heads/{}", session.branch),
        ],
        true,
    )
    .map(|out| out.trim().to_string())
    .filter(|out| !out.is_empty())
}

/// Whether the HOST REPO has the commit `sha` — reachable from one of
/// its refs or its HEAD (the gvisor set -e semantics adapted: a
/// fetch, a merge, a tag all count, however the operator filed the
/// work; a dangling object counts for nothing, and a `sha` the host
/// does not know is simply not had).
///
/// The probe: `git -C <repo> for-each-ref --contains <sha> --format
/// %(refname)` — a non-empty answer names the refs holding the
/// commit — plus the `merge-base --is-ancestor` form for a DETACHED
/// HEAD, which `for-each-ref` (refs only) cannot see. Both fail
/// closed: a `git` that cannot run, or a `sha` the host cannot
/// resolve, means "not had".
fn host_contains(repo: &Repo, sha: &str) -> bool {
    let refs = git_output(
        &[
            "-C",
            &repo.root.to_string_lossy(),
            "for-each-ref",
            "--contains",
            sha,
            "--format=%(refname)",
        ],
        true,
    );
    match refs {
        Some(out) if !out.trim().is_empty() => true,
        // A detached HEAD is no ref; its history still counts as the
        // host repo's.
        _ => git_status(
            &[
                "-C",
                &repo.root.to_string_lossy(),
                "merge-base",
                "--is-ancestor",
                sha,
                "HEAD",
            ],
            true,
        ),
    }
}

/// The argv of the ahead-count probe in the clone (after
/// [`host_contains`] said no): `git -C <clone> rev-list --count
/// <tip> --not --remotes=origin [<ferry-tip>]` — the session's
/// commits that are reachable from neither the host-as-of-creation
/// (the clone's `origin` remote-tracking refs) nor the current ferry
/// prefix of an earlier `fetch` (D6).
pub fn count_argv(session: &Session, tip: &str, ferry_tip: Option<&str>) -> Vec<String> {
    let mut argv = vec![
        "-C".to_string(),
        session.clone.to_string_lossy().into_owned(),
        "rev-list".to_string(),
        "--count".to_string(),
        tip.to_string(),
        "--not".to_string(),
        "--remotes=origin".to_string(),
    ];
    if let Some(ferry) = ferry_tip {
        argv.push(ferry.to_string());
    }
    argv
}

/// The argv of [`host_contains`]'s ref probe: `git -C <repo>
/// for-each-ref --contains <sha> --format=%(refname)`.
pub fn contains_argv(repo: &Repo, sha: &str) -> Vec<String> {
    vec![
        "-C".to_string(),
        repo.root.to_string_lossy().into_owned(),
        "for-each-ref".to_string(),
        "--contains".to_string(),
        sha.to_string(),
        "--format=%(refname)".to_string(),
    ]
}

/// The removal a real `session destroy NAME` performs: `rm -rf
/// <clone>` — the standalone clone directory, the whole of the
/// session (its `.git` included, so its branch dies with it); plus
/// the session's result file `clones/NAME.json` (D5), the one other
/// thing in the sidecar that belongs to THIS session. The host repo
/// is untouched, and a host-local session branch is deliberately NOT
/// part of the argv — see [`destroy`].
///
/// `rm`, not `git`: there is no git command for this, and the
/// removal is plain filesystem work (D7: "The clone is plain
/// `rm -rf`'d — it is standalone, no worktree bookkeeping").
pub fn remove_argv(session: &Session) -> Vec<Vec<String>> {
    vec![
        vec![
            "-rf".to_string(),
            session.clone.to_string_lossy().into_owned(),
        ],
        vec![
            "-f".to_string(),
            session.result.to_string_lossy().into_owned(),
        ],
    ]
}

/// Run one `git` probe and return its stdout, or `None` when it
/// failed (nonzero or unrunnable). `null_stderr` keeps a `--verify
/// --quiet` probe quiet, like the session module's own helpers.
fn git_output(args: &[&str], quiet_stderr: bool) -> Option<String> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(args);
    if quiet_stderr {
        cmd.stderr(std::process::Stdio::null());
    }
    let out = cmd.output().ok()?;
    if out.status.success() {
        Some(String::from_utf8_lossy(&out.stdout).into_owned())
    } else {
        None
    }
}

/// Run one `git` probe for its exit status alone (the
/// `merge-base --is-ancestor` form returns only a status).
fn git_status(args: &[&str], quiet_stderr: bool) -> bool {
    let mut cmd = std::process::Command::new("git");
    cmd.args(args);
    if quiet_stderr {
        cmd.stderr(std::process::Stdio::null());
    }
    cmd.status().map(|s| s.success()).unwrap_or(false)
}

/// `mysbx session list` (D7): print one line per `clones/` entry —
/// name, session branch, ahead-count — debris marked, sorted, exit
/// `0` (a listing, not a verdict: an empty registry lists nothing).
///
/// Exit codes (cli.md D8): `2` for a wrong command line, `70` when
/// the repo the cwd resolves to could not be resolved (the D9
/// inside-clone refusal fires there).
pub fn list(args: &[String], dry_run: bool) -> i32 {
    if let Some(arg) = args.first() {
        eprintln!("mysbx session list: unexpected argument: {arg}");
        eprintln!("{USAGE_LIST}");
        return 2;
    }
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    if dry_run {
        // cli.md D9: the exact git commands the listing would run,
        // one argument per line, the executable first — and nothing
        // runs, nothing is listed. The probes are reads, so this is
        // the honest dry run of a read: it shows the work, not the
        // result.
        print_commands(&dry_run_argv(&repo, &entries_probe_names(&repo)));
        return 0;
    }
    for line in list_lines(&repo) {
        println!("{line}");
    }
    0
}

/// The lines `session list` prints — the SAME lines this verb's
/// stdout is made of, extracted so `mysbx status` (cli.md D19) can
/// embed the listing without a second format that could drift from
/// the list verb's contract (the verb's stdout IS the contract).
/// `lines[0]` is the header, one row per line after it — an empty
/// registry is the header alone.
pub fn list_lines(repo: &Repo) -> Vec<String> {
    let mut lines = vec![format!(
        "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {}",
        "SESSION", "BRANCH", "AHEAD"
    )];
    for e in &entries(repo) {
        if e.debris {
            lines.push(format!(
                "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {}",
                e.name, "-", "debris (interrupted creation)"
            ));
        } else {
            let ahead = match e.ahead {
                Some(n) => n.to_string(),
                None => "-".to_string(),
            };
            lines.push(format!(
                "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {ahead}",
                e.name, e.branch
            ));
        }
    }
    lines
}

/// The session names [`list`] would probe — the dry run's knowledge
/// of what it would ask.
fn entries_probe_names(repo: &Repo) -> Vec<String> {
    entries(repo).into_iter().map(|e| e.name).collect()
}

/// The commands a `--dry-run` of [`list`] would run, in order: for
/// every live session the tip probe, the host-contains probe and —
/// when the host does not have the tip — the count in the clone.
fn dry_run_argv(repo: &Repo, names: &[String]) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    for name in names {
        let session = Session::new(repo, name);
        if !session.clone.join(".git").exists() {
            continue; // debris: no probe, the row is the marker
        }
        if let Some(tip) = tip_of(&session) {
            let contains = contains_argv(repo, &tip);
            out.push(contains.clone());
            if !host_contains(repo, &tip) {
                let ferry = git_output(
                    &[
                        "-C",
                        &repo.root.to_string_lossy(),
                        "rev-parse",
                        "--verify",
                        "--quiet",
                        &format!("refs/heads/{}", session.branch),
                    ],
                    false,
                );
                out.push(count_argv(&session, &tip, ferry.as_deref()));
            }
        }
    }
    out
}

/// `mysbx session destroy NAME [--force]` (D7): the guarded removal.
///
/// The order of the checks is the order D7 states them in — the
/// intrinsic guards decide what the path IS before any work question
/// is asked:
///
/// 1. the NAME grammar, at parse time (cli.md D8, `2`);
/// 2. the repo resolution (the D9 inside-clone refusal, `70`);
/// 3. the session must exist — `clones/` is the registry (D2):
///    a missing entry is the unknown-session refusal (`70`), naming
///    the session and how to start one, the same words the handoff
///    verbs use;
/// 4. the intrinsic guards of the microvm precedent: the resolved
///    path strictly inside `<repo>.mysbx/clones/`, its basename
///    equal to `NAME`, never the sidecar root, never `/` (`70` —
///    the command line was fine, the world is not what it must be);
/// 5. the unmerged-work refusal — the session branch holds commits
///    the host repo does not have (`70`), unless `--force`;
/// 6. the removal: plain `rm -rf` of the clone plus the session's
///    result file, the host repo untouched, a host-local session
///    branch of a `fetch` deliberately kept.
///
/// `--force` overrides ONLY the unmerged-work refusal: the guards
/// of step 4 are about what the path is, not about the work, and no
/// flag may move them.
pub fn destroy(args: &[String], dry_run: bool) -> i32 {
    // 1. the one positional NAME, then at most `--force`.
    let (name, force) = match parse_destroy(args) {
        Ok(x) => x,
        Err(code) => return code,
    };

    // 2. the host repo (cli.md D1). The D9 inside-clone refusal of
    // the resolver fires here too: the session verbs belong to the
    // host side of a session.
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let session = Session::new(&repo, &name);

    // 3. the registry (D2): no clone directory ⇒ no session ⇒ the
    // unknown-session refusal, the same words the handoff verbs use.
    if !session.clone_exists() {
        eprintln!(
            "mysbx: unknown session: {name} — the clone {} does not exist",
            session.clone.display()
        );
        eprintln!("  start one with `mysbx run --session {name} -- CMD`");
        return crate::EXIT_INFRASTRUCTURE;
    }

    // 4. the intrinsic guards (the microvm precedent: intrinsic
    // properties, not location trust — the NAME grammar already
    // made the path a single component, the guards PROVE what the
    // resolved path is before anything removes it).
    if let Err(msg) = intrinsic_guards(&repo, &session) {
        eprintln!("mysbx: refusing to destroy session {name}: {msg}");
        return crate::EXIT_INFRASTRUCTURE;
    }

    // 5. the unmerged-work refusal. Debris — an entry without `.git`
    // — has no branch and no work, so it passes straight through
    // (D2 names `session destroy` as the verb for it).
    let debris = !session.clone.join(".git").exists();
    // Whether the guard proved the work safe (the host has the tip)
    // or --force discarded it — the success message below says
    // which, honestly.
    let mut work_discarded = false;
    if !debris {
        match tip_of(&session) {
            Some(tip) => {
                if !force && !host_contains(&repo, &tip) {
                    eprintln!(
                        "mysbx: refusing to destroy session {name}: the session branch {} \
                         holds commits the host repo does not have — fetch or merge them \
                         first (`mysbx merge {name}`), or pass --force to discard them",
                        session.branch
                    );
                    return crate::EXIT_INFRASTRUCTURE;
                }
                if force && !host_contains(&repo, &tip) {
                    work_discarded = true;
                }
            }
            None => {
                if !force {
                    eprintln!(
                        "mysbx: refusing to destroy session {name}: the session branch {} \
                         cannot be read from {} — the clone may be broken; its work cannot \
                         be proven merged, pass --force to destroy it anyway",
                        session.branch,
                        session.clone.display()
                    );
                    return crate::EXIT_INFRASTRUCTURE;
                }
                work_discarded = true;
            }
        }
    }

    // 6. `--dry-run` (cli.md D9): the exact commands the destroy
    // would run — the probe git commands first (they already ran;
    // the refusals above fired, like the handoff verbs'), then the
    // removal — printed unprefixed, one argument per line, the
    // executable first — and nothing is removed.
    if dry_run {
        if debris {
            eprintln!("mysbx: session {name} is debris of an interrupted creation — a dry run removes nothing");
        } else if let Some(tip) = tip_of(&session) {
            print_commands(&[contains_argv(&repo, &tip)]);
        }
        let removal = remove_argv(&session);
        print_removal(&removal);
        return 0;
    }

    // The removal itself. A failure to remove is an infrastructure
    // failure (70), the path named — half a removal must not pass
    // unnoticed.
    for argv in &remove_argv(&session) {
        let path = Path::new(&argv[1]);
        let removed = if argv[0] == "-rf" {
            std::fs::remove_dir_all(path)
        } else {
            std::fs::remove_file(path)
        };
        if let Err(e) = removed {
            if e.kind() == std::io::ErrorKind::NotFound {
                continue; // the result file is optional state
            }
            eprintln!(
                "mysbx: could not destroy session {name}: cannot remove {}: {e}",
                path.display()
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
    }
    if debris {
        println!(
            "## destroyed: {} (session {name}, debris of an interrupted creation)",
            session.clone.display()
        );
    } else if work_discarded {
        // The message stays honest about what --force did: the work
        // is discarded, not handed over.
        println!(
            "## destroyed: {} (session {name}, branch {} — unmerged work discarded by --force)",
            session.clone.display(),
            session.branch
        );
    } else {
        println!(
            "## destroyed: {} (session {name}, branch {})",
            session.clone.display(),
            session.branch
        );
    }
    // A host-local session branch of a fetch (D6) is deliberately
    // NOT deleted: it is the operator's imported copy (D7).
    0
}

/// `hunk diff HEAD...refs/heads/agent/mysbx/NAME` — the interactive
/// review of the session's diff, the same THREE-DOT range
/// [`crate::handoff::diff_argv`] shows (D6): the changes on the
/// session branch since it diverged from the host's HEAD, not the
/// host's own drift. The invocation mysbx execs, run with the HOST
/// repo as the working directory — the fetched session branch lives
/// there, like the `git -C <repo> diff` of `mysbx diff` (D6); the
/// `--dry-run` block prints it without the cwd note, which is an
/// environment fact, not an argument.
pub fn hunk_argv(session: &Session) -> Vec<String> {
    vec![
        "diff".into(),
        format!("HEAD...refs/heads/{}", session.branch),
    ]
}

/// `mysbx session hunk NAME` (D7): the interactive `hunk` review of
/// the session's diff — the implicit fetch of `mysbx diff` (D6:
/// `git -C <repo> fetch --no-tags <clone> <refspec>`), then the same
/// three-dot range in the viewer, exec'd like `$EDITOR` is for
/// `mysbx edit` (cli.md D12, the `worktree hunk` precedent of
/// docs/design/worktree.md W4) — the tool replaces this process,
/// owns the terminal, and its exit code propagates unchanged
/// (cli.md D8).
///
/// The refusal order mirrors [`destroy`]'s first three steps: the
/// NAME grammar at parse time (`2`), the repo resolution (the D9
/// inside-clone refusal, `70`), then the registry (D2) — a missing
/// clone is the unknown-session refusal (`70`), the same words
/// `destroy` and the handoff verbs use. A debris entry — a clone
/// without `.git` — has no branch to fetch and no diff to review:
/// the refusal names the fact (`70`).
pub fn hunk(args: &[String], dry_run: bool) -> i32 {
    // 1. the one positional NAME — exactly one, no flags, no `--`
    // (the same closed shape as the handoff verbs, workspace.md D6).
    let name = match parse_hunk(args) {
        Ok(n) => n,
        Err(code) => return code,
    };

    // 2. the host repo (cli.md D1). The D9 inside-clone refusal of
    // the resolver fires here too: the session verbs belong to the
    // HOST side of a session.
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let session = Session::new(&repo, &name);

    // 3. the registry (D2): no clone directory ⇒ no session ⇒ the
    // unknown-session refusal, the same words `destroy` and the
    // handoff verbs use.
    if !session.clone_exists() {
        eprintln!(
            "mysbx: unknown session: {name} — the clone {} does not exist",
            session.clone.display()
        );
        eprintln!("  start one with `mysbx run --session {name} -- CMD`");
        return crate::EXIT_INFRASTRUCTURE;
    }

    // Debris — an entry without `.git` — has no session branch and
    // no diff: the fetch would fail on a broken clone, and the
    // refusal names the fact instead.
    if !session.clone.join(".git").exists() {
        eprintln!(
            "mysbx: refusing to hunk session {name}: {} carries no .git — it is debris of an interrupted creation, not a clone to diff (workspace.md D7)",
            session.clone.display()
        );
        return crate::EXIT_INFRASTRUCTURE;
    }

    // 4. the implicit fetch of `mysbx diff` (D6): the reviewed ref is
    // current — the exact fetched host-local ref
    // `refs/heads/agent/mysbx/NAME`, never a stale or absent one.
    let fetch = crate::handoff::fetch_argv(&repo, &session);

    // 5. `--dry-run` (cli.md D9): the exact commands the hunk would
    // run — the fetch first, then the `hunk` invocation, one
    // argument per line, the executable first — and nothing runs
    // (a dry run of an interactive tool shows the work, not the
    // result). The refusals above already ran, like `destroy`'s.
    if dry_run {
        print_commands(&[fetch]);
        let argv = hunk_argv(&session);
        println!("hunk");
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }

    // 6. the fetch, then the exec. Progress lines go to stderr like
    // the diagnostics (cli.md D9: stdout belongs to results — the
    // viewer's own output IS the answer the operator asked for).
    eprintln!(
        "mysbx: fetching branch {} from the session clone into {}",
        session.branch,
        repo.root.display()
    );
    match crate::handoff::run_git(&fetch) {
        Some(0) => {}
        _ => {
            eprintln!(
                "mysbx: fetch from the session clone failed; the clone may be missing or broken, or the host branch {} may have diverged from the session branch (workspace.md D6)",
                session.branch
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
    }
    let mut cmd = std::process::Command::new("hunk");
    cmd.args(hunk_argv(&session)).current_dir(&repo.root);
    // `exec` like the sandbox path and `mysbx edit`: the viewer
    // replaces this process, so it owns the terminal and its exit
    // code propagates unchanged (cli.md D8). A `hunk` that cannot
    // be exec'd is the plain runtime failure of the exec (worktree
    // W4).
    use std::os::unix::process::CommandExt;
    let e = cmd.exec();
    eprintln!("mysbx: cannot exec hunk: {e}");
    crate::EXIT_INFRASTRUCTURE
}

/// Parse the `session hunk` tail: exactly one NAME, no flags and no
/// `--` (the verb has no payload for one to separate); anything else
/// is a usage error (`2`) naming the accepted shape. The NAME
/// grammar is enforced here, at parse time, like every schema edge
/// (workspace.md D2, cli.md D8).
fn parse_hunk(args: &[String]) -> Result<String, i32> {
    let name = match args.first() {
        Some(n) => n.clone(),
        None => {
            eprintln!("mysbx session hunk: a session name is required");
            eprintln!("try `mysbx --help`");
            return Err(2);
        }
    };
    if !crate::session::valid_name(&name) {
        eprintln!("mysbx session hunk: invalid session name `{name}`");
        eprintln!(
            "  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot"
        );
        eprintln!("try `mysbx --help`");
        return Err(2);
    }
    if let Some(arg) = args.get(1) {
        eprintln!("mysbx session hunk: unexpected argument: {arg}");
        eprintln!("{USAGE_HUNK}");
        return Err(2);
    }
    Ok(name)
}

/// Parse the `session destroy` tail: exactly one NAME plus at most
/// one `--force`, either order; anything else is a usage error (`2`)
/// naming the accepted shape. The NAME grammar is enforced here, at
/// parse time, like every schema edge (workspace.md D2, cli.md D8).
fn parse_destroy(args: &[String]) -> Result<(String, bool), i32> {
    let mut name: Option<String> = None;
    let mut force = false;
    for arg in args {
        match arg.as_str() {
            "--force" => {
                if force {
                    eprintln!("mysbx session destroy: repeated flag: --force");
                    eprintln!("{USAGE_DESTROY}");
                    return Err(2);
                }
                force = true;
            }
            "--" => {
                eprintln!("mysbx session destroy: unexpected argument: --");
                eprintln!("{USAGE_DESTROY}");
                return Err(2);
            }
            other if other.starts_with('-') => {
                eprintln!("mysbx session destroy: unknown option: {other}");
                eprintln!("{USAGE_DESTROY}");
                return Err(2);
            }
            other => {
                if name.is_some() {
                    eprintln!(
                        "mysbx session destroy: destroy takes exactly one session name: {other}"
                    );
                    eprintln!("{USAGE_DESTROY}");
                    return Err(2);
                }
                if !crate::session::valid_name(other) {
                    eprintln!("mysbx session destroy: invalid session name `{other}`");
                    eprintln!(
                        "  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot"
                    );
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                name = Some(other.to_string());
            }
        }
    }
    match name {
        Some(n) => Ok((n, force)),
        None => {
            eprintln!("mysbx session destroy: a session name is required");
            eprintln!("try `mysbx --help`");
            Err(2)
        }
    }
}

/// The intrinsic guards of the removal (D7, the microvm precedent:
/// intrinsic properties, not location trust). The guards:
///
/// - the RESOLVED path must be strictly inside `<repo>.mysbx/clones/`
///   — its parent, both canonicalized, must BE the clones directory,
///   so neither a symlink nor a `..` component can move the removal
///   target out from under the guard;
/// - the resolved path's basename must equal `NAME`;
/// - the path must never be the sidecar root and never `/` —
///   implied by the strict-inside check but stated as its own guard
///   because these are the two paths whose removal is catastrophic.
///
/// `Err` carries the guard's own wording, so the refusal names the
/// property that was not met, not just the path.
fn intrinsic_guards(repo: &Repo, session: &Session) -> Result<(), String> {
    let clones_dir = repo.sidecar.join(crate::session::CLONES_DIR);
    let resolved = resolve_dir(&session.clone).ok_or_else(|| {
        format!(
            "{} does not exist or cannot be resolved",
            session.clone.display()
        )
    })?;
    let clones_resolved = resolve_dir(&clones_dir)
        .ok_or_else(|| format!("{} does not exist", clones_dir.display()))?;
    if resolved == std::path::Path::new("/") {
        return Err("the resolved path is / — never a removal target".into());
    }
    if resolved == repo.sidecar {
        return Err(format!(
            "the resolved path is the sidecar root {} — the sidecar is never removed with a session",
            repo.sidecar.display()
        ));
    }
    if resolved.parent() != Some(clones_resolved.as_path()) {
        return Err(format!(
            "the resolved path {} is not strictly inside {} — \
             a symlink or path component moved it out of the clones directory",
            resolved.display(),
            clones_resolved.display()
        ));
    }
    let basename = resolved
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    if basename != session.name {
        return Err(format!(
            "the resolved path's basename {} does not equal the session name {}",
            basename, session.name
        ));
    }
    Ok(())
}

/// Resolve `path` as a real directory — `canonicalize`, refusing
/// when the path does not exist. The LAST component may be a symlink
/// (canonicalize resolves it); the GUARDS above are what make that
/// safe: they verify the resolved target's intrinsic properties, so
/// a symlink that points out of `clones/` is refused by the
/// strict-inside check rather than followed into a removal.
fn resolve_dir(path: &Path) -> Option<PathBuf> {
    if path.is_dir() {
        std::fs::canonicalize(path).ok()
    } else {
        None
    }
}

/// Print command argvs the way cli.md D9 prescribes: unprefixed, one
/// argument per line, the executable first — the same block the
/// handoff verbs print, so a dry run is one format everywhere.
fn print_commands(commands: &[Vec<String>]) {
    for argv in commands {
        println!("git");
        for arg in argv {
            println!("{arg}");
        }
    }
}

/// The removal block of a dry run: `rm` is not `git`, but the format
/// is the same (cli.md D9: the executable is part of what a dry run
/// audits) — `rm`, then its flags and paths, one argument per line.
fn print_removal(argvs: &[Vec<String>]) {
    for argv in argvs {
        println!("rm");
        for arg in argv {
            println!("{arg}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    // ---- the argv builders (the same vectors the probes run) ------

    #[test]
    fn the_count_probe_excludes_clone_time_and_ferry() {
        // D7's ahead-count: commits reachable from the session tip
        // but from neither the host-as-of-creation (`--remotes=origin`
        // of the clone) nor the ferry prefix of an earlier fetch.
        assert_eq!(
            count_argv(&synth_session(), "deadbeef", Some("cafe")),
            vec![
                "-C".to_string(),
                "/synth/repo.mysbx/clones/fix-1".to_string(),
                "rev-list".to_string(),
                "--count".to_string(),
                "deadbeef".to_string(),
                "--not".to_string(),
                "--remotes=origin".to_string(),
                "cafe".to_string(),
            ]
        );
        assert_eq!(
            count_argv(&synth_session(), "deadbeef", None),
            vec![
                "-C".to_string(),
                "/synth/repo.mysbx/clones/fix-1".to_string(),
                "rev-list".to_string(),
                "--count".to_string(),
                "deadbeef".to_string(),
                "--not".to_string(),
                "--remotes=origin".to_string(),
            ]
        );
    }

    #[test]
    fn the_contains_probe_names_the_ref() {
        // The host-side "does the host have it" probe: reachability
        // from the host's own refs.
        assert_eq!(
            contains_argv(&synth_repo(), "deadbeef"),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "for-each-ref".to_string(),
                "--contains".to_string(),
                "deadbeef".to_string(),
                "--format=%(refname)".to_string(),
            ]
        );
    }

    #[test]
    fn the_removal_is_plain_rm_of_clone_and_result_file() {
        // D7: "The clone is plain rm -rf'd" — plus the session's own
        // result file (D5). Two argvs, no git anywhere.
        assert_eq!(
            remove_argv(&synth_session()),
            vec![
                vec![
                    "-rf".to_string(),
                    "/synth/repo.mysbx/clones/fix-1".to_string(),
                ],
                vec![
                    "-f".to_string(),
                    "/synth/repo.mysbx/clones/fix-1.json".to_string(),
                ],
            ]
        );
    }

    // ---- the destroy tail parser ------------------------------------

    #[test]
    fn the_hunk_invocation_is_the_diff_range() {
        // D6's three-dot range in the interactive viewer — the same
        // range `mysbx diff` shows, `HEAD...refs/heads/<branch>`.
        assert_eq!(
            hunk_argv(&synth_session()),
            vec![
                "diff".to_string(),
                "HEAD...refs/heads/agent/mysbx/fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn destroy_takes_one_name_and_at_most_one_force() {
        assert_eq!(
            parse_destroy(&["fix-1".to_string()]).unwrap(),
            ("fix-1".to_string(), false)
        );
        assert_eq!(
            parse_destroy(&["fix-1".to_string(), "--force".to_string()]).unwrap(),
            ("fix-1".to_string(), true)
        );
        assert_eq!(
            parse_destroy(&["--force".to_string(), "fix-1".to_string()]).unwrap(),
            ("fix-1".to_string(), true)
        );
    }

    #[test]
    fn destroy_usage_errors() {
        for args in [
            vec!["--force"],
            vec!["fix-1", "fix-2"],
            vec!["a/b"],
            vec!["-x", "fix-1"],
            vec!["--"],
            vec!["fix-1", "--force", "--force"],
            vec![],
        ] {
            let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
            assert!(parse_destroy(&owned).is_err(), "{args:?}");
        }
    }

    // ---- the hunk tail parser ----------------------------------------

    #[test]
    fn hunk_takes_exactly_one_name() {
        let name = "fix-1".to_string();
        assert_eq!(parse_hunk(&[name.clone()]), Ok(name));
    }

    #[test]
    fn hunk_usage_errors() {
        for args in [
            vec!["fix-1", "extra"],
            vec!["a/b"],
            vec!["-x"],
            vec!["--"],
            vec!["fix-1", "--force"],
            vec![],
        ] {
            let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
            assert!(parse_hunk(&owned).is_err(), "{args:?}");
        }
    }
}
