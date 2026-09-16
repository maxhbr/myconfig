// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The worktree noun group (docs/design/worktree.md, W1):
//! `worktree list` / `worktree diff NAME` / `worktree hunk NAME` —
//! the SECOND closed nested-verb exception to cli.md D3's single-verb
//! rule, beside `session`. Three verbs, not an open tree: a fourth is
//! a decision, not a given.
//!
//! All three are host-side like the handoff verbs (workspace.md D6):
//! no sandbox is started, the repo is the one the cwd resolves to
//! (cli.md D1), `--dry-run` prints the exact commands (cli.md D9) and
//! `--verbose` is refused — there is no run to report on.
//!
//! They answer the host's **workmux worktrees** — the linked
//! worktrees of the host repo in the `<repo>__worktrees` sibling
//! (config.md D13, [`Repo::worktrees`]) — and are deliberately
//! READ-ONLY (W5): creating, merging and removing worktrees stay with
//! `workmux add` / `workmux merge` / `workmux remove`, which already
//! own them and their tmux side effects; agent status stays with
//! `workmux status`, whose live tmux server and state files are a
//! channel mysbx has no part of.
//!
//! - **`list`** — the worktrees sibling IS the registry (W2): one
//!   line per directory entry with a `.git` FILE (a linked worktree's
//!   pointer); an entry without one is debris and marked as such, the
//!   `session list` incomplete-inventory precedent. Each row shows the
//!   handle, the checked-out branch and the ahead-count — the commits
//!   in the worktree's branch that the base branch does not have,
//!   `-` when no honest number can be computed.
//! - **`diff NAME`** — the THREE-DOT diff `<base>...<branch>` (W3):
//!   the changes on the worktree's branch since it diverged from its
//!   base — the same semantic as `mysbx diff` (workspace.md D6),
//!   applied to worktrees. The base is workmux's own record,
//!   `branch.<branch>.workmux-base` of the host repo's config, with
//!   the master/main/HEAD fallback chain for a worktree no workmux
//!   base names; the fallback that fired is reported to stderr, so a
//!   configured base is never confused with a guessed one.
//! - **`hunk NAME`** — the same three-dot range in the interactive
//!   `hunk` viewer (W4), exec'd like `$EDITOR` is for `mysbx edit`
//!   (cli.md D12): the tool replaces this process, owns the terminal,
//!   and its exit code propagates unchanged (cli.md D8).
//!
//! The argv vectors the `--dry-run` contract prints are pure
//! functions of the resolved facts ([`branch_argv`],
//! [`base_argv`], [`count_argv`], [`diff_argv`], [`hunk_argv`]) —
//! the same data-where-it-can-be discipline as
//! [`crate::session::Session::plan`] and the handoff builders of
//! [`crate::handoff`] — so the tests assert against the same builders
//! the verbs use, and a dry run audits exactly what a real invocation
//! would do (cli.md D9), running nothing.

use crate::repo::Repo;
use std::path::{Path, PathBuf};

/// The group's own usage lines, printed by its usage errors (the same
/// pairing rule usage.txt follows, cli.md D5).
const USAGE_LIST: &str = "usage: mysbx worktree list";
const USAGE_DIFF: &str = "usage: mysbx worktree diff NAME";
const USAGE_HUNK: &str = "usage: mysbx worktree hunk NAME";

/// The column widths of `worktree list`'s table — the same shape as
/// `session list` (three columns, a name, a branch, a count). A handle
/// longer than the width overflows the column rather than truncating:
/// a worktree handle is data, and truncating it would make two rows
/// look like one worktree.
const NAME_WIDTH: usize = 24;
const BRANCH_WIDTH: usize = 28;

/// One row of `worktree list` — the facts of a `<repo>__worktrees`
/// entry, with the parts the probes could not answer as [`None`]
/// (rendered `-`). Plain data, so the list tests assert the rows
/// before the printing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Entry {
    /// The entry's handle — a `__worktrees` directory name, and the
    /// NAME of `diff`/`hunk`.
    pub name: String,
    /// The checked-out branch of the worktree, when the pointer and
    /// its gitdir answer (`-` otherwise: a detached HEAD names no
    /// branch, a broken clone answers nothing).
    pub branch: Option<String>,
    /// The ahead-count: commits in the worktree's branch that the
    /// BASE branch does not have (W3). `None` when no honest number
    /// could be computed — no branch, no base, a probe failure.
    pub ahead: Option<u32>,
    /// The entry has no `.git` pointer: debris (an interrupted
    /// `workmux add`, a leftover directory), not a worktree. Such a
    /// row shows the debris marker instead of a count.
    pub debris: bool,
}

/// The `worktree list` rows of `repo` (W2): one per DIRECTORY in the
/// `<repo>__worktrees` sibling — the sibling is the registry — sorted
/// by name, so the listing is deterministic.
///
/// Pure apart from the read_dir and the git probes — the facts the
/// listing is about. An absent sibling is the empty registry: no rows,
/// not an error (a repo without worktrees lists nothing), the same
/// honesty as `session list` over a missing `clones/`.
pub fn entries(repo: &Repo) -> Vec<Entry> {
    // The absent sibling is None on the resolved Repo; the empty
    // registry and no-registry-at-all list the same: nothing.
    let Some(worktrees) = &repo.worktrees else {
        return Vec::new();
    };
    let mut names: Vec<String> = match std::fs::read_dir(worktrees) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            // Only directories are registry entries; anything else
            // in the sibling (a stray file) is not a worktree.
            .filter(|e| e.file_type().map(|t| t.is_dir()).unwrap_or(false))
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .collect(),
        Err(_) => Vec::new(),
    };
    names.sort();
    names
        .into_iter()
        .map(|name| {
            let worktree = worktrees.join(&name);
            // A linked worktree's `.git` is a FILE — the pointer at
            // the per-worktree gitdir. A directory of that name is a
            // plain repository someone put there, not a linked
            // worktree of the host repo; an ABSENT `.git` is debris
            // (an interrupted `workmux add`), the same marker as
            // `session list`'s interrupted creation.
            let debris = !worktree.join(".git").is_file();
            let (branch, ahead) = if debris {
                (None, None)
            } else {
                match checked_out_branch(repo, &worktree) {
                    Some(b) => {
                        let base = base_branch(repo, &b);
                        let ahead = base
                            .as_deref()
                            .and_then(|base| ahead_count(&worktree, base, &b));
                        (Some(b), ahead)
                    }
                    None => (None, None),
                }
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

/// The worktree NAME resolves to its path — the registry spelling of
/// W2: `<worktrees>/NAME`. The name grammar is the session grammar of
/// workspace.md D2 (no slashes, no leading dot), enforced by the
/// caller at parse time, so the join can never escape the sibling.
pub fn worktree_path(repo: &Repo, name: &str) -> Option<PathBuf> {
    repo.worktrees.as_ref().map(|w| w.join(name))
}

/// `git -C <repo> symbolic-ref --short HEAD` — the checked-out
/// branch of the worktree, or `None` for a detached HEAD (the probe
/// fails exactly there) and for a worktree git cannot answer.
///
/// Run against the HOST REPO with `-C <repo>`, then `--git-dir`? No:
/// a linked worktree answers `symbolic-ref` from its own gitdir,
/// which the plain `-C <worktree>` invocation finds through the
/// `.git` pointer — the one indirection every git command already
/// takes. The argv stays the simple, honest one.
pub fn branch_argv(worktree: &Path) -> Vec<String> {
    vec![
        "-C".into(),
        worktree.to_string_lossy().into_owned(),
        "symbolic-ref".into(),
        "--short".into(),
        "HEAD".into(),
    ]
}

/// `git -C <repo> config --get branch.<branch>.workmux-base` — the
/// base branch workmux recorded when it created the worktree (W3),
/// read from the HOST REPO's config (where workmux wrote it), not the
/// worktree's own view of it.
pub fn base_argv(repo: &Repo, branch: &str) -> Vec<String> {
    vec![
        "-C".into(),
        repo.root.to_string_lossy().into_owned(),
        "config".into(),
        "--get".into(),
        format!("branch.{branch}.workmux-base"),
    ]
}

/// The fallback chain of W3, tried in order when no `workmux-base`
/// key names the base: the FIRST of `master`, `main`, `HEAD` that the
/// HOST REPO resolves. A repo whose main branch is neither spelling
/// falls back to its own HEAD — the diff is then against the current
/// checkout, the same anchor `git diff`'s three-dot form uses when no
/// other end is named.
pub const BASE_FALLBACKS: &[&str] = &["master", "main", "HEAD"];

/// The base branch of `branch` (W3): the workmux record when it
/// exists, the first fallback the host repo resolves otherwise. The
/// second half of the pair says which half answered — `true` for the
/// workmux record, `false` for a fallback — so the verbs can report a
/// guessed base instead of presenting it as configured.
pub fn resolve_base(repo: &Repo, branch: &str) -> (Option<String>, bool) {
    if let Some(base) = git_output(&base_argv(repo, branch), false) {
        let base = base.trim().to_string();
        if !base.is_empty() {
            return (Some(base), true);
        }
    }
    for candidate in BASE_FALLBACKS {
        if git_status(
            &[
                "-C",
                &repo.root.to_string_lossy(),
                "rev-parse",
                "--verify",
                "--quiet",
                candidate,
            ],
            true,
        ) {
            return (Some((*candidate).to_string()), false);
        }
    }
    (None, false)
}

/// The base branch of `branch` as a plain value — the verbs that do
/// not care about configured-versus-fallback use this spelling.
pub fn base_branch(repo: &Repo, branch: &str) -> Option<String> {
    resolve_base(repo, branch).0
}

/// The checked-out branch of the worktree at `worktree`, as a plain
/// probe ([`branch_argv`] with its output taken).
pub fn checked_out_branch(repo: &Repo, worktree: &Path) -> Option<String> {
    let _ = repo;
    let out = git_output(&branch_argv(worktree), true)?;
    let branch = out.trim().to_string();
    if branch.is_empty() {
        None
    } else {
        Some(branch)
    }
}

/// `git -C <worktree> rev-list --count <base>..<branch>` — the
/// ahead-count of the list row (W1): the commits in the worktree's
/// branch that the BASE branch does not have. The TWO-DOT range is
/// the honest count here (unlike the three-dot DIFF of `diff`): the
/// question is "how much work is in this worktree the base does not
/// have", and a base that itself advanced does not make the worktree
/// behind — that is the base's own drift, not the worktree's work.
pub fn count_argv(worktree: &Path, base: &str, branch: &str) -> Vec<String> {
    vec![
        "-C".into(),
        worktree.to_string_lossy().into_owned(),
        "rev-list".into(),
        "--count".into(),
        format!("{base}..{branch}"),
    ]
}

/// The ahead-count, or `None` when no honest number can be computed
/// (a probe failure, a base or branch git cannot resolve).
pub fn ahead_count(worktree: &Path, base: &str, branch: &str) -> Option<u32> {
    git_output(&count_argv(worktree, base, branch), true)?
        .trim()
        .parse()
        .ok()
}

/// `git -C <worktree> diff <base>...<branch>` — the THREE-DOT diff of
/// W1/W3: the changes on the worktree's branch since it diverged from
/// its base, not the base's own drift (the same semantic as
/// `mysbx diff`, workspace.md D6 — a two-dot diff would report the
/// base's unpushed commits as removals the worktree "made").
pub fn diff_argv(worktree: &Path, base: &str, branch: &str) -> Vec<String> {
    vec![
        "-C".into(),
        worktree.to_string_lossy().into_owned(),
        "diff".into(),
        format!("{base}...{branch}"),
    ]
}

/// `hunk diff <base>...<branch>` — the interactive review of W4, run
/// with the worktree as the working directory (the invocation mysbx
/// execs; the `--dry-run` block prints it without the cwd note, which
/// is an environment fact, not an argument).
pub fn hunk_argv(base: &str, branch: &str) -> Vec<String> {
    vec!["diff".into(), format!("{base}...{branch}")]
}

/// Run one `git` probe and return its stdout, or `None` when it
/// failed (nonzero or unrunnable). The same helper shape as
/// `sessionverbs.rs`.
pub fn git_output(args: &[String], quiet_stderr: bool) -> Option<String> {
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
/// `rev-parse --verify --quiet` form returns only a status).
pub fn git_status(args: &[&str], quiet_stderr: bool) -> bool {
    let mut cmd = std::process::Command::new("git");
    cmd.args(args);
    if quiet_stderr {
        cmd.stderr(std::process::Stdio::null());
    }
    cmd.status().map(|s| s.success()).unwrap_or(false)
}

/// Run one `git` argv with inherited stdio — git's own output (the
/// diff's) passes through. Returns the process's exit code, or `None`
/// when the command could not be spawned at all (the same contract as
/// [`crate::handoff::run_git`]).
fn run_git(argv: &[String]) -> Option<i32> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(argv);
    match cmd.status() {
        Ok(status) => status.code(),
        Err(_) => None,
    }
}

/// `mysbx worktree list` (W1/W2): print one line per `__worktrees`
/// entry — handle, checked-out branch, ahead-count — debris marked,
/// sorted, exit `0` (a listing, not a verdict: an empty registry — or
/// no registry at all — lists nothing).
///
/// Exit codes (cli.md D8): `2` for a wrong command line, `70` when
/// the repo the cwd resolves to could not be resolved.
pub fn list(args: &[String], dry_run: bool) -> i32 {
    if let Some(arg) = args.first() {
        eprintln!("mysbx worktree list: unexpected argument: {arg}");
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
        // result. An absent sibling prints nothing at all — no probes
        // exist to print.
        print_commands(&dry_run_argv(&repo));
        return 0;
    }
    for line in list_lines(&repo) {
        println!("{line}");
    }
    0
}

/// The lines `worktree list` prints — the SAME lines this verb's
/// stdout is made of, extracted so `mysbx status` (cli.md D19) can
/// embed the listing without a second format that could drift from
/// the list verb's contract (the verb's stdout IS the contract).
/// `lines[0]` is the header, one row per line after it — an empty
/// registry (or none at all) is the header alone.
pub fn list_lines(repo: &Repo) -> Vec<String> {
    let mut lines = vec![format!(
        "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {}",
        "WORKTREE", "BRANCH", "AHEAD"
    )];
    for e in &entries(repo) {
        if e.debris {
            lines.push(format!(
                "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {}",
                e.name, "-", "debris (no .git pointer)"
            ));
        } else {
            let branch = e.branch.as_deref().unwrap_or("-");
            let ahead = match e.ahead {
                Some(n) => n.to_string(),
                None => "-".to_string(),
            };
            lines.push(format!(
                "{:<NAME_WIDTH$} {:<BRANCH_WIDTH$} {ahead}",
                e.name, branch
            ));
        }
    }
    lines
}

/// The commands a `--dry-run` of [`list`] would run, in order: for
/// every live worktree the branch probe and — when the branch and a
/// base resolved — the base probe and the count. Pure collection, so
/// the tests assert the plan.
pub fn dry_run_argv(repo: &Repo) -> Vec<Vec<String>> {
    let mut out = Vec::new();
    for entry in entries(repo) {
        if entry.debris {
            continue; // debris: no probe, the row is the marker
        }
        let Some(worktrees) = &repo.worktrees else {
            break;
        };
        let worktree = worktrees.join(&entry.name);
        out.push(branch_argv(&worktree));
        if let Some(branch) = &entry.branch {
            if let Some(base) = base_branch(repo, branch) {
                out.push(count_argv(&worktree, &base, branch));
            }
        }
    }
    out
}

/// `mysbx worktree diff NAME` (W1/W3): the three-dot diff of the
/// named worktree's branch against its base. git's own exit code and
/// diagnostics pass through — the same contract as `mysbx diff`
/// (workspace.md D6): the diff's output on stdout IS the answer the
/// operator asked for.
pub fn diff(args: &[String], dry_run: bool) -> i32 {
    let Some(name) = parse_name("diff", args) else {
        return 2;
    };
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let (worktree, branch, base) = match resolve_worktree(&repo, "diff", &name) {
        Ok(x) => x,
        Err(code) => return code,
    };
    if let Some((base, configured)) = base {
        report_base(&base, configured);
        let argv = diff_argv(&worktree, &base, &branch);
        if dry_run {
            print_commands(&[argv]);
            return 0;
        }
        match run_git(&argv) {
            Some(code) => code,
            None => {
                eprintln!("mysbx: cannot run git");
                crate::EXIT_INFRASTRUCTURE
            }
        }
    } else {
        // resolve_worktree only returns None for the base when the
        // branch resolved; the message there names the fact.
        eprintln!(
            "mysbx: no base branch could be resolved for branch {branch} of worktree {name} \
             — set one with `git -C {} config branch.{branch}.workmux-base <base>`",
            repo.root.display()
        );
        crate::EXIT_INFRASTRUCTURE
    }
}

/// `mysbx worktree hunk NAME` (W1/W4): the interactive `hunk` review
/// of the same three-dot range, exec'd like `$EDITOR` is for
/// `mysbx edit` (cli.md D12) — the tool replaces this process, owns
/// the terminal, and its exit code propagates unchanged (cli.md D8).
pub fn hunk(args: &[String], dry_run: bool) -> i32 {
    let Some(name) = parse_name("hunk", args) else {
        return 2;
    };
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let (worktree, branch, base) = match resolve_worktree(&repo, "hunk", &name) {
        Ok(x) => x,
        Err(code) => return code,
    };
    let Some((base, configured)) = base else {
        eprintln!(
            "mysbx: no base branch could be resolved for branch {branch} of worktree {name} \
             — set one with `git -C {} config branch.{branch}.workmux-base <base>`",
            repo.root.display()
        );
        return crate::EXIT_INFRASTRUCTURE;
    };
    report_base(&base, configured);
    let argv = hunk_argv(&base, &branch);
    if dry_run {
        // cli.md D9: the exact invocation, one argument per line, the
        // executable (`hunk`) first — the working directory the exec
        // would use is reported to stderr above the block, since a
        // cwd is an environment fact, not an argument.
        println!("hunk");
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }
    let mut cmd = std::process::Command::new("hunk");
    cmd.args(&argv).current_dir(&worktree);
    // `exec` like the sandbox path and `mysbx edit`: the viewer
    // replaces this process, so it owns the terminal and its exit
    // code propagates unchanged (cli.md D8). A `hunk` that cannot be
    // exec'd is the plain runtime failure of the exec (W4).
    use std::os::unix::process::CommandExt;
    let e = cmd.exec();
    eprintln!("mysbx: cannot exec hunk: {e}");
    crate::EXIT_INFRASTRUCTURE
}

/// Parse the one positional NAME of `diff`/`hunk` (W1) and refuse any
/// extra argument — the same closed shape as the handoff verbs
/// (workspace.md D6). The grammar is enforced at parse time like
/// every schema edge (workspace.md D2, cli.md D8).
fn parse_name(verb: &str, args: &[String]) -> Option<String> {
    let usage = match verb {
        "diff" => USAGE_DIFF,
        "hunk" => USAGE_HUNK,
        _ => "usage: mysbx worktree {verb} NAME",
    };
    let usage = if usage.contains("{verb}") {
        usage.replace("{verb}", verb)
    } else {
        usage.to_string()
    };
    let name = match args.first() {
        Some(n) => n.clone(),
        None => {
            eprintln!("mysbx worktree {verb}: a worktree name is required");
            eprintln!("try `mysbx --help`");
            return None;
        }
    };
    if !crate::session::valid_name(&name) {
        eprintln!("mysbx worktree {verb}: invalid worktree name `{name}`");
        eprintln!(
            "  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot"
        );
        eprintln!("try `mysbx --help`");
        return None;
    }
    if let Some(arg) = args.get(1) {
        eprintln!("mysbx worktree {verb}: unexpected argument: {arg}");
        eprintln!("{usage}");
        return None;
    }
    Some(name)
}

/// Resolve the named worktree to its facts: the path, the checked-out
/// branch and the base (with its configured flag of W3). The refusal
/// order is the registry, then the entry, then the branch:
///
/// - no `<repo>__worktrees` sibling at all — the registry does not
///   exist (`70`): a `worktree list` would have said so quietly, but
///   a NAME was asked for and named a thing no registry holds;
/// - no entry of that NAME — the unknown-worktree refusal (`70`),
///   naming the handle and how worktrees come to exist;
/// - no `.git` pointer — debris: not a worktree to diff (`70`);
/// - no checked-out branch — a detached HEAD names no branch to diff
///   (`70`), the same refusal fact as the handoff merge's (workspace
///   D6).
///
/// The base is `None` when nothing resolved — the caller reports it.
fn resolve_worktree(
    repo: &Repo,
    verb: &str,
    name: &str,
) -> Result<(PathBuf, String, Option<(String, bool)>), i32> {
    let Some(worktrees) = &repo.worktrees else {
        eprintln!(
            "mysbx: worktree {verb}: {} has no worktrees sibling — {} does not exist (docs/design/worktree.md W2)",
            repo.root.display(),
            worktrees_display(repo)
        );
        return Err(crate::EXIT_INFRASTRUCTURE);
    };
    let worktree = worktrees.join(name);
    if !worktree.is_dir() {
        eprintln!(
            "mysbx: unknown worktree: {name} — {} does not exist",
            worktree.display()
        );
        eprintln!("  create one with `workmux add {name}` (from the main checkout)");
        return Err(crate::EXIT_INFRASTRUCTURE);
    }
    if !worktree.join(".git").is_file() {
        eprintln!(
            "mysbx: refusing to {verb} worktree {name}: {} carries no .git pointer — it is debris, not a linked worktree (docs/design/worktree.md W2)",
            worktree.display()
        );
        return Err(crate::EXIT_INFRASTRUCTURE);
    }
    let Some(branch) = checked_out_branch(repo, &worktree) else {
        eprintln!(
            "mysbx: cannot {verb} worktree {name}: {} is in detached HEAD state — it names no branch to diff (docs/design/worktree.md W3)",
            worktree.display()
        );
        return Err(crate::EXIT_INFRASTRUCTURE);
    };
    // The base of W3, with its configured flag: the workmux record
    // when it exists, the first fallback the host repo resolves
    // otherwise — `None` when neither answered, which the caller
    // reports (the message names the config key that would fix it).
    let (base, configured) = resolve_base(repo, &branch);
    Ok((worktree, branch, base.map(|b| (b, configured))))
}

/// The spelling of the absent sibling in the registry refusal — the
/// derived path, without re-deriving it twice.
fn worktrees_display(repo: &Repo) -> String {
    match &repo.worktrees {
        Some(w) => w.display().to_string(),
        None => format!("{}__worktrees", repo.root.display()),
    }
}

/// Tell the operator which base answered (W3): a fallback is reported
/// as one, so a configured base is never confused for a guessed one.
fn report_base(base: &str, configured: bool) {
    if configured {
        eprintln!("mysbx: base branch of the diff: {base} (workmux-base record)");
    } else {
        eprintln!("mysbx: base branch of the diff: {base} (fallback — no workmux-base record for this branch)");
    }
}

/// Print command argvs the way cli.md D9 prescribes: unprefixed, one
/// argument per line, the executable first — the same block the
/// handoff verbs print, so a dry run is one format everywhere.
fn print_commands(commands: &[Vec<String>]) {
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
            worktrees: Some(PathBuf::from("/synth/repo__worktrees")),
        }
    }

    fn synth_repo_no_worktrees() -> Repo {
        Repo {
            worktrees: None,
            ..synth_repo()
        }
    }

    // ---- the argv builders (the same vectors the probes run) ------

    #[test]
    fn the_branch_probe_reads_the_pointer() {
        assert_eq!(
            branch_argv(Path::new("/synth/repo__worktrees/fix-1")),
            vec![
                "-C".to_string(),
                "/synth/repo__worktrees/fix-1".to_string(),
                "symbolic-ref".to_string(),
                "--short".to_string(),
                "HEAD".to_string(),
            ]
        );
    }

    #[test]
    fn the_base_probe_reads_the_host_repos_config() {
        // W3: the workmux record is read from the HOST REPO, where
        // workmux wrote it.
        assert_eq!(
            base_argv(&synth_repo(), "fix-1"),
            vec![
                "-C".to_string(),
                "/synth/repo".to_string(),
                "config".to_string(),
                "--get".to_string(),
                "branch.fix-1.workmux-base".to_string(),
            ]
        );
    }

    #[test]
    fn the_count_is_two_dot_against_the_base() {
        // The TWO-DOT count: "how much work is in this worktree the
        // base does not have" — the base's own drift is not the
        // worktree's work.
        assert_eq!(
            count_argv(Path::new("/synth/repo__worktrees/fix-1"), "master", "fix-1"),
            vec![
                "-C".to_string(),
                "/synth/repo__worktrees/fix-1".to_string(),
                "rev-list".to_string(),
                "--count".to_string(),
                "master..fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn the_diff_is_the_three_dot_form() {
        // W1/W3: the three-dot diff — the changes since the
        // divergence, not the base's own drift (the same semantic as
        // `mysbx diff`, workspace.md D6).
        assert_eq!(
            diff_argv(Path::new("/synth/repo__worktrees/fix-1"), "master", "fix-1"),
            vec![
                "-C".to_string(),
                "/synth/repo__worktrees/fix-1".to_string(),
                "diff".to_string(),
                "master...fix-1".to_string(),
            ]
        );
    }

    #[test]
    fn the_hunk_invocation_is_the_same_range() {
        // W4: `hunk diff <base>...<branch>` — the same range the
        // diff verb shows, in the interactive viewer.
        assert_eq!(
            hunk_argv("master", "fix-1"),
            vec!["diff".to_string(), "master...fix-1".to_string()]
        );
    }

    #[test]
    fn the_worktree_path_is_the_registry_spelling() {
        assert_eq!(
            worktree_path(&synth_repo(), "fix-1"),
            Some(PathBuf::from("/synth/repo__worktrees/fix-1"))
        );
        // No sibling: no path at all (the caller refuses).
        assert_eq!(worktree_path(&synth_repo_no_worktrees(), "fix-1"), None);
    }

    // ---- the name tail parser --------------------------------------

    #[test]
    fn diff_takes_exactly_one_name() {
        let name = "fix-1".to_string();
        assert_eq!(parse_name("diff", &[name.clone()]), Some(name));
    }

    #[test]
    fn diff_usage_errors() {
        for args in [vec!["fix-1", "extra"], vec!["a/b"], vec!["-x"], vec![]] {
            let owned: Vec<String> = args.iter().map(|s| s.to_string()).collect();
            assert!(parse_name("diff", &owned).is_none(), "{args:?}");
        }
    }
}
