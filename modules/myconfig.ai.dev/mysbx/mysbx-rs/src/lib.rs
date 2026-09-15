// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `mysbx` CLI surface (docs/TODOs/mvp-5-cli-and-dry-run.md,
//! docs/design/cli.md):
//!
//! ```text
//! mysbx [FLAGS]                     enter an interactive sandbox shell
//! mysbx run [FLAGS] -- CMD...       run one command in the sandbox
//! mysbx init                        create the sidecar (idempotent)
//! mysbx edit                        edit the sidecar config in $EDITOR
//! mysbx version | help
//! ```
//!
//! The bare form is the primary action (cli.md D2): it resolves the repo,
//! requires an already initialized sidecar (D13: `mysbx init` is the only
//! command that creates one) and execs the backend's argv. `--dry-run`
//! and `--verbose` are *global* flags (before
//! the subcommand, in any order): `--dry-run` runs the whole pipeline —
//! resolve, guards, load, merge, backend check, argv build — and stops
//! immediately before `exec`, printing the backend executable followed
//! by the argv, one argument per line, on stdout; `--verbose` prints the
//! `## `-prefixed run report before that (cli.md D10). `--multiplexer
//! <mux>` (cli.md D14) is a global flag of the bare form only: it
//! overrides the merged `multiplexer` of the layers for THIS run —
//! per D6's precedence, a flag wins over the configuration.

pub mod bwrap;
pub mod config;
pub mod handoff;
pub mod loadimage;
pub mod merge;
pub mod podman_gvisor;
pub mod repo;
pub mod report;
pub mod result;
pub mod session;
pub mod sessionverbs;
pub mod toml;
pub mod worktreeverbs;

/// The usage text.
pub const USAGE: &str = include_str!("usage.txt");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The exit code of mysbx's own runtime failure (cli.md D8, bd
/// myconfig-0ql): the command line was fine, but the run it named
/// could not happen — the repo could not be resolved, the sidecar is
/// missing, a configuration cannot be parsed or laid out, the backend
/// cannot be started or waited for.
///
/// `70` (agent-microvm's `infrastructure-error`), not `1`: `1` is a
/// PAYLOAD's own failure — under the passthrough of a plain run and
/// as the `failed` state of a `--result` run — and a tool that runs
/// payloads must not be able to forge its own infrastructure failures
/// into that code. Public so the tests assert the contract, not a
/// hand-copied number.
pub const EXIT_INFRASTRUCTURE: i32 = 70;

/// Print the usage to stdout.
pub fn usage() {
    print!("{USAGE}");
}

/// Host environment variables forwarded into the sandbox — exactly this
/// list, each only when actually set in the process environment
/// (docs/plan.md, "Environment"). Nothing else is forwarded implicitly.
/// Public so the integration tests assert against the same list the
/// pipeline reads, not a hand-copied one.
///
/// The model-credential block (`OPENAI_*`, `ANTHROPIC_*`, `OPENROUTER_*`)
/// mirrors the jail/nono tiers (`fns/bubblewrap-app.nix`,
/// `fns/nono-app.nix`: always-`OPENAI_API_KEY` plus the claude-code
/// module's `anthropicFwdEnv`): a credential lives only in the host
/// environment — never in a store path — so an `[env]` entry cannot
/// forward it, and a sandboxed agent without it cannot reach its model
/// endpoint at all (bd myconfig-20j). Each name still forwards only
/// when set, so a host without a proxy loses nothing.
pub const FORWARDED_ENV_VARS: &[&str] = &[
    "TERM",
    "COLORTERM",
    "LANG",
    "LC_ALL",
    "EDITOR",
    "VISUAL",
    "OPENAI_API_KEY",
    "OPENAI_BASE_URL",
    "ANTHROPIC_API_KEY",
    "ANTHROPIC_BASE_URL",
    "ANTHROPIC_AUTH_TOKEN",
    "OPENROUTER_API_KEY",
    "OPENROUTER_BASE_URL",
];

/// Dispatch on the argument list (without argv[0]); returns the exit code.
///
/// Exit codes (cli.md D8, extended by bd myconfig-0ql): `0` success,
/// `2` usage error, `70` mysbx's own runtime failure — the old `1`,
/// renumbered (agent-microvm's `infrastructure-error`) so a payload's
/// own `1` can never be mistaken for the tool's failure — plus `124`
/// timed out and `130`/`143` cancelled for a `--result` run, whose
/// `failed` state is `1`. A payload's own exit code propagates
/// unchanged in a plain run — the exec replaces this process — while
/// a `--result` run waits and interprets instead (cli.md D17).
pub fn run(args: Vec<String>) -> i32 {
    // The global flags are accepted only BEFORE the subcommand / bare
    // form; anything after `--` is payload and never parsed (cli.md D4,
    // D5, D10). `gui` takes no `--`: its whole argument tail is the
    // payload of the inner `mysbx` (D15) and passes through verbatim.
    let (flags, rest) = match split_global_flags(&args) {
        Ok(x) => x,
        Err(code) => return code,
    };
    match rest.first().map(String::as_str) {
        // Bare `mysbx` is the primary action (docs/design/cli.md D2): enter
        // the sandbox for the current repository. The one-shot flags
        // of the D8/D17 extension are refused here too (bd
        // myconfig-0ql): `--result`/`--timeout` name the outcome of a
        // payload run, and an interactive shell has no consumable one —
        // the usage error teaches the flag belongs to `run -- CMD`
        // instead of silently ignoring it.
        None if flags.result || flags.timeout.is_some() => {
            eprintln!(
                "mysbx: {} names the outcome of a payload run — use it with `run -- CMD`",
                flags.first_run_scoped_name()
            );
            eprintln!("try `mysbx --help`");
            2
        }
        // Bare `mysbx` is the primary action (docs/design/cli.md D2): enter
        // the sandbox for the current repository.
        None => sandbox(flags, bwrap::Payload::Shell, RunMode::Exec),
        // The run-scoped flags are refused with the verb before any
        // verb arm runs (D14 for `--multiplexer`, D16 for
        // `--ro`/`--rw`, the D8/D17 extension for `--result`/
        // `--timeout`, the D1 `--session` for the handoff verbs of
        // workspace.md D6 — a handoff names its session positionally,
        // and is not a run): they name the mounts, the payload or the
        // outcome of a run, and no OTHER verb has one to choose or
        // add. `--multiplexer` is refused for `run` too (D11: a
        // one-shot never starts a session), while `--ro`/`--rw` and
        // `--backend` (cli.md D18) are accepted before `run` exactly
        // like `--dry-run` is (D10: one position rule for all global
        // flags) — `run_command` appends to the same lists, so both
        // spellings are the same run.
        Some(other)
            if (flags.multiplexer.is_some()
                || (flags.backend.is_some() && other != "run")
                || (flags.session.is_some() && other != "run")
                || ((flags.result || flags.timeout.is_some()) && other != "run")
                || ((!flags.ro.is_empty() || !flags.rw.is_empty()) && other != "run"))
                && matches!(
                    other,
                    "run"
                        | "gui"
                        | "init"
                        | "edit"
                        | "version"
                        | "help"
                        | "fetch"
                        | "merge"
                        | "push"
                        | "diff"
                        | "session"
                        | "worktree"
                        | "gvisor-load-image"
                ) =>
        {
            eprintln!(
                "mysbx: {} is not valid with `{other}`",
                flags.first_run_scoped_name()
            );
            eprintln!("try `mysbx --help`");
            2
        }
        Some("run") => run_command(flags, &rest[1..]),
        Some("gui") => {
            // cli.md D15: `mysbx gui ARG...` re-invokes `mysbx` — the SAME
            // executable, by absolute path, so a PATH lookup cannot find
            // a different one — inside a terminal window, from the current
            // directory, with the whole argument tail passed verbatim. It
            // is NOT a sandbox run itself: nothing of `sandbox` runs here,
            // the inner `mysbx` is the run and reports its own errors in
            // the window it opens. That is also why the global flags are
            // rejected with the verb in the arms below: `--dry-run` would
            // have nothing to print — the argv this form builds is the
            // terminal's, not the sandbox's — and `--multiplexer` belongs
            // to the INNER invocation (`mysbx gui --multiplexer herdr`
            // passes it through verbatim).
            gui(flags, &rest[1..])
        }
        // The host-side handoff verbs of the workspace model
        // (workspace.md D6): git plumbing between the host repo and a
        // session's clone — no sandbox is started. They sit BEFORE the
        // generic `flags.any()` arm on purpose: `--dry-run` is valid
        // with them (cli.md D9 — it prints the git commands instead
        // of running them), while `--verbose` is refused — there is
        // no run to report on (the run-scoped flags are refused by
        // the arm above: a handoff is not a run, there is no
        // workspace to choose and no payload).
        Some("fetch") if flags.verbose => reject_verbose("fetch"),
        Some("merge") if flags.verbose => reject_verbose("merge"),
        Some("push") if flags.verbose => reject_verbose("push"),
        Some("diff") if flags.verbose => reject_verbose("diff"),
        // The session noun group (workspace.md D7) — the one CLOSED
        // nested-verb exception to cli.md D3, three verbs, not an open
        // tree. Host-side like the handoff verbs: no sandbox is
        // started, the repo is the one the cwd resolves to, `--dry-run`
        // prints the exact commands (cli.md D9) and `--verbose` is
        // refused — there is no run to report on.
        Some("session") if flags.verbose => reject_verbose("session"),
        // The worktree noun group (docs/design/worktree.md, W1) — the
        // second closed nested-verb exception to cli.md D3, beside
        // `session`. Host-side like the handoff verbs: no sandbox is
        // started, the repo is the one the cwd resolves to, `--dry-run`
        // prints the exact commands (cli.md D9) and `--verbose` is
        // refused — there is no run to report on.
        Some("worktree") if flags.verbose => reject_verbose("worktree"),
        Some("worktree") => {
            match rest.get(1).map(String::as_str) {
                Some("list") => worktreeverbs::list(&rest[2..], flags.dry_run),
                Some("diff") => worktreeverbs::diff(&rest[2..], flags.dry_run),
                Some("hunk") => worktreeverbs::hunk(&rest[2..], flags.dry_run),
                // The group is closed: a fourth verb is a decision,
                // not a given — unknown members are usage errors
                // naming the three that exist (W1).
                Some(other) => {
                    eprintln!("mysbx worktree: unknown worktree verb: {other}");
                    eprintln!("  the worktree group is closed: list, diff, hunk (docs/design/worktree.md W1)");
                    eprintln!("try `mysbx --help`");
                    2
                }
                None => {
                    eprintln!("mysbx worktree: a verb is required: list, diff or hunk");
                    eprintln!("try `mysbx --help`");
                    2
                }
            }
        }
        Some("session") => {
            match rest.get(1).map(String::as_str) {
                Some("list") => sessionverbs::list(&rest[2..], flags.dry_run),
                Some("destroy") => sessionverbs::destroy(&rest[2..], flags.dry_run),
                Some("hunk") => sessionverbs::hunk(&rest[2..], flags.dry_run),
                // The group is closed: a fourth verb is a decision,
                // not a given — unknown members are usage errors
                // naming the three that exist (D7).
                Some(other) => {
                    eprintln!("mysbx session: unknown session verb: {other}");
                    eprintln!(
                        "  the session group is closed: list, destroy, hunk (workspace.md D7)"
                    );
                    eprintln!("try `mysbx --help`");
                    2
                }
                None => {
                    eprintln!("mysbx session: a verb is required: list, destroy or hunk");
                    eprintln!("try `mysbx --help`");
                    2
                }
            }
        }
        // The run-scoped flags mean nothing here either: `--dry-run`
        // would promise side-effect-freeness while the verb's whole job
        // is a side effect (loading an image into the local store), and
        // `--verbose` has no run to report on. Refused like on the other
        // non-run verbs (usage error, exit 2).
        Some("gvisor-load-image") if flags.any() => {
            eprintln!(
                "mysbx: {} is not valid with `gvisor-load-image`",
                flags.first_name()
            );
            eprintln!("try `mysbx --help`");
            2
        }
        Some("gvisor-load-image") => loadimage::run(&rest[1..]),
        Some("fetch") => handoff::verb(&rest[1..], handoff::Kind::Fetch, flags.dry_run),
        Some("merge") => handoff::verb(&rest[1..], handoff::Kind::Merge, flags.dry_run),
        Some("push") => handoff::verb(&rest[1..], handoff::Kind::Push, flags.dry_run),
        Some("diff") => handoff::verb(&rest[1..], handoff::Kind::Diff, flags.dry_run),
        // The run-scoped flags are only meaningful for the bare form and
        // `run`: on `init` `--dry-run` would promise side-effect-freeness
        // while files are still created, and there is no run to report on
        // for `help`/`version` — reject them instead (usage error, D8).
        Some(other) if flags.any() => {
            eprintln!("mysbx: {} is not valid with `{other}`", flags.first_name());
            eprintln!("try `mysbx --help`");
            2
        }
        Some("help") | Some("-h") | Some("--help") => {
            usage();
            0
        }
        Some("version") | Some("-V") | Some("--version") => {
            println!("mysbx {VERSION}");
            0
        }
        Some("init") => init(&rest[1..]),
        Some("edit") => edit(&rest[1..]),
        Some(other) => {
            eprintln!("mysbx: unknown command: {other}");
            eprintln!("try `mysbx --help`");
            2
        }
    }
}

/// The global flags of a sandbox run (cli.md D9, D10, D14, D16).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Flags {
    pub dry_run: bool,
    pub verbose: bool,
    /// The `--multiplexer <name>` override (cli.md D14, config.md
    /// D17): the multiplexer of THIS interactive run, replacing the
    /// merged `multiplexer` of the configuration layers. `None` means
    /// the flag was not given and the configuration decides.
    pub multiplexer: Option<config::Multiplexer>,
    /// The `--ro <path>` additions (cli.md D16): host paths bound
    /// read-only into THIS run's sandbox, on top of the merged
    /// mounts. The raw spellings as typed; the pipeline resolves and
    /// canonicalizes them like a `[[mounts]]` path (D8), a relative
    /// one against the cwd.
    pub ro: Vec<String>,
    /// The `--rw <path>` additions (cli.md D16): the same, read-write.
    pub rw: Vec<String>,
    /// The `--timeout <seconds>` budget of a `--result` run (bd
    /// myconfig-0ql, the D8 extension): when set, a run whose backend
    /// outlives it is killed and recorded as `timed-out`. `None`
    /// means no budget — the run may wait forever. Accepted before
    /// the verb (for `run`) and after it, the same position rule as
    /// every run-scoped flag (D10/D16); `run_command` merges the two
    /// spellings.
    pub timeout: Option<u64>,
    /// The `--result` mode of a run (bd myconfig-0ql, the D8/D17
    /// extension): wait for the backend and record the outcome in
    /// the sidecar's `result.json` instead of exec'ing. Accepted
    /// before the verb and after `run` like `--timeout`; refused
    /// for every other verb by the dispatcher — only a one-shot has
    /// a consumable outcome.
    pub result: bool,
    /// The `--backend <name>` override (bd myconfig-veg): the sandbox
    /// backend of THIS run, replacing the merged `backend` of the
    /// configuration layers (cli.md D18). `None` means the flag was
    /// not given and the configuration decides. The value is checked
    /// only against the pipeline's backend set (step 4): an unknown
    /// name is a refused run there (`70`), not a parse-time usage
    /// error — the command line is fine, the backend it names does
    /// not exist.
    pub backend: Option<String>,
    /// The `--session NAME` flag (workspace.md D1): select clone mode
    /// — the named session's clone at `<repo>.mysbx/clones/NAME` is
    /// the workspace of this run, bound rw at the repo's own path
    /// while the host repo is not mounted at all (D3). `None` means
    /// the live mode, the default — and there is NO TOML surface for
    /// it (D1): the flag is the whole switch.
    pub session: Option<String>,
}

impl Flags {
    fn any(&self) -> bool {
        self.dry_run || self.verbose || !self.ro.is_empty() || !self.rw.is_empty()
    }

    /// The flag named in the "not valid with `<verb>`" usage error —
    /// whichever was set, `--dry-run` first (it is the older, more
    /// dangerous-sounding promise), then `--verbose`, then the run-
    /// scoped `--ro`/`--rw`. `--multiplexer` is not listed:
    /// it is run-scoped, not a promise about the output, and the
    /// refusal names the flags that are.
    fn first_name(&self) -> &'static str {
        if self.dry_run {
            "--dry-run"
        } else if self.verbose {
            "--verbose"
        } else if !self.ro.is_empty() {
            "--ro"
        } else {
            "--rw"
        }
    }

    /// The run-scoped flag named in the dispatcher's verb refusal —
    /// `--multiplexer` first (it is the older flag), then `--backend`,
    /// then the first `--ro`/`--rw` addition, then `--timeout`/`--result`.
    /// `--timeout`
    /// and `--result` are parsed here so they follow the one position
    /// rule of D10 (before the verb and after it, for `run` only), and
    /// the dispatcher names whichever was set when another verb is
    /// typed with them.
    fn first_run_scoped_name(&self) -> &'static str {
        if self.multiplexer.is_some() {
            "--multiplexer"
        } else if self.backend.is_some() {
            "--backend"
        } else if self.session.is_some() {
            "--session"
        } else if !self.ro.is_empty() {
            "--ro"
        } else if !self.rw.is_empty() {
            "--rw"
        } else if self.timeout.is_some() {
            "--timeout"
        } else {
            "--result"
        }
    }
}

/// Split the leading global flags off the argument list. They may appear
/// in any order but never twice: a repeated flag is a typo, not an
/// intensifier, and staying strict keeps the surface honest (D5: the
/// parser is hand-written, so every accepted spelling is a deliberate
/// one). Returns the exit code of the usage error on rejection.
///
/// `--multiplexer` takes its value from the following argument and is
/// accepted here only BEFORE the subcommand — the same position rule
/// as `--dry-run`/`--verbose` (D10), so one rule governs all three
/// (D14). An unknown multiplexer spelling is a usage error (`2`, D8):
/// the command line is wrong, not the world it names.
fn split_global_flags(args: &[String]) -> Result<(Flags, &[String]), i32> {
    let mut flags = Flags::default();
    let mut rest = args;
    while let Some((first, tail)) = rest.split_first() {
        match first.as_str() {
            "--dry-run" => {
                if flags.dry_run {
                    eprintln!("mysbx: repeated flag: --dry-run");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                flags.dry_run = true;
            }
            "--verbose" => {
                if flags.verbose {
                    eprintln!("mysbx: repeated flag: --verbose");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                flags.verbose = true;
            }
            "--multiplexer" => {
                if flags.multiplexer.is_some() {
                    eprintln!("mysbx: repeated flag: --multiplexer");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                let value = match tail.split_first() {
                    Some((v, _)) => v,
                    None => {
                        eprintln!("mysbx: --multiplexer requires a value");
                        eprintln!(
                            "  one of: {}",
                            config::Multiplexer::NAMES
                                .iter()
                                .map(|n| format!("`{n}`"))
                                .collect::<Vec<_>>()
                                .join(" ")
                        );
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                };
                flags.multiplexer = Some(match config::Multiplexer::parse_cli(value) {
                    Ok(m) => m,
                    Err(msg) => {
                        eprintln!("mysbx: {msg}");
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                });
                // The value argument is consumed with the flag.
                rest = tail.split_first().map(|(_, t)| t).unwrap_or(&[]);
                continue;
            }
            "--ro" | "--rw" => {
                // cli.md D16: a value-taking, REPEATABLE run flag. The
                // same three-path spellings as a `[[mounts]]` path
                // (D8) are accepted — absolute, `~/…` and relative
                // (resolved against the cwd later, by the pipeline
                // that knows it). A `~` that is not the `~/` prefix
                // cannot be resolved by anything, so it is the
                // parser's own refusal like it is the config
                // parser's; everything else is resolved — and
                // existence-checked — by `resolve_cli_path`.
                let flag = first.as_str();
                let value = match tail.split_first() {
                    Some((v, _)) => v,
                    None => {
                        eprintln!("mysbx: {flag} requires a path");
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                };
                if value.is_empty() {
                    eprintln!("mysbx: {flag} must not be empty");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                if value.starts_with('~') && !value.starts_with("~/") {
                    eprintln!("mysbx: {flag}: only the `~/` prefix is supported, not `~` alone or `~user`: `{value}`");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                if flag == "--ro" {
                    flags.ro.push(value.clone());
                } else {
                    flags.rw.push(value.clone());
                }
                // The value argument is consumed with the flag.
                rest = tail.split_first().map(|(_, t)| t).unwrap_or(&[]);
                continue;
            }
            "--backend" => {
                // bd myconfig-veg: the backend of THIS run (cli.md
                // D18). A value-taking, run-scoped flag like
                // `--multiplexer`/`--session`: before the verb here,
                // accepted again after `run` by `run_command`, refused
                // elsewhere by the dispatcher. The VALUE is not
                // validated here: the pipeline's backend check (step
                // 4) owns the accepted set and refuses an unknown
                // name as a run refusal (`70`), listing the valid
                // ones — the command line is fine, the backend it
                // names does not exist. A missing value is still a
                // usage error (`2`), like every value-taking flag.
                if flags.backend.is_some() {
                    eprintln!("mysbx: repeated flag: --backend");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                let value = match tail.split_first() {
                    Some((v, _)) => v,
                    None => {
                        eprintln!(
                            "mysbx: --backend requires a value — one of: `bubblewrap`, `podman-gvisor`"
                        );
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                };
                flags.backend = Some(value.clone());
                // The value argument is consumed with the flag.
                rest = tail.split_first().map(|(_, t)| t).unwrap_or(&[]);
                continue;
            }
            "--session" => {
                // workspace.md D1/D2: `--session NAME` selects clone
                // mode for this run. The value is validated HERE, at
                // parse time — a bad NAME grammar is a usage error
                // (D2: "anything else is a usage error at parse
                // time, cli.md D8, like every schema edge") — so no
                // run form can reach the pipeline with a name that
                // could ever become a path component. Not
                // repeatable: a repeated flag is a typo (D5), and
                // two sessions in one run is not a thing — the flag
                // names THE workspace of the run.
                if flags.session.is_some() {
                    eprintln!("mysbx: repeated flag: --session");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                let value = match tail.split_first() {
                    Some((v, _)) => v,
                    None => {
                        eprintln!("mysbx: --session requires a name");
                        eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                };
                if !session::valid_name(value) {
                    eprintln!("mysbx: --session: invalid session name `{value}`");
                    eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                flags.session = Some(value.clone());
                // The value argument is consumed with the flag.
                rest = tail.split_first().map(|(_, t)| t).unwrap_or(&[]);
                continue;
            }
            "--result" => {
                // bd myconfig-0ql (the D8/D17 extension): the run mode
                // itself, so it follows the one position rule of D10 —
                // before the verb and after it for `run`, refused for
                // every other verb by the dispatcher.
                if flags.result {
                    eprintln!("mysbx: repeated flag: --result");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                flags.result = true;
            }
            "--timeout" => {
                // The budget of a `--result` run, the same position
                // rule as `--result` (bd myconfig-0ql). A repeated
                // flag is a typo (D5), a bad value is a usage error —
                // the command line is wrong, not the world it names
                // (D8).
                if flags.timeout.is_some() {
                    eprintln!("mysbx: repeated flag: --timeout");
                    eprintln!("try `mysbx --help`");
                    return Err(2);
                }
                let value = match tail.split_first() {
                    Some((v, _)) => v,
                    None => {
                        eprintln!("mysbx: --timeout requires a positive whole number of seconds");
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                };
                flags.timeout = Some(match value.parse::<u64>() {
                    Ok(secs) if secs > 0 => secs,
                    _ => {
                        eprintln!("mysbx: --timeout requires a positive whole number of seconds, got `{value}`");
                        eprintln!("try `mysbx --help`");
                        return Err(2);
                    }
                });
                // The value argument is consumed with the flag.
                rest = tail.split_first().map(|(_, t)| t).unwrap_or(&[]);
                continue;
            }
            _ => break,
        }
        rest = tail;
    }
    Ok((flags, rest))
}

/// The `--verbose` refusal of the handoff verbs (workspace.md D6):
/// they start no sandbox, so there is no run report to print — the
/// same reason `init`/`help` reject the flag (cli.md D10), with the
/// verb named like every other flag/verb refusal.
fn reject_verbose(verb: &str) -> i32 {
    eprintln!("mysbx: --verbose is not valid with `{verb}`");
    if verb == "session" || verb == "worktree" {
        eprintln!("  it reports a run's configuration — the {verb} verbs start no sandbox");
    } else {
        eprintln!("  it reports a run's configuration — a handoff starts no sandbox");
    }
    eprintln!("try `mysbx --help`");
    2
}

/// `mysbx run [--dry-run] [--verbose] -- CMD...` — parse the `run`
/// arguments and hand the payload to the same pipeline as the bare form
/// (spec "Watch out": one code path, two entry points).
///
/// Everything after `--` is the payload, verbatim — including things that
/// look like flags (cli.md D4). The global flags are accepted before `--`
/// only; `run` without `--` or without a command is a usage error (`2`),
/// not an empty sandbox.
fn run_command(global: Flags, args: &[String]) -> i32 {
    let mut flags = global;
    // The structured-result mode (bd myconfig-0ql, the D8/D17
    // extension): `--result` asks for a waited run that records its
    // outcome in the sidecar's `result.json`, `--timeout <seconds>`
    // bounds it. Both are `run`-only — the bare form is interactive,
    // nobody consumes its result — so they are parsed here too, in the
    // same position rule as the flags D10 already accepts after the
    // verb, and refused everywhere else by the dispatcher (never
    // accept-and-ignore: an operator typing `mysbx --result` must
    // learn the flag does not do that for a shell).
    let mut idx = 0;
    while let Some(arg) = args.get(idx) {
        match arg.as_str() {
            "--dry-run" => {
                flags.dry_run = true;
                idx += 1;
            }
            "--verbose" => {
                flags.verbose = true;
                idx += 1;
            }
            "--result" => {
                if flags.result {
                    eprintln!("mysbx run: repeated flag: --result");
                    eprintln!("usage: {RUN_USAGE}");
                    return 2;
                }
                flags.result = true;
                idx += 1;
            }
            "--timeout" => {
                if flags.timeout.is_some() {
                    eprintln!("mysbx run: repeated flag: --timeout");
                    eprintln!("usage: {RUN_USAGE}");
                    return 2;
                }
                let value = match args.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!("mysbx run: --timeout requires a number of seconds");
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                };
                flags.timeout = Some(match value.parse::<u64>() {
                    Ok(secs) if secs > 0 => secs,
                    _ => {
                        eprintln!("mysbx run: --timeout requires a positive whole number of seconds, got `{value}`");
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                });
                idx += 2;
            }
            "--multiplexer" => {
                // cli.md D11/D14: `run -- CMD` never starts a session, so
                // there is no interactive payload for the flag to select —
                // accept-and-ignore would let an operator believe the
                // one-shot ran inside a session it did not.
                eprintln!("mysbx run: --multiplexer is not valid with `run`");
                eprintln!("  it selects the interactive payload only: `mysbx --multiplexer <mux>` starts the session");
                eprintln!("usage: {RUN_USAGE}");
                return 2;
            }
            "--backend" => {
                // bd myconfig-veg: the same flag after the verb, the
                // same one position rule as `--ro`/`--rw` (cli.md
                // D18) — one rule for every run-scoped flag, both
                // spellings one run. Not repeatable: a repeated flag
                // is a typo (D5), and one run starts exactly one
                // backend.
                if flags.backend.is_some() {
                    eprintln!("mysbx run: repeated flag: --backend");
                    eprintln!("usage: {RUN_USAGE}");
                    return 2;
                }
                let value = match args.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!(
                            "mysbx run: --backend requires a value — one of: `bubblewrap`, `podman-gvisor`"
                        );
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                };
                flags.backend = Some(value);
                idx += 2;
            }
            "--ro" => {
                // cli.md D16: the additions are run flags, so `run`
                // accepts them too — the same one pipeline runs both
                // forms. Repeatable, like the bare form.
                let value = match args.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!("mysbx run: --ro requires a path");
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                };
                flags.ro.push(value);
                idx += 2;
            }
            "--rw" => {
                let value = match args.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!("mysbx run: --rw requires a path");
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                };
                flags.rw.push(value);
                idx += 2;
            }
            "--session" => {
                // workspace.md D1: the same flag after the verb, the
                // same position rule as `--ro`/`--rw` (D10/D16) — one
                // rule for every run-scoped flag, both spellings one
                // run. The grammar refusal is identical to the one
                // before the verb.
                if flags.session.is_some() {
                    eprintln!("mysbx run: repeated flag: --session");
                    eprintln!("usage: {RUN_USAGE}");
                    return 2;
                }
                let value = match args.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!("mysbx run: --session requires a name");
                        eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                        eprintln!("usage: {RUN_USAGE}");
                        return 2;
                    }
                };
                if !session::valid_name(&value) {
                    eprintln!("mysbx run: --session: invalid session name `{value}`");
                    eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                    eprintln!("usage: {RUN_USAGE}");
                    return 2;
                }
                flags.session = Some(value);
                idx += 2;
            }
            "--" => {
                idx += 1;
                break;
            }
            other => {
                eprintln!("mysbx run: unexpected argument: {other}");
                eprintln!("usage: {RUN_USAGE}");
                return 2;
            }
        }
    }
    let cmd = &args[idx..];
    if cmd.is_empty() {
        eprintln!("mysbx run: no command given after `--`");
        eprintln!("usage: {RUN_USAGE}");
        return 2;
    }
    // A timeout without the result mode is the one combination the
    // flags allow syntactically but cannot act on: a plain run ends
    // in an `exec`, there is no mysbx left to enforce a budget. It
    // is a usage error (the command line promises a behaviour no
    // form of this invocation has), not a runtime failure.
    if flags.timeout.is_some() && !flags.result {
        eprintln!("mysbx run: --timeout is not valid without --result");
        eprintln!(
            "  a plain run ends in an exec — the payload's code propagates and no budget can apply"
        );
        eprintln!("usage: {RUN_USAGE}");
        return 2;
    }
    let mode = if flags.result {
        RunMode::Result
    } else {
        RunMode::Exec
    };
    sandbox(flags, bwrap::Payload::Command(cmd.to_vec()), mode)
}

/// The usage line every `run` error prints — the run flags of D8's
/// extension included, so an operator reading a refusal sees the full
/// accepted set without opening the help (the same pairing rule
/// usage.txt follows, D5).
const RUN_USAGE: &str =
    "mysbx run [--dry-run] [--verbose] [--result] [--timeout <seconds>] [--backend <name>] [--session <name>] [--ro <path>]... [--rw <path>]... -- COMMAND...";

/// How a sandbox run hands the terminal — and the exit code — over
/// (cli.md D8/D17): exec, or wait-and-record.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RunMode {
    /// The default: `exec` the backend argv, the payload's own exit
    /// code propagates unchanged.
    Exec,
    /// `run --result`: spawn the backend, wait (bounded by
    /// `Flags::timeout`), record the outcome in the sidecar's
    /// `result.json` and exit by the interpreted contract.
    Result,
}

/// `mysbx gui ARG...` — start the terminal emulator and run `mysbx`
/// (the same executable) inside it, from the current directory, with
/// `ARG...` passed verbatim (cli.md D15).
///
/// The argv built here is the TERMINAL's, not the sandbox's: the inner
/// `mysbx` builds the sandbox argv itself, so the outer invocation takes
/// no global flags — a `--dry-run` before `gui` is rejected by the
/// dispatcher, and everything after the verb is passed through
/// unparsed (the same rule `--` gives `run`, D4 — including flags the
/// inner invocation understands, like `--multiplexer`).
///
/// The terminal is pinned by the Nix wrapper as `MYSBX_TERMINAL`
/// (the `alacritty` of `myconfig.ai.mysbx.terminal.package`); the
/// `alacritty` fallback keeps a plain `cargo run` working unwrapped,
/// like `MYSBX_BWRAP`'s `bwrap` fallback. `--working-directory` and
/// `--command` are alacritty's own options — the terminal is a GUI
/// program, so this is one place the crate knowingly names another
/// program's command line rather than re-exec'ing itself.
///
/// The `gui` form waits for neither the sandbox nor the terminal: it
/// forks ([`gui_detached`]) and the parent returns as soon as the
/// window was started — detached like `mysbx gui & disown`. Only a
/// terminal that cannot be started at all is reported synchronously
/// (`1`), naming it; one that starts and fails afterwards is as silent
/// as the `& disown` form.
fn gui(flags: Flags, args: &[String]) -> i32 {
    // The dispatcher's `gui` arm sits BEFORE the generic `flags.any()`
    // arm on purpose: `gui` owns its refusal message, because unlike
    // `init`/`edit` there IS a position where the flags are valid —
    // after the verb, passed to the inner invocation. The hint says
    // so instead of the generic "not valid with `gui`" wording.
    if flags.any() {
        eprintln!(
            "mysbx gui: {} is not valid before `gui`",
            flags.first_name()
        );
        eprintln!("  pass flags after the verb instead: `mysbx gui --dry-run`");
        eprintln!("usage: mysbx gui [ARG...] — start mysbx in a terminal window");
        return 2;
    }
    let terminal = env_or("MYSBX_TERMINAL", "alacritty");
    // The inner mysbx is THIS mysbx: `current_exe` (never argv[0], which
    // a wrapper or symlink can change to name something else) resolves
    // the real binary — under Nix, the wrapped store path with its
    // `MYSBX_*` pins, so the sandbox the window opens is the one the
    // operator configured.
    let self_exe = match std::env::current_exe() {
        Ok(p) => p.to_string_lossy().into_owned(),
        Err(e) => {
            eprintln!("mysbx gui: cannot locate my own executable: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    let cwd = match std::env::current_dir() {
        Ok(d) => d.to_string_lossy().into_owned(),
        Err(e) => {
            eprintln!("mysbx gui: cannot determine current directory: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    let mut cmd = std::process::Command::new(&terminal);
    cmd.arg("--working-directory")
        .arg(&cwd)
        .arg("--command")
        .arg(&self_exe)
        .args(args)
        // Detached from THIS terminal: the window opens on the desktop,
        // not as a child of the shell the command was typed in. The
        // inner mysbx is alacritty's child, and inherits its env — which
        // is what passes the wrapper's `MYSBX_*` pins through.
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    gui_detached(cmd, &terminal)
}

/// The one-byte verdicts [`gui_detached_child`] sends its parent through
/// the report pipe: the terminal was started, or a failure whose message
/// follows the byte until EOF.
const GUI_STARTED: u8 = 0;
const GUI_FAILED: u8 = 1;

/// The detach half of `mysbx gui` (cli.md D15): fork the terminal off the
/// invoking shell, exactly what `mysbx gui & disown` would give.
///
/// The parent half returns as soon as it knows whether the terminal was
/// STARTED — the shell gets its prompt back with the window open, not
/// when the window closes. The forked half ([`gui_detached_child`])
/// becomes a session leader (`setsid`, so no controlling terminal is
/// inherited), ignores `SIGHUP` (a terminal closing under the old session
/// cannot take the window with it), points its stdin/stdout/stderr at
/// `/dev/null` (the descriptors of the invoking terminal are released)
/// and then waits for the terminal the way the foreground process did
/// before the detach — the background job the shell never has to reap.
///
/// The pipe carries the launch outcome to the parent, because that one
/// failure must stay synchronous: a `$MYSBX_TERMINAL` that cannot be
/// executed is a runtime failure (`1`) naming it, reported on the
/// invoking shell's stderr. A terminal that STARTS and fails afterwards
/// (no Wayland socket, say) is silent — nobody waits for its status,
/// the same as the `& disown` form.
fn gui_detached(cmd: std::process::Command, terminal: &str) -> i32 {
    use std::io::Read;
    use std::os::unix::net::UnixStream;

    // Both opened BEFORE the fork, so the child needs nothing from the
    // allocator until its process context is reorganized — the standard
    // daemonization discipline.
    let (mut parent_end, child_end) = match UnixStream::pair() {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("mysbx gui: cannot detach: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    let null = match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open("/dev/null")
    {
        Ok(f) => f,
        Err(e) => {
            eprintln!("mysbx gui: cannot open /dev/null: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    match unsafe { libc_fork() } {
        0 => unsafe { gui_detached_child(cmd, terminal, child_end, null) },
        -1 => {
            eprintln!("mysbx gui: cannot fork to detach");
            EXIT_INFRASTRUCTURE
        }
        _ => {
            // The shell's half: NOT waiting for the child is the point —
            // it is orphaned here and reaped by init when the window
            // closes. Close OUR copy of the child's end first, so the
            // message read below sees EOF as soon as the child is done.
            drop(child_end);
            let mut status = [0u8; 1];
            match parent_end.read(&mut status) {
                Ok(1) if status[0] == GUI_STARTED => 0,
                Ok(1) => {
                    let mut msg = String::new();
                    let _ = parent_end.read_to_string(&mut msg);
                    eprintln!("mysbx gui: {}", msg.trim_end());
                    EXIT_INFRASTRUCTURE
                }
                // The child writes exactly one byte before anything
                // else; a clean EOF means it died before it could
                // report, i.e. the terminal was never started.
                Ok(_) => {
                    eprintln!(
                        "mysbx gui: the detached starter died before the terminal was started"
                    );
                    EXIT_INFRASTRUCTURE
                }
                Err(e) => {
                    eprintln!("mysbx gui: cannot detach: {e}");
                    EXIT_INFRASTRUCTURE
                }
            }
        }
    }
}

/// The detached half of [`gui_detached`], running in the forked child.
/// Never returns: it reports the launch outcome through `report`, waits
/// for the terminal (the window's lifetime is the starter's) and exits —
/// its status is observed by nobody, which is the detach.
unsafe fn gui_detached_child(
    mut cmd: std::process::Command,
    terminal: &str,
    mut report: std::os::unix::net::UnixStream,
    null: std::fs::File,
) -> ! {
    use std::io::Write;
    use std::os::unix::io::AsRawFd;

    // A new session drops the controlling terminal. `setsid` only
    // fails for a process group leader, which a fresh fork never is;
    // the SIGHUP ignore below is the second line of defense anyway.
    libc_setsid();
    // Survive the closing of the terminal the command was typed in.
    libc_signal_ignore(SIGHUP);
    // The report write below must not kill us if the parent is gone
    // already (the default action on a broken pipe would).
    libc_signal_ignore(SIGPIPE);
    // Release the invoking terminal's descriptors: the starter holds
    // no fd of the shell's terminal open, and the wait below can never
    // block on an interactive stream.
    let null_fd = null.as_raw_fd();
    libc_dup2(null_fd, 0);
    libc_dup2(null_fd, 1);
    libc_dup2(null_fd, 2);
    match cmd.spawn() {
        Ok(mut child) => {
            // The window is up: tell the parent, then keep waiting —
            // reaping the terminal is this process's one job, and it
            // makes the starter live exactly as long as the window,
            // the process `& disown` would have backgrounded.
            let _ = report.write_all(&[GUI_STARTED]);
            drop(report);
            let _ = child.wait();
            std::process::exit(0);
        }
        Err(e) => {
            let _ = report.write_all(&[GUI_FAILED]);
            let _ = report.write_all(format!("cannot start {terminal}: {e}").as_bytes());
            std::process::exit(1);
        }
    }
}

// The four libc calls of the `gui` detach, as raw externs in the
// zero-dependency style of main.rs's `signal(2)`: the crate is
// dependency-free by design (Cargo.toml), and none of these needs a
// type beyond plain integers. The signal numbers are the usual Linux
// ones (the crate targets NixOS/Linux only).
const SIGHUP: i32 = 1;
const SIGPIPE: i32 = 13;
const SIG_IGN: usize = 1;

unsafe fn libc_fork() -> i32 {
    extern "C" {
        fn fork() -> i32;
    }
    fork()
}

unsafe fn libc_setsid() -> i32 {
    extern "C" {
        fn setsid() -> i32;
    }
    setsid()
}

unsafe fn libc_signal_ignore(signum: i32) {
    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    signal(signum, SIG_IGN);
}

unsafe fn libc_dup2(from: i32, to: i32) {
    extern "C" {
        fn dup2(oldfd: i32, newfd: i32) -> i32;
    }
    dup2(from, to);
}

/// The real effective UID (libc `geteuid`) — the same zero-dependency
/// raw extern as the `gui` detach block above; the gvisor tier's
/// state.rs uses the identical idiom.
unsafe fn libc_geteuid() -> u32 {
    extern "C" {
        fn geteuid() -> u32;
    }
    geteuid()
}

/// Whether stdin is a terminal (libc `isatty`) — decides the podman
/// backend's `--tty` (bd myconfig-jho). The same zero-dependency raw
/// extern idiom as `libc_geteuid`.
fn stdin_is_tty() -> bool {
    extern "C" {
        fn isatty(fd: i32) -> i32;
    }
    // SAFETY: `isatty` only inspects the fd, it has no side effects.
    // The return is nonzero on a terminal — not specified to be 1.
    unsafe { isatty(0) != 0 }
}

/// The shared pipeline of the bare form and `run`: resolve the repo, run
/// the guards, require an initialized sidecar, load and merge both layers,
/// check the backend, build the argv — then print it (`--dry-run`) or exec
/// it.
///
/// Nothing here creates the sidecar (cli.md D13): a run that finds no
/// sidecar config fails with the `mysbx init` hint.
fn sandbox(flags: Flags, payload: bwrap::Payload, mode: RunMode) -> i32 {
    let dry_run = flags.dry_run;
    // workspace.md D4: `--rw` in a clone run is a REFUSED run (exit
    // `70`, cli.md D8) naming the flag and the mode — never a silent
    // downgrade: an operator who believes a directory is writable
    // while the payload meets `EROFS` is the worse failure. The
    // refusal fires before anything else of the pipeline: the flag
    // combination is wrong as a unit, no matter what the layers or
    // the filesystem say. `--ro` behaves as usual.
    if flags.session.is_some() && !flags.rw.is_empty() {
        eprintln!(
            "mysbx: --rw is refused in a clone run (workspace.md D4): every mount is forced read-only, the session clone is the only writable bind"
        );
        eprintln!("  use --ro instead, or run without --session for the live repo");
        return EXIT_INFRASTRUCTURE;
    }
    // The `--multiplexer` override (cli.md D14): the run flag wins over
    // the merged `multiplexer` of the layers, per the precedence of D6
    // (flags > sidecar > user > defaults). Applied AFTER the merge so
    // the report and the argv below see one effective value — and only
    // for the interactive payload it can select (D11): a `run -- CMD`
    // never starts a session, and the dispatcher already refuses the
    // combination for `run`, so the guard here is structural, not a
    // second parser.
    let cli_mux = if let bwrap::Payload::Shell = payload {
        flags.multiplexer
    } else {
        None
    };
    // 1. repo resolution and guards (docs/TODOs/mvp-2-repo-discovery.md).
    // The guard runs BEFORE anything is created, so a `$HOME`-resolved run
    // never even creates a sidecar on disk.
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    // 1a. the session of a clone run (workspace.md D1/D2): the named
    // clone and its derived paths, from the resolved repo. A run
    // without `--session` never constructs one and stays in the
    // live mode — the default that D1 leaves untouched.
    let session = flags
        .session
        .as_deref()
        .map(|name| session::Session::new(&repo, name));

    // 2. the sidecar must already exist (cli.md D13). A run — real or
    // dry — creates nothing: `mysbx init` is the one command that
    // writes to the host filesystem here. The check runs for
    // `--dry-run` too, so the argv a dry run prints is always the argv
    // of a run that could actually happen.
    if let Err(msg) = require_initialized_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return EXIT_INFRASTRUCTURE;
    }
    // Always true past the check; kept as the value the report shows so
    // the report keeps describing what the run FOUND.
    let sidecar_existed = repo.sidecar.is_dir();

    // 3. both layers, merged (docs/TODOs/mvp-3-layer-merge.md). Merge
    // errors (broken paths, unparseable files, an `[env]` override) are
    // runtime failures: the full validation still runs under --dry-run —
    // a dry run that skipped it would exercise the wrong function.
    let home = std::env::var_os("HOME").unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    let layers =
        match merge::load_layers(std::path::Path::new(&home), xdg.as_deref(), &repo.sidecar) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("mysbx: {e}");
                return EXIT_INFRASTRUCTURE;
            }
        };
    // Kept for the report before the configs are consumed by the merge:
    // which files were loaded, and how many mounts the user layer
    // contributed (the merge keeps them first and in order, so this one
    // number attributes every merged mount to its layer).
    let user_config_path = layers.user.1.clone();
    let sidecar_config_path = layers.sidecar.1.clone();
    let user_config_exists = user_config_path.exists();
    let sidecar_config_exists = sidecar_config_path.exists();
    let user_mount_count = layers.user.0.mounts.len();
    // `home` is also what `~/…` mount paths expand against (config.md
    // D8) — on the HOST, before bwrap runs, so the sandbox's own
    // (cleared) environment never enters the resolution.
    let mut merged = match merge::merge(
        layers.user.0,
        layers.sidecar.0,
        &layers.user.1,
        &layers.sidecar.1,
        std::path::Path::new(&home),
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };

    let mut cli_set_backend = false;
    // 3a: the `--backend` override (cli.md D18, bd myconfig-veg): the
    // flag wins over the merged `backend` of both layers, the same
    // precedence every flag has (D6: flags > sidecar > user >
    // defaults) — the CLI is the outermost layer for this key. The
    // layers stay untouched: the override lives for THIS run only,
    // and the next `mysbx` uses the configuration again. The run is
    // refused HERE, before the session clone (step 4a, which a
    // broken configuration must not precede) and before the argv:
    // the accepted set is the pipeline's (step 4's), the source a
    // flag is not — so the refusal names the flag and lists the
    // valid values (D8: the command line was fine, the backend it
    // names does not exist).
    if let Some(name) = &flags.backend {
        if !matches!(name.as_str(), "bubblewrap" | "podman-gvisor") {
            eprintln!(
                "mysbx: unknown backend `{name}` (from --backend) — available: `bubblewrap`, `podman-gvisor`"
            );
            return EXIT_INFRASTRUCTURE;
        }
        merged.backend = Some(name.clone());
        cli_set_backend = true;
    }

    // 3a. the `--multiplexer` override (cli.md D14): the flag wins over
    // the merged `multiplexer` of both layers, the same precedence every
    // flag has (D6: flags > sidecar > user > defaults). The layers stay
    // untouched — the override lives for THIS run only, and the next
    // `mysbx` starts whatever the configuration says again. The value
    // was validated by the parser (one of `Multiplexer::NAMES`), so it
    // can be assigned directly.
    if let Some(mux) = cli_mux {
        merged.multiplexer = mux;
    }

    // 3aa. the `--ro`/`--rw` additions (cli.md D16): appended to the
    // merged mounts, after every configured entry — every `--ro`
    // addition before every `--rw` one, the values of each flag in
    // the order they were given, so `--rw` wins a same-path tie no
    // matter the typing order. Mount order is
    // argv order and a later bind wins inside the sandbox, so the
    // flags that override nothing a layer decided and apply on top
    // of everything are the LAST binds — the same precedence D6
    // gives every flag (flags > sidecar > user > defaults). Like a
    // `[[mounts]]` path each value is resolved and canonicalized
    // eagerly (D8): `~/…` against `$HOME`, a relative path against
    // the cwd (a config file resolves against its own directory; the
    // command line's "own directory" is the one it was typed in),
    // and a path that does not exist is a runtime failure — bwrap
    // would refuse it anyway, and the run that names the exact
    // spelling is the honest diagnosis. The home-exposure refusal
    // (review-3 item 4) applies too: the flags may not re-expose the
    // host home any more than a layer entry may.
    if !flags.ro.is_empty() || !flags.rw.is_empty() {
        let cwd = match std::env::current_dir() {
            Ok(d) => d,
            Err(e) => {
                eprintln!("mysbx: cannot determine the current directory: {e}");
                return EXIT_INFRASTRUCTURE;
            }
        };
        for (flag, values, mode) in [
            ("--ro", &flags.ro, config::Mode::Ro),
            ("--rw", &flags.rw, config::Mode::Rw),
        ] {
            for raw in values {
                let canon =
                    match merge::resolve_cli_path(raw, flag, std::path::Path::new(&home), &cwd) {
                        Ok(p) => p,
                        Err(e) => {
                            eprintln!("mysbx: {e}");
                            return EXIT_INFRASTRUCTURE;
                        }
                    };
                merged.mounts.push(config::Mount {
                    path: canon.to_string_lossy().into_owned(),
                    dest: None,
                    mode,
                });
            }
        }
    }
    // How many of the merged mounts came from the command line — the
    // report's provenance cut, one past the last configured entry.
    let cli_mount_count = flags.ro.len() + flags.rw.len();

    // 3b. the state-dir backing stores (docs/design/config.md D15):
    // one directory per merged `state-dirs` entry under
    // `<sidecar>/state/`, so the rw binds of the argv have an existing
    // source (bwrap needs one) and the payload's writes persist there
    // across runs. After the merge (the entries come from both layers),
    // before the backend check (a broken state dir is as fatal as a
    // broken config), and only for a real run — `--dry-run` stays
    // side-effect-free and prints the argv with the would-be sources.
    // Creation is idempotent; a failure is a runtime error like the
    // sidecar creation above.
    //
    // In a CLONE run the `state-dirs` are NOT handled at all
    // (workspace.md D4): no backing store is created, nothing is
    // bound — per-session versus shared agent state is deliberately
    // deferred to a follow-up bead. Scratch space stays the tmpfs
    // home and `/tmp`, as in any run.
    if !dry_run && session.is_none() {
        if let Err(msg) = ensure_state_dirs(&repo, &merged.state_dirs) {
            eprintln!("mysbx: {msg}");
            return EXIT_INFRASTRUCTURE;
        }
    }

    // 4. the backend is explicit, never auto-detected (cli.md D7): a
    // silently downgraded isolation level would be a security bug.
    // The accepted set is enforced here for BOTH sources — a config
    // layer and `--backend` (cli.md D18) put the value into
    // `merged.backend` alike; the flag's own refusal one merge above
    // echoed it early, this arm is the authoritative one.
    let backend = match merged.backend.as_deref() {
        Some("bubblewrap" | "podman-gvisor") => merged.backend.as_deref().unwrap(),
        Some(other) => {
            eprintln!(
                "mysbx: unsupported backend `{other}` — available: `bubblewrap`, `podman-gvisor`"
            );
            return EXIT_INFRASTRUCTURE;
        }
        None => {
            eprintln!(
                "mysbx: no backend configured — set `backend = \"bubblewrap\"` or `backend = \"podman-gvisor\"` in the user or sidecar config"
            );
            return EXIT_INFRASTRUCTURE;
        }
    };

    // 4a. the session clone (workspace.md D2): the FIRST `--session`
    // run creates it — the one deliberate exception to "a run creates
    // nothing" (cli.md D13), because `--session NAME` is an explicit
    // operator decision that names the thing to be created, like
    // `init` names the sidecar. It creates exactly one thing, at a
    // derived path, reported before the run starts. The refusals
    // name the offending fact (D2): an empty host repo has nothing
    // to clone; a host branch literally named `agent` collides with
    // the reserved `refs/heads/agent/mysbx/*` namespace.
    //
    // `--dry-run` prints the exact `git` commands instead — side
    // effect-free like everything else it prints (cli.md D9), in the
    // same one-argument-per-line format, before the bwrap argv — so
    // the dry run of a first session audits the creation too.
    // Checked AFTER the merge and the backend check, so a broken
    // configuration creates nothing.
    if let Some(session) = &session {
        match session.plan(&repo) {
            session::Plan::Exists => {}
            session::Plan::Create(steps) => {
                if dry_run {
                    session::print_steps(&steps);
                } else if let Err(msg) = session::execute(session, &steps) {
                    eprintln!("mysbx: {msg}");
                    return EXIT_INFRASTRUCTURE;
                }
            }
            session::Plan::EmptyHostRepo => {
                eprintln!(
                    "mysbx: cannot create the session clone: {} has no commits — there is nothing to clone (workspace.md D2)",
                    repo.root.display()
                );
                return EXIT_INFRASTRUCTURE;
            }
            session::Plan::AgentBranch => {
                eprintln!(
                    "mysbx: cannot create the session clone: {} has a branch named `agent`, which collides with the reserved agent/mysbx/* namespace of the session branches (workspace.md D2)",
                    repo.root.display()
                );
                return EXIT_INFRASTRUCTURE;
            }
        }
    }

    // 5. the argv — its single source is `bwrap::bwrap_argv` (mvp-4).
    let host_env = collect_host_env();
    // Without the Nix wrapper (item 6 pins MYSBX_SHELL and
    // MYSBX_TOOLS_PATH) the fallbacks are silent and deliberate: /bin/sh
    // for the interactive shell, /usr/bin (a base-bound path) for PATH —
    // so a plain `cargo run` still produces a sane argv. An empty value
    // counts as unset (the same treatment XDG_CONFIG_HOME gets in
    // merge.rs): `MYSBX_BWRAP=""` must not become `Command::new("")`.
    let shell = env_or("MYSBX_SHELL", "/bin/sh");
    let tools_path = env_or("MYSBX_TOOLS_PATH", "/usr/bin");
    // The sanitized nix client configuration (review-2 item 3). There
    // is no fallback on purpose: unset means "bind no nix.conf", never
    // "bind the host's" — that file may carry access-tokens, and a
    // read-only bind hands them to the payload all the same.
    let nix_conf = env_opt("MYSBX_NIX_CONF");
    // The multiplexer entry (docs/design/config.md D17): the
    // interactive payload of a run that selected one. Exactly the pin
    // of the SELECTED multiplexer is read (`entry_var`), so a host
    // that carries workmux but not herdr cannot accidentally start
    // the wrong payload. No fallback either — unset means "this build
    // has no such integration", and the argv builder refuses the run
    // instead of quietly starting a plain shell where a session was
    // asked for.
    let mux_entry = merged.multiplexer.entry_var().and_then(env_opt);
    // `/bin/sh` for the sandbox (see [`bwrap::Params::bin_sh`]):
    // a pin like `MYSBX_NIX_CONF` — unset means "no `/bin/sh` bind",
    // never "the host's" (a host `/bin/sh` is outside mysbx's own
    // closure, so what it resolves to is not reproducible). The Nix
    // wrapper pins bash's `bin/sh`; an unwrapped build runs without
    // the bind, and tmux `run-shell` jobs & co. then fail like they
    // did before the pin existed.
    let bin_sh = env_opt("MYSBX_BINSH");
    // The pinned CA bundle (bd myconfig-938), a pin like
    // `MYSBX_NIX_CONF`: unset means "no env variables are set", never
    // "point at the host's" — the resolver binds of `/etc/ssl` +
    // `/etc/static` carry the host's bundle already, and inventing a
    // path here would point every `SSL_CERT_FILE`-honoring tool at a
    // nonexistent file. The Nix wrapper pins `nss-cacert`'s
    // `ca-bundle.crt` from mysbx's own closure, so the sandbox's TLS
    // trust anchors are reproducible and independent of the host's
    // `/etc` layout.
    let ca_bundle = env_opt("MYSBX_CA_BUNDLE");
    // The podman-gvisor backend's own payload pins (bd
    // myconfig-wao): the container mounts NOTHING from the host
    // `/nix/store`, so the host pins above cannot serve as its
    // payload — the image's own userland does. The defaults are the
    // gVisor agent image's OCI config (agent-image.nix):
    // `/bin/bash` (`Cmd`) and `/bin:/usr/bin` (`Env`), the same
    // userland agent-gvisor sessions run against. Both are
    // operator-overridable per invocation for other images.
    let gvisor_shell = env_or("MYSBX_GVISOR_SHELL", "/bin/bash");
    let gvisor_tools_path = env_or("MYSBX_GVISOR_TOOLS_PATH", "/bin:/usr/bin");
    // Review-3 item 3: the trusted policy files of THIS run, handed to
    // the argv builder so it can refuse any `rw` bind that would expose
    // one to the payload.
    //
    // Only files that exist are named: an absent file granted nothing,
    // so exposing its would-be location writes nothing this run — and
    // the check is not thereby bypassable, because the run that FOLLOWS
    // a payload-created `config.toml` sees an existing file and refuses
    // the very `rw` source that allowed creating it. Steering the next
    // run requires the next run to launch with the policy writable —
    // and that is exactly what this guard forbids.
    //
    // Each path is walked, not merely canonicalized (review-4 item 1):
    // the resolved target AND every directory entry the next run
    // traverses to find it are protected — see [`trusted_policy`] and
    // [`bwrap::PolicyPath`]. Canonicalizing alone would protect the
    // Nix-store target of a Home-Manager-generated user config while
    // leaving the symlink that names it replaceable.
    let policy_paths: Vec<bwrap::PolicyPath> = [
        (user_config_exists, &user_config_path),
        (sidecar_config_exists, &sidecar_config_path),
    ]
    .into_iter()
    .filter(|(exists, _)| *exists)
    .map(|(_, path)| trusted_policy(path))
    .collect();
    // Build argv based on backend
    // Also determine workspace for the report
    let workspace = match &session {
        Some(s) => bwrap::Workspace::Clone { clone: &s.clone },
        None => bwrap::Workspace::Live,
    };
    // The report renders the common run parameters (shell, tool PATH,
    // pins) through the bwrap-shaped `Params` — every backend shares
    // those fields, so one struct serves both, and the backend-specific
    // extras (the container image) travel as separate fields below.
    // Under podman-gvisor those fields carry the backend's IMAGE pins
    // (bd myconfig-wao), not the bwrap host pins: the report must not
    // describe a shell/PAYH the argv never sets, and its `nix.conf` /
    // `/bin/sh` / `ca-bundle` absence lines would be false inside an
    // image that provides all three. The multiplexer pin is the same
    // `None` the argv builder gets.
    let (report_shell, report_tools_path, report_mux_entry) = if backend == "podman-gvisor" {
        (gvisor_shell.clone(), gvisor_tools_path.clone(), None)
    } else {
        (shell.clone(), tools_path.clone(), mux_entry.clone())
    };
    let report_params = bwrap::Params {
        shell: &report_shell,
        tools_path: &report_tools_path,
        bin_sh: if backend == "podman-gvisor" {
            None
        } else {
            bin_sh.as_deref()
        },
        nix_conf: if backend == "podman-gvisor" {
            None
        } else {
            nix_conf.as_deref()
        },
        ca_bundle: if backend == "podman-gvisor" {
            None
        } else {
            ca_bundle.as_deref()
        },
        policy_paths: &policy_paths,
        mux_entry: report_mux_entry.as_deref(),
        workspace: workspace.clone(),
    };
    let (backend_bin, argv, image) = match backend {
        "bubblewrap" => {
            let params = bwrap::Params {
                shell: &shell,
                tools_path: &tools_path,
                bin_sh: bin_sh.as_deref(),
                nix_conf: nix_conf.as_deref(),
                ca_bundle: ca_bundle.as_deref(),
                policy_paths: &policy_paths,
                mux_entry: mux_entry.as_deref(),
                workspace: workspace.clone(),
            };
            let argv = match bwrap::bwrap_argv(&merged, &repo, &payload, &host_env, &params) {
                Ok(a) => a,
                Err(e) => {
                    eprintln!("mysbx: {e}");
                    return EXIT_INFRASTRUCTURE;
                }
            };
            let bwrap_bin = env_or("MYSBX_BWRAP", "bwrap");
            (bwrap_bin, argv, None::<String>)
        }
        "podman-gvisor" => {
            use std::borrow::Cow;

            // The image reference the runs use — the same pin
            // gvisor-load-image loads (MYSBX_GVISOR_IMAGE, set by the Nix
            // wrapper when the host builds a gVisor agent image; see
            // loadimage.rs). No fallback: an invented `localhost/…` ref
            // would run a nonexistent image and mislead the operator
            // (bd myconfig-xrt).
            let Some(gvisor_image) = env_opt("MYSBX_GVISOR_IMAGE") else {
                eprintln!("mysbx: podman-gvisor: no container image configured");
                eprintln!(
                    "  the Nix wrapper pins MYSBX_GVISOR_IMAGE when the host \
                     builds a gVisor agent image; an unwrapped build sets none"
                );
                eprintln!("  set MYSBX_GVISOR_IMAGE <ref>, or switch backends");
                return EXIT_INFRASTRUCTURE;
            };

            // Runtime flags and cgroup manager, mirroring the gvisor
            // tier's rootless defaults (rust/src/state.rs
            // `Env::from_euid` in ../../sandboxes/
            // myconfig.ai.gvisor-agent-sandbox/): a rootless runsc
            // cannot write the (non-delegated) cgroup of its pod, so
            // it must run with the `ignore-cgroups` runtime flag under
            // the cgroupfs manager — without it, runsc fails with
            // "cannot set up cgroup for root: configuring cgroup: …
            // permission denied" (bd myconfig-b13). A root run
            // configures cgroups normally: no runtime flags, and the
            // cgroup-manager flag omitted (podman's own default,
            // usually systemd, owns the hierarchy). Both are
            // operator-overridable per invocation.
            let rootless = unsafe { libc_geteuid() != 0 };
            let cgroup_manager = match env_opt("MYSBX_GVISOR_CGROUP_MANAGER") {
                Some(manager) => Some(manager),
                None if rootless => Some("cgroupfs".to_owned()),
                None => None,
            };
            let runtime_flags_raw = if rootless {
                env_or("MYSBX_GVISOR_RUNTIME_FLAGS", "ignore-cgroups")
            } else {
                env_opt("MYSBX_GVISOR_RUNTIME_FLAGS").unwrap_or_default()
            };
            let runtime_flags: Vec<String> = runtime_flags_raw
                .split_whitespace()
                .map(|s| s.to_string())
                .filter(|s| !s.is_empty())
                .collect();

            // Resource limits (only applied when cgroups are enabled)
            // Using Cow to handle both borrowed and owned strings
            let pids_limit = env_opt("MYSBX_GVISOR_PIDS_LIMIT").map(Cow::from);
            let memory = env_opt("MYSBX_GVISOR_MEMORY").map(Cow::from);
            let cpus = env_opt("MYSBX_GVISOR_CPUS").map(Cow::from);

            // Cgroups handling: when ignore-cgroups flag is set, skip resource limits
            let ignore_cgroups = runtime_flags.iter().any(|f| f == "ignore-cgroups");
            if ignore_cgroups && (pids_limit.is_some() || memory.is_some() || cpus.is_some()) {
                eprintln!(
                    "mysbx: warning: memory/cpu/pids limits not enforced, \
                     the runtime ignores cgroups"
                );
            }

            // Network spec: explicit "none" when network is denied, otherwise podman default (shared)
            let pasta_spec = env_opt("MYSBX_GVISOR_PASTA_SPEC");
            let network_spec: Option<&str> = if !merged.network {
                Some("none")
            } else {
                pasta_spec.as_deref()
            };

            // The backend's payload pins are the image's own userland
            // (bd myconfig-wao), never the host store paths of the
            // bwrap pins: `MYSBX_SHELL` & co. name host `/nix/store`
            // paths this backend deliberately does not mount, so the
            // container would die with `no such file or directory` on
            // the first run. `MYSBX_MUX_ENTRY_*` likewise stays out: a
            // host store entry script cannot be the payload here —
            // until an image ships one, a selected multiplexer is
            // refused by the argv builder
            // (`MultiplexerUnavailable`), the same refusal a bwrap host
            // without that multiplexer gets.
            let params = podman_gvisor::Params {
                shell: &gvisor_shell,
                tools_path: &gvisor_tools_path,
                policy_paths: &policy_paths,
                mux_entry: None,
                workspace: match &session {
                    Some(s) => crate::bwrap::Workspace::Clone { clone: &s.clone },
                    None => crate::bwrap::Workspace::Live,
                },
                // Stdio wiring (bd myconfig-jho): the backend is exec'd
                // with mysbx's own stdio, so the container must be
                // attached — `--interactive` always (a one-shot
                // `run -- CMD` may read piped stdin too), `--tty`
                // only when stdin is a real terminal, so a piped
                // one-shot is not forced onto a pty.
                interactive: true,
                tty: stdin_is_tty(),
                // Podman-gvisor specific params
                image: &gvisor_image,
                runtime_flags: &runtime_flags,
                cgroup_manager: cgroup_manager.as_deref(),
                ignore_cgroups,
                network_spec,
                pids_limit,
                memory,
                cpus,
            };
            let argv = match podman_gvisor::podman_run_argv(
                &merged, &repo, &payload, &host_env, &params,
            ) {
                Ok(a) => a,
                Err(e) => {
                    eprintln!("mysbx: {e}");
                    return EXIT_INFRASTRUCTURE;
                }
            };
            let podman_bin = env_or("MYSBX_PODMAN", "podman");
            (podman_bin, argv, Some(gvisor_image))
        }
        _ => unreachable!(),
    };

    // 6. the `--verbose` report (cli.md D10), BEFORE the argv and before
    // the exec: it describes the run that is about to happen, and every
    // line is `## `-prefixed so the unprefixed argv block below stays
    // byte-identical to a plain `--dry-run`.
    if flags.verbose {
        for line in report::lines(&report::Report {
            repo: &repo,
            sidecar_exists: sidecar_existed,
            user_config: &user_config_path,
            user_config_exists,
            sidecar_config: &sidecar_config_path,
            sidecar_config_exists,
            merged: &merged,
            backend_from_cli: cli_set_backend,
            user_mount_count,
            cli_mount_count,
            host_env: &host_env,
            params: &report_params,
            bwrap_bin: &backend_bin,
            backend,
            image: image.as_deref(),
            payload: &payload,
            dry_run,
            result: mode == RunMode::Result,
        }) {
            println!("{line}");
        }
    }

    // 7. print the argv, or exec it.
    if dry_run {
        // argv[0] first, then one argument per line, no prefix, no
        // quoting: this is the *result*, not a diagnostic (cli.md D9),
        // and the executable is part of what `--dry-run` audits — the
        // packaging definition of done requires the wrapped store path
        // as argv[0], which was invisible before (review-1 finding 7:
        // `MYSBX_BWRAP` was read only after the early return). Golden
        // tests compare bytes and `mysbx run --dry-run -- ls | wc -l`
        // stays meaningful — one line more.
        println!("{backend_bin}");
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }

    // 7a. under `--verbose` the exact executed command is part of the
    // report (bd myconfig-jho): the argv block above is dry-run-only,
    // so a real verbose run printed every *configuration* line but
    // never the argv it actually execs — and an operator reproducing
    // a failed run by hand had to guess it. One `## `-prefixed line
    // per argument, argv[0] first, the same shape the report uses, so
    // `grep -v '^## '` keeps stripping it too.
    if flags.verbose {
        println!("## exec: {backend_bin}");
        for arg in &argv {
            println!("## arg:  {arg}");
        }
    }

    let mut cmd = std::process::Command::new(&backend_bin);
    cmd.args(&argv);
    match mode {
        RunMode::Exec => {
            // `exec` replaces this process on success, so the payload's
            // exit code propagates unchanged (cli.md D8); the call only
            // returns on failure, with the error as its return value.
            // podman's own stderr/stdout/stdin are INHERITED by the
            // exec, so a failing podman prints its own message and
            // exit code surfaces unchanged — nothing is swallowed
            // here (the f13 silent exit was the *payload* exiting on
            // EOF, not a lost podman error, bd myconfig-jho).
            use std::os::unix::process::CommandExt;
            let e = cmd.exec();
            eprintln!("mysbx: cannot exec {backend_bin}: {e}");
            EXIT_INFRASTRUCTURE
        }
        RunMode::Result => run_with_result(
            cmd,
            &repo,
            &payload,
            flags.timeout,
            // workspace.md D5: a `--result` clone run writes its
            // outcome to the per-session `clones/NAME.json`, so
            // parallel sessions cannot overwrite each other's
            // results; a live run keeps `<sidecar>/result.json`.
            session.as_ref().map(|s| s.result.clone()),
        ),
    }
}

/// The waited half of a `--result` run (cli.md D8/D17, bd
/// myconfig-0ql): start the backend as a child, wait for its outcome
/// — bounded by `timeout_secs` when given — record that outcome in
/// the sidecar's `result.json` and exit by the interpreted
/// contract. The backend inherits mysbx's stdin/stdout/stderr, so a
/// payload's own output and its terminal stay exactly what a plain
/// run gives it; only WHO outlives the payload differs.
///
/// Cancellation (SIGINT/SIGTERM) is mysbx's own answer to Ctrl-C
/// while a run is being waited for: the signal goes to the process
/// GROUP — the backend is in it, a shell payload's children too — so
/// everything dies, the outcome is recorded as `cancelled` with the
/// signal named, and the exit code is the shell's `128 + signum`
/// (130 for SIGINT, 143 for SIGTERM). The record distinguishes the
/// signals a terminal sends (a cancellation) from everything else:
/// anything else is NOT recorded — mysbx dies with the default
/// action and no file is written, exactly like a plain run that is
/// killed mid-exec.
fn run_with_result(
    mut cmd: std::process::Command,
    repo: &repo::Repo,
    payload: &bwrap::Payload,
    timeout_secs: Option<u64>,
    // The per-session result file of a clone run (workspace.md D5),
    // or `None` for the live `<sidecar>/result.json` (cli.md D17).
    result_path: Option<std::path::PathBuf>,
) -> i32 {
    let payload_vec = match payload {
        bwrap::Payload::Command(args) => args.clone(),
        // Unreachable: the dispatcher refuses `--result` for the bare
        // form (D8/D17), so a waited run always has a command payload.
        bwrap::Payload::Shell => Vec::new(),
    };
    let started = std::time::Instant::now();
    let started_at = result::epoch_secs(std::time::SystemTime::now());

    // The signal dance: SIGINT/SIGTERM/SIGALRM are caught with one
    // flag-setting handler — everything else about the outcome is the
    // wait loop's business. `alarm(2)` delivers the timeout budget as
    // SIGALRM, zero dependencies, so raw libc symbols it is (the same
    // idiom main.rs and the `gui` detach use).
    CANCELLED.store(0, std::sync::atomic::Ordering::SeqCst);
    unsafe {
        libc_signal_catch(SIGINT);
        libc_signal_catch(SIGTERM);
        libc_signal_catch(SIGALRM);
        if let Some(secs) = timeout_secs {
            libc_alarm(secs);
        }
    }
    let spawn_failed = |e: std::io::Error| {
        // A backend that never started is an infrastructure error —
        // but it is still recorded: the run DID happen, it failed at
        // the boundary, and a batch driver polling `result.json` for
        // THIS run needs the outcome to exist.
        let record = result::Record {
            state: result::State::InfrastructureError,
            repo: repo.root.to_string_lossy().into_owned(),
            sidecar: repo.sidecar.to_string_lossy().into_owned(),
            payload: payload_vec.clone(),
            started_at,
            finished_at: result::epoch_secs(std::time::SystemTime::now()),
            duration_ms: started.elapsed().as_millis() as u64,
            timeout: timeout_secs,
            payload_exit: None,
            payload_signal: None,
            error: Some(format!("cannot exec the backend: {e}")),
        };
        record_result(&record, repo, result_path.as_deref());
        eprintln!("mysbx: cannot exec the backend: {e}");
        EXIT_INFRASTRUCTURE
    };
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            // The flags must not fire on the NEXT command — a leftover
            // alarm or handler is not its budget.
            unsafe {
                libc_alarm(0);
                libc_signal_restore();
            }
            return spawn_failed(e);
        }
    };

    // The wait loop: poll the child, watch the cancellation flag.
    // SIGALRM (the exhausted budget) sets the flag like a
    // cancellation; the loop cannot tell them apart by flag alone, so
    // the handler records WHICH signal arrived and the state comes
    // from that: SIGALRM → timed-out, SIGINT/SIGTERM → cancelled.
    let mut waited: Option<std::process::ExitStatus> = None;
    let mut signum: i32;
    loop {
        signum = CANCELLED.load(std::sync::atomic::Ordering::SeqCst);
        if signum != 0 {
            break;
        }
        match child.try_wait() {
            Ok(Some(status)) => {
                waited = Some(status);
                break;
            }
            Ok(None) => std::thread::sleep(std::time::Duration::from_millis(50)),
            Err(e) => {
                unsafe {
                    libc_alarm(0);
                    libc_signal_restore();
                }
                return spawn_failed(e);
            }
        }
    }
    // The payload's own fate — threaded as data, not statics: the
    // backend IS the payload process (it execs it inside the
    // sandbox), so its exit status is the record's `payloadExitCode`
    // (or `payloadSignal` when it died by one). Only a run that
    // reached an end on its own has one; a killed run does not — its
    // outcome is mysbx's kill, not the payload's.
    let ended = if signum != 0 {
        // Cancelled or timed out: kill the whole process group — the
        // backend is in it, a shell payload's children too — then
        // reap it, so no sandbox process outlives the budget. KILL,
        // not TERM: a graceful escalation would need a second wait
        // loop with its own budget, and the operator asked for THIS
        // run to end now. The child is reaped BEFORE the state is
        // derived, so the record is final when it is written.
        unsafe {
            libc_kill(-libc_getpgrp(), SIGKILL);
        }
        let _ = child.wait();
        if signum == SIGALRM {
            result::State::TimedOut
        } else {
            result::State::Cancelled { signum }
        }
    } else {
        outcome_of(waited.expect("the loop only exits with a status")).0
    };
    let (payload_exit, payload_signal) = if signum != 0 {
        (None, None)
    } else {
        let (_, exit, signal) = outcome_of(waited.expect("the loop only exits with a status"));
        (exit, signal)
    };
    unsafe {
        libc_alarm(0);
        libc_signal_restore();
    }
    let record = result::Record {
        state: ended.clone(),
        repo: repo.root.to_string_lossy().into_owned(),
        sidecar: repo.sidecar.to_string_lossy().into_owned(),
        payload: payload_vec,
        started_at,
        finished_at: result::epoch_secs(std::time::SystemTime::now()),
        duration_ms: started.elapsed().as_millis() as u64,
        timeout: timeout_secs,
        payload_exit,
        payload_signal,
        error: None,
    };
    record_result(&record, repo, result_path.as_deref());
    ended.exit_code()
}

/// Interpret the backend's final [`ExitStatus`](std::process::ExitStatus)
/// as the record's state plus the payload's own fate: exited `0` →
/// completed, exited non-zero → failed, died by a signal → failed with
/// the signal recorded (a payload killed by its own crash is a FAILED
/// run, not a cancellation — nothing cancelled it).
fn outcome_of(status: std::process::ExitStatus) -> (result::State, Option<i32>, Option<String>) {
    use std::os::unix::process::ExitStatusExt;
    if let Some(code) = status.code() {
        if code == 0 {
            (result::State::Completed, Some(0), None)
        } else {
            (result::State::Failed, Some(code), None)
        }
    } else if let Some(signum) = status.signal() {
        (
            result::State::Failed,
            None,
            Some(result::signal_name(signum)),
        )
    } else {
        // Neither a code nor a signal — the stop of a traced process,
        // which a plain run would not stop for either. A failed run
        // with no payload fate: the file stays honest by omission.
        (result::State::Failed, None, None)
    }
}

/// Write `r` to `<sidecar>/result.json` — or, for a clone run, to the
/// per-session `<sidecar>/clones/NAME.json` (workspace.md D5) handed in
/// as `session_result` — atomically, via the same [`write_atomically`]
/// the config writer uses, so a batch driver never reads a half-written
/// file — and point the operator at it on stderr (stdout is the
/// payload's, cli.md D9). The pointer line goes to stderr in both
/// cases (D5).
///
/// A failure to WRITE the result does not change the run's outcome:
/// the payload ran, its state is what it is, and the exit code comes
/// from the record — the operator just also learns that the outcome
/// was not recorded, from the `mysbx: ` diagnosis. Silent data loss
/// it must not be; a wrong exit code it must not cause either.
fn record_result(r: &result::Record, repo: &repo::Repo, session_result: Option<&std::path::Path>) {
    let path = session_result
        .map(std::path::Path::to_path_buf)
        .unwrap_or_else(|| repo.sidecar.join(result::FILE_NAME));
    if let Err(e) = write_atomically(&path, &result::render(r)) {
        eprintln!("mysbx: cannot write {}: {e}", path.display());
    } else {
        eprintln!("mysbx: result: {}", path.display());
    }
}

/// The flag the SIGINT/SIGTERM/SIGALRM handlers set: `0` no signal,
/// otherwise the number of the signal that arrived. A plain atomic
/// store is all a handler may do (no allocation, no locks — async
/// signal safety); the wait loop loads it with `SeqCst` and derives
/// everything else.
static CANCELLED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

// The signal numbers (the usual Linux ones — the crate targets
// NixOS/Linux only, the same list main.rs and the `gui` detach use).
const SIGINT: i32 = 2;
const SIGALRM: i32 = 14;
const SIGTERM: i32 = 15;
const SIGKILL: i32 = 9;

/// The one handler for all three caught signals: record the number,
/// return. Everything else is the wait loop's business.
unsafe extern "C" fn on_signal(signum: i32) {
    CANCELLED.store(signum, std::sync::atomic::Ordering::SeqCst);
}

/// Install [`on_signal`] for the given signal — `signal(2)` via the
/// raw libc symbol, the zero-dependency idiom of main.rs.
unsafe fn libc_signal_catch(signum: i32) {
    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    signal(signum, on_signal as unsafe extern "C" fn(i32) as usize);
}

/// Restore the default action for the three caught signals — a
/// leftover handler would eat the NEXT command's Ctrl-C (the same
/// reason [`run_with_result`] clears the alarm).
unsafe fn libc_signal_restore() {
    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    const SIG_DFL: usize = 0;
    signal(SIGINT, SIG_DFL);
    signal(SIGALRM, SIG_DFL);
    signal(SIGTERM, SIG_DFL);
}

/// `alarm(2)`: deliver SIGALRM after `secs` seconds — the timeout
/// budget, zero dependencies. `alarm(0)` cancels a pending one.
unsafe fn libc_alarm(secs: u64) {
    extern "C" {
        fn alarm(secs: std::os::raw::c_uint) -> std::os::raw::c_uint;
    }
    alarm(secs as std::os::raw::c_uint);
}

/// `kill(2)` on a process group (negative pid). Raw libc, as above.
unsafe fn libc_kill(pgrp: i32, signum: i32) {
    extern "C" {
        fn kill(pid: i32, signum: i32) -> i32;
    }
    kill(pgrp, signum);
}

/// `getpgrp(2)` — the group the kill above aims at. The backend is
/// in mysbx's group (a spawned child stays there unless it calls
/// `setpgid` itself, which bwrap does not), so killing the group
/// reaches the backend AND every process it spawned.
unsafe fn libc_getpgrp() -> i32 {
    extern "C" {
        fn getpgrp() -> i32;
    }
    getpgrp()
}
/// Everything that must stay unwritable for `path` to still be THIS
/// policy file on the next run (review-4 item 1): the pathname is
/// walked component by component on the host filesystem, and every
/// directory entry it traverses is recorded — with the parents of that
/// entry already resolved, so an intermediate symlink is recorded as
/// the entry it is *and* followed — followed by the fully resolved
/// target.
///
/// Why the entries and not only the target: mysbx finds its policy by
/// PATHNAME. Home Manager writes `~/.config/mysbx/config.toml` as a
/// symlink into the immutable `/nix/store`; a payload that can write
/// `~/.config/mysbx` cannot touch the target, but it can unlink the
/// symlink and put its own `config.toml` there — and the next run
/// reads that one. The same holds for every directory above it (a
/// writable parent can rename the directory out of the way) and for
/// symlinks in intermediate components.
///
/// Fail-closed by construction: a component that cannot be read as a
/// symlink is treated as a plain entry, and a symlink chain longer
/// than [`SYMLINK_BUDGET`] stops being followed — in both cases the
/// entries collected so far stay protected, so an error can only
/// remove the FOLLOWING of a link, never the protection of the
/// pathname the run actually used.
fn trusted_policy(path: &std::path::Path) -> bwrap::PolicyPath {
    use std::collections::VecDeque;
    use std::ffi::OsString;

    // A relative path can only come from a caller that built it from
    // the CWD; resolve it the same way the rest of the pipeline would,
    // so the guarded set is absolute like every `rw` source it is
    // compared against.
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        match std::env::current_dir() {
            Ok(cwd) => cwd.join(path),
            Err(_) => return bwrap::PolicyPath::lexical(path),
        }
    };
    let mut pending: VecDeque<OsString> = absolute
        .components()
        .map(|c| c.as_os_str().to_owned())
        .collect();
    let mut guarded: Vec<std::path::PathBuf> = Vec::new();
    let mut cur = std::path::PathBuf::from("/");
    let mut budget = SYMLINK_BUDGET;
    while let Some(component) = pending.pop_front() {
        if component == *std::ffi::OsStr::new("/") {
            cur = std::path::PathBuf::from("/");
            continue;
        }
        if component == *std::ffi::OsStr::new(".") {
            continue;
        }
        if component == *std::ffi::OsStr::new("..") {
            cur.pop();
            continue;
        }
        let entry = cur.join(&component);
        if !guarded.contains(&entry) {
            guarded.push(entry.clone());
        }
        match std::fs::read_link(&entry) {
            // A symlink: the ENTRY stays protected (it is what the next
            // run traverses) and the walk continues through its target,
            // relative ones against the directory the link lives in —
            // exactly how the kernel resolves it.
            Ok(target) if budget > 0 => {
                budget -= 1;
                for c in target
                    .components()
                    .map(|c| c.as_os_str().to_owned())
                    .collect::<Vec<_>>()
                    .into_iter()
                    .rev()
                {
                    pending.push_front(c);
                }
            }
            _ => cur = entry,
        }
    }
    if !guarded.contains(&cur) {
        guarded.push(cur);
    }
    bwrap::PolicyPath {
        path: absolute,
        guarded,
    }
}

/// How many symlinks [`trusted_policy`] follows before it stops — the
/// usual kernel limit (`ELOOP` at 40). A loop or a deeper chain leaves
/// the entries collected so far protected; nothing is silently
/// unprotected.
const SYMLINK_BUDGET: usize = 40;

/// The forwarded host environment (docs/plan.md, "Environment"): exactly
/// [`FORWARDED_ENV_VARS`], each only when actually set. This is the one
/// place that reads the real process environment; the argv builder stays
/// pure and receives the values as a parameter.
fn collect_host_env() -> bwrap::HostEnv {
    let mut env = bwrap::HostEnv::new();
    for name in FORWARDED_ENV_VARS {
        if let Ok(value) = std::env::var(name) {
            env.insert(name.to_string(), value);
        }
    }
    env
}

/// `std::env::var` with the empty-means-unset rule, for pins that have
/// no fallback at all: `MYSBX_NIX_CONF` unset means "bind no nix
/// configuration", never "bind the host's" (review-2 item 3).
fn env_opt(name: &str) -> Option<String> {
    std::env::var(name).ok().filter(|v| !v.is_empty())
}

/// `std::env::var` with the empty-means-unset rule: an empty value falls
/// back like an absent one (see `sandbox` for why that matters).
fn env_or(name: &str, fallback: &str) -> String {
    match std::env::var(name) {
        Ok(v) if !v.is_empty() => v,
        _ => fallback.to_owned(),
    }
}

/// `mysbx init` — resolve the repository (docs/TODOs/mvp-2-repo-discovery.md)
/// and create its sidecar directory `<repo>.mysbx/` with a default
/// `config.toml`.
fn init(args: &[String]) -> i32 {
    // Review-3 item 5: `--approve-git-dirs` is the recovery for a
    // sidecar config that already exists WITHOUT approvals — one
    // created by `mysbx edit`, written by hand, or `init`ed before the
    // checkout became a linked worktree. Wanting the discovered git
    // metadata approved afterwards used to leave the operator stuck —
    // plain `init` reports `exists` and never touches an existing config,
    // so the only way forward was hand-editing. The flag makes that
    // a deliberate, idempotent one-command action: the same trust
    // decision a fresh explicit `init` would have recorded, taken
    // explicitly after the fact. Only ADDED entries are written —
    // anything already approved (or deliberately removed, but still
    // discovered) keeps its state; removal stays the operator's word.
    let mut approve_git_dirs = false;
    for arg in args {
        if arg == "--approve-git-dirs" {
            approve_git_dirs = true;
        } else {
            eprintln!("mysbx init: unexpected argument: {arg}");
            eprintln!("try `mysbx --help`");
            return 2;
        }
    }
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    if let Err(msg) = ensure_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return EXIT_INFRASTRUCTURE;
    }
    match ensure_sidecar_config(&repo, true) {
        Ok(Outcome::Created) => {}
        Ok(Outcome::Existed) => {
            // The config exists (an earlier `init`, an `edit`, or a
            // hand-written one): plain `init` leaves it alone (D12). The recovery flag
            // adds the discovered-but-unapproved git dirs to it.
            if approve_git_dirs {
                if let Err(msg) = approve_git_dirs_in_existing_config(&repo) {
                    eprintln!("mysbx: {msg}");
                    return EXIT_INFRASTRUCTURE;
                }
            } else {
                println!("## exists: {}", repo.sidecar.join("config.toml").display());
            }
        }
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return EXIT_INFRASTRUCTURE;
        }
    }
    0
}

/// `mysbx edit` — open the repo's **sidecar** `config.toml` in the
/// editor named by the environment (docs/design/cli.md D12).
///
/// The sidecar config is the one file a person is expected to edit by
/// hand: it is the per-repo policy, it lives outside the repo (D2) and
/// the sandbox cannot write it. The host-wide user config is
/// deliberately NOT what this opens — on myconfig hosts it is a
/// generated symlink into the immutable `/nix/store`, so an editor
/// pointed at it either fails or (worse) replaces the symlink and
/// silently detaches the layer from Home Manager. Editing that layer
/// means editing `myconfig.ai.mysbx.config` and rebuilding.
///
/// The file is created first when missing — the same commented template
/// `mysbx init` writes, without the git-dir approvals (D12/D13): an
/// editor opening a nonexistent path would leave the operator writing a
/// config from memory instead of editing the commented template. `edit`
/// is therefore, next to `init`, the second way to initialize a repo —
/// deliberately: it is an explicit command whose whole purpose is to
/// write that file.
fn edit(args: &[String]) -> i32 {
    if let Some(arg) = args.first() {
        eprintln!("mysbx edit: unexpected argument: {arg}");
        eprintln!("usage: mysbx edit");
        return 2;
    }
    let repo = match repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    // Resolve the editor BEFORE creating anything: a run that cannot
    // edit must not leave a sidecar behind as its only effect.
    let editor = match editor_command() {
        Ok(e) => e,
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return EXIT_INFRASTRUCTURE;
        }
    };
    if let Err(msg) = ensure_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return EXIT_INFRASTRUCTURE;
    }
    // `false`: editing approves nothing — the git-dir approval stays
    // the explicit `mysbx init` (config.md D13).
    // The operator can of course write approvals in the editor that is
    // about to open, which is the point.
    if let Err(msg) = ensure_sidecar_config(&repo, false) {
        eprintln!("mysbx: {msg}");
        return EXIT_INFRASTRUCTURE;
    }
    let config = repo.sidecar.join("config.toml");
    let (bin, editor_args) = editor.split_first().expect("non-empty, see editor_command");
    let mut cmd = std::process::Command::new(bin);
    cmd.args(editor_args).arg(&config);
    // `exec` like the sandbox path: the editor replaces this process, so
    // it owns the terminal and its exit code propagates unchanged (D8).
    use std::os::unix::process::CommandExt;
    let e = cmd.exec();
    eprintln!("mysbx: cannot exec {bin}: {e}");
    EXIT_INFRASTRUCTURE
}

/// The editor to run, as a command vector: `$EDITOR` when set,
/// otherwise `$VISUAL` (docs/design/cli.md D12).
///
/// Two deliberate limits:
///
/// - **No built-in default.** Guessing `vi` would open an editor the
///   operator did not choose, on a policy file, with no hint that the
///   variable is unset. Unset is a runtime failure naming both
///   variables instead.
/// - **Whitespace splitting, not shell evaluation.** `EDITOR="code
///   --wait"` and `EDITOR="nvim -u NONE"` are the common shapes and
///   they work; quoting and shell metacharacters do not, because
///   running the value through a shell would make `$EDITOR` a code
///   execution surface of every `mysbx edit` (config.md D4 refuses that
///   for configuration; the same argument holds here). A value whose
///   argument list cannot be written this way can always be a wrapper
///   script.
fn editor_command() -> Result<Vec<String>, String> {
    let value = env_opt("EDITOR")
        .or_else(|| env_opt("VISUAL"))
        .ok_or_else(|| {
            "no editor configured \u{2014} set $EDITOR (or $VISUAL) to the editor \
             `mysbx edit` should run"
                .to_string()
        })?;
    let argv: Vec<String> = value.split_whitespace().map(str::to_owned).collect();
    if argv.is_empty() {
        // Whitespace only: set, but naming no program.
        return Err(format!(
            "the configured editor is blank ({value:?}) \u{2014} set $EDITOR (or \
             $VISUAL) to the editor `mysbx edit` should run"
        ));
    }
    Ok(argv)
}

/// The sidecar config of `repo`, or the error that tells the operator to
/// create it (docs/design/cli.md D13).
///
/// This is the single load-or-fail gate of the bare form and of `run`:
/// initialization is explicit, so a run never writes to the host
/// filesystem on its own. Naming the missing path is the point of the
/// message — the sidecar lives *outside* the repo (config.md D2), so a
/// user who has not seen it before cannot guess where it would be.
///
/// The *config file* is what is required, not merely the directory: it
/// is what `mysbx init` writes, what the merge reads and what the
/// operator edits. A bare `<repo>.mysbx/` directory (a leftover
/// `state/` tree, say) would otherwise silently run with an empty
/// policy layer.
fn require_initialized_sidecar(repo: &repo::Repo) -> Result<(), String> {
    let config = repo.sidecar.join("config.toml");
    if config.exists() {
        return Ok(());
    }
    Err(format!(
        "this repository has no sandbox yet: {} does not exist \
         \u{2014} run `mysbx init` in {} to create it",
        config.display(),
        repo.root.display()
    ))
}

/// Create the sidecar directory if it is missing and report it (idempotent:
/// docs/design/config.md D12). Shared by `init` and `edit` — the only two
/// commands that create anything (cli.md D13).
fn ensure_sidecar(repo: &repo::Repo) -> Result<(), String> {
    if repo.sidecar.is_dir() {
        return Ok(());
    }
    std::fs::create_dir_all(&repo.sidecar)
        .map_err(|e| format!("cannot create {}: {e}", repo.sidecar.display()))?;
    println!("## created: {}/", repo.sidecar.display());
    Ok(())
}

/// Create the `<sidecar>/state/<entry>` backing directory of every
/// merged `state-dirs` entry (docs/design/config.md D15), idempotently,
/// and report each creation like `ensure_sidecar` does. The argv builder
/// binds them rw at `/mysbx-home/<entry>`; bubblewrap needs an existing
/// source, and the writes the payload makes there are exactly the state
/// that survives the sandbox.
///
/// The entry SPELLING is already unambiguous (the parser rejects
/// absolute, `~/`, `.`/`..` and empty components), but the spelling is
/// only half of the path: the state tree is the one part of the sidecar
/// the payload can write, so a symlink planted there would make a plain
/// `create_dir_all` follow it OUT of the sidecar and bind whatever it
/// points at rw into the sandbox home — a host-home path could re-enter
/// the sandbox that way, which is exactly what D14 forbids. Every level
/// is therefore created and verified one component at a time with
/// [`ensure_plain_dir`]: no component of a backing path may be a
/// symlink, so the source of a state bind is always a real directory
/// below `<sidecar>/state/` (D15, "Trust").
fn ensure_state_dirs(repo: &repo::Repo, state_dirs: &[String]) -> Result<(), String> {
    if state_dirs.is_empty() {
        return Ok(());
    }
    let root = repo.sidecar.join("state");
    ensure_plain_dir(&root)?;
    for entry in state_dirs {
        let mut dir = root.clone();
        let mut created = false;
        // Component by component: each level's parent has been checked
        // before the child is touched, so nothing is ever created
        // through a symlink.
        for component in entry.split('/') {
            dir.push(component);
            created |= ensure_plain_dir(&dir)?;
        }
        if created {
            println!("## created: {}/", dir.display());
        }
    }
    Ok(())
}

/// Make `path` an existing, real directory, creating it when missing.
/// Returns whether it was created.
///
/// "Real" excludes a symlink to a directory: inside the sidecar's state
/// tree a symlink is payload-plantable, and following it would silently
/// move a state bind's source somewhere no configuration named (see
/// [`ensure_state_dirs`]). Refusing is the safe direction — the run
/// fails with the offending path named instead of binding it.
fn ensure_plain_dir(path: &std::path::Path) -> Result<bool, String> {
    match std::fs::symlink_metadata(path) {
        Ok(md) if md.file_type().is_symlink() => Err(format!(
            "state directory {} is a symlink — the state tree is writable by the sandbox, \
             so a symlink there would redirect a state bind out of the sidecar \
             (docs/design/config.md D15); remove it, or delete the sidecar's state/ tree",
            path.display()
        )),
        Ok(md) if md.is_dir() => Ok(false),
        Ok(_) => Err(format!(
            "state directory {} exists and is not a directory (docs/design/config.md D15)",
            path.display()
        )),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            std::fs::create_dir(path)
                .map_err(|e| format!("cannot create {}: {e}", path.display()))?;
            Ok(true)
        }
        Err(e) => Err(format!("cannot inspect {}: {e}", path.display())),
    }
}

/// The `multiplexer` block of a fresh sidecar config
/// (docs/design/config.md D17): the enum in a comment, and the value
/// the USER layer currently names as the recorded key.
///
/// Recording the user layer's own value is what makes writing the key
/// safe: the sidecar WINS over the user config (D17), so any other
/// value — a hardcoded `"none"`, say — would silently downgrade the
/// host default for every newly initialized repository. `init` copies
/// the default; it does not invent one.
///
/// `None` (the user layer could not be read or parsed) leaves the key
/// commented out: writing a guessed value would be exactly the silent
/// downgrade above, and an unparsable user layer fails every run
/// anyway — with its own message, naming the file.
fn multiplexer_template(user_value: Option<config::Multiplexer>) -> String {
    let head = format!(
        "\n# The interactive payload: which terminal multiplexer `mysbx`\n\
         # starts instead of a plain shell (docs/design/config.md D17).\n\
         # One of: {}.\n\
         # `mysbx run -- CMD` is never wrapped in a session (cli.md D11).\n",
        config::Multiplexer::NAMES
            .iter()
            .map(|n| format!("\"{n}\""))
            .collect::<Vec<_>>()
            .join(", ")
    );
    match user_value {
        Some(m) => format!(
            "{head}# Recorded from the host-wide user configuration layer, so this\n\
             # file changes nothing until you edit it \u{2014} the sidecar wins.\n\
             multiplexer = \"{m}\"\n"
        ),
        None => format!(
            "{head}# Left commented out: the user configuration layer could not be\n\
             # read, and a guessed value here would OVERRIDE it (the sidecar\n\
             # wins). Uncomment to decide it for this repository.\n\
             # multiplexer = \"none\"\n"
        ),
    }
}

/// The `multiplexer` value of the user configuration layer, for
/// [`multiplexer_template`]: `Some(Multiplexer::None)` when the layer
/// exists and decides nothing (which is what the merge resolves to
/// anyway), `None` when it cannot be read or parsed.
///
/// Reads the same path `merge::load_layers` reads — via
/// [`merge::user_config_path`], never a second fallback of its own.
fn user_layer_multiplexer() -> Option<config::Multiplexer> {
    let home = std::env::var_os("HOME").unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    let path = merge::user_config_path(std::path::Path::new(&home), xdg.as_deref());
    if !path.exists() {
        // An absent user layer is an EMPTY layer, not an unknown one
        // (merge.rs `load_optional`): it decides nothing, so the
        // effective default is the plain shell.
        return Some(config::Multiplexer::None);
    }
    match config::Config::load(&path) {
        Ok(c) => Some(c.multiplexer.unwrap_or(config::Multiplexer::None)),
        Err(_) => None,
    }
}

/// What [`ensure_sidecar_config`] found: writing the default config or
/// finding an existing one. `init` reports the difference; `edit` does
/// not care (it opens the file either way).
enum Outcome {
    Created,
    Existed,
}

/// Write the default sidecar `config.toml` if it is missing and report
/// it. Shared by `init` and `edit`: a sidecar without a config file is
/// not an initialized repository at all
/// ([`require_initialized_sidecar`]), and the operator could not *see*
/// the file they are expected to review and edit.
///
/// `snapshot_git_dirs` records the discovered git metadata directories
/// into the fresh config as a `git-dirs` approval list (review-2
/// item 1). Only the EXPLICIT `mysbx init` does that: the pointer it
/// reads lives inside the repo and is untrusted content (config.md
/// D3), so turning it into an approval must be a deliberate operator
/// action, taken in a file outside the repo (D2) and printed on
/// stdout — never something a first bare run does for a freshly
/// cloned repository on its own. The bare form therefore creates the
/// sidecar WITHOUT approvals and refuses the bind with the message
/// naming what to approve.
fn ensure_sidecar_config(repo: &repo::Repo, snapshot_git_dirs: bool) -> Result<Outcome, String> {
    let config = repo.sidecar.join("config.toml");
    if config.exists() {
        return Ok(Outcome::Existed);
    }
    // The repo itself is implicit (docs/design/config.md D13): it is the
    // repo this sidecar belongs to, always mounted rw at its real path. It
    // is deliberately not written into the config — the schema has no
    // `[repo]` table (docs/design/config.md D11).
    let contents = "# mysbx sidecar config\n\
# The repo this sidecar belongs to is implicit: it is always mounted\n\
# read-write at its real host path and cannot be changed here\n\
# (docs/design/config.md D13).\n\
#\n\
# Everything else in the sandbox is opt-in. Give a host-home path a\n\
# `dest` under /mysbx-home: HOME is /mysbx-home inside the sandbox, so\n\
# a config bound at its host path is invisible there (config.md D14).\n\
# Examples:\n\
#\n\
# [[mounts]]\n\
# path = \"/home/user/.config/git\"\n\
# dest = \"/mysbx-home/.config/git\"\n\
# mode = \"ro\"\n\
#\n\
# State directories persist across runs (config.md D15): each entry\n\
# is backed by <repo>.mysbx/state/<entry> and bound rw at\n\
# /mysbx-home/<entry>:\n\
#\n\
# state-dirs = [\".local/share/opencode\"]\n\
#\n\
# [env]\n\
# EDITOR = \"nvim\"\n";
    let mut contents = contents.to_owned();
    contents.push_str(&multiplexer_template(user_layer_multiplexer()));
    if snapshot_git_dirs && !repo.git_dirs.is_empty() {
        contents.push_str(
            "\n# Git metadata this repository needs from outside the work tree\n\
             # (linked worktree or submodule). Recorded when the sidecar was\n\
             # created: the `.git` file inside the repo is untrusted content,\n\
             # so only directories approved HERE (or in the user config) are\n\
             # bound (docs/design/config.md D3, review-2 item 1). Remove an\n\
             # entry to refuse the bind; git then fails inside the sandbox.\n\
             git-dirs = [\n",
        );
        for dir in &repo.git_dirs {
            // A path is bytes, not text: one that is not UTF-8, or
            // that carries a control character, cannot be written as
            // a TOML basic string without either mangling it (a
            // lossy conversion produces a DIFFERENT path, i.e. a
            // dangling approval) or emitting a file mysbx itself
            // could not parse on the next run. Such a path is left
            // out and named on stdout instead — approving it stays
            // possible, by hand, with a literal string.
            let Some(text) = dir.to_str() else {
                println!(
                    "## not recorded (path is not valid UTF-8, approve it by hand): {}",
                    dir.display()
                );
                continue;
            };
            if text.chars().any(|c| c.is_control()) {
                println!(
                    "## not recorded (path contains a control character, approve it by hand): {}",
                    dir.display()
                );
                continue;
            }
            // Basic-string escaping: a path may legally contain `"` or
            // `\`, and an unescaped one would make the file we just
            // wrote unparsable on the next run.
            let escaped = text.replace('\\', "\\\\").replace('"', "\\\"");
            contents.push_str(&format!("  \"{escaped}\",\n"));
            println!("## approved git metadata: {}", dir.display());
        }
        contents.push_str("]\n");
    }
    if let Err(e) = std::fs::write(&config, contents) {
        return Err(format!("cannot write {}: {e}", config.display()));
    }
    println!("## created: {}", config.display());
    Ok(Outcome::Created)
}

/// The review-3 item 5 recovery: add the git metadata directories the
/// repo still needs to an EXISTING sidecar `config.toml` (written by
/// `mysbx edit`, by hand, or by an earlier `init`).
/// Idempotent — a second call finds nothing missing and writes
/// nothing — and additive only: entries already approved stay
/// (whoever put them there, including by hand), and the approval of a
/// removed-but-still-discovered entry is the operator's explicit word
/// again (the flag says exactly that). Comment lines and formatting
/// of the rest of the file are preserved byte-for-byte.
fn approve_git_dirs_in_existing_config(repo: &repo::Repo) -> Result<(), String> {
    let config = repo.sidecar.join("config.toml");
    let text = std::fs::read_to_string(&config)
        .map_err(|e| format!("cannot read {}: {e}", config.display()))?;
    let parsed =
        crate::config::Config::parse(&text).map_err(|e| format!("{}: {e}", config.display()))?;
    // Compare on absolute paths: `git-dirs` entries may be written in
    // any D8 spelling (`~/…`, relative); the runtime resolves them
    // against HOME and canonicalizes both sides anyway (review-2
    // item 1). Raw-text matching would both miss a `~` spelling of
    // an approved dir and duplicate it; resolved matching keeps the
    // flag idempotent across spellings.
    let home = std::env::var_os("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_default();
    let resolve = |raw: &str| -> std::path::PathBuf {
        let p = std::path::Path::new(raw);
        if let Some(rest) = raw.strip_prefix("~/") {
            home.join(rest)
        } else if p.is_absolute() {
            p.to_path_buf()
        } else {
            config.parent().unwrap_or(std::path::Path::new(".")).join(p)
        }
    };
    let approved: std::collections::BTreeSet<std::path::PathBuf> = parsed
        .git_dirs
        .iter()
        .map(|raw| std::fs::canonicalize(resolve(raw)).unwrap_or_else(|_| resolve(raw)))
        .collect();
    // NOTE: an entry that does not EXIST cannot canonicalize and is
    // compared in its raw spelling — a duplicate spelling of it may
    // then be added. That is harmless: the next `run` hard-fails on
    // the dangling entry either way (merge.rs names the file), so the
    // broken entry is surfaced, not silently normalized.
    let missing: Vec<std::path::PathBuf> = repo
        .git_dirs
        .iter()
        .filter(|d| !approved.contains(d.as_path()))
        .cloned()
        .collect();
    if missing.is_empty() {
        println!("## git-dirs already lists everything this repo needs");
        return Ok(());
    }
    // A path is bytes, not text — the create-side refusal applies
    // here too: a non-UTF-8 or control-character path mangled through
    // `to_string_lossy` would record a DIFFERENT path (a dangling
    // approval) or an unparsable file. It is left out and named on
    // stdout instead (reviewer finding: the recovery must not be
    // weaker than the initial snapshot).
    let missing: Vec<&std::path::PathBuf> = missing
        .iter()
        .filter(|d| {
            let Some(text) = d.to_str() else {
                println!(
                    "## not recorded (path is not valid UTF-8, approve it by hand): {}",
                    d.display()
                );
                return false;
            };
            if text.chars().any(|c| c.is_control()) {
                println!(
                    "## not recorded (path contains a control character, approve it by hand): {}",
                    d.display()
                );
                return false;
            }
            true
        })
        .collect();
    if missing.is_empty() {
        return Ok(());
    }
    // Extend (or create) the top-level `git-dirs` array in place: a
    // table- and string-aware splice that keeps every other byte of
    // the file (review-4 item 3). Appending at EOF is NOT an option:
    // TOML never returns to the root table, so a file ending in
    // `[env]` would gain an `env.git-dirs` key and one ending in
    // `[[mounts]]` a mount field — both unparsable as a config, both
    // silently "successful" before.
    let entries: Vec<&str> = missing
        .iter()
        .map(|d| d.to_str().expect("filtered above"))
        .collect();
    let new_text = crate::toml::add_git_dirs(&text, &entries, APPROVAL_COMMENT)
        .map_err(|e| format!("{}: {e}", config.display()))?;
    // Validate with the REAL parser before anything is replaced: a
    // rewrite that mysbx itself could not read on the next run must
    // fail here, with the original file untouched, rather than be
    // reported as a successful approval.
    let reparsed = crate::config::Config::parse(&new_text).map_err(|e| {
        format!(
            "{}: refusing to write an unparsable config: {e}",
            config.display()
        )
    })?;
    if reparsed.git_dirs.len() < parsed.git_dirs.len() + entries.len() {
        return Err(format!(
            "{}: the rewritten config does not carry the new approvals — refusing to write it",
            config.display()
        ));
    }
    write_atomically(&config, &new_text)?;
    for d in &missing {
        println!("## approved git metadata: {}", d.display());
    }
    Ok(())
}

/// The comment written above a `git-dirs` key the approval CREATES (an
/// existing array keeps whatever documentation it already carries).
const APPROVAL_COMMENT: &str = "# Added by `mysbx init --approve-git-dirs`: git metadata this\n\
     # repository needs from outside the work tree (review-3 item 5).\n\
     # The `.git` pointer inside the repo is untrusted content\n\
     # (docs/design/config.md D3); remove an entry to refuse the\n\
     # bind.\n";

/// Replace a policy file's contents by writing a temporary file next
/// to it and renaming it over the original (review-4 item 3): an
/// interruption mid-write must never leave a TRUNCATED policy — that
/// would be a config granting less than the operator wrote, or none at
/// all, discovered only on the next run. `rename(2)` within the same
/// directory is atomic, so the file is either the old one or the new
/// one.
///
/// The temporary name carries the pid, so two concurrent approvals
/// cannot clobber each other's staging file; the leftover is removed
/// on failure.
fn write_atomically(path: &std::path::Path, contents: &str) -> Result<(), String> {
    let dir = path.parent().unwrap_or(std::path::Path::new("."));
    let name = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "config.toml".to_string());
    let tmp = dir.join(format!(".{name}.mysbx-{}.tmp", std::process::id()));
    if let Err(e) = std::fs::write(&tmp, contents) {
        let _ = std::fs::remove_file(&tmp);
        return Err(format!("cannot write {}: {e}", tmp.display()));
    }
    if let Err(e) = std::fs::rename(&tmp, path) {
        let _ = std::fs::remove_file(&tmp);
        return Err(format!("cannot replace {}: {e}", path.display()));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn help_and_version_succeed() {
        assert_eq!(run(vec!["--help".into()]), 0);
        assert_eq!(run(vec!["--version".into()]), 0);
        assert_eq!(run(vec!["-h".into()]), 0);
        assert_eq!(run(vec!["-V".into()]), 0);
    }

    #[test]
    fn unknown_command_fails() {
        assert_eq!(run(vec!["nope".into()]), 2);
        // The global flags do not make an unknown verb acceptable.
        assert_eq!(run(vec!["--dry-run".into(), "nope".into()]), 2);
        assert_eq!(run(vec!["--verbose".into(), "nope".into()]), 2);
    }

    #[test]
    fn global_flags_parse_in_any_order_but_never_twice() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };
        let args = s(&["--verbose", "--dry-run", "run"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(
            flags,
            Flags {
                dry_run: true,
                verbose: true,
                multiplexer: None,
                backend: None,
                ro: Vec::new(),
                rw: Vec::new(),
                timeout: None,
                result: false,
                session: None,
            }
        );
        assert_eq!(rest, &s(&["run"])[..]);

        let args = s(&["--dry-run", "--verbose"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert!(flags.dry_run && flags.verbose);
        assert!(rest.is_empty());

        // A flag-less argument list stops the loop immediately.
        let args = s(&["init"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags, Flags::default());
        assert_eq!(rest, &s(&["init"])[..]);

        // Repeats are usage errors, per flag.
        assert_eq!(split_global_flags(&s(&["--verbose", "--verbose"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--dry-run", "--dry-run"])), Err(2));
    }

    // The `--multiplexer` flag (cli.md D14): it parses with the same
    // position rule as the other global flags, its value is validated
    // against the closed set of the config key, and a repeat or a
    // missing value is the same usage error a repeated `--verbose` is.
    #[test]
    fn multiplexer_flag_parses_overrides_and_rejects_bad_values() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };

        // Every accepted spelling parses, including `none`.
        for (text, want) in [
            ("tmux", config::Multiplexer::Tmux),
            ("workmux", config::Multiplexer::Workmux),
            ("herdr", config::Multiplexer::Herdr),
            ("aoe", config::Multiplexer::Aoe),
            ("orca", config::Multiplexer::Orca),
            ("none", config::Multiplexer::None),
        ] {
            let args = s(&["--multiplexer", text, "--dry-run"]);
            let (flags, rest) = split_global_flags(&args).unwrap();
            assert_eq!(flags.multiplexer, Some(want), "`{text}`");
            assert!(flags.dry_run);
            assert!(rest.is_empty(), "`{text}`");
        }

        // An unknown spelling is a usage error, with the set named.
        let code = split_global_flags(&s(&["--multiplexer", "screen"])).unwrap_err();
        assert_eq!(code, 2);

        // A missing value is a usage error, not a `Multiplexer::None`.
        assert_eq!(split_global_flags(&s(&["--multiplexer"])), Err(2));

        // Repeats are usage errors, like the other flags.
        assert_eq!(
            split_global_flags(&s(&["--multiplexer", "tmux", "--multiplexer", "tmux"])),
            Err(2)
        );

        // The flag does not make an unknown verb acceptable, and never
        // applies to one (D14: bare form and nothing else).
        assert_eq!(
            run(vec!["--multiplexer".into(), "tmux".into(), "nope".into()]),
            2
        );
        assert_eq!(
            run(vec!["--multiplexer".into(), "tmux".into(), "init".into()]),
            2
        );
        assert_eq!(
            run(vec!["--multiplexer".into(), "tmux".into(), "edit".into()]),
            2
        );
        assert_eq!(
            run(vec!["--multiplexer".into(), "tmux".into(), "help".into()]),
            2
        );
        // And `run` never starts a session (D11), so it rejects the
        // combination instead of silently ignoring the flag.
        assert_eq!(
            run(vec!["run".into(), "--multiplexer".into(), "tmux".into()]),
            2
        );
    }

    // The `--backend` flag (cli.md D18, bd myconfig-veg): parsed in
    // both positions like every run-scoped flag, its repeat and
    // missing-value refusals are usage errors like the other flags',
    // and the flag is refused with every verb except `run`.
    #[test]
    fn backend_flag_parses_positions_and_rejects_misuse() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };

        // Both backends parse from the pre-verb position; the VALUE is
        // deliberately NOT validated here (the pipeline's step 4 owns
        // the accepted set, cli.md D18).
        for text in ["bubblewrap", "podman-gvisor", "something-else"] {
            let args = s(&["--backend", text, "--dry-run"]);
            let (flags, rest) = split_global_flags(&args).unwrap();
            assert_eq!(flags.backend.as_deref(), Some(text), "`{text}`");
            assert!(flags.dry_run);
            assert!(rest.is_empty(), "`{text}`");
        }

        // A missing value is a usage error, never a "keep configured".
        assert_eq!(split_global_flags(&s(&["--backend"])), Err(2));

        // Repeats are usage errors, like the other flags.
        assert_eq!(
            split_global_flags(&s(&["--backend", "bubblewrap", "--backend", "bubblewrap"])),
            Err(2)
        );

        // The flag has no verb it may accompany except `run`: the
        // dispatcher's guarded arm refuses it before the verb arm runs.
        for verb in ["init", "edit", "version", "help", "gvisor-load-image"] {
            let args = vec!["--backend".into(), "bubblewrap".into(), verb.into()];
            assert_eq!(run(args), 2, "{verb}");
        }
    }

    // The `--session` flag (workspace.md D1/D2): parsed in both
    // positions (before the verb and after `run`), the NAME grammar
    // validated at parse time, refused for every other verb — and
    // `--rw` in a clone run is a refused RUN (exit `70`, D4), not a
    // usage error: the command line is fine, the run it names
    // cannot happen.
    #[test]
    fn session_flag_parses_positions_grammar_and_verb_scoping() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };

        // Before the verb, bare form and `run` alike...
        let args = s(&["--session", "fix-1", "run"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags.session.as_deref(), Some("fix-1"));
        assert_eq!(rest, &s(&["run"])[..]);
        let args = s(&["--session", "fix-1"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags.session.as_deref(), Some("fix-1"));
        assert!(rest.is_empty());
        // ...and in any order with the other global flags.
        let (flags, _rest) =
            split_global_flags(&s(&["--dry-run", "--session", "fix-1", "--verbose"])).unwrap();
        assert_eq!(flags.session.as_deref(), Some("fix-1"));
        assert!(flags.dry_run && flags.verbose);

        // The grammar of D2, refused at parse time (exit `2`): the
        // NAME becomes a path component under the sidecar, so no
        // spelling that could ever escape `clones/` is accepted.
        for bad in [
            "",
            ".",
            "..",
            "-x",
            "_x",
            "a/b",
            "../evil",
            "a b",
            "ä",
            "a\n",
            "012345678901234567890123456789012345678901234567890123456789012345",
        ] {
            assert_eq!(
                split_global_flags(&s(&["--session", bad])),
                Err(2),
                "`{bad}` should be a usage error"
            );
        }
        // The longest valid name parses.
        let args = s(&["--session", &"a".repeat(64)]);
        let (flags, _) = split_global_flags(&args).unwrap();
        assert!(flags.session.is_some());

        // A missing value and a repeat are usage errors (D5/D8).
        assert_eq!(split_global_flags(&s(&["--session"])), Err(2));
        assert_eq!(
            split_global_flags(&s(&["--session", "a", "--session", "a"])),
            Err(2)
        );

        // After the verb, `run` accepts it too (one position rule,
        // D10/D16) — the whole run form goes through `run()` here:
        // with a bad grammar the usage error fires before anything
        // else, and with a good one the run proceeds into the
        // pipeline (which fails later in this synthetic environment,
        // never at the parser).
        assert_eq!(run(s(&["run", "--session", "a/b", "--", "true"])), 2);
        assert_eq!(run(s(&["run", "--session"])), 2);
        assert_eq!(run(s(&["run", "--session", "a", "--session", "a"])), 2);

        // Every other verb refuses it, like every run-scoped flag.
        for args in [
            vec!["--session", "fix-1", "init"],
            vec!["--session", "fix-1", "edit"],
            vec!["--session", "fix-1", "gui"],
            vec!["--session", "fix-1", "version"],
            vec!["--session", "fix-1", "help"],
        ] {
            assert_eq!(run(s(&args)), 2, "{args:?}");
        }

        // The FULL first-verb spelling of D1 — `mysbx --session NAME
        // run ...` — is accepted: the dispatcher must not refuse a
        // session before `run` (a regression the e2e walk caught:
        // the run-scoped refusal arm matched `run` itself). With a
        // valid name the run proceeds into the pipeline, which fails
        // later in this synthetic environment — never with the
        // `is not valid with \`run\`` usage error.
        let code = run(s(&["--session", "fix-1", "run", "--", "true"]));
        assert_ne!(code, 2, "the before-verb spelling must reach the run");
    }

    // workspace.md D4: `--rw` in a clone run is a refused RUN — exit
    // `70`, naming the flag and the mode — never a silent downgrade.
    // The refusal fires at the top of the pipeline, before the repo
    // is even resolved, so the test needs no fixture.
    #[test]
    fn rw_is_refused_in_a_clone_run() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };
        // Both positions of both flags — the refusal is about the
        // combination, not the spelling.
        assert_eq!(run(s(&["--session", "fix-1", "--rw", "/tmp"])), 70);
        assert_eq!(
            run(s(&[
                "run",
                "--session",
                "fix-1",
                "--rw",
                "/tmp",
                "--",
                "true"
            ])),
            70
        );
        assert_eq!(run(s(&["--rw", "/tmp", "--session", "fix-1"])), 70);
        // `--ro` behaves as usual (D4): the run proceeds into the
        // pipeline (and fails there — this synthetic invocation has
        // no repo — with the infrastructure error, not a refusal of
        // the flags).
        let code = run(s(&["--session", "fix-1", "--ro", "/tmp"]));
        assert_ne!(code, 2, "--ro must not be a usage error in a clone run");
    }

    // The `--result`/`--timeout` flags (bd myconfig-0ql, the D8/D17
    // extension): parsed before the verb and after it for `run`, kept
    // in `Flags` like every other run-scoped flag, refused for every
    // other verb and for the bare form.
    #[test]
    fn result_and_timeout_flags_parse_and_are_run_scoped() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };

        // Before the verb...
        let args = s(&["--result", "run"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert!(flags.result);
        assert_eq!(rest, &s(&["run"])[..]);
        // ...and after it, both spellings one run.
        let args = s(&["--timeout", "30", "run"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags.timeout, Some(30));
        assert_eq!(rest, &s(&["run"])[..]);

        // Repeats are usage errors, a typo like every other flag (D5).
        assert_eq!(split_global_flags(&s(&["--result", "--result"])), Err(2));
        assert_eq!(
            split_global_flags(&s(&["--timeout", "30", "--timeout", "30"])),
            Err(2)
        );

        // A missing value and a non-positive or non-numeric one are
        // usage errors (D8: the command line is wrong).
        assert_eq!(split_global_flags(&s(&["--timeout"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--timeout", "0"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--timeout", "-5"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--timeout", "soon"])), Err(2));

        // Every verb but `run` refuses both, and the bare form (an
        // interactive shell) has no consumable outcome either.
        for args in [
            vec!["--result", "init"],
            vec!["--result", "edit"],
            vec!["--result", "gui"],
            vec!["--result", "version"],
            vec!["--result", "help"],
            vec!["--timeout", "30", "init"],
            vec!["--timeout", "30", "help"],
        ] {
            assert_eq!(run(s(&args)), 2, "{args:?}");
        }
        // The bare form too — with its own message naming the flags
        // as payload-run flags, not the generic verb refusal (there is
        // no verb to name).
        assert_eq!(run(s(&["--result"])), 2);
        assert_eq!(run(s(&["--timeout", "30"])), 2);

        // `--timeout` without `--result` is a usage error even WITH
        // `run`: a plain run ends in an exec, no budget can apply.
        assert_eq!(run(vec!["run".into(), "--timeout".into(), "30".into()]), 2);
        // ...and with a payload too — the refusal fires after the
        // command parses, so it is not shadowed by the missing-`--`
        // error.
        assert_eq!(
            run(vec![
                "run".into(),
                "--timeout".into(),
                "30".into(),
                "--".into(),
                "true".into()
            ]),
            2
        );
    }

    // The `--ro`/`--rw` additions (cli.md D16): value-taking, repeatable,
    // stored raw (the pipeline resolves them against `$HOME` and the
    // cwd), and refused with every verb but `run`.
    #[test]
    fn ro_rw_flags_parse_repeat_and_are_verb_scoped() {
        let s = |v: &[&str]| -> Vec<String> { v.iter().map(|x| (*x).to_string()).collect() };

        // Repeatable, in order, both flags interleaved with the others.
        let args = s(&["--ro", "/a", "--verbose", "--rw", "/b", "--ro", "/c"]);
        let (flags, rest) = split_global_flags(&args).unwrap();
        assert_eq!(flags.ro, s(&["/a", "/c"]));
        assert_eq!(flags.rw, s(&["/b"]));
        assert!(flags.verbose);
        assert!(rest.is_empty());

        // A missing value is a usage error.
        assert_eq!(split_global_flags(&s(&["--ro"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--rw"])), Err(2));

        // An empty or bare-`~` value is the parser's own refusal (the
        // same spellings the config parser rejects).
        assert_eq!(split_global_flags(&s(&["--ro", ""])), Err(2));
        assert_eq!(split_global_flags(&s(&["--rw", "~"])), Err(2));
        assert_eq!(split_global_flags(&s(&["--ro", "~user/x"])), Err(2));

        // Relative and `~/…` spellings parse — the pipeline resolves
        // them, so the parser must not reject what D8 defines.
        let (flags, _) = split_global_flags(&s(&["--ro", "rel/sub", "--rw", "~/cache"])).unwrap();
        assert_eq!(flags.ro, s(&["rel/sub"]));
        assert_eq!(flags.rw, s(&["~/cache"]));

        // Every verb but `run` refuses them.
        assert_eq!(run(vec!["--ro".into(), "/a".into(), "init".into()]), 2);
        assert_eq!(run(vec!["--rw".into(), "/a".into(), "edit".into()]), 2);
        assert_eq!(run(vec!["--ro".into(), "/a".into(), "gui".into()]), 2);
        assert_eq!(run(vec!["--ro".into(), "/a".into(), "help".into()]), 2);
        assert_eq!(run(vec!["--ro".into(), "/a".into(), "version".into()]), 2);
        // `run` accepts them after the verb; whether the run succeeds
        // depends on the repo (pinned end-to-end by the CLI tests).
    }

    // The guard-ordering property of the bare form (docs/TODOs/
    // mvp-2-repo-discovery.md): resolve-and-guard BEFORE anything
    // touches the filesystem, so a `$HOME`-resolved run never creates a
    // sidecar (`init`/`edit`) nor even reports a missing one (D13).
    // `run()` uses the real CWD and `$HOME`, so run it *from* a temp dir by
    // spawning a subprocess of the test binary — no: cheaper and still
    // faithful, call the pieces directly.
    #[test]
    fn bare_form_in_home_creates_no_sidecar() {
        let home = std::env::temp_dir().join(format!("mysbx-lib-test-home-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&home);
        std::fs::create_dir_all(&home).unwrap();

        // resolve() is the pure core of the bare form; it must reject the
        // home without any filesystem side effect.
        let e = repo::resolve(&home, Some(&home)).unwrap_err();
        assert!(matches!(e, repo::Error::HomeDir(_)), "{e}");

        // And the sidecar `mysbx init` would create does not exist,
        // i.e. nothing ran past the guard.
        let sidecar = {
            let mut name = home.as_os_str().to_owned();
            name.push(".mysbx");
            std::path::PathBuf::from(name)
        };
        assert!(!sidecar.exists());

        let _ = std::fs::remove_dir_all(&home);
    }

    // The explicit-init gate (cli.md D13): the message must name the
    // missing sidecar config (it lives outside the repo, so it cannot
    // be guessed) and the command that creates it. The CLI tests pin
    // the behaviour end to end; this pins the wording next to the
    // decision.
    #[test]
    fn the_uninitialized_error_names_the_path_and_the_command() {
        let base = std::env::temp_dir().join(format!("mysbx-lib-test-init-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let root = base.join("repo");
        std::fs::create_dir_all(&root).unwrap();
        let sidecar = base.join("repo.mysbx");
        let repo = repo::Repo {
            root: root.clone(),
            sidecar: sidecar.clone(),
            git_dirs: Vec::new(),
            worktrees: None,
        };

        let msg = require_initialized_sidecar(&repo).unwrap_err();
        assert!(
            msg.contains(&sidecar.join("config.toml").display().to_string()),
            "{msg}"
        );
        assert!(msg.contains("mysbx init"), "{msg}");
        // Nothing was created by asking.
        assert!(!sidecar.exists());

        // A sidecar DIRECTORY alone is not an initialized repo; the
        // config file is.
        std::fs::create_dir_all(&sidecar).unwrap();
        assert!(require_initialized_sidecar(&repo).is_err());
        std::fs::write(sidecar.join("config.toml"), "").unwrap();
        assert!(require_initialized_sidecar(&repo).is_ok());

        let _ = std::fs::remove_dir_all(&base);
    }

    // Usage pairing (cli.md D5): there is no derive macro keeping the
    // parser and usage.txt in sync, so this test is the guard — every
    // accepted verb and flag must appear in the help text, and editing one
    // without the other turns the suite red.
    #[test]
    fn usage_documents_every_accepted_flag_and_verb() {
        for token in [
            "run",
            "gui",
            "init",
            "edit",
            "version",
            "help",
            "--dry-run",
            "--verbose",
            "--multiplexer",
            "--backend",
            "--ro",
            "--rw",
            "--result",
            "--timeout",
            "--help",
            "--version",
            "-h",
            "-V",
        ] {
            assert!(
                USAGE.contains(token),
                "usage.txt does not mention `{token}`"
            );
        }
    }

    // ---- trusted policy pathnames (review-4 item 1) --------------------

    /// A canonical temporary directory for the walker tests (the
    /// crate has no dependencies, and `/tmp` itself may be a symlink
    /// on some systems — canonicalize so the expected entries are
    /// spelled the way the walk records them).
    fn walk_tmpdir(name: &str) -> std::path::PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-policy-walk-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::canonicalize(&dir).unwrap()
    }

    #[test]
    fn trusted_policy_guards_the_symlink_entry_and_its_target() {
        // The Home-Manager shape: `<dir>/config.toml` is a symlink to
        // an immutable store-like file. BOTH must be guarded — the
        // target because it is the policy, the entry because whoever
        // can replace it decides what the NEXT run reads.
        let base = walk_tmpdir("hm-symlink");
        let store = base.join("store");
        std::fs::create_dir_all(&store).unwrap();
        std::fs::write(store.join("config.toml"), "").unwrap();
        let dir = base.join("cfg");
        std::fs::create_dir_all(&dir).unwrap();
        let link = dir.join("config.toml");
        std::os::unix::fs::symlink(store.join("config.toml"), &link).unwrap();

        let p = trusted_policy(&link);
        assert_eq!(p.path, link);
        assert!(p.guarded.contains(&link), "{:?}", p.guarded);
        assert!(
            p.guarded.contains(&store.join("config.toml")),
            "{:?}",
            p.guarded
        );
        // The containing directory entry too: a writable parent can
        // rename it out of the way and put a new one in its place.
        assert!(p.guarded.contains(&dir), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_guards_intermediate_symlinks() {
        // `<base>/link/config.toml` where `link -> real`: the
        // intermediate entry is recorded as the entry it is, and the
        // walk continues through its target.
        let base = walk_tmpdir("intermediate");
        let real = base.join("real");
        std::fs::create_dir_all(&real).unwrap();
        std::fs::write(real.join("config.toml"), "").unwrap();
        let link = base.join("link");
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let p = trusted_policy(&link.join("config.toml"));
        assert!(p.guarded.contains(&link), "{:?}", p.guarded);
        assert!(
            p.guarded.contains(&real.join("config.toml")),
            "{:?}",
            p.guarded
        );
        // The final entry is spelled with its parents resolved — the
        // directory entry that actually exists on disk.
        assert!(p.guarded.contains(&real), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_survives_a_symlink_loop() {
        // Fail-closed: a loop stops the FOLLOWING, never the
        // protection of the entries already walked.
        let base = walk_tmpdir("loop");
        let a = base.join("a");
        let b = base.join("b");
        std::os::unix::fs::symlink(&b, &a).unwrap();
        std::os::unix::fs::symlink(&a, &b).unwrap();

        let p = trusted_policy(&a);
        assert!(p.guarded.contains(&a), "{:?}", p.guarded);
        assert!(p.guarded.contains(&b), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn trusted_policy_of_a_plain_file_is_its_own_chain() {
        let base = walk_tmpdir("plain");
        let file = base.join("config.toml");
        std::fs::write(&file, "").unwrap();

        let p = trusted_policy(&file);
        assert!(p.guarded.contains(&file), "{:?}", p.guarded);
        assert!(p.guarded.contains(&base), "{:?}", p.guarded);
        // An unrelated sibling is NOT guarded: the protection stays
        // the pathname chain, not the whole filesystem.
        assert!(!p.guarded.contains(&base.join("other")), "{:?}", p.guarded);

        let _ = std::fs::remove_dir_all(&base);
    }

    #[test]
    fn edit_rejects_arguments_and_the_global_flags() {
        // D12: `edit` takes nothing. The checks that need no repo run
        // first, so this is safe from the test's real CWD.
        assert_eq!(run(vec!["edit".into(), "--user".into()]), 2);
        assert_eq!(run(vec!["edit".into(), "/some/path".into()]), 2);
        // The global flags are not valid with `edit` either (there is
        // no run to report on, and nothing to dry-run).
        assert_eq!(run(vec!["--dry-run".into(), "edit".into()]), 2);
        assert_eq!(run(vec!["--verbose".into(), "edit".into()]), 2);
    }

    #[test]
    fn gui_rejects_the_global_flags_but_keeps_the_tail_verbatim() {
        // cli.md D15: `gui` builds the TERMINAL's argv, not the
        // sandbox's, so a `--dry-run` before the verb has nothing to
        // print and is a usage error — the inner mysbx is where flags
        // belong (`mysbx gui --dry-run` passes the tail verbatim and
        // the INNER invocation refuses it as the bare form's flag,
        // which is the honest error in the window).
        assert_eq!(run(vec!["--dry-run".into(), "gui".into()]), 2);
        assert_eq!(run(vec!["--verbose".into(), "gui".into()]), 2);
        // `--multiplexer` before the verb is the same refusal as every
        // other verb (D14): a flag the dispatcher parsed half of.
        // AFTER the verb it is the inner run's flag and passes through.
        assert_eq!(
            run(vec!["--multiplexer".into(), "tmux".into(), "gui".into()]),
            2
        );
        // The tail is NOT parsed (D4): flags, a `--`, a `run` — anything
        // can sit after `gui`. That acceptance lives in the cli.rs
        // subprocess suite with a pinned MYSBX_TERMINAL, because only a
        // spawned terminal (a stub, here) can show the tail reached it.
    }

    #[test]
    fn editor_command_splits_arguments_and_refuses_a_blank_value() {
        // The env is process-global: this test owns both variables for
        // its duration and restores nothing else.
        let restore = |k: &str, v: Option<String>| match v {
            Some(v) => unsafe { std::env::set_var(k, v) },
            None => unsafe { std::env::remove_var(k) },
        };
        let old_editor = std::env::var("EDITOR").ok();
        let old_visual = std::env::var("VISUAL").ok();

        unsafe { std::env::set_var("EDITOR", "code --wait") };
        unsafe { std::env::remove_var("VISUAL") };
        assert_eq!(editor_command().unwrap(), vec!["code", "--wait"]);

        // `$EDITOR` wins over `$VISUAL`; an empty value counts as
        // unset, like every other variable mysbx reads.
        unsafe { std::env::set_var("VISUAL", "gvim") };
        assert_eq!(editor_command().unwrap(), vec!["code", "--wait"]);
        unsafe { std::env::set_var("EDITOR", "") };
        assert_eq!(editor_command().unwrap(), vec!["gvim"]);

        // Neither set: a runtime failure naming both, never a guessed
        // `vi` on a policy file.
        unsafe { std::env::remove_var("EDITOR") };
        unsafe { std::env::remove_var("VISUAL") };
        let e = editor_command().unwrap_err();
        assert!(e.contains("$EDITOR"), "{e}");
        assert!(e.contains("$VISUAL"), "{e}");

        // Whitespace only: set, but naming no program.
        unsafe { std::env::set_var("EDITOR", "   ") };
        let e = editor_command().unwrap_err();
        assert!(e.contains("blank"), "{e}");

        restore("EDITOR", old_editor);
        restore("VISUAL", old_visual);
    }

    // `run` without `--` and without a command is a usage error (`2`),
    // decided before any repo resolution — safe to call from the test's
    // real CWD.
    #[test]
    fn run_without_dashdash_is_a_usage_error() {
        assert_eq!(run(vec!["run".into()]), 2);
        assert_eq!(run(vec!["run".into(), "ls".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--dry-run".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--verbose".into()]), 2);
    }
}
