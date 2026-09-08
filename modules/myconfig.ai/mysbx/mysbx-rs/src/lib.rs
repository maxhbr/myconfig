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
pub mod merge;
pub mod repo;
pub mod report;
pub mod toml;

/// The usage text.
pub const USAGE: &str = include_str!("usage.txt");
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Print the usage to stdout.
pub fn usage() {
    print!("{USAGE}");
}

/// Host environment variables forwarded into the sandbox — exactly this
/// list, each only when actually set in the process environment
/// (docs/plan.md, "Environment"). Nothing else is forwarded implicitly.
/// Public so the integration tests assert against the same list the
/// pipeline reads, not a hand-copied one.
pub const FORWARDED_ENV_VARS: &[&str] =
    &["TERM", "COLORTERM", "LANG", "LC_ALL", "EDITOR", "VISUAL"];

/// Dispatch on the argument list (without argv[0]); returns the exit code.
///
/// Exit codes (cli.md D8): `0` success, `1` runtime failure, `2` usage
/// error. A payload's own exit code propagates unchanged, because the real
/// run ends in an `exec` that replaces this process.
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
        // the sandbox for the current repository.
        None => sandbox(flags, bwrap::Payload::Shell),
        // `--multiplexer` is rejected with the same words (D14): it names
        // the interactive payload of a run, and no verb has one to choose
        // — not even `run`, which never starts a session (D11). The
        // guard sits BEFORE the verb arms so it holds for every verb,
        // `run` included: a `--multiplexer` that reached `run` would be
        // a flag the dispatcher and the runner each parse half of.
        Some(other)
            if flags.multiplexer.is_some()
                && matches!(other, "run" | "gui" | "init" | "edit" | "version" | "help") =>
        {
            eprintln!("mysbx: --multiplexer is not valid with `{other}`");
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

/// The global flags of a sandbox run (cli.md D9, D10, D14).
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Flags {
    pub dry_run: bool,
    pub verbose: bool,
    /// The `--multiplexer <name>` override (cli.md D14, config.md
    /// D17): the multiplexer of THIS interactive run, replacing the
    /// merged `multiplexer` of the configuration layers. `None` means
    /// the flag was not given and the configuration decides.
    pub multiplexer: Option<config::Multiplexer>,
}

impl Flags {
    fn any(self) -> bool {
        self.dry_run || self.verbose
    }

    /// The flag named in the "not valid with `<verb>`" usage error —
    /// whichever was set, `--dry-run` first (it is the older, more
    /// dangerous-sounding promise). `--multiplexer` is not listed:
    /// it is run-scoped, not a promise about the output, and the
    /// refusal names the flags that are.
    fn first_name(self) -> &'static str {
        if self.dry_run {
            "--dry-run"
        } else {
            "--verbose"
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
            _ => break,
        }
        rest = tail;
    }
    Ok((flags, rest))
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
            "--multiplexer" => {
                // cli.md D11/D14: `run -- CMD` never starts a session, so
                // there is no interactive payload for the flag to select —
                // accept-and-ignore would let an operator believe the
                // one-shot ran inside a session it did not.
                eprintln!("mysbx run: --multiplexer is not valid with `run`");
                eprintln!("  it selects the interactive payload only: `mysbx --multiplexer <mux>` starts the session");
                eprintln!("usage: mysbx run [--dry-run] [--verbose] -- COMMAND...");
                return 2;
            }
            "--" => {
                idx += 1;
                break;
            }
            other => {
                eprintln!("mysbx run: unexpected argument: {other}");
                eprintln!(
                    "usage: mysbx run [--dry-run] [--verbose] [--multiplexer <mux>] -- COMMAND..."
                );
                return 2;
            }
        }
    }
    let cmd = &args[idx..];
    if cmd.is_empty() {
        eprintln!("mysbx run: no command given after `--`");
        eprintln!("usage: mysbx run [--dry-run] [--verbose] -- COMMAND...");
        return 2;
    }
    sandbox(flags, bwrap::Payload::Command(cmd.to_vec()))
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
/// The `gui` form never waits for the sandbox: alacritty runs the
/// inner mysbx as its child, and the outer process exits with the
/// terminal's status (0 once the window opened), not the payload's.
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
            return 1;
        }
    };
    let cwd = match std::env::current_dir() {
        Ok(d) => d.to_string_lossy().into_owned(),
        Err(e) => {
            eprintln!("mysbx gui: cannot determine current directory: {e}");
            return 1;
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
    match cmd.status() {
        Ok(status) if status.success() => 0,
        Ok(status) => {
            eprintln!("mysbx gui: {terminal} exited with {status}");
            1
        }
        Err(e) => {
            eprintln!("mysbx gui: cannot start {terminal}: {e}");
            1
        }
    }
}

/// The shared pipeline of the bare form and `run`: resolve the repo, run
/// the guards, require an initialized sidecar, load and merge both layers,
/// check the backend, build the argv — then print it (`--dry-run`) or exec
/// it.
///
/// Nothing here creates the sidecar (cli.md D13): a run that finds no
/// sidecar config fails with the `mysbx init` hint.
fn sandbox(flags: Flags, payload: bwrap::Payload) -> i32 {
    let dry_run = flags.dry_run;
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
            return 1;
        }
    };

    // 2. the sidecar must already exist (cli.md D13). A run — real or
    // dry — creates nothing: `mysbx init` is the one command that
    // writes to the host filesystem here. The check runs for
    // `--dry-run` too, so the argv a dry run prints is always the argv
    // of a run that could actually happen.
    if let Err(msg) = require_initialized_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return 1;
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
                return 1;
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
            return 1;
        }
    };

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
    if !dry_run {
        if let Err(msg) = ensure_state_dirs(&repo, &merged.state_dirs) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }

    // 4. the backend is explicit, never auto-detected (cli.md D7): a
    // silently downgraded isolation level would be a security bug. The MVP
    // accepts exactly `bubblewrap`.
    match merged.backend.as_deref() {
        Some("bubblewrap") => {}
        Some(other) => {
            eprintln!(
                "mysbx: unsupported backend `{other}` — the MVP implements only `bubblewrap`"
            );
            return 1;
        }
        None => {
            eprintln!(
                "mysbx: no backend configured — set `backend = \"bubblewrap\"` in the user or sidecar config"
            );
            return 1;
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
    let params = bwrap::Params {
        shell: &shell,
        tools_path: &tools_path,
        nix_conf: nix_conf.as_deref(),
        policy_paths: &policy_paths,
        mux_entry: mux_entry.as_deref(),
    };
    let argv = match bwrap::bwrap_argv(&merged, &repo, &payload, &host_env, &params) {
        Ok(a) => a,
        // Review-2 item 4: a config that cannot be laid out safely is an
        // ordinary runtime failure — `mysbx:` on stderr, exit 1 — like
        // the merge errors above, never a Rust panic.
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };
    // The Nix wrapper (item 6) pins the binary via MYSBX_BWRAP; the
    // fallback is a plain PATH lookup so `cargo run` works unwrapped.
    let bwrap_bin = env_or("MYSBX_BWRAP", "bwrap");

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
            user_mount_count,
            host_env: &host_env,
            params: &params,
            bwrap_bin: &bwrap_bin,
            payload: &payload,
            dry_run,
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
        println!("{bwrap_bin}");
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }

    let mut cmd = std::process::Command::new(&bwrap_bin);
    cmd.args(&argv);
    // `exec` replaces this process on success, so the payload's exit code
    // propagates unchanged (cli.md D8); the call only returns on failure,
    // with the error as its return value.
    use std::os::unix::process::CommandExt;
    let e = cmd.exec();
    eprintln!("mysbx: cannot exec {bwrap_bin}: {e}");
    1
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
            return 1;
        }
    };
    if let Err(msg) = ensure_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return 1;
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
                    return 1;
                }
            } else {
                println!("## exists: {}", repo.sidecar.join("config.toml").display());
            }
        }
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return 1;
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
            return 1;
        }
    };
    // Resolve the editor BEFORE creating anything: a run that cannot
    // edit must not leave a sidecar behind as its only effect.
    let editor = match editor_command() {
        Ok(e) => e,
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    };
    if let Err(msg) = ensure_sidecar(&repo) {
        eprintln!("mysbx: {msg}");
        return 1;
    }
    // `false`: editing approves nothing — the git-dir approval stays
    // the explicit `mysbx init` (config.md D13).
    // The operator can of course write approvals in the editor that is
    // about to open, which is the point.
    if let Err(msg) = ensure_sidecar_config(&repo, false) {
        eprintln!("mysbx: {msg}");
        return 1;
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
    1
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
                multiplexer: None
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
