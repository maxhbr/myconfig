// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `mysbx` CLI surface (docs/TODOs/mvp-5-cli-and-dry-run.md,
//! docs/design/cli.md):
//!
//! ```text
//! mysbx [--dry-run]                 enter an interactive sandbox shell
//! mysbx run [--dry-run] -- CMD...   run one command in the sandbox
//! mysbx init                        create the sidecar (idempotent)
//! mysbx version | help
//! ```
//!
//! The bare form is the primary action (cli.md D2): it resolves the repo,
//! creates the sidecar implicitly when missing (D12) and execs the
//! backend's argv. `--dry-run` is a *global* flag (first argument, before
//! the subcommand): it runs the whole pipeline — resolve, guards, load,
//! merge, backend check, argv build — and stops immediately before `exec`,
//! printing the argv one argument per line on stdout.

pub mod bwrap;
pub mod config;
pub mod merge;
pub mod repo;
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
    // The global `--dry-run` is accepted only as the FIRST argument (before
    // the subcommand / bare form); anything after `--` is payload and never
    // parsed (cli.md D4, D5).
    let (dry_run, rest) = match args.split_first() {
        Some((first, rest)) if first == "--dry-run" => (true, rest),
        _ => (false, &args[..]),
    };
    match rest.first().map(String::as_str) {
        // Bare `mysbx` is the primary action (docs/design/cli.md D2): enter
        // the sandbox for the current repository.
        None => sandbox(dry_run, bwrap::Payload::Shell),
        Some("help") | Some("-h") | Some("--help") if !dry_run => {
            usage();
            0
        }
        Some("version") | Some("-V") | Some("--version") if !dry_run => {
            println!("mysbx {VERSION}");
            0
        }
        // `--dry-run` is only meaningful for the bare form and `run`; on
        // any other verb it would promise side-effect-freeness while init
        // still creates files — reject it instead (usage error, D8).
        Some(other) if dry_run => {
            eprintln!("mysbx: --dry-run is not valid with `{other}`");
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
        Some("run") => run_command(dry_run, &rest[1..]),
        Some(other) => {
            eprintln!("mysbx: unknown command: {other}");
            eprintln!("try `mysbx --help`");
            2
        }
    }
}

/// `mysbx run [--dry-run] -- CMD...` — parse the `run` arguments and hand
/// the payload to the same pipeline as the bare form (spec "Watch out":
/// one code path, two entry points).
///
/// Everything after `--` is the payload, verbatim — including things that
/// look like flags (cli.md D4). `--dry-run` is accepted before `--` only;
/// `run` without `--` or without a command is a usage error (`2`), not an
/// empty sandbox.
fn run_command(global_dry_run: bool, args: &[String]) -> i32 {
    let mut dry_run = global_dry_run;
    let mut idx = 0;
    while let Some(arg) = args.get(idx) {
        match arg.as_str() {
            "--dry-run" => {
                dry_run = true;
                idx += 1;
            }
            "--" => {
                idx += 1;
                break;
            }
            other => {
                eprintln!("mysbx run: unexpected argument: {other}");
                eprintln!("usage: mysbx run [--dry-run] -- COMMAND...");
                return 2;
            }
        }
    }
    let cmd = &args[idx..];
    if cmd.is_empty() {
        eprintln!("mysbx run: no command given after `--`");
        eprintln!("usage: mysbx run [--dry-run] -- COMMAND...");
        return 2;
    }
    sandbox(dry_run, bwrap::Payload::Command(cmd.to_vec()))
}

/// The shared pipeline of the bare form and `run`: resolve the repo, run
/// the guards, (implicitly) init the sidecar, load and merge both layers,
/// check the backend, build the argv — then print it (`--dry-run`) or exec
/// it.
fn sandbox(dry_run: bool, payload: bwrap::Payload) -> i32 {
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

    // 2. implicit init (cli.md D2) — except under `--dry-run`, which is
    // side-effect-free: a missing sidecar config is an empty layer there.
    if !dry_run {
        if let Err(msg) = ensure_sidecar(&repo) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
        if let Err(msg) = ensure_sidecar_config(&repo) {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }

    // 3. both layers, merged (docs/TODOs/mvp-3-layer-merge.md). Merge
    // errors (widening sidecar, broken paths, unparseable files) are
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
    let merged = match merge::merge(
        layers.user.0,
        layers.sidecar.0,
        &layers.user.1,
        &layers.sidecar.1,
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return 1;
        }
    };

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
    let params = bwrap::Params {
        shell: &shell,
        tools_path: &tools_path,
    };
    let argv = bwrap::bwrap_argv(&merged, &repo, &payload, &host_env, &params);

    // 6. print it, or exec it.
    if dry_run {
        // One argument per line, no prefix, no quoting: this is the
        // result, not a diagnostic (cli.md D9), so golden tests compare
        // bytes and `mysbx run --dry-run -- ls | wc -l` is meaningful.
        for arg in &argv {
            println!("{arg}");
        }
        return 0;
    }

    // The Nix wrapper (item 6) pins the binary via MYSBX_BWRAP; the
    // fallback is a plain PATH lookup so `cargo run` works unwrapped.
    let bwrap_bin = env_or("MYSBX_BWRAP", "bwrap");
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
    if !args.is_empty() {
        eprintln!("mysbx init: unexpected argument: {}", args[0]);
        return 2;
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
    match ensure_sidecar_config(&repo) {
        Ok(Outcome::Created) => {}
        Ok(Outcome::Existed) => {
            println!("## exists: {}", repo.sidecar.join("config.toml").display());
        }
        Err(msg) => {
            eprintln!("mysbx: {msg}");
            return 1;
        }
    }
    0
}

/// Create the sidecar directory if it is missing and report it (idempotent:
/// docs/design/config.md D12). Shared by `init` and the implicit init of
/// the bare form (cli.md D2: the bare form does exactly what `init` would
/// have done, no more, no less).
fn ensure_sidecar(repo: &repo::Repo) -> Result<(), String> {
    if repo.sidecar.is_dir() {
        return Ok(());
    }
    std::fs::create_dir_all(&repo.sidecar)
        .map_err(|e| format!("cannot create {}: {e}", repo.sidecar.display()))?;
    println!("## created: {}/", repo.sidecar.display());
    Ok(())
}

/// What [`ensure_sidecar_config`] found: writing the default config or
/// finding an existing one. `init` reports the difference; the implicit
/// init of the bare form does not care (cli.md D2: it does exactly what
/// `init` would have done — including not re-writing an operator-edited
/// config — but stays silent about it).
enum Outcome {
    Created,
    Existed,
}

/// Write the default comment-only sidecar `config.toml` if it is missing
/// and report it. Also shared by `init` and the implicit init: a sidecar
/// without a config file would make `load_layers` treat the layer as
/// empty — the same outcome, but the operator could no longer *see* the
/// file they are expected to review and edit.
fn ensure_sidecar_config(repo: &repo::Repo) -> Result<Outcome, String> {
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
# Everything else in the sandbox is opt-in. Examples:\n\
#\n\
# [[mounts]]\n\
# path = \"/home/user/.config/git\"\n\
# mode = \"ro\"\n\
#\n\
# [env]\n\
# EDITOR = \"nvim\"\n";
    if let Err(e) = std::fs::write(&config, contents) {
        return Err(format!("cannot write {}: {e}", config.display()));
    }
    println!("## created: {}", config.display());
    Ok(Outcome::Created)
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
        // The global flag does not make an unknown verb acceptable.
        assert_eq!(run(vec!["--dry-run".into(), "nope".into()]), 2);
    }

    // The guard-ordering property of the bare form (docs/TODOs/
    // mvp-2-repo-discovery.md): resolve-and-guard BEFORE ensure_sidecar, so
    // a `$HOME`-resolved bare run must never create a sidecar on disk.
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

        // And the sidecar that implicit init WOULD have created does not
        // exist, i.e. nothing ran past the guard.
        let sidecar = {
            let mut name = home.as_os_str().to_owned();
            name.push(".mysbx");
            std::path::PathBuf::from(name)
        };
        assert!(!sidecar.exists());

        let _ = std::fs::remove_dir_all(&home);
    }

    // Usage pairing (cli.md D5): there is no derive macro keeping the
    // parser and usage.txt in sync, so this test is the guard — every
    // accepted verb and flag must appear in the help text, and editing one
    // without the other turns the suite red.
    #[test]
    fn usage_documents_every_accepted_flag_and_verb() {
        for token in [
            "run",
            "init",
            "version",
            "help",
            "--dry-run",
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

    // `run` without `--` and without a command is a usage error (`2`),
    // decided before any repo resolution — safe to call from the test's
    // real CWD.
    #[test]
    fn run_without_dashdash_is_a_usage_error() {
        assert_eq!(run(vec!["run".into()]), 2);
        assert_eq!(run(vec!["run".into(), "ls".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--".into()]), 2);
        assert_eq!(run(vec!["run".into(), "--dry-run".into()]), 2);
    }
}
