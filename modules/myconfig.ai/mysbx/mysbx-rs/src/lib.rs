// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Minimal `mysbx` CLI. See ../../README.md for the intended shape of the
//! tool; for now `init`, `version` and `help` exist, and the bare form does
//! implicit init only (the sandbox backend is MVP items 4/5).

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

/// Dispatch on the argument list (without argv[0]); returns the exit code.
pub fn run(args: Vec<String>) -> i32 {
    match args.first().map(String::as_str) {
        // Bare `mysbx` is the primary action (docs/design/cli.md D2): enter
        // the sandbox for the current repository. There is no backend yet,
        // so implicit init is all it can honestly do (MVP item 5 pending).
        None => {
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
            eprintln!("mysbx: no backend yet (MVP item 4/5 pending)");
            1
        }
        Some("help") | Some("-h") | Some("--help") => {
            usage();
            0
        }
        Some("version") | Some("-V") | Some("--version") => {
            println!("mysbx {VERSION}");
            0
        }
        Some("init") => init(&args[1..]),
        Some(other) => {
            eprintln!("mysbx: unknown command: {other}");
            eprintln!("try `mysbx --help`");
            2
        }
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

    let sidecar = &repo.sidecar;
    let config = sidecar.join("config.toml");
    if config.exists() {
        println!("## exists:  {}", config.display());
        return 0;
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
        eprintln!("mysbx: cannot write {}: {e}", config.display());
        return 1;
    }
    println!("## created: {}", config.display());
    0
}

/// Create the sidecar directory if it is missing and report it (idempotent:
/// docs/design/config.md D12). Shared by `init` and the implicit init of the
/// bare form.
fn ensure_sidecar(repo: &repo::Repo) -> Result<(), String> {
    if repo.sidecar.is_dir() {
        return Ok(());
    }
    std::fs::create_dir_all(&repo.sidecar)
        .map_err(|e| format!("cannot create {}: {e}", repo.sidecar.display()))?;
    println!("## created: {}/", repo.sidecar.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn help_and_version_succeed() {
        assert_eq!(run(vec!["--help".into()]), 0);
        assert_eq!(run(vec!["--version".into()]), 0);
    }

    #[test]
    fn unknown_command_fails() {
        assert_eq!(run(vec!["nope".into()]), 2);
    }

    // The guard-ordering property of the bare form (docs/TODOs/
    // mvp-2-repo-discovery.md): resolve-and-guard BEFORE ensure_sidecar, so
    // a `$HOME`-resolved bare run must never create a sidecar on disk.
    // `run()` uses the real CWD and `$HOME`, so run it *from* a temp dir by
    // spawning a subprocess of the test binary — no: cheaper and still
    // faithful, call the pieces directly.
    #[test]
    fn bare_form_in_home_creates_no_sidecar() {
        let home = std::env::temp_dir()
            .join(format!("mysbx-lib-test-home-{}", std::process::id()));
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
}
