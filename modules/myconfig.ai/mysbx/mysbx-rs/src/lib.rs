// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Minimal `mysbx` CLI skeleton. See ../../README.md for the intended shape
//! of the tool; for now only `init`, `version` and `help` exist.

pub mod config;
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
        None | Some("help") | Some("-h") | Some("--help") => {
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

/// `mysbx init` — create the sidecar directory `<repo>.mysbx/` next to the
/// current working directory, with a default `config.toml`.
fn init(args: &[String]) -> i32 {
    if !args.is_empty() {
        eprintln!("mysbx init: unexpected argument: {}", args[0]);
        return 2;
    }
    let cwd = match std::env::current_dir() {
        Ok(c) => c,
        Err(e) => {
            eprintln!("mysbx: cannot determine current directory: {e}");
            return 1;
        }
    };
    let sidecar = {
        let mut p = cwd.clone().into_os_string();
        p.push(".mysbx");
        std::path::PathBuf::from(p)
    };
    if let Err(e) = std::fs::create_dir_all(&sidecar) {
        eprintln!("mysbx: cannot create {}: {e}", sidecar.display());
        return 1;
    }
    println!("## created: {}/", sidecar.display());

    let config = sidecar.join("config.toml");
    if config.exists() {
        println!("## exists:  {}", config.display());
        return 0;
    }
    let contents = format!(
        "# mysbx sidecar config\n[repo]\npath = \"{}\"\nmode = \"rw\"\n",
        cwd.display()
    );
    if let Err(e) = std::fs::write(&config, contents) {
        eprintln!("mysbx: cannot write {}: {e}", config.display());
        return 1;
    }
    println!("## created: {}", config.display());
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn help_and_version_succeed() {
        assert_eq!(run(vec!["--help".into()]), 0);
        assert_eq!(run(vec!["--version".into()]), 0);
        assert_eq!(run(vec![]), 0);
    }

    #[test]
    fn unknown_command_fails() {
        assert_eq!(run(vec!["nope".into()]), 2);
    }
}
