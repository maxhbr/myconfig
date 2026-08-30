// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `agent-gvisor` — rootless Podman + gVisor coding-agent worktree sessions.
//!
//! Library crate so the integration tests in `tests/` can exercise the pure
//! logic (the podman argument vector builder, the shell-quoting layer, the
//! session-state layout) directly; `src/main.rs` is a thin wrapper around
//! [`run`]. The authoritative behavioural contract is `docs/spec.md`.

pub mod cli;
pub mod doctor;
pub mod error;
pub mod podman;
pub mod seed;
pub mod session;
pub mod shellwords;
pub mod state;

/// Entry point: environment setup, subcommand dispatch.
///
/// Mirrors `main`/`cmd_*` dispatch of the historical bash implementation:
/// `start|list|status|run|logs|shell|stop|merge|destroy|doctor`,
/// `''|-h|--help|help` → usage (exit 0), any other non-`-` word is the
/// positional session-name shorthand for `start`.
pub fn run() -> ! {
    use crate::error::die;
    use crate::session;
    let args: Vec<String> = std::env::args().skip(1).collect();
    let env = state::Env::from_env();
    // bash `${1:?session name required}` for the name-taking subcommands.
    let name_arg = |args: &[String]| -> String {
        args.first()
            .cloned()
            .unwrap_or_else(|| die("session name required"))
    };
    // The subcommands that parse options after the NAME need the name in
    // args[0]; die when it is missing (bash `${1:?}`).
    let rest1 = |args: &[String]| -> Vec<String> {
        if args.len() < 2 {
            die("session name required");
        }
        args[1..].to_vec()
    };
    match args.first().map(String::as_str) {
        None | Some("") => {
            cli::usage();
            std::process::exit(0);
        }
        Some("start") => session::cmd_start(env, &args[1..]),
        Some("list") => session::cmd_list(&env),
        Some("status") => {
            let name = name_arg(&args[1..]);
            session::cmd_status(&env, &name)
        }
        Some("run") => session::cmd_run(env, &rest1(&args)),
        Some("logs") => session::cmd_logs(env, &rest1(&args)),
        Some("shell") => session::cmd_shell(env, &rest1(&args)),
        Some("stop") => {
            let name = name_arg(&args[1..]);
            session::cmd_stop(&env, &name)
        }
        Some("merge") => session::cmd_merge(env, &rest1(&args)),
        Some("destroy") => session::cmd_destroy(env, &rest1(&args)),
        Some("doctor") => session::cmd_doctor(env),
        Some("-h") | Some("--help") | Some("help") => {
            cli::usage();
            std::process::exit(0);
        }
        Some(sub) if sub.starts_with('-') => die(&format!("unknown subcommand: {sub}")),
        Some(sub) => {
            // Any first argument that is not an action word is a session
            // name: `agent-gvisor NAME` is shorthand for `agent-gvisor
            // start NAME` with the current directory as the repository.
            let mut start_args = vec![sub.to_string()];
            start_args.extend(args[1..].iter().cloned());
            session::cmd_start(env, &start_args)
        }
    }
}
