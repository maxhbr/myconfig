// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The subcommand bodies (docs/spec.md §3, §9): `start`, `list`, `status`,
//! `run`, `logs`, `shell`, `stop`, `merge`, `destroy`, plus the shared
//! `run_container` orchestration and the incomplete-session recovery.

use crate::state::{Env, Session};

/// `agent-gvisor start …` — full ordering in docs/spec.md §9 "start ordering".
/// Always terminates: runs the container via `exec`.
pub fn cmd_start(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// `agent-gvisor list` — registry table incl. `incomplete` and
/// `incompatible (pre-rewrite layout)` rows. Exits 0.
pub fn cmd_list(env: &Env) -> ! {
    todo!("M3")
}

/// `agent-gvisor status NAME` — meta fields, container state, git status.
/// Exits with the trailing `git status --short --branch`'s code.
pub fn cmd_status(env: &Env, name: &str) -> ! {
    todo!("M3")
}

/// `agent-gvisor run NAME [--detach] -- [COMMAND…]` — refuses a running
/// container, then `run_container`.
pub fn cmd_run(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// `agent-gvisor logs NAME [PODMAN-LOGS-ARGS…]` — `exec`s podman logs.
pub fn cmd_logs(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// `agent-gvisor shell NAME [COMMAND…]` — `exec`s podman exec; defaults to
/// `/bin/bash`.
pub fn cmd_shell(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// `agent-gvisor stop NAME`.
pub fn cmd_stop(env: &Env, name: &str) -> ! {
    todo!("M3")
}

/// `agent-gvisor merge NAME …` (docs/spec.md §9 "merge").
pub fn cmd_merge(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// `agent-gvisor destroy NAME [--force] [--delete-branch]`.
pub fn cmd_destroy(env: Env, args: &[String]) -> ! {
    todo!("M3")
}

/// The destroy body, as a `Result` so `cmd_start`'s existing-session path
/// can report `could not destroy the existing session` while the standalone
/// `destroy` simply `die`s. `Err` carries the `die` message (not yet
/// printed).
pub fn destroy_session(
    env: &Env,
    session: &Session,
    force: bool,
    delete_branch: bool,
) -> Result<(), String> {
    todo!("M3")
}

/// `agent-gvisor doctor` — see docs/spec.md §10 and `src/doctor.rs`.
pub fn cmd_doctor(env: Env) -> ! {
    todo!("M3")
}
