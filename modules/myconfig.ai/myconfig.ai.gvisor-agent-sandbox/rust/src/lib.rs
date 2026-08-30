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
    todo!("M3: dispatch")
}
