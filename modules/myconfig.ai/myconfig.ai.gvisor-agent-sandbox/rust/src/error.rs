// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Terminal output and process exit.
//!
//! The exact prefixes are part of the documented interface (docs/spec.md
//! §13): every fatal error goes through [`die`] and prints
//! `agent-gvisor: error: <message>` to stderr, then exits 1 — matching the
//! bash CLI's `die`.

/// Print `agent-gvisor: <message>` to stderr (bash `log`).
pub fn log(msg: &str) {
    eprintln!("agent-gvisor: {msg}");
}

/// Print `agent-gvisor: warning: <message>` to stderr.
pub fn warn(msg: &str) {
    log(&format!("warning: {msg}"));
}

/// Print `agent-gvisor: error: <message>` to stderr and exit 1 (bash `die`).
pub fn die(msg: &str) -> ! {
    log(&format!("error: {msg}"));
    std::process::exit(1);
}

/// Print a raw diagnostic (no `agent-gvisor:` prefix) and exit 1.
///
/// Used where the bash CLI died on an external command's own error without
/// adding a `die` prefix (a failing `realpath -e` under `set -e`).
pub fn fail_raw(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
