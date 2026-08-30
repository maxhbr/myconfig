// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Shell word quoting and unquoting (the `meta` / `last-command` layer).
//!
//! `meta` keeps the historical shell-quoted `key=value` format so sessions
//! written by the bash CLI stay loadable (docs/spec.md §8, §12):
//!
//! - [`quote`] reproduces bash's `printf %q` byte-for-byte for every value
//!   the CLI can produce (verified against bash 5.3's `bstab` + `ansic_quote`
//!   in the tests);
//! - [`unquote`] is a general shell-word parser (bare words, backslash
//!   escapes, `'…'`, `"…"`, `$'…'` ANSI-C) so bash-written meta parses;
//! - [`split_ws`] is the IFS-whitespace splitter for the space-separated list
//!   environment variables.

/// Quote like bash's `printf '%s'` … `printf %q`.
pub fn quote(s: &str) -> String {
    todo!("M3")
}

/// Parse one shell word (assignment-value context: no word splitting, no
/// globbing). `Err` carries the `die` message.
pub fn unquote(s: &str) -> Result<String, String> {
    todo!("M3")
}

/// Split on IFS whitespace (space / tab / newline), dropping empty items.
pub fn split_ws(s: &str) -> Vec<String> {
    todo!("M3")
}
