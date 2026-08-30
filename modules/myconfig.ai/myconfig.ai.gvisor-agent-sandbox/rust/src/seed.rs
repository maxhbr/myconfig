// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Home seeding: `/home/agent` from the activated home-manager generation
//! (docs/spec.md §11).
//!
//! Seeding must never abort `start`: the `home-files` tree regularly contains
//! symlinks whose `/nix/store` target is gone, so entries are copied
//! dereferenced (`cp -RL --no-preserve=mode` semantics) and failures are
//! reported, not fatal. Files are copies, not symlinks, because the sandbox
//! has no `/nix`.

use std::path::PathBuf;

/// Resolve the activated home-manager generation's `home-files` tree:
/// `$XDG_STATE_HOME/home-manager/gcroots/current-home` …
/// `/nix/var/nix/gcroots/per-user/$USER/current-home` …
/// `$XDG_STATE_HOME/nix/profiles/home-manager` …
/// `/nix/var/nix/profiles/per-user/$USER/home-manager`, first hit wins,
/// canonicalized (docs/spec.md §11).
pub fn resolve_home_files() -> Option<PathBuf> {
    todo!("M3")
}

/// Copy the allowlisted paths from `seed` into `home`, dereferenced, modes
/// cleared then `chmod -R u+rwX` applied; logs the summary line and every
/// warning. Never fails (bash `seed_home`).
pub fn seed_home(env: &crate::state::Env, home: &std::path::Path, seed: &std::path::Path) {
    todo!("M3")
}

/// Apply the literal `OLD=NEW` rewrite rules (binary files skipped, rewritten
/// files end with exactly one newline, `grep -rIlZF` semantics); `die`s on
/// an invalid rule, logs the change count.
pub fn rewrite_seeded_home(env: &crate::state::Env, home: &std::path::Path) {
    todo!("M3")
}

/// Parse one `OLD=NEW` rule; `Err` carries the `die` message.
pub fn parse_rewrite_rule(rule: &str) -> Result<(String, String), String> {
    todo!("M3")
}
