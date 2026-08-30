// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Usage text and argument parsing (docs/spec.md §1–§3).
//!
//! Hand-rolled on purpose: the CLI is small, stable and fully specified, and
//! embedding the historical usage heredoc verbatim keeps `agent-gvisor
//! --help` byte-identical to the bash CLI (a help formatter would not).

/// The usage text, byte-identical to the bash heredoc.
pub const USAGE: &str = include_str!("usage.txt");

/// Print the usage to stdout (exit 0 is the caller's job).
pub fn usage() {
    print!("{USAGE}");
}

/// One resolved bind mount from `--config` / `--mount`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MountSpec {
    /// Canonicalized host path.
    pub host: String,
    /// Absolute container destination.
    pub dest: String,
    /// `ro` or `rw`.
    pub mode: String,
}

/// Parsed `start` arguments (docs/spec.md §2).
#[derive(Debug, Clone)]
pub struct StartArgs {
    /// Positional or `--name`; validated non-empty by [`parse_start`].
    pub name: String,
    pub repo: Option<String>,
    pub base: String,
    pub branch: Option<String>,
    pub image: String,
    /// `--config` entries (default mode `ro`), then `--mount` (`rw`), in
    /// command-line order — the same order they are written to `mounts.tsv`.
    pub mounts: Vec<MountSpec>,
    /// `--env KEY=VALUE` entries, verbatim, in order.
    pub envs: Vec<String>,
    /// Canonicalized `--env-file` path.
    pub env_file: Option<String>,
    /// Empty string ⇒ omit `--network`.
    pub network: String,
    pub detach: bool,
    pub memory: String,
    pub cpus: String,
    pub pids_limit: String,
    pub seccomp_unconfined: bool,
    pub force: bool,
    /// Canonicalized `--home-seed` path.
    pub home_seed: Option<String>,
    pub seed_home_enabled: bool,
    /// COMMAND: positionals after the name plus everything after `--`.
    pub command: Vec<String>,
}

/// Parse `start` arguments (after the subcommand word). Mount specs are
/// validated and canonicalized inline, exactly like the bash command
/// substitution around `parse_mount`; errors `die`.
pub fn parse_start(env: &super::state::Env, args: &[String]) -> StartArgs {
    todo!("M3")
}

/// Parse `HOST:DEST[:MODE]`; `Err` carries the exact `die` message.
pub fn try_parse_mount(spec: &str, default_mode: &str) -> Result<MountSpec, String> {
    todo!("M3")
}

/// Like [`try_parse_mount`], but `die`s on error.
pub fn parse_mount(spec: &str, default_mode: &str) -> MountSpec {
    todo!("M3")
}
