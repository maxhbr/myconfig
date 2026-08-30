// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Session names, registry, repo-adjacent paths, environment defaults and
//! the `meta` record (docs/spec.md §4, §8).

use std::path::{Path, PathBuf};

/// Runtime environment, derived once per invocation from the process
/// environment and the EUID (docs/spec.md §4, §5).
///
/// All fields are public so the integration tests can construct isolated
/// scenarios without touching the real environment.
#[derive(Debug, Clone)]
pub struct Env {
    /// `$AGENT_GVISOR_STATE` / `${XDG_STATE_HOME:-$HOME/.local/state}/agent-gvisor`
    pub state_root: PathBuf,
    /// `$AGENT_GVISOR_PODMAN_RUNTIME` / `$AGENT_GVISOR_DEFAULT_RUNTIME` / `runsc`
    pub podman_runtime: String,
    /// Podman `--cgroup-manager` value; empty string ⇒ flag omitted.
    pub cgroup_manager: String,
    /// `--runtime-flag=<entry>` per non-empty entry.
    pub runtime_flags: Vec<String>,
    /// `--image` default after the `AGENT_GVISOR_IMAGE` override chain.
    pub default_image: String,
    /// `AGENT_GVISOR_DEFAULT_COMMAND` (unset ⇒ `/bin/bash`), word-split at use.
    pub default_command: Option<String>,
    /// `AGENT_GVISOR_NETWORK` (empty string ⇒ omit `--network`).
    pub network: String,
    /// `AGENT_GVISOR_LOOPBACK_FORWARD` (unset or empty ⇒ absent).
    pub loopback_forward: Option<String>,
    /// `AGENT_GVISOR_MODEL_ENDPOINT`
    pub model_endpoint: Option<String>,
    /// `AGENT_GVISOR_WORKTREES` (empty ⇒ repo-adjacent worktrees)
    pub worktrees: Option<String>,
    /// `AGENT_GVISOR_HOME_SEED`
    pub home_seed: Option<String>,
    /// `AGENT_GVISOR_HOME_SEED_PATHS` allowlist
    pub home_seed_paths: Vec<String>,
    /// `AGENT_GVISOR_HOME_SEED_REWRITE` `OLD=NEW` rules
    pub home_seed_rewrite: Vec<String>,
}

impl Env {
    /// Read the environment for the current (real) EUID.
    pub fn from_env() -> Env {
        Env::from_euid(euid())
    }

    /// Read the environment as seen by the given EUID — the rootless
    /// defaults (cgroup manager, runtime flags) differ for root (§5).
    pub fn from_euid(euid: u32) -> Env {
        let _ = euid;
        todo!("M3")
    }
}

/// The real effective UID (libc `geteuid`).
pub fn euid() -> u32 {
    todo!("M3")
}

/// Lowercase, collapse `[^a-z0-9_.-]` runs to `-`, strip leading/trailing `-`.
pub fn sanitize_container_name(s: &str) -> String {
    todo!("M3")
}

/// Session names must match `^[A-Za-z0-9][A-Za-z0-9_.-]*$`.
/// `Err` carries the `die` message.
pub fn validate_name(name: &str) -> Result<(), String> {
    todo!("M3")
}

/// `<dirname repo>/$(basename repo)_agent-gvisor` — the repo-adjacent root
/// hosting `__pools`, `__sessions` and (by default) the worktrees.
pub fn repo_agent_root(repo: &Path) -> PathBuf {
    todo!("M3")
}

/// First 16 hex chars of `sha256(<realpath repo>)`, computed by exec'ing
/// `sha256sum` so IDs match sessions created by the bash CLI (no trailing
/// newline in the hashed string).
pub fn repo_id(repo: &Path) -> String {
    todo!("M3")
}

/// One loaded session: the parsed `meta` plus the session directory that
/// contains it.
#[derive(Debug, Clone)]
pub struct Session {
    /// The registry name under `$STATE_ROOT/sessions/`.
    pub name: String,
    /// Parsed `meta`.
    pub meta: Meta,
    /// `<repo>_agent-gvisor/__sessions/<name>` (symlink target).
    pub meta_dir: PathBuf,
}

/// The `meta` record. All values are strings, like the bash variables the
/// file assigns (docs/spec.md §8): `seccomp_unconfined` is the literal
/// `true`/`false`, and the `--security-opt=seccomp=unconfined` flag applies
/// whenever it is anything but exactly `false`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Meta {
    pub name: String,
    pub repo: String,
    pub repo_id: String,
    pub pool: String,
    pub worktree: String,
    pub home: String,
    pub container: String,
    pub branch: String,
    pub image: String,
    pub memory: String,
    pub cpus: String,
    pub pids_limit: String,
    pub network: String,
    pub seccomp_unconfined: String,
    pub env_file: String,
}

impl Meta {
    /// Serialize to the historical `key=%q` line format, field order fixed
    /// (docs/spec.md §8).
    pub fn to_text(&self) -> String {
        todo!("M3")
    }

    /// Parse the shell-quoted `key=value` lines. Unknown keys are ignored,
    /// missing keys default to empty. `Err` carries the `die` message.
    pub fn parse(text: &str) -> Result<Meta, String> {
        todo!("M3")
    }
}

/// The registry entry `$STATE_ROOT/sessions/<name>`.
pub fn registry_path(env: &Env, name: &str) -> PathBuf {
    env.state_root.join("sessions").join(name)
}

/// Resolve a session by registry name. `Err` carries the exact `die`
/// message: unknown session, pre-rewrite layout (§14), incomplete debris.
pub fn try_load_session(env: &Env, name: &str) -> Result<Session, String> {
    todo!("M3")
}

/// Like [`try_load_session`], but `die`s on error.
pub fn load_session(env: &Env, name: &str) -> Session {
    todo!("M3")
}
