// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Podman interaction: global arguments, checks, and the `run` argv builder
//! (docs/spec.md §5, §6, §10).
//!
//! [`build_run_args`] is a pure function so the tests can assert the exact
//! argument vector without spawning podman; [`Pod`] wraps actual execution
//! (still exec'ing `podman` from PATH).

use std::path::Path;

use crate::state::{Env, Meta};

/// Global podman arguments, in order: `--runtime=…`, then
/// `--cgroup-manager=…` (when non-empty), then `--runtime-flag=…` per flag.
pub fn global_args(env: &Env) -> Vec<String> {
    todo!("M3")
}

/// True when the runtime flags contain `ignore-cgroups` (⇒ podman's resource
/// limits cannot be enforced, docs/spec.md §5).
pub fn cgroups_ignored(env: &Env) -> bool {
    todo!("M3")
}

/// Absolute runtimes must be executable; named runtimes are probed with
/// `podman info`. `Err` carries the exact `die` message (docs/spec.md §10).
pub fn try_check_runtime(env: &Env) -> Result<(), String> {
    todo!("M3")
}

/// `podman image exists <image>`. `Err` carries the exact `die` message.
pub fn try_check_image(env: &Env, image: &str) -> Result<(), String> {
    todo!("M3")
}

/// Build the exact `podman run` argument vector (docs/spec.md §10),
/// INCLUDING the leading literal `podman` (argv[0], like the bash array).
/// Reads `mounts.tsv` / `env.list` from `meta_dir`; emits the
/// `memory/cpu/pids limits` warning when limits are dropped.
pub fn build_run_args(
    env: &Env,
    meta: &Meta,
    meta_dir: &Path,
    detach: bool,
    command: &[String],
) -> Vec<String> {
    todo!("M3")
}

/// Execution wrapper around the `podman` binary found on PATH.
pub struct Pod<'a> {
    pub env: &'a Env,
}

impl<'a> Pod<'a> {
    pub fn new(env: &'a Env) -> Pod<'a> {
        Pod { env }
    }

    /// Run podman (without replacing the process) and return its status.
    pub fn run(&self, args: &[String]) -> std::process::ExitStatus {
        todo!("M3")
    }

    /// Run podman and replace the process with it (bash `podman_exec_c`):
    /// exits with podman's exit code.
    pub fn exec(&self, args: &[String]) -> ! {
        todo!("M3")
    }

    /// True when the command exits 0.
    pub fn ok(&self, args: &[String]) -> bool {
        todo!("M3")
    }

    /// Trimmed stdout of a successful run, `None` on failure
    /// (bash `$(cmd 2>/dev/null || printf fallback)` fallbacks are the
    /// caller's job).
    pub fn output(&self, args: &[String]) -> Option<String> {
        todo!("M3")
    }

    /// `podman container exists <name>`.
    pub fn container_exists(&self, container: &str) -> bool {
        todo!("M3")
    }

    /// `podman inspect --format <fmt> <name>`, trimmed.
    pub fn inspect(&self, container: &str, fmt: &str) -> Option<String> {
        todo!("M3")
    }
}
