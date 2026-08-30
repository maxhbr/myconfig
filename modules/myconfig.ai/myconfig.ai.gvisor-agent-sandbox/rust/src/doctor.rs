// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `agent-gvisor doctor` internals: the diagnostic header, the throwaway
//! sandbox probe, the model-endpoint probe and the in-sandbox loopback relay
//! probes (docs/spec.md §10).
//!
//! `cmd_doctor` lives in `session.rs` (subcommand dispatch); this module
//! holds the probe builders so the harness can assert the exact podman
//! arguments they produce.

use crate::state::Env;

/// The throwaway-sandbox probe argument vector (without the global args —
/// `Pod::exec` prepends those): `run --rm --read-only … <image> /bin/sh -c
/// 'uname -srmo; id'`.
pub fn sandbox_probe_args(env: &Env) -> Vec<String> {
    todo!("M3")
}

/// The model-endpoint probe argument vector: like the sandbox probe, with
/// the session network (when set) and the curl command; the endpoint is
/// passed as `$0` to the shell command, like the bash original.
pub fn endpoint_probe_args(env: &Env) -> Vec<String> {
    todo!("M3")
}

/// The loopback-relay probe argument vector for one `LPORT:RHOST:RPORT`
/// rule: like the endpoint probe, but through `/bin/agent-gvisor-init` with
/// the single rule exported, probing `http://127.0.0.1:<lport>/`.
pub fn relay_probe_args(env: &Env, rule: &str) -> Vec<String> {
    todo!("M3")
}
