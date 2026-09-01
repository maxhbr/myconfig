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

/// The `--rm` sandbox hardening flags shared by every doctor probe, in the
/// historical order.
fn probe_flags() -> Vec<String> {
    [
        "--rm",
        "--read-only",
        "--read-only-tmpfs=true",
        "--cap-drop=ALL",
        "--security-opt=no-new-privileges",
        "--userns=keep-id",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect()
}

/// The curl probe command: any HTTP response counts as reachable, the
/// endpoint travels as `$0` so it needs no quoting inside the shell command
/// (the `\n` stays a literal backslash-n, like in the bash original — podman
/// does not interpret it either).
const CURL_CMD: &str = "curl -sS -o /dev/null --max-time 5 -w 'HTTP %{http_code}\\n' \"$0\"";

/// The models smoke test: fetch the model list, print the status line plus a
/// short excerpt of the body, and fail when the body is empty or lists no
/// model id (an endpoint that answers but serves no model is as useless as
/// an unreachable one).
const MODELS_CMD: &str = concat!(
    "body=$(curl -sS --max-time 10 -w '\\nHTTP %{http_code}' \"$0\") || exit 1; ",
    "printf '%s\\n' \"$body\" | head -c 400; ",
    "case \"$body\" in *'\"id\"'*) exit 0 ;; *) exit 1 ;; esac",
);

/// The models URL for a model endpoint: `<endpoint>/models`, with any
/// trailing slashes on the endpoint collapsed.
pub fn models_url(endpoint: &str) -> String {
    format!("{}/models", endpoint.trim_end_matches('/'))
}

/// The throwaway-sandbox probe argument vector (without the global args —
/// `Pod::exec` prepends those): `run --rm --read-only … <image> /bin/sh -c
/// 'uname -srmo; id'`.
pub fn sandbox_probe_args(env: &Env) -> Vec<String> {
    let mut args = vec!["run".to_string()];
    args.extend(probe_flags());
    args.push(env.default_image.clone());
    args.extend([
        "/bin/sh".to_string(),
        "-c".to_string(),
        "uname -srmo; id".to_string(),
    ]);
    args
}

/// The model-endpoint probe argument vector: like the sandbox probe, with
/// the session network (when set) and the curl command; the endpoint is
/// passed as `$0` to the shell command, like the bash original.
pub fn endpoint_probe_args(env: &Env) -> Vec<String> {
    let mut args = vec!["run".to_string()];
    args.extend(probe_flags());
    if !env.network.is_empty() {
        args.extend(["--network".to_string(), env.network.clone()]);
    }
    args.push(env.default_image.clone());
    args.extend([
        "/bin/sh".to_string(),
        "-c".to_string(),
        CURL_CMD.to_string(),
        env.model_endpoint.clone().unwrap_or_default(),
    ]);
    args
}

/// The models probe argument vector: like the endpoint probe, but fetching
/// `<endpoint>/models` and asserting the answer actually lists a model.
pub fn models_probe_args(env: &Env) -> Vec<String> {
    let mut args = vec!["run".to_string()];
    args.extend(probe_flags());
    if !env.network.is_empty() {
        args.extend(["--network".to_string(), env.network.clone()]);
    }
    args.push(env.default_image.clone());
    args.extend([
        "/bin/sh".to_string(),
        "-c".to_string(),
        MODELS_CMD.to_string(),
        models_url(&env.model_endpoint.clone().unwrap_or_default()),
    ]);
    args
}

/// The loopback-relay probe argument vector for one `LPORT:RHOST:RPORT`
/// rule: like the endpoint probe, but through `/bin/agent-gvisor-init` with
/// the single rule exported, probing `http://127.0.0.1:<lport>/`.
pub fn relay_probe_args(env: &Env, rule: &str) -> Vec<String> {
    let lport = rule.split(':').next().unwrap_or_default();
    let mut args = vec!["run".to_string()];
    args.extend(probe_flags());
    args.extend([
        "--env".to_string(),
        format!("AGENT_GVISOR_LOOPBACK_FORWARD={rule}"),
    ]);
    if !env.network.is_empty() {
        args.extend(["--network".to_string(), env.network.clone()]);
    }
    args.push(env.default_image.clone());
    args.extend([
        "/bin/agent-gvisor-init".to_string(),
        "/bin/sh".to_string(),
        "-c".to_string(),
        CURL_CMD.to_string(),
        format!("http://127.0.0.1:{lport}/"),
    ]);
    args
}

/// The `die` message when the sandbox probe fails (multi-line).
pub fn sandbox_failed_message() -> String {
    "sandbox startup failed; see the Podman/runsc error above.\n\
     Common causes:\n\
     \x20 - \"systemd error: Access denied\": the systemd cgroup manager\n\
     \x20   needs the system bus; set AGENT_GVISOR_PODMAN_CGROUP_MANAGER=cgroupfs\n\
     \x20 - \"cannot set up cgroup for root\": runsc cannot write the root\n\
     \x20   cgroup; set AGENT_GVISOR_PODMAN_RUNTIME_FLAGS=ignore-cgroups\n\
     \x20 - unprivileged user namespaces disabled, or missing\n\
     \x20   /etc/subuid and /etc/subgid ranges for this user"
        .to_string()
}

/// The warning when the model endpoint does not answer from a sandbox.
pub fn endpoint_unreachable_message(endpoint: &str) -> String {
    format!(
        "warning: model endpoint {endpoint} is NOT reachable from a sandbox.\n\
         runsc runs its own network stack: neither the host loopback nor\n\
         a pasta \"-T\" listener in the container netns is visible to it.\n\
         The supported path is pasta --map-guest-addr, which maps a chosen\n\
         address to the host global address, where a port-scoped forwarder\n\
         (systemd-socket-proxyd) proxies to the loopback-only LiteLLM proxy.\n\
         Set it via the AGENT_GVISOR_NETWORK env (a \"pasta:...\" podman network\n\
         spec, e.g. pasta:--map-guest-addr,<endpoint-host>). Also confirm the\n\
         host litellm proxy is up on 127.0.0.1:<port> and the forwarder socket\n\
         (agent-litellm-forward) is active on 0.0.0.0:<forward-port>."
    )
}

/// The warning when the model list is unusable (unreachable, empty, or
/// without a single model id).
pub fn models_empty_message(url: &str) -> String {
    format!(
        "warning: {url} did not return a usable model list.\n\
         The endpoint answers, but the sandbox would see no model to talk to.\n\
         Check that the host litellm proxy has models configured and that the\n\
         API key expected by the proxy is present in the sandbox environment\n\
         (an auth failure also shows up as an empty list here)."
    )
}

/// The warning when one loopback relay does not answer.
pub fn relay_unreachable_message(lport: &str, rule: &str) -> String {
    format!(
        "warning: the relay for 127.0.0.1:{lport} does not answer.\n\
         It is started by /bin/agent-gvisor-init inside the sandbox and\n\
         needs socat in the image; check that the image is current\n\
         (agent-gvisor-load-image --test) and that the target of the rule\n\
         {} is reachable from a sandbox (see the model endpoint check above).",
        crate::shellwords::quote(rule)
    )
}
