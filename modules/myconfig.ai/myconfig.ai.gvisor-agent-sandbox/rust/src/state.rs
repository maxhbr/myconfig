// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Session names, registry, repo-adjacent paths, environment defaults and
//! the `meta` record (docs/spec.md §4, §8).

use std::path::{Path, PathBuf};

use crate::error::die;
use crate::shellwords::{quote, split_ws, unquote};

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

/// `${VAR:-default}`: the default also applies to a set-but-empty value.
fn env_or_nonempty(key: &str, default: &str) -> String {
    match std::env::var(key) {
        Ok(v) if !v.is_empty() => v,
        _ => default.to_string(),
    }
}

/// `${VAR-default}`: the default only applies when the variable is UNSET
/// (a set-but-empty value is kept, which is how the rootless cgroup
/// defaults can be disabled, docs/spec.md §5).
fn env_or_unset(key: &str, default: &str) -> String {
    match std::env::var(key) {
        Ok(v) => v,
        Err(_) => default.to_string(),
    }
}

/// A non-empty optional value (bash `[[ -n "${VAR-}" ]]` guards).
fn env_opt_nonempty(key: &str) -> Option<String> {
    match std::env::var(key) {
        Ok(v) if !v.is_empty() => Some(v),
        _ => None,
    }
}

impl Env {
    /// Read the environment for the current (real) EUID.
    pub fn from_env() -> Env {
        Env::from_euid(euid())
    }

    /// Read the environment as seen by the given EUID — the rootless
    /// defaults (cgroup manager, runtime flags) differ for root (§5).
    pub fn from_euid(euid: u32) -> Env {
        let state_root = match env_opt_nonempty("AGENT_GVISOR_STATE") {
            Some(p) => PathBuf::from(p),
            None => {
                let base = env_or_nonempty("XDG_STATE_HOME", &home_state_home());
                PathBuf::from(base).join("agent-gvisor")
            }
        };
        let podman_runtime = env_or_nonempty(
            "AGENT_GVISOR_PODMAN_RUNTIME",
            &env_or_nonempty("AGENT_GVISOR_DEFAULT_RUNTIME", "runsc"),
        );
        // `read -r -a` word-splits the value; the `${VAR-default}` default
        // only applies when the variable is unset (see docs/spec.md §5).
        let (cgroup_manager, runtime_flags) = if euid == 0 {
            (
                env_or_unset("AGENT_GVISOR_PODMAN_CGROUP_MANAGER", ""),
                split_ws(&env_or_unset("AGENT_GVISOR_PODMAN_RUNTIME_FLAGS", "")),
            )
        } else {
            (
                env_or_unset("AGENT_GVISOR_PODMAN_CGROUP_MANAGER", "cgroupfs"),
                split_ws(&env_or_unset(
                    "AGENT_GVISOR_PODMAN_RUNTIME_FLAGS",
                    "ignore-cgroups",
                )),
            )
        };
        Env {
            state_root,
            podman_runtime,
            cgroup_manager,
            runtime_flags,
            default_image: env_or_nonempty(
                "AGENT_GVISOR_IMAGE",
                &env_or_nonempty("AGENT_GVISOR_DEFAULT_IMAGE", "localhost/agent-dev:latest"),
            ),
            default_command: env_opt_nonempty("AGENT_GVISOR_DEFAULT_COMMAND"),
            network: env_or_unset("AGENT_GVISOR_NETWORK", ""),
            loopback_forward: env_opt_nonempty("AGENT_GVISOR_LOOPBACK_FORWARD"),
            model_endpoint: env_opt_nonempty("AGENT_GVISOR_MODEL_ENDPOINT"),
            worktrees: env_opt_nonempty("AGENT_GVISOR_WORKTREES"),
            home_seed: env_opt_nonempty("AGENT_GVISOR_HOME_SEED"),
            home_seed_paths: split_ws(&env_or_unset("AGENT_GVISOR_HOME_SEED_PATHS", "")),
            home_seed_rewrite: split_ws(&env_or_unset("AGENT_GVISOR_HOME_SEED_REWRITE", "")),
        }
    }
}

/// `$HOME/.local/state` — `${XDG_STATE_HOME:-$HOME/.local/state}` needs HOME.
fn home_state_home() -> String {
    let home = env_or_nonempty("HOME", "/");
    format!("{home}/.local/state")
}

/// The real effective UID (libc `geteuid`).
pub fn euid() -> u32 {
    extern "C" {
        fn geteuid() -> u32;
    }
    unsafe { geteuid() }
}

/// Lowercase, collapse `[^a-z0-9_.-]` runs to `-`, strip leading/trailing `-`.
pub fn sanitize_container_name(s: &str) -> String {
    let mut out = String::new();
    let mut pending_dash = false;
    for &b in s.as_bytes() {
        let b = b.to_ascii_lowercase(); // tr '[:upper:]' '[:lower:]': ASCII only
        if b.is_ascii_alphanumeric() || b == b'.' || b == b'_' || b == b'-' {
            out.push(b as char);
            pending_dash = false;
        } else if !pending_dash {
            out.push('-');
            pending_dash = true;
        }
    }
    // sed 's/^-+//; s/-+$//': strip leading AND trailing dashes, whether
    // they came from the input or from a collapsed run.
    while out.starts_with('-') {
        out.remove(0);
    }
    while out.ends_with('-') {
        out.pop();
    }
    out
}

/// Session names must match `^[A-Za-z0-9][A-Za-z0-9_.-]*$`.
/// `Err` carries the `die` message.
pub fn validate_name(name: &str) -> Result<(), String> {
    let mut chars = name.chars();
    let ok = match chars.next() {
        Some(c) => c.is_ascii_alphanumeric(),
        None => false,
    } && chars.all(|c| c.is_ascii_alphanumeric() || c == '.' || c == '_' || c == '-');
    if ok {
        Ok(())
    } else {
        Err(format!(
            "invalid session name '{name}' (allowed: letters, digits, dot, underscore, hyphen)"
        ))
    }
}

/// `<dirname repo>/$(basename repo)_agent-gvisor` — the repo-adjacent root
/// hosting `__pools`, `__sessions` and (by default) the worktrees.
pub fn repo_agent_root(repo: &Path) -> PathBuf {
    let parent = repo.parent().unwrap_or_else(|| Path::new("/"));
    let name = repo
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    parent.join(format!("{name}_agent-gvisor"))
}

/// First 16 hex chars of `sha256(<realpath repo>)`, computed by exec'ing
/// `sha256sum` so IDs match sessions created by the bash CLI (no trailing
/// newline in the hashed string).
pub fn repo_id(repo: &Path) -> String {
    use std::io::Write;
    use std::process::{Command, Stdio};
    let repo_str = repo.to_string_lossy().into_owned();
    let mut child = match Command::new("sha256sum")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
    {
        Ok(c) => c,
        Err(_) => die("missing command: sha256sum"),
    };
    // `printf '%s' "$repo"` — no trailing newline.
    let _ = child.stdin.take().unwrap().write_all(repo_str.as_bytes());
    let out = match child.wait_with_output() {
        Ok(o) => o,
        Err(_) => die("missing command: sha256sum"),
    };
    if !out.status.success() {
        std::process::exit(out.status.code().unwrap_or(1));
    }
    let stdout = String::from_utf8_lossy(&out.stdout);
    stdout
        .split_whitespace()
        .next()
        .unwrap_or_default()
        .chars()
        .take(16)
        .collect()
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
        format!(
            "name={}\nrepo={}\nrepo_id={}\npool={}\nworktree={}\nhome={}\ncontainer={}\nbranch={}\nimage={}\nmemory={}\ncpus={}\npids_limit={}\nnetwork={}\nseccomp_unconfined={}\nenv_file={}\n",
            quote(&self.name),
            quote(&self.repo),
            quote(&self.repo_id),
            quote(&self.pool),
            quote(&self.worktree),
            quote(&self.home),
            quote(&self.container),
            quote(&self.branch),
            quote(&self.image),
            quote(&self.memory),
            quote(&self.cpus),
            quote(&self.pids_limit),
            quote(&self.network),
            quote(&self.seccomp_unconfined),
            quote(&self.env_file),
        )
    }

    /// Parse the shell-quoted `key=value` lines. Unknown keys are ignored,
    /// missing keys default to empty. `Err` carries the `die` message.
    pub fn parse(text: &str) -> Result<Meta, String> {
        let mut meta = Meta {
            name: String::new(),
            repo: String::new(),
            repo_id: String::new(),
            pool: String::new(),
            worktree: String::new(),
            home: String::new(),
            container: String::new(),
            branch: String::new(),
            image: String::new(),
            memory: String::new(),
            cpus: String::new(),
            pids_limit: String::new(),
            network: String::new(),
            seccomp_unconfined: String::new(),
            env_file: String::new(),
        };
        for line in text.lines() {
            if line.is_empty() {
                continue;
            }
            let Some((key, value)) = line.split_once('=') else {
                return Err(format!("cannot parse session meta line: {line:?}"));
            };
            let parsed = unquote(value).map_err(|e| format!("cannot parse session meta: {e}"))?;
            match key {
                "name" => meta.name = parsed,
                "repo" => meta.repo = parsed,
                "repo_id" => meta.repo_id = parsed,
                "pool" => meta.pool = parsed,
                "worktree" => meta.worktree = parsed,
                "home" => meta.home = parsed,
                "container" => meta.container = parsed,
                "branch" => meta.branch = parsed,
                "image" => meta.image = parsed,
                "memory" => meta.memory = parsed,
                "cpus" => meta.cpus = parsed,
                "pids_limit" => meta.pids_limit = parsed,
                "network" => meta.network = parsed,
                "seccomp_unconfined" => meta.seccomp_unconfined = parsed,
                "env_file" => meta.env_file = parsed,
                _ => {} // unknown keys are ignored (forward compatibility)
            }
        }
        Ok(meta)
    }
}

/// The registry entry `$STATE_ROOT/sessions/<name>`.
pub fn registry_path(env: &Env, name: &str) -> PathBuf {
    env.state_root.join("sessions").join(name)
}

/// The pre-rewrite rejection (docs/spec.md §14.1): the registry entry is a
/// real directory — the OLD layout, where it WAS the session directory.
pub fn pre_rewrite_message(name: &str, reg: &Path) -> String {
    format!(
        "session {name} is from the pre-rewrite layout; remove it by hand with:\nrm -rf {}",
        quote(&reg.to_string_lossy())
    )
}

/// The incomplete-debris rejection (docs/spec.md §9): a session directory
/// whose `meta` is absent (start writes `meta` last).
pub fn incomplete_message(name: &str, meta_dir: &Path) -> String {
    format!(
        "session {name} is incomplete: an earlier start was interrupted before it\n\
         registered the session. Re-run the start (it cleans the leftovers up),\n\
         or remove {} by hand.",
        meta_dir.display()
    )
}

/// Resolve a session by registry name. `Err` carries the exact `die`
/// message: unknown session, pre-rewrite layout (§14), incomplete debris.
pub fn try_load_session(env: &Env, name: &str) -> Result<Session, String> {
    let reg = registry_path(env, name);
    // `[[ -e "$reg" ]]` follows symlinks: a dangling registry entry counts
    // as absent, exactly like the bash probe.
    if !reg.exists() {
        return Err(format!("unknown session: {name}"));
    }
    if !reg
        .symlink_metadata()
        .map(|m| m.file_type().is_symlink())
        .unwrap_or(false)
    {
        return Err(pre_rewrite_message(name, &reg));
    }
    let meta_dir = std::fs::canonicalize(&reg)
        .map_err(|_| format!("unknown session: {name}"))?;
    let meta_path = meta_dir.join("meta");
    if !meta_path.is_file() {
        if meta_dir.is_dir() {
            return Err(incomplete_message(name, &meta_dir));
        }
        return Err(format!("unknown session: {name}"));
    }
    let text = std::fs::read_to_string(&meta_path)
        .map_err(|e| format!("cannot read session meta: {e}"))?;
    let meta = Meta::parse(&text)?;
    Ok(Session {
        name: name.to_string(),
        meta,
        meta_dir,
    })
}

/// Like [`try_load_session`], but `die`s on error.
pub fn load_session(env: &Env, name: &str) -> Session {
    match try_load_session(env, name) {
        Ok(s) => s,
        Err(msg) => die(&msg),
    }
}
