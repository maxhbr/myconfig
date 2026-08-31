// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Podman interaction: global arguments, checks, and the `run` argv builder
//! (docs/spec.md §5, §6, §10).
//!
//! [`build_run_args`] is a pure function so the tests can assert the exact
//! argument vector without spawning podman; [`Pod`] wraps actual execution
//! (still exec'ing `podman` from PATH).

use std::path::Path;

use crate::error::warn;
use crate::state::{Env, Meta};

/// Global podman arguments, in order: `--runtime=…`, then
/// `--cgroup-manager=…` (when non-empty), then `--runtime-flag=…` per flag.
pub fn global_args(env: &Env) -> Vec<String> {
    let mut args = vec![format!("--runtime={}", env.podman_runtime)];
    if !env.cgroup_manager.is_empty() {
        args.push(format!("--cgroup-manager={}", env.cgroup_manager));
    }
    for flag in &env.runtime_flags {
        if !flag.is_empty() {
            args.push(format!("--runtime-flag={flag}"));
        }
    }
    args
}

/// True when the runtime flags contain `ignore-cgroups` (⇒ podman's resource
/// limits cannot be enforced, docs/spec.md §5).
pub fn cgroups_ignored(env: &Env) -> bool {
    env.runtime_flags.iter().any(|f| f == "ignore-cgroups")
}

/// Absolute runtimes must be executable; named runtimes are probed with
/// `podman info`. `Err` carries the exact `die` message (docs/spec.md §10).
pub fn try_check_runtime(env: &Env) -> Result<(), String> {
    let runtime = &env.podman_runtime;
    if runtime.starts_with('/') {
        use std::os::unix::fs::PermissionsExt;
        let executable = std::fs::metadata(runtime)
            .map(|m| m.permissions().mode() & 0o111 != 0)
            .unwrap_or(false);
        if !executable {
            return Err(format!("OCI runtime is not executable: {runtime}"));
        }
        return Ok(());
    }
    // Named runtimes must be registered in containers.conf; Podman only
    // reports its default one, so probe the requested name via `podman info`.
    let pod = Pod::new(env);
    if pod.ok(&["info".to_string()]) {
        return Ok(());
    }
    Err(format!(
        "Podman OCI runtime {} is not registered.\n\
         Register it in containers.conf (on NixOS:\n\
         virtualisation.containers.containersConf.settings.engine.runtimes), or\n\
         set AGENT_GVISOR_PODMAN_RUNTIME to the absolute path of a runsc binary.",
        crate::shellwords::quote(runtime)
    ))
}

/// `podman image exists <image>`. `Err` carries the exact `die` message.
pub fn try_check_image(env: &Env, image: &str) -> Result<(), String> {
    // NB: the bash original probes with a PLAIN `podman` here (no global
    // args), and so does this rewrite.
    let pod = Pod::new(env);
    if pod.ok_no_globals(&["image".to_string(), "exists".to_string(), image.to_string()]) {
        return Ok(());
    }
    Err(format!(
        "container image {} is not in the local Podman store.\n\
         Build and load it with: agent-gvisor-load-image\n\
         (or: nix run .#load-image), or pass --image with another reference.",
        crate::shellwords::quote(image)
    ))
}

/// The Podman volume backing the writable Nix store of a `--nix` session:
/// `<container>-nix`, a per-session name derived from the already sanitized
/// container name (docs/nix-in-sandbox.md).
pub fn nix_volume_name(meta: &Meta) -> String {
    format!("{}-nix", meta.container)
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
    // Stricter than `seccomp_unconfined` (see docs/spec.md §8): empty
    // (a session predating the field) must NOT gain a Nix store mount,
    // because the CLI never created the backing volume for such sessions.
    let nix_on = meta.nix == "true";
    let mut cmd: Vec<String> = vec!["podman".to_string()];
    cmd.extend(global_args(env));
    cmd.push("run".to_string());
    cmd.push("--replace".to_string());
    if detach {
        cmd.push("--detach".to_string());
    } else {
        cmd.push("--interactive".to_string());
        cmd.push("--tty".to_string());
    }
    cmd.extend([
        "--name".to_string(),
        meta.container.clone(),
        "--hostname".to_string(),
        meta.name.clone(),
        "--userns=keep-id".to_string(),
        "--read-only".to_string(),
        "--read-only-tmpfs=true".to_string(),
        "--cap-drop=ALL".to_string(),
        "--security-opt=no-new-privileges".to_string(),
        "--workdir".to_string(),
        meta.repo.clone(),
    ]);
    // The session worktree is mounted AT THE ORIGINAL REPO'S PATH inside the
    // container (host src = worktree, dst = repo), so the in-container path
    // matches where the repository normally lives on the host. The host
    // checkout itself is never mounted.
    for (src, dst) in [
        (meta.worktree.as_str(), meta.repo.as_str()),
        (meta.home.as_str(), "/home/agent"),
    ] {
        cmd.extend([
            "--mount".to_string(),
            format!("type=bind,src={src},dst={dst},rw"),
        ]);
    }
    // Writable Nix store for `--nix` sessions: a named volume bound at
    // /nix/store, populated by Podman's copy-up with the image's own store
    // (so the /bin toolchain — all symlinks into /nix/store — keeps
    // working) and writable by the agent. The volume outlives container
    // recreation (`run --replace`) and is removed by `destroy`.
    if nix_on {
        cmd.extend([
            "--mount".to_string(),
            format!("type=volume,src={},dst=/nix/store", nix_volume_name(meta)),
        ]);
    }
    cmd.extend([
        "--env".to_string(),
        "HOME=/home/agent".to_string(),
        "--env".to_string(),
        "XDG_CONFIG_HOME=/home/agent/.config".to_string(),
        "--env".to_string(),
        "XDG_CACHE_HOME=/home/agent/.cache".to_string(),
        "--env".to_string(),
        "XDG_STATE_HOME=/home/agent/.local/state".to_string(),
        "--env".to_string(),
        format!("AGENT_SESSION={}", meta.name),
        "--env".to_string(),
        // The worktree's in-container path (the original repo's path).
        format!("AGENT_WORKTREE={}", meta.repo),
    ]);

    // Reverse port forwards, set up by /bin/agent-gvisor-init inside the
    // sandbox: gVisor's loopback can only be bound from within, so this is
    // the only way to make a host-side endpoint answer on the sandbox's
    // 127.0.0.1.
    if let Some(fwd) = &env.loopback_forward {
        cmd.extend([
            "--env".to_string(),
            format!("AGENT_GVISOR_LOOPBACK_FORWARD={fwd}"),
        ]);
    }

    // Nix inside the sandbox (docs/nix-in-sandbox.md): daemon-less local
    // store (the image's /nix/var stays read-only, so the state lives on
    // the session home), disk-backed TMPDIR (the container /tmp is a
    // tmpfs and large builds would hit the memory limit) and the gate env
    // for /bin/agent-gvisor-init.
    if nix_on {
        cmd.extend([
            "--env".to_string(),
            "NIX_REMOTE=local".to_string(),
            "--env".to_string(),
            "NIX_STATE_DIR=/home/agent/.local/state/nix".to_string(),
            "--env".to_string(),
            "NIX_LOG_DIR=/home/agent/.local/state/nix/log".to_string(),
            "--env".to_string(),
            "TMPDIR=/home/agent/.cache/nix-tmp".to_string(),
            "--env".to_string(),
            "AGENT_GVISOR_NIX=1".to_string(),
        ]);
        if let Some(nix_config) = &env.nix_config {
            cmd.extend([
                "--env".to_string(),
                format!("NIX_CONFIG={nix_config}"),
            ]);
        }
    }

    if cgroups_ignored(env) {
        warn("memory/cpu/pids limits not enforced, the runtime ignores cgroups");
    } else {
        cmd.extend([
            "--pids-limit".to_string(),
            meta.pids_limit.clone(),
            "--memory".to_string(),
            meta.memory.clone(),
            "--cpus".to_string(),
            meta.cpus.clone(),
        ]);
    }

    if !meta.network.is_empty() {
        cmd.extend(["--network".to_string(), meta.network.clone()]);
    }
    // bash-source semantics: anything but the exact string "false" keeps
    // the inner OCI seccomp profile disabled.
    if meta.seccomp_unconfined != "false" {
        cmd.push("--security-opt=seccomp=unconfined".to_string());
    }
    if !meta.env_file.is_empty() {
        cmd.extend(["--env-file".to_string(), meta.env_file.clone()]);
    }

    if let Ok(text) = std::fs::read_to_string(meta_dir.join("mounts.tsv")) {
        for line in text.lines() {
            let mut fields = line.splitn(3, '\t');
            let (Some(host), Some(dest), mode) = (
                fields.next(),
                fields.next(),
                fields.next().unwrap_or_default(),
            ) else {
                continue;
            };
            if host.is_empty() {
                continue;
            }
            cmd.extend([
                "--mount".to_string(),
                format!("type=bind,src={host},dst={dest},{mode}"),
            ]);
        }
    }
    if let Ok(text) = std::fs::read_to_string(meta_dir.join("env.list")) {
        for item in text.lines() {
            if item.is_empty() {
                continue;
            }
            cmd.extend(["--env".to_string(), item.to_string()]);
        }
    }

    cmd.push(meta.image.clone());
    // Wrap the payload so the relays exist before it starts. The wrapper
    // execs, so the payload still gets PID 1, the TTY and the signals.
    if env.loopback_forward.is_some() || nix_on {
        cmd.push("/bin/agent-gvisor-init".to_string());
    }
    if command.is_empty() {
        // No COMMAND given: run the configured default. Word-split so a
        // default like "herdr --flag" works; `agent-gvisor shell` is
        // unaffected and always gives a plain shell.
        let default = match &env.default_command {
            Some(d) => crate::shellwords::split_ws(d),
            None => Vec::new(),
        };
        if default.is_empty() {
            cmd.push("/bin/bash".to_string());
        } else {
            cmd.extend(default);
        }
    } else {
        cmd.extend(command.iter().cloned());
    }
    cmd
}

/// Execution wrapper around the `podman` binary found on PATH.
pub struct Pod<'a> {
    pub env: &'a Env,
}

impl<'a> Pod<'a> {
    pub fn new(env: &'a Env) -> Pod<'a> {
        Pod { env }
    }

    /// `podman <globals> <args…>` without globals.
    fn command_no_globals(args: &[String]) -> std::process::Command {
        let mut c = std::process::Command::new("podman");
        c.args(args);
        c
    }

    /// `podman <globals> <args…>`, stdio inherited.
    fn command(&self, args: &[String]) -> std::process::Command {
        let mut c = std::process::Command::new("podman");
        c.args(global_args(self.env));
        c.args(args);
        c
    }

    /// Run podman (without replacing the process) and return its status.
    pub fn run(&self, args: &[String]) -> std::process::ExitStatus {
        self.command(args)
            .status()
            .unwrap_or_else(|_| crate::error::die("missing command: podman"))
    }

    /// Like [`Pod::run`] but without the global arguments (plain `podman …`
    /// calls, no globals).
    pub fn run_no_globals(&self, args: &[String]) -> std::process::ExitStatus {
        Self::command_no_globals(args)
            .status()
            .unwrap_or_else(|_| crate::error::die("missing command: podman"))
    }

    /// Run podman and replace the process with it (bash `podman_exec_c`):
    /// exits with podman's exit code.
    pub fn exec(&self, args: &[String]) -> ! {
        use std::os::unix::process::CommandExt;
        let err = self.command(args).exec();
        let _ = err; // exec only returns on failure
        eprintln!("agent-gvisor: error: failed to run podman");
        std::process::exit(127);
    }

    /// True when the command exits 0 (stderr suppressed, like the bash
    /// `2>/dev/null` probes).
    pub fn ok(&self, args: &[String]) -> bool {
        self.command(args)
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    /// Like [`Pod::ok`] but without the global arguments.
    pub fn ok_no_globals(&self, args: &[String]) -> bool {
        Self::command_no_globals(args)
            .stderr(std::process::Stdio::null())
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    }

    /// Stdout of a successful run (raw, NOT trimmed — bash `$(…)` trimming is
    /// the caller's job), `None` on failure.
    pub fn output(&self, args: &[String]) -> Option<String> {
        let out = self
            .command(args)
            .stderr(std::process::Stdio::null())
            .output()
            .ok()?;
        if out.status.success() {
            Some(String::from_utf8_lossy(&out.stdout).into_owned())
        } else {
            None
        }
    }

    /// `podman container exists <name>`.
    pub fn container_exists(&self, container: &str) -> bool {
        self.ok(&[
            "container".to_string(),
            "exists".to_string(),
            container.to_string(),
        ])
    }

    /// `podman inspect --format <fmt> <name>` — raw stdout on success.
    pub fn inspect(&self, container: &str, fmt: &str) -> Option<String> {
        self.output(&[
            "inspect".to_string(),
            "--format".to_string(),
            fmt.to_string(),
            container.to_string(),
        ])
    }
}
