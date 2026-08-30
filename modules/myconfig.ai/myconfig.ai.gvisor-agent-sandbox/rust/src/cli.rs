// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Usage text and argument parsing (docs/spec.md §1–§3).
//!
//! Hand-rolled on purpose: the CLI is small, stable and fully specified, and
//! embedding the historical usage heredoc verbatim keeps `agent-gvisor
//! --help` byte-identical to the bash CLI (a help formatter would not).

use crate::error::{die, fail_raw};
use crate::state::{validate_name, Env};

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

/// `realpath -e` — `Err` carries the RAW diagnostic GNU realpath prints
/// itself (the bash CLI died on it under `set -e`, without a `die` prefix).
fn realpath_e(p: &str) -> Result<String, String> {
    std::fs::canonicalize(p)
        .map(|c| c.to_string_lossy().into_owned())
        .map_err(|_| format!("realpath: {p}: No such file or directory"))
}

/// Parse `start` arguments (after the subcommand word; for the shorthand the
/// session name is PREPENDED by the dispatcher, see [`super::session`]).
/// Mount specs are validated and canonicalized inline, exactly like the
/// bash command substitution around `parse_mount`; errors `die` (or exit
/// raw for `realpath`, like the bash CLI).
pub fn parse_start(env: &Env, args: &[String]) -> StartArgs {
    // Bash `${2:?}`: missing value ⇒ normalized `option requires a value`
    // error (docs/spec.md §14.7); a set-but-empty value is ACCEPTED.
    let val = |flag: &str, args: &[String], i: &mut usize| -> String {
        if *i >= args.len() {
            die(&format!("option requires a value: {flag}"));
        }
        let v = args[*i].clone();
        *i += 1;
        v
    };

    let mut name: Option<String> = None;
    let mut repo: Option<String> = None;
    let mut base = "HEAD".to_string();
    let mut branch: Option<String> = None;
    let mut image = env.default_image.clone();
    let mut mounts: Vec<MountSpec> = Vec::new();
    let mut envs: Vec<String> = Vec::new();
    let mut env_file: Option<String> = None;
    let mut network = env.network.clone();
    let mut detach = false;
    let mut memory = "8g".to_string();
    let mut cpus = "4".to_string();
    let mut pids_limit = "2048".to_string();
    let mut seccomp_unconfined = false;
    let mut force = false;
    let mut home_seed: Option<String> = None;
    let mut seed_home_enabled = true;
    let mut positional: Vec<String> = Vec::new();
    let mut command: Vec<String> = Vec::new();

    let mut i = 0;
    while i < args.len() {
        let arg = args[i].clone();
        i += 1;
        match arg.as_str() {
            "--name" => name = Some(val("--name", args, &mut i)),
            "--repo" => repo = Some(val("--repo", args, &mut i)),
            "--base" => base = val("--base", args, &mut i),
            "--branch" => branch = Some(val("--branch", args, &mut i)),
            "--image" => image = val("--image", args, &mut i),
            "--config" => {
                mounts.push(parse_mount(&val("--config", args, &mut i), "ro"));
            }
            "--mount" => {
                mounts.push(parse_mount(&val("--mount", args, &mut i), "rw"));
            }
            "--env" => envs.push(val("--env", args, &mut i)),
            "--env-file" => match realpath_e(&val("--env-file", args, &mut i)) {
                Ok(p) => env_file = Some(p),
                Err(raw) => fail_raw(&raw), // raw realpath diagnostic, no prefix
            },
            "--network" => network = val("--network", args, &mut i),
            "--detach" => detach = true,
            "--memory" => memory = val("--memory", args, &mut i),
            "--cpus" => cpus = val("--cpus", args, &mut i),
            "--pids-limit" => pids_limit = val("--pids-limit", args, &mut i),
            "--seccomp-unconfined" => seccomp_unconfined = true,
            "--force" => force = true,
            "--home-seed" => match realpath_e(&val("--home-seed", args, &mut i)) {
                Ok(p) => home_seed = Some(p),
                Err(raw) => fail_raw(&raw), // raw realpath diagnostic, no prefix
            },
            "--no-home-seed" => seed_home_enabled = false,
            "--" => {
                command.extend(args[i..].iter().cloned());
                break;
            }
            "-h" | "--help" => {
                usage();
                std::process::exit(0);
            }
            a if a.starts_with('-') => die(&format!("unknown start option: {a}")),
            a => positional.push(a.to_string()),
        }
    }

    // The session name may be given positionally; anything after it is the
    // command, exactly like the arguments after `--`.
    if let Some(first) = positional.first().cloned() {
        if name.is_some() {
            die("session name given twice (--name and positional)");
        }
        name = Some(first);
        command.splice(..0, positional[1..].iter().cloned());
    }
    let name = match name {
        Some(n) => n,
        // Without --repo, start from the current directory, which is what
        // the shorthand `agent-gvisor NAME` is for.
        None => die("session name is required (NAME or --name)"),
    };
    // The session name must be valid; the bash CLI checks it right after
    // resolving the name and $PWD-repo default.
    if let Err(msg) = validate_name(&name) {
        die(&msg);
    }
    StartArgs {
        name,
        repo,
        base,
        branch,
        image,
        mounts,
        envs,
        env_file,
        network,
        detach,
        memory,
        cpus,
        pids_limit,
        seccomp_unconfined,
        force,
        home_seed,
        seed_home_enabled,
        command,
    }
}

/// Parse `HOST:DEST[:MODE]` (bash `IFS=: read -r host dest mode extra`);
/// `Err` carries the exact `die` message.
pub fn try_parse_mount(spec: &str, default_mode: &str) -> Result<MountSpec, String> {
    // `read` semantics: the first three colon-separated fields, the fourth
    // var swallows the rest — a non-empty fourth field means >3 fields.
    let mut fields = spec.split(':');
    let host = fields.next().unwrap_or_default();
    let dest = fields.next().unwrap_or_default();
    let mode = fields.next().unwrap_or_default();
    let extra = fields.next().unwrap_or_default();
    // bash `read` gives the 4th var the remainder RE-JOINED with ':'; only
    // an empty remainder passes, which the next() already guarantees.
    if host.is_empty() || dest.is_empty() || !extra.is_empty() {
        return Err(format!("invalid mount '{spec}'; expected HOST:DEST[:ro|rw]"));
    }
    if !dest.starts_with('/') {
        return Err(format!("container mount destination must be absolute: {dest}"));
    }
    let mode = if mode.is_empty() { default_mode } else { mode };
    if mode != "ro" && mode != "rw" {
        return Err(format!("mount mode must be ro or rw: {spec}"));
    }
    // NOTE: the bash CLI loses the host in this message (the failed
    // `host=$(realpath …)` assignment clobbers the variable); this rewrite
    // deliberately keeps the original host (docs/spec.md §14.3).
    let host = realpath_e(host)
        .map_err(|_| format!("mount source does not exist: {host}"))?;
    Ok(MountSpec {
        host,
        dest: dest.to_string(),
        mode: mode.to_string(),
    })
}

/// Like [`try_parse_mount`], but `die`s on error.
pub fn parse_mount(spec: &str, default_mode: &str) -> MountSpec {
    match try_parse_mount(spec, default_mode) {
        Ok(m) => m,
        Err(msg) => die(&msg),
    }
}
