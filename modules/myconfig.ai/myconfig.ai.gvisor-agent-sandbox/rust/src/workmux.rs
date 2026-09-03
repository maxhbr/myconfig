// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `agent-gvisor workmux` — run the WHOLE workmux/tmux session of the
//! current repository inside ONE gVisor sandbox (docs/spec.md §16).
//!
//! This is the gVisor tier of the "one sandbox owns the whole session"
//! family that also exists for bubblewrap (`agent-bubblewrap-workmux-tmux`)
//! and for microVMs (`agent-qemu-workmux-tmux`). Unlike `start`, it
//!
//!   * does NOT clone the repository — the real checkout is bind-mounted at
//!     its own host path, so the session works on the user's files
//!     directly;
//!   * does NOT create or manage any git worktree — `workmux` does that
//!     itself, inside the sandbox, under the `<repo>__worktrees` sibling,
//!     which is bind-mounted at its host path too so workmux's
//!     `dirname(top)/basename(top)__worktrees` convention resolves;
//!   * is NOT a registered session: there is no name, no registry entry and
//!     no `merge`/`fetch`/`push`/`destroy` around it. Its container is
//!     derived from the repository, so one repository has exactly one
//!     workmux sandbox.
//!
//! Everything else (the podman argument vector, home seeding, the network
//! and nix defaults) is shared verbatim with `start`.

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

use crate::error::{die, log, need};
use crate::podman::{self, Pod};
use crate::seed;
use crate::session::git_stdout;
use crate::shellwords::{quote, split_ws};
use crate::state::{self, Env, Meta};

/// The in-sandbox entrypoint run when no `-- COMMAND` is given: a plain
/// `/bin` script baked into the image by the NixOS module
/// (../workmux.nix), which writes the in-sandbox workmux configuration and
/// boots the workmux tmux session on a private socket. Overridable with
/// `$AGENT_GVISOR_WORKMUX_COMMAND` (word-split) or `-- COMMAND…`.
pub const DEFAULT_ENTRY: &str = "/bin/workmux-gvisor-entry";

/// `--hostname` and `AGENT_SESSION` of the workmux sandbox, matching the
/// user-facing wrapper name (and the qemu guest hostname of the microVM
/// tier).
pub const SESSION_NAME: &str = "agent-gvisor-workmux-tmux";

/// `chmod 700` (mode bits REPLACED).
fn chmod_700(p: &Path) {
    if let Ok(meta) = fs::metadata(p) {
        let mut perm = meta.permissions();
        perm.set_mode(0o700);
        let _ = fs::set_permissions(p, perm);
    }
}

/// `agent-gvisor workmux [-- COMMAND…]`. Always terminates: `exec`s podman.
pub fn cmd_workmux(env: Env, args: &[String]) -> ! {
    // The only argument is the optional COMMAND override, with or without
    // the `--` separator (like `run`).
    let mut command: Vec<String> = match args.first().map(String::as_str) {
        None => Vec::new(),
        Some("--") => args[1..].to_vec(),
        Some("-h") | Some("--help") => {
            crate::cli::usage();
            std::process::exit(0);
        }
        Some(a) if a.starts_with('-') => die(&format!("unknown workmux option: {a}")),
        Some(_) => args.to_vec(),
    };

    need("git");
    need("podman");
    need("sha256sum");
    podman::try_check_runtime(&env).unwrap_or_else(|m| die(&m));
    podman::try_check_image(&env, &env.default_image).unwrap_or_else(|m| die(&m));

    // --- the repository -------------------------------------------------
    // Must be run from a git checkout; the sandbox owns the MAIN checkout
    // plus every worktree workmux creates next to it.
    let cwd = std::env::current_dir().unwrap_or_default();
    let toplevel = git_stdout(&["rev-parse".to_string(), "--show-toplevel".to_string()])
        .filter(|t| !t.is_empty())
        .unwrap_or_else(|| die(&format!("not a Git working tree: {}", cwd.display())));
    let repo = fs::canonicalize(&toplevel).unwrap_or_else(|_| PathBuf::from(&toplevel));
    let repo_str = repo.display().to_string();

    // Refuse a LINKED worktree: in one the per-worktree git dir differs from
    // the shared common dir, and the `<basename>__worktrees` sibling of a
    // worktree is not where workmux keeps its worktrees.
    let git_dir = git_stdout(&[
        "-C".to_string(),
        repo_str.clone(),
        "rev-parse".to_string(),
        "--path-format=absolute".to_string(),
        "--git-dir".to_string(),
    ])
    .unwrap_or_default();
    let git_common_dir = git_stdout(&[
        "-C".to_string(),
        repo_str.clone(),
        "rev-parse".to_string(),
        "--path-format=absolute".to_string(),
        "--git-common-dir".to_string(),
    ])
    .unwrap_or_default();
    if git_dir != git_common_dir {
        let main = Path::new(&git_common_dir)
            .parent()
            .map(|p| p.display().to_string())
            .unwrap_or_default();
        die(&format!(
            "refusing to run from a linked worktree; run it from the main checkout ({main})"
        ));
    }

    // The worktrees sibling workmux uses: `dirname(top)/basename(top)__worktrees`.
    // Bind-mounted at its HOST path, so the same convention resolves inside.
    let worktrees = {
        let parent = repo.parent().unwrap_or_else(|| Path::new("/"));
        let base = repo
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_default();
        parent.join(format!("{base}__worktrees"))
    };
    fs::create_dir_all(&worktrees)
        .unwrap_or_else(|e| die(&format!("cannot create {}: {e}", worktrees.display())));

    // --- sandbox state ----------------------------------------------------
    // One sandbox per repository, its state next to the repository like the
    // session state of `start` (docs/spec.md §8), under a `__workmux`
    // directory that can never collide with a session name (`__sessions`
    // and the session clones are siblings of it).
    let repo_id = state::repo_id(&repo);
    let meta_dir = state::repo_agent_root(&repo).join("__workmux");
    let home = meta_dir.join("home");
    fs::create_dir_all(&home).unwrap_or_else(|e| die(&format!("cannot create {home:?}: {e}")));
    // The sandbox home is bind-mounted over /home/agent, masking the XDG
    // directories the image created, so create them on the host side.
    for d in [".cache", ".config", ".local/state"] {
        let _ = fs::create_dir_all(home.join(d));
    }
    chmod_700(&meta_dir);
    chmod_700(&home);

    let container = state::sanitize_container_name(&format!("agent-workmux-{repo_id}"));
    let pod = Pod::new(&env);
    if pod.container_exists(&container) {
        let running = pod
            .inspect(&container, "{{.State.Running}}")
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| "false".to_string());
        if running == "true" {
            die(&format!(
                "a workmux sandbox is already running for this repository: {container}\n\
                 Attach to its tmux session with:\n\
                 \x20 podman exec -it {container} {DEFAULT_ENTRY}"
            ));
        }
    }

    // Seed /home/agent exactly like `start` (docs/spec.md §11), so the
    // agents workmux launches inside the sandbox find their configuration.
    let seed_str = env
        .home_seed
        .clone()
        .or_else(|| seed::resolve_home_files().map(|p| p.display().to_string()));
    if let Some(seed_str) = seed_str.filter(|s| !s.is_empty()) {
        let seed_path = PathBuf::from(&seed_str);
        if !seed_path.is_dir() {
            die(&format!("home seed is not a directory: {seed_str}"));
        }
        seed::seed_home(&env, &home, &seed_path);
    }

    // --- the container ----------------------------------------------------
    // `worktree = repo`: the REAL checkout is mounted at its own host path
    // (no clone, see the module docs). `branch` is informational only —
    // nothing here creates or merges a branch.
    let branch = git_stdout(&[
        "-C".to_string(),
        repo_str.clone(),
        "symbolic-ref".to_string(),
        "--short".to_string(),
        "HEAD".to_string(),
    ])
    .unwrap_or_default();
    let meta = Meta {
        name: SESSION_NAME.to_string(),
        repo: repo_str.clone(),
        repo_id,
        worktree: repo_str.clone(),
        home: home.display().to_string(),
        container: container.clone(),
        branch,
        image: env.default_image.clone(),
        memory: "8g".to_string(),
        cpus: "4".to_string(),
        pids_limit: "2048".to_string(),
        network: env.network.clone(),
        seccomp_unconfined: "false".to_string(),
        env_file: String::new(),
        nix: if env.nix { "true" } else { "false" }.to_string(),
    };
    fs::write(meta_dir.join("meta"), meta.to_text()).unwrap_or_else(|e| die(&format!("cannot write meta: {e}")));
    // The extra bind mount read back by `build_run_args`: the worktrees
    // sibling, at its host path, read-write (workmux creates and removes
    // linked worktrees in it).
    let _ = fs::write(
        meta_dir.join("mounts.tsv"),
        format!(
            "{0}\t{0}\trw\n",
            worktrees.display()
        ),
    );
    let _ = fs::write(meta_dir.join("env.list"), "\n");

    if command.is_empty() {
        let entry = std::env::var("AGENT_GVISOR_WORKMUX_COMMAND")
            .ok()
            .filter(|v| !v.is_empty())
            .unwrap_or_else(|| DEFAULT_ENTRY.to_string());
        command = split_ws(&entry);
    }

    log(&format!(
        "starting the workmux sandbox for {repo_str} (worktrees: {})",
        worktrees.display()
    ));
    let argv = podman::build_run_args(&env, &meta, &meta_dir, false, &command);
    let mut lc = String::new();
    for a in &argv {
        lc.push_str(&quote(a));
        lc.push(' ');
    }
    lc.push('\n');
    let _ = fs::write(meta_dir.join("last-command"), lc);
    use std::os::unix::process::CommandExt;
    let err = std::process::Command::new(&argv[0]).args(&argv[1..]).exec();
    let _ = err; // exec only returns on failure
    eprintln!("agent-gvisor: error: failed to run podman");
    std::process::exit(127);
}
