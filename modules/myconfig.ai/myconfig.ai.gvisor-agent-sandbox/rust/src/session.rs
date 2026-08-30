// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The subcommand bodies (docs/spec.md §3, §9): `start`, `list`, `status`,
//! `run`, `logs`, `shell`, `stop`, `merge`, `destroy`, plus the shared
//! `run_container` orchestration and the incomplete-session recovery.
//!
//! External-command failure handling mirrors bash `set -e`: a failing
//! `git`/`podman` step exits with the external command's code and its own
//! stderr (no `die` prefix), except where the bash original wrapped the
//! call in `|| die …`.

use std::fs;
use std::os::unix::fs::{symlink, PermissionsExt};
use std::os::unix::io::AsRawFd;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus, Stdio};

use crate::cli::StartArgs;
use crate::error::{die, log, need, warn};
use crate::podman::{self, Pod};
use crate::seed;
use crate::shellwords::quote;
use crate::state::{self, Env, Meta, Session};

// --- small shared helpers ------------------------------------------------

/// `git …` with stdio inherited; `Err` only when git cannot be spawned.
fn git_status(args: &[String]) -> ExitStatus {
    Command::new("git")
        .args(args)
        .status()
        .unwrap_or_else(|_| die("missing command: git"))
}

/// Run `git …` and exit with ITS code on failure (bash `set -e` parity).
fn git_check(args: &[String]) {
    let st = git_status(args);
    if !st.success() {
        std::process::exit(st.code().unwrap_or(1));
    }
}

/// True when `git …` exits 0 (stderr suppressed, like the `2>/dev/null`
/// probes).
fn git_ok(args: &[String]) -> bool {
    Command::new("git")
        .args(args)
        .stderr(Stdio::null())
        .stdout(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// Stdout of a successful `git …` with ALL trailing newlines stripped
/// (bash `$(…)`); `None` on failure. Stderr passes through unchanged.
fn git_stdout(args: &[String]) -> Option<String> {
    let out = Command::new("git")
        .args(args)
        .stderr(Stdio::null())
        .output()
        .ok()?;
    if out.status.success() {
        Some(String::from_utf8_lossy(&out.stdout).trim_end_matches('\n').to_string())
    } else {
        None
    }
}

/// `rm -rf` that also removes a SYMLINK entry itself, not its target.
fn rm_rf(p: &Path) {
    match fs::symlink_metadata(p) {
        Ok(m) if m.file_type().is_symlink() || m.file_type().is_file() => {
            let _ = fs::remove_file(p);
        }
        Ok(_) => {
            let _ = fs::remove_dir_all(p);
        }
        Err(_) => {}
    }
}

/// `ln -sfn TARGET LINK` (the registry entry).
fn ln_sfn(target: &Path, link: &Path) {
    let _ = fs::remove_file(link); // replaces a dangling/existing symlink
    symlink(target, link).unwrap_or_else(|e| die(&format!("cannot create registry entry: {e}")));
}

/// `chmod 700` on a path (mode bits REPLACED, like the chmod call).
fn chmod_700(p: &Path) {
    if let Ok(meta) = fs::metadata(p) {
        let mut perm = meta.permissions();
        perm.set_mode(0o700);
        let _ = fs::set_permissions(p, perm);
    }
}

/// Open `path` and take an exclusive `flock` on it (bash `exec 9>…; flock 9`).
/// The lock is held until the returned File is dropped.
fn flock_exclusive(path: &Path) -> fs::File {
    let f = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .open(path)
        .unwrap_or_else(|e| die(&format!("cannot open lockfile: {e}")));
    extern "C" {
        fn flock(fd: std::os::unix::io::RawFd, operation: i32) -> i32;
    }
    const LOCK_EX: i32 = 2;
    let rc = unsafe { flock(f.as_raw_fd(), LOCK_EX) };
    if rc != 0 {
        die("cannot lock the pool lockfile");
    }
    f
}

/// Recover from interrupted-start debris (docs/spec.md §9): log, remove the
/// leftover worktree (via git when the pool still owns it), session dir and
/// registry entry.
fn reset_partial_session(
    name: &str,
    reg: &Path,
    meta_dir: &Path,
    pool: &Path,
    worktree: &Path,
) {
    log(&format!(
        "session {name} is incomplete (interrupted start); removing {}",
        meta_dir.display()
    ));
    if worktree.exists() {
        let wt = worktree.display().to_string();
        let inside = pool.is_dir()
            && git_ok(&[
                "-C".to_string(),
                wt.clone(),
                "rev-parse".to_string(),
                "--is-inside-work-tree".to_string(),
            ]);
        if inside {
            let dirty = git_stdout(&[
                "-C".to_string(),
                wt.clone(),
                "status".to_string(),
                "--porcelain".to_string(),
            ])
            .unwrap_or_default();
            if !dirty.is_empty() {
                die(&format!(
                    "leftover worktree has uncommitted changes: {wt}\n\
                     Inspect it, then remove it with:\n\
                     \x20 git --git-dir={} worktree remove --force {}",
                    quote(&pool.display().to_string()),
                    quote(&wt)
                ));
            }
            let st = git_status(&[
                format!("--git-dir={}", pool.display()),
                "worktree".to_string(),
                "remove".to_string(),
                "--force".to_string(),
                wt.clone(),
            ]);
            if !st.success() {
                // bash parity: `|| die "could not remove leftover worktree: …"`
                die(&format!("could not remove leftover worktree: {wt}"));
            }
        } else {
            rm_rf(worktree);
        }
    }
    rm_rf(meta_dir);
    rm_rf(reg);
}

/// Build the session `meta` record from the resolved start parameters.
fn meta_from_start(
    name: &str,
    repo: &Path,
    repo_id: &str,
    pool: &Path,
    worktree: &Path,
    home: &Path,
    container: &str,
    branch: &str,
    parsed: &StartArgs,
) -> Meta {
    Meta {
        name: name.to_string(),
        repo: repo.display().to_string(),
        repo_id: repo_id.to_string(),
        pool: pool.display().to_string(),
        worktree: worktree.display().to_string(),
        home: home.display().to_string(),
        container: container.to_string(),
        branch: branch.to_string(),
        image: parsed.image.clone(),
        memory: parsed.memory.clone(),
        cpus: parsed.cpus.clone(),
        pids_limit: parsed.pids_limit.clone(),
        network: parsed.network.clone(),
        seccomp_unconfined: if parsed.seccomp_unconfined {
            "true".to_string()
        } else {
            "false".to_string()
        },
        env_file: parsed.env_file.clone().unwrap_or_default(),
    }
}

/// `agent-gvisor start …` — full ordering in docs/spec.md §9 "start ordering".
/// Always terminates: runs the container via `exec`.
pub fn cmd_start(env: Env, args: &[String]) -> ! {
    let parsed = crate::cli::parse_start(&env, args);
    let name = parsed.name.clone();

    // Without --repo, start from the current directory, which is what the
    // shorthand `agent-gvisor NAME` is for.
    let repo_arg = parsed
        .repo
        .clone()
        .unwrap_or_else(|| std::env::current_dir().unwrap_or_default().display().to_string());
    need("git");
    need("podman");
    need("sha256sum");

    // Fail before any worktree or session state is created.
    podman::try_check_runtime(&env).unwrap_or_else(|m| die(&m));
    podman::try_check_image(&env, &parsed.image).unwrap_or_else(|m| die(&m));

    // bash parity: a failing `realpath -e` dies RAW (no `die` prefix).
    let repo = match fs::canonicalize(&repo_arg) {
        Ok(p) => p,
        Err(_) => crate::error::fail_raw(&format!(
            "realpath: {repo_arg}: No such file or directory"
        )),
    };
    if !git_ok(&[
        "-C".to_string(),
        repo.display().to_string(),
        "rev-parse".to_string(),
        "--is-inside-work-tree".to_string(),
    ]) {
        die(&format!("not a Git working tree: {}", repo.display()));
    }
    // Anchor at the repository ROOT even when started from a subdirectory,
    // so <root>__agent-gvisor/ (pools, session state, by default the
    // worktrees) always sits NEXT TO the root, never inside it — and the
    // in-container --workdir / AGENT_WORKTREE (§10) is the root path.
    let toplevel = git_stdout(&[
        "-C".to_string(),
        repo.display().to_string(),
        "rev-parse".to_string(),
        "--show-toplevel".to_string(),
    ])
    .filter(|t| !t.is_empty())
    .unwrap_or_else(|| {
        die(&format!(
            "cannot determine the repository root of: {}",
            repo.display()
        ))
    });
    let repo = fs::canonicalize(&toplevel).unwrap_or(PathBuf::from(toplevel));
    let base_commit = git_stdout(&[
        "-C".to_string(),
        repo.display().to_string(),
        "rev-parse".to_string(),
        "--verify".to_string(),
        format!("{}^{{commit}}", parsed.base),
    ])
    .unwrap_or_else(|| die(&format!("cannot resolve base ref: {}", parsed.base)));
    let branch = parsed
        .branch
        .clone()
        .unwrap_or_else(|| format!("agent/gvisor/{name}"));

    let agent_root = state::repo_agent_root(&repo);
    for d in [
        env.state_root.clone(),
        env.state_root.join("sessions"),
        agent_root.join("__pools"),
        agent_root.join("__sessions"),
    ] {
        fs::create_dir_all(&d).unwrap_or_else(|e| die(&format!("cannot create {d:?}: {e}")));
    }
    let repo_id = state::repo_id(&repo);
    // Pools, session state and worktrees all live NEXT TO the host
    // repository (docs/spec.md §8): `__pools/<repo-id>.git` disposable bare
    // pools, `__sessions/<name>/` session state, `<name>/` worktrees.
    // `$STATE_ROOT/sessions` stays the name→session REGISTRY.
    let pool = agent_root.join("__pools").join(format!("{repo_id}.git"));
    let lockfile = agent_root.join("__pools").join(format!("{repo_id}.lock"));
    let reg = state::registry_path(&env, &name);
    let worktree = match &env.worktrees {
        Some(wt) => PathBuf::from(wt).join(&repo_id).join(&name),
        None => agent_root.join(&name),
    };
    let meta_dir = agent_root.join("__sessions").join(&name);
    let home = meta_dir.join("home");
    let container = state::sanitize_container_name(&format!("agent-{repo_id}-{name}"));

    // Probe the registry entry, not $meta_dir (docs/spec.md §9): the
    // entry is the session's source of truth.
    if reg.exists() {
        let meta_present = fs::canonicalize(&reg)
            .map(|d| d.join("meta").is_file())
            .unwrap_or(false);
        if !meta_present {
            // Debris of an interrupted start: report and clean it up.
            reset_partial_session(&name, &reg, &meta_dir, &pool, &worktree);
        } else {
            match state::try_load_session(&env, &name) {
                Ok(old) => {
                    let old_branch = old.meta.branch.clone();
                    if parsed.force {
                        log(&format!(
                            "--force: destroying existing session {name} and deleting branch {old_branch}"
                        ));
                    } else if !confirm_destroy_existing(&name, &old_branch) {
                        die(&format!(
                            "session already exists: {name} (pass --force, or remove it with \
                             'agent-gvisor destroy {name} --force --delete-branch')"
                        ));
                    }
                    destroy_session(&env, &old, true, true)
                        .unwrap_or_else(|_| die(&format!("could not destroy the existing session: {name}")));
                }
                // Pre-rewrite (or otherwise unloadable) entry: keep the
                // exact bash outcome for both force and non-force.
                Err(_) => {
                    if !parsed.force {
                        die(&format!(
                            "session already exists: {name} (pass --force, or remove it with \
                             'agent-gvisor destroy {name} --force --delete-branch')"
                        ));
                    }
                    die(&format!("could not destroy the existing session: {name}"));
                }
            }
        }
    }
    if let Some(parent) = worktree.parent() {
        fs::create_dir_all(parent).unwrap_or_else(|e| die(&format!("cannot create worktree parent: {e}")));
    }
    fs::create_dir_all(&meta_dir).unwrap();
    fs::create_dir_all(&home).unwrap();
    // Register the session so name-only commands find the repo-adjacent state.
    ln_sfn(&meta_dir, &reg);
    // The session home is bind-mounted over /home/agent, which masks the
    // XDG directories created in the image, so create them on the host side.
    for d in [".cache", ".config", ".local/state"] {
        fs::create_dir_all(home.join(d)).unwrap();
    }
    chmod_700(&meta_dir);
    chmod_700(&home);

    if parsed.seed_home_enabled {
        let seed_str = parsed
            .home_seed
            .clone()
            .or_else(|| env.home_seed.clone())
            .or_else(|| seed::resolve_home_files().map(|p| p.display().to_string()));
        if let Some(seed_str) = seed_str.filter(|s| !s.is_empty()) {
            let seed_path = PathBuf::from(&seed_str);
            if !seed_path.is_dir() {
                die(&format!("home seed is not a directory: {seed_str}"));
            }
            seed::seed_home(&env, &home, &seed_path);
        }
    }

    let _lock = flock_exclusive(&lockfile);

    if !pool.is_dir() {
        git_check(&[
            "init".to_string(),
            "--bare".to_string(),
            pool.display().to_string(),
        ]);
        git_check(&[
            format!("--git-dir={}", pool.display()),
            "remote".to_string(),
            "add".to_string(),
            "host".to_string(),
            repo.display().to_string(),
        ]);
    } else {
        git_check(&[
            format!("--git-dir={}", pool.display()),
            "remote".to_string(),
            "set-url".to_string(),
            "host".to_string(),
            repo.display().to_string(),
        ]);
    }
    git_check(&[
        format!("--git-dir={}", pool.display()),
        "fetch".to_string(),
        "--prune".to_string(),
        "--no-recurse-submodules".to_string(),
        "host".to_string(),
        "+refs/heads/*:refs/remotes/host/*".to_string(),
        "+refs/tags/*:refs/tags/*".to_string(),
    ]);
    let have_base = Command::new("git")
        .args([
            format!("--git-dir={}", pool.display()),
            "cat-file".to_string(),
            "-e".to_string(),
            format!("{base_commit}^{{commit}}"),
        ])
        .stderr(Stdio::null())
        .stdout(Stdio::null())
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    if !have_base {
        git_check(&[
            format!("--git-dir={}", pool.display()),
            "fetch".to_string(),
            "--no-tags".to_string(),
            "host".to_string(),
            base_commit.clone(),
        ]);
    }

    if worktree.exists() {
        die(&format!("worktree path already exists: {}", worktree.display()));
    }
    let branch_exists = git_ok(&[
        format!("--git-dir={}", pool.display()),
        "show-ref".to_string(),
        "--verify".to_string(),
        "--quiet".to_string(),
        format!("refs/heads/{branch}"),
    ]);
    if branch_exists {
        git_check(&[
            format!("--git-dir={}", pool.display()),
            "worktree".to_string(),
            "add".to_string(),
            worktree.display().to_string(),
            branch.clone(),
        ]);
    } else {
        git_check(&[
            format!("--git-dir={}", pool.display()),
            "worktree".to_string(),
            "add".to_string(),
            "-b".to_string(),
            branch.clone(),
            worktree.display().to_string(),
            base_commit.clone(),
        ]);
    }

    let meta = meta_from_start(
        &name,
        &repo,
        &repo_id,
        &pool,
        &worktree,
        &home,
        &container,
        &branch,
        &parsed,
    );
    fs::write(meta_dir.join("meta"), meta.to_text()).unwrap();
    // `printf '%s\n' "${a[@]}"`: an EMPTY array still writes one newline.
    let mut tsv = String::new();
    for m in &parsed.mounts {
        tsv.push_str(&format!("{}\t{}\t{}\n", m.host, m.dest, m.mode));
    }
    if tsv.is_empty() {
        tsv.push('\n');
    }
    fs::write(meta_dir.join("mounts.tsv"), tsv).unwrap();
    let mut list = String::new();
    for e in &parsed.envs {
        list.push_str(e);
        list.push('\n');
    }
    if list.is_empty() {
        list.push('\n');
    }
    fs::write(meta_dir.join("env.list"), list).unwrap();

    drop(_lock);
    log(&format!(
        "created worktree {} on branch {branch}",
        worktree.display()
    ));
    log(&format!(
        "if the container fails to start the session is kept; retry with \
         'agent-gvisor run {name}', diagnose with 'agent-gvisor doctor', or \
         clean up with 'agent-gvisor destroy {name} --force'"
    ));
    run_container(&env, &name, parsed.detach, &parsed.command)
}

/// Load the session, build the `podman run` argv, record `last-command` and
/// `exec` podman (docs/spec.md §9 "run_container").
pub fn run_container(env: &Env, name: &str, detach: bool, command: &[String]) -> ! {
    let session = state::load_session(env, name);
    podman::try_check_runtime(env).unwrap_or_else(|m| die(&m));
    podman::try_check_image(env, &session.meta.image).unwrap_or_else(|m| die(&m));
    let argv = podman::build_run_args(env, &session.meta, &session.meta_dir, detach, command);
    // bash parity: `printf '%q ' "${cmd[@]}" > last-command; printf '\n'`.
    let mut lc = String::new();
    for a in &argv {
        lc.push_str(&quote(a));
        lc.push(' ');
    }
    lc.push('\n');
    let _ = fs::write(session.meta_dir.join("last-command"), lc);
    // `exec "${cmd[@]}"` — the CLI becomes podman.
    use std::os::unix::process::CommandExt;
    let err = Command::new(&argv[0]).args(&argv[1..]).exec();
    let _ = err;
    eprintln!("agent-gvisor: error: failed to run podman");
    std::process::exit(127);
}

/// The interactive `start`-on-existing-session prompt (docs/spec.md §13):
/// on a terminal (stdin AND stderr), ask before destroying; `y`/`yes` in
/// any case confirms, EOF or anything else fails like the non-interactive
/// case. Returns false without printing when not on a terminal.
fn confirm_destroy_existing(name: &str, old_branch: &str) -> bool {
    use std::io::{BufRead, IsTerminal, Write};
    if !std::io::stdin().is_terminal() || !std::io::stderr().is_terminal() {
        return false;
    }
    // No trailing newline, like the bash `printf '… [y/N] ' >&2`.
    eprint!(
        "agent-gvisor: session {name} already exists; destroy it and delete branch {old_branch}? [y/N] "
    );
    let _ = std::io::stderr().flush();
    let mut reply = String::new();
    if std::io::stdin().lock().read_line(&mut reply).unwrap_or(0) == 0 {
        return false;
    }
    let reply = reply.trim_end_matches(['\n', '\r']);
    matches!(reply.to_lowercase().as_str(), "y" | "yes")
}

/// `agent-gvisor list` — registry table incl. `incomplete` and
/// `incompatible (pre-rewrite layout)` rows. Exits 0.
pub fn cmd_list(env: &Env) -> ! {
    let sessions = env.state_root.join("sessions");
    fs::create_dir_all(&sessions).unwrap();
    println!(
        "{:<24} {:<12} {:<28} {}",
        "SESSION", "STATUS", "BRANCH", "WORKTREE"
    );
    let pod = Pod::new(env);
    let mut names: Vec<String> = match fs::read_dir(&sessions) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .filter(|n| !n.starts_with('.')) // bash glob `*` skips dotfiles
            .collect(),
        Err(_) => Vec::new(),
    };
    names.sort();
    for name in names {
        let reg = sessions.join(&name);
        let is_symlink = reg
            .symlink_metadata()
            .map(|m| m.file_type().is_symlink())
            .unwrap_or(false);
        let meta_dir = fs::canonicalize(&reg).ok();
        let meta_present = meta_dir
            .as_ref()
            .map(|d| d.join("meta").is_file())
            .unwrap_or(false);
        if is_symlink && !meta_present {
            // Debris of an interrupted start: report it instead of dying on
            // the missing meta file.
            println!(
                "{:<24} {:<12} {:<28} {}",
                name,
                "incomplete",
                "-",
                reg.display()
            );
            continue;
        }
        if !is_symlink {
            // The pre-rewrite layout: the registry entry IS the session
            // directory (docs/spec.md §14.1). list never dies.
            println!(
                "{:<24} {:<12} {:<28} {}",
                name,
                "incompatible (pre-rewrite layout)",
                "-",
                reg.display()
            );
            continue;
        }
        let meta_dir = meta_dir.unwrap();
        let text = match fs::read_to_string(meta_dir.join("meta")) {
            Ok(t) => t,
            Err(e) => die(&format!("cannot read session meta: {e}")),
        };
        let meta = match Meta::parse(&text) {
            Ok(m) => m,
            Err(m) => die(&m),
        };
        let status = if pod.container_exists(&meta.container) {
            pod.inspect(&meta.container, "{{.State.Status}}")
                .map(|s| s.trim().to_string())
                .filter(|s| !s.is_empty())
                .unwrap_or_else(|| "unknown".to_string())
        } else {
            "stopped".to_string()
        };
        println!(
            "{:<24} {:<12} {:<28} {}",
            name,
            status,
            meta.branch,
            meta.worktree
        );
    }
    std::process::exit(0);
}

/// `agent-gvisor status NAME` — meta fields, container state, git status.
/// Exits with the trailing `git status --short --branch`'s code.
pub fn cmd_status(env: &Env, name: &str) -> ! {
    let session = state::load_session(env, name);
    let meta = &session.meta;
    print!(
        "session:   {}\nrepo:      {}\nbranch:    {}\nworktree:  {}\npool:      {}\ncontainer: {}\nimage:     {}\n",
        meta.name,
        meta.repo,
        meta.branch,
        meta.worktree,
        meta.pool,
        meta.container,
        meta.image
    );
    let pod = Pod::new(env);
    if pod.container_exists(&meta.container) {
        // The prefix lives in the --format string itself, like the bash
        // original; podman does not interpret the literal `\n` in it.
        pod.run(&[
            "inspect".to_string(),
            "--format".to_string(),
            "status:    {{.State.Status}}\\npid:       {{.State.Pid}}\\nstarted:   {{.State.StartedAt}}".to_string(),
            meta.container.clone(),
        ]);
    } else {
        println!("status:    stopped/absent");
    }
    let st = git_status(&[
        "-C".to_string(),
        meta.worktree.clone(),
        "status".to_string(),
        "--short".to_string(),
        "--branch".to_string(),
    ]);
    std::process::exit(st.code().unwrap_or(1));
}

/// `agent-gvisor run NAME [--detach] -- [COMMAND…]` — refuses a running
/// container, then `run_container`.
pub fn cmd_run(env: Env, args: &[String]) -> ! {
    let name = args[0].clone();
    let mut detach = false;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--detach" => detach = true,
            "--" => {
                i += 1;
                break;
            }
            _ => break, // everything from here on is the COMMAND
        }
        i += 1;
    }
    let command: Vec<String> = args[i..].to_vec();
    let session = state::load_session(&env, &name);
    let pod = Pod::new(&env);
    if pod.container_exists(&session.meta.container) {
        let running = pod
            .inspect(&session.meta.container, "{{.State.Running}}")
            .map(|s| s.trim().to_string())
            .unwrap_or_else(|| "false".to_string());
        if running == "true" {
            die(&format!("container is already running: {name}"));
        }
    }
    run_container(&env, &name, detach, &command)
}

/// `agent-gvisor logs NAME [PODMAN-LOGS-ARGS…]` — `exec`s podman logs.
pub fn cmd_logs(env: Env, args: &[String]) -> ! {
    let name = args[0].clone();
    let session = state::load_session(&env, &name);
    let pod = Pod::new(&env);
    if !pod.container_exists(&session.meta.container) {
        die(&format!("container is absent: {name}"));
    }
    let mut podman_args = vec!["logs".to_string()];
    podman_args.extend(args[1..].iter().cloned());
    podman_args.push(session.meta.container.clone());
    pod.exec(&podman_args)
}

/// `agent-gvisor shell NAME [COMMAND…]` — `exec`s podman exec; defaults to
/// `/bin/bash`.
pub fn cmd_shell(env: Env, args: &[String]) -> ! {
    let name = args[0].clone();
    let session = state::load_session(&env, &name);
    let pod = Pod::new(&env);
    if !pod.container_exists(&session.meta.container) {
        die(&format!("container is not running: {name}"));
    }
    let mut podman_args = vec![
        "exec".to_string(),
        "--interactive".to_string(),
        "--tty".to_string(),
        session.meta.container.clone(),
    ];
    if args.len() <= 1 {
        podman_args.push("/bin/bash".to_string());
    } else {
        podman_args.extend(args[1..].iter().cloned());
    }
    pod.exec(&podman_args)
}

/// `agent-gvisor stop NAME`.
pub fn cmd_stop(env: &Env, name: &str) -> ! {
    let session = state::load_session(env, name);
    let pod = Pod::new(env);
    if pod.container_exists(&session.meta.container) {
        let st = pod.run(&[
            "stop".to_string(),
            "--time".to_string(),
            "10".to_string(),
            session.meta.container.clone(),
        ]);
        std::process::exit(st.code().unwrap_or(1));
    }
    log(&format!(
        "container already absent: {}",
        session.meta.container
    ));
    std::process::exit(0);
}

/// `agent-gvisor merge NAME …` (docs/spec.md §9 "merge").
pub fn cmd_merge(env: Env, args: &[String]) -> ! {
    let name = args[0].clone();
    let mut repo_override: Option<String> = None;
    let mut merge_args: Vec<String> = Vec::new();
    let mut ff_set = false;
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--repo" => {
                if i + 1 >= args.len() {
                    die("option requires a value: --repo");
                }
                repo_override = Some(args[i + 1].clone());
                i += 2;
            }
            "--no-ff" | "--ff" | "--squash" => {
                merge_args.push(args[i].clone());
                ff_set = true;
                i += 1;
            }
            "--" => {
                merge_args.extend(args[i + 1..].iter().cloned());
                break;
            }
            other => {
                merge_args.push(other.to_string());
                i += 1;
            }
        }
    }
    let session = state::load_session(&env, &name);
    let meta = &session.meta;

    // The original repository the session was started from. Allow --repo to
    // override for the unusual case of merging into a different clone.
    let target_repo = match &repo_override {
        Some(p) => {
            // bash parity: realpath prints its own RAW diagnostic, then the
            // `|| die` fires with the --repo message.
            let resolved = fs::canonicalize(p).map(|r| r.display().to_string()).ok();
            match resolved {
                Some(r) => r,
                None => {
                    eprintln!("realpath: {p}: No such file or directory");
                    die(&format!("--repo: not a path: {p}"));
                }
            }
        }
        None => meta.repo.clone(),
    };
    let dot_git = Path::new(&target_repo).join(".git");
    if !(dot_git.is_dir() || dot_git.is_file()) {
        die(&format!("--repo: not a Git work tree: {target_repo}"));
    }

    // Refuse to merge into a detached HEAD or a dirty tree, so a conflict
    // does not leave the host checkout half-merged.
    let current_branch = match git_stdout(&[
        "-C".to_string(),
        target_repo.clone(),
        "symbolic-ref".to_string(),
        "--short".to_string(),
        "HEAD".to_string(),
    ]) {
        Some(b) if !b.is_empty() => b,
        _ => die(&format!(
            "the repository at {target_repo} is in detached HEAD state; \
             switch to the branch you want to merge into first"
        )),
    };
    let porcelain = git_stdout(&[
        "-C".to_string(),
        target_repo.clone(),
        "status".to_string(),
        "--porcelain".to_string(),
    ])
    .unwrap_or_default();
    if !porcelain.is_empty() {
        die(&format!(
            "working tree of {target_repo} is dirty; commit or stash before merging"
        ));
    }

    // Default to a merge commit (--no-ff) so the feature work stays
    // traceable, unless the caller picks otherwise.
    if !ff_set {
        merge_args.insert(0, "--no-ff".to_string());
    }

    log(&format!(
        "fetching branch {} from pool {} into {target_repo}",
        meta.branch, meta.pool
    ));
    let fetch_ok = git_stdout(&[
        "-C".to_string(),
        target_repo.clone(),
        "fetch".to_string(),
        "--no-tags".to_string(),
        meta.pool.clone(),
        format!("+{}:refs/heads/{}", meta.branch, meta.branch),
    ])
    .is_some();
    if !fetch_ok {
        die("fetch from pool failed; is the session pool still present?");
    }
    log(&format!(
        "merging {} into {current_branch} of {target_repo}",
        meta.branch
    ));
    let mut merge_cmd = vec![
        "-C".to_string(),
        target_repo.clone(),
        "merge".to_string(),
    ];
    merge_cmd.extend(merge_args.iter().cloned());
    merge_cmd.push(meta.branch.clone());
    if git_status(&merge_cmd).success() {
        let _ = Command::new("git")
            .args([
                "-C".to_string(),
                target_repo.clone(),
                "branch".to_string(),
                "-D".to_string(),
                meta.branch.clone(),
            ])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status();
    } else {
        die(&format!(
            "merge failed; resolve conflicts in {target_repo}, then delete the \
             leftover ref with 'git -C \"{target_repo}\" branch -D {}'",
            meta.branch
        ));
    }
    std::process::exit(0);
}

/// The destroy body, as a `Result` so `cmd_start`'s existing-session path
/// can report `could not destroy the existing session` while the standalone
/// `destroy` simply `die`s. `Err` carries the `die` message (not yet
/// printed).
pub fn destroy_session(
    env: &Env,
    session: &Session,
    force: bool,
    delete_branch: bool,
) -> Result<(), String> {
    let meta = &session.meta;
    let pod = Pod::new(env);
    let pool = meta.pool.clone();
    let worktree = meta.worktree.clone();

    if pod.container_exists(&meta.container) {
        let st = pod.run(&[
            "rm".to_string(),
            "--force".to_string(),
            "--time".to_string(),
            "5".to_string(),
            meta.container.clone(),
        ]);
        if !st.success() {
            // bash `set -e` parity: podman's own stderr, its exit code.
            std::process::exit(st.code().unwrap_or(1));
        }
    }

    if Path::new(&worktree).is_dir() {
        if !force {
            let porcelain = git_stdout(&[
                "-C".to_string(),
                worktree.clone(),
                "status".to_string(),
                "--porcelain".to_string(),
            ])
            .unwrap_or_default();
            if !porcelain.is_empty() {
                return Err(
                    "worktree has uncommitted changes; commit them or use --force".to_string(),
                );
            }
        }
        let mut rm_args = vec![
            format!("--git-dir={pool}"),
            "worktree".to_string(),
            "remove".to_string(),
        ];
        if force {
            rm_args.push("--force".to_string());
        }
        rm_args.push(worktree.clone());
        let st = git_status(&rm_args);
        if !st.success() {
            std::process::exit(st.code().unwrap_or(1));
        }
    }

    if delete_branch {
        let st = git_status(&[
            format!("--git-dir={pool}"),
            "branch".to_string(),
            "-D".to_string(),
            meta.branch.clone(),
        ]);
        if !st.success() {
            std::process::exit(st.code().unwrap_or(1));
        }
    }
    rm_rf(&session.meta_dir);
    // Remove the registry entry too: for old-layout sessions it IS the
    // session dir (already removed above), for new-layout ones a symlink.
    rm_rf(&state::registry_path(env, &session.name));
    log(&format!("destroyed session {}", session.name));
    Ok(())
}

/// `agent-gvisor destroy NAME [--force] [--delete-branch]`.
pub fn cmd_destroy(env: Env, args: &[String]) -> ! {
    let name = args[0].clone();
    let mut force = false;
    let mut delete_branch = false;
    for a in &args[1..] {
        match a.as_str() {
            "--force" => force = true,
            "--delete-branch" => delete_branch = true,
            other => die(&format!("unknown destroy option: {other}")),
        }
    }
    let session = state::load_session(&env, &name);
    destroy_session(&env, &session, force, delete_branch).unwrap_or_else(|m| die(&m));
    std::process::exit(0);
}

/// `agent-gvisor doctor` — see docs/spec.md §10 and `src/doctor.rs`.
pub fn cmd_doctor(env: Env) -> ! {
    use crate::doctor;
    need("podman");
    let podman_path = crate::error::which("podman")
        .map(|p| p.display().to_string())
        .unwrap_or_default();
    println!("podman:          {podman_path}");
    println!("runtime:         {}", env.podman_runtime);
    println!(
        "cgroup manager:  {}",
        if env.cgroup_manager.is_empty() {
            "<podman default>".to_string()
        } else {
            env.cgroup_manager.clone()
        }
    );
    println!("runtime flags:   {}", env.runtime_flags.join(" "));
    if podman::cgroups_ignored(&env) {
        println!("limits:          not enforced, the runtime ignores cgroups");
        println!("                 unset AGENT_GVISOR_PODMAN_RUNTIME_FLAGS and delegate");
        println!("                 cgroup controllers to enforce them");
    } else {
        println!("limits:          enforced via cgroups");
    }
    println!("image:           {}", env.default_image);
    println!(
        "state:           {} (session name registry)",
        env.state_root.display()
    );
    println!("pools/sessions:  <repo>__agent-gvisor/{{__pools,__sessions}} next to each repo");
    println!(
        "model endpoint:  {}",
        env.model_endpoint.clone().unwrap_or_else(|| "<unset>".to_string())
    );
    println!(
        "loopback fwd:    {}",
        env.loopback_forward.clone().unwrap_or_else(|| "<none>".to_string())
    );

    podman::try_check_runtime(&env).unwrap_or_else(|m| die(&m));
    podman::try_check_image(&env, &env.default_image).unwrap_or_else(|m| die(&m));

    log("running a throwaway sandbox container");
    let pod = Pod::new(&env);
    if !pod.run(&doctor::sandbox_probe_args(&env)).success() {
        die(&doctor::sandbox_failed_message());
    }
    log("sandbox works");

    // Model access is the one thing a working sandbox still gets wrong
    // silently; probe the endpoint from inside a sandbox, which is the only
    // place where the answer is meaningful (docs/spec.md §10).
    if let Some(endpoint) = env.model_endpoint.clone() {
        log(&format!("checking model endpoint {endpoint} from inside a sandbox"));
        if pod.run(&doctor::endpoint_probe_args(&env)).success() {
            log("model endpoint reachable from the sandbox");
        } else {
            warn(&doctor::endpoint_unreachable_message(&endpoint));
        }
    }

    // The in-sandbox relays are what make the endpoint answer on the
    // sandbox's OWN 127.0.0.1; probe them through the entrypoint wrapper,
    // i.e. exactly as a session starts them.
    if let Some(fwd) = env.loopback_forward.clone() {
        for rule in crate::shellwords::split_ws(&fwd) {
            if rule.is_empty() {
                continue;
            }
            let lport = rule.split(':').next().unwrap_or_default();
            log(&format!("checking in-sandbox relay 127.0.0.1:{lport} ({rule})"));
            if pod.run(&doctor::relay_probe_args(&env, &rule)).success() {
                log(&format!("127.0.0.1:{lport} is served inside the sandbox"));
            } else {
                warn(&doctor::relay_unreachable_message(lport, &rule));
            }
        }
    }
    std::process::exit(0);
}
