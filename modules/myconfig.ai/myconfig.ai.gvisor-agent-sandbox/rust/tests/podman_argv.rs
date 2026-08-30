// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The exact `podman run` argument vector (docs/spec.md §10) — pure builder
//! tests plus a binary-level test asserting the argv the recording podman
//! stub saw for a full `start`.

mod common;

use std::fs;
use std::path::PathBuf;

use agent_gvisor::podman::build_run_args;
use agent_gvisor::shellwords::quote;
use agent_gvisor::state::{Env, Meta};
use common::{Scenario, TempDir, IMAGE};

fn test_env() -> Env {
    Env {
        state_root: PathBuf::from("/tmp/agent-gvisor-test-state"),
        podman_runtime: "runsc".to_string(),
        cgroup_manager: "cgroupfs".to_string(),
        runtime_flags: vec!["ignore-cgroups".to_string()],
        default_image: IMAGE.to_string(),
        default_command: None,
        network: String::new(),
        loopback_forward: None,
        model_endpoint: None,
        worktrees: None,
        home_seed: None,
        home_seed_paths: vec![],
        home_seed_rewrite: vec![],
        nix: false,
        nix_config: None,
    }
}

fn test_meta() -> Meta {
    Meta {
        name: "s1".to_string(),
        repo: "/repo".to_string(),
        repo_id: "abc012".to_string(),
        pool: "/pools/abc012.git".to_string(),
        worktree: "/w/s1".to_string(),
        home: "/meta/s1/home".to_string(),
        container: "agent-abc012-s1".to_string(),
        branch: "agent/gvisor/s1".to_string(),
        image: IMAGE.to_string(),
        memory: "8g".to_string(),
        cpus: "4".to_string(),
        pids_limit: "2048".to_string(),
        network: "pasta:--map-guest-addr,10.0.2.2".to_string(),
        seccomp_unconfined: "true".to_string(),
        env_file: "/envs/s1.list".to_string(),
        // Empty = a session predating the `nix` field (parse default).
        nix: String::new(),
    }
}

/// A meta_dir with the given `mounts.tsv` / `env.list` contents. The TempDir
/// is returned so the files outlive the test body.
fn meta_dir_with(mounts: &str, env_list: &str) -> (PathBuf, TempDir) {
    let dir = TempDir::new("podman-argv");
    let meta_dir = dir.path.join("meta");
    fs::create_dir_all(&meta_dir).unwrap();
    fs::write(meta_dir.join("mounts.tsv"), mounts).unwrap();
    fs::write(meta_dir.join("env.list"), env_list).unwrap();
    (meta_dir, dir)
}

#[test]
fn build_run_args_nix() {
    let mut env = test_env();
    env.nix_config = Some("sandbox = false\nsubstituters = https://cache.nixos.org".to_string());
    let mut meta = test_meta();
    meta.nix = "true".to_string();
    let (meta_dir, _keep) = meta_dir_with("\n", "\n");

    let args = build_run_args(&env, &meta, &meta_dir, true, &[]);

    // The store volume mount directly follows the 3 fixed binds.
    let home_mount = format!("type=bind,src={},dst=/home/agent,rw", meta.home);
    let pos = args.iter().position(|a| a == &home_mount).expect("home bind");
    assert_eq!(
        &args[pos + 1..pos + 3],
        &[
            "--mount".to_string(),
            format!("type=volume,src={},dst=/nix/store", agent_gvisor::podman::nix_volume_name(&meta)),
        ]
    );
    // The Nix env block directly follows the fixed envs (no loopback forward
    // set in this scenario).
    let wt = args
        .iter()
        .position(|a| a == "AGENT_WORKTREE=/repo")
        .expect("worktree env");
    let expected: Vec<String> = [
        "NIX_REMOTE=local",
        "NIX_STATE_DIR=/home/agent/.local/state/nix",
        "NIX_LOG_DIR=/home/agent/.local/state/nix/log",
        "TMPDIR=/home/agent/.cache/nix-tmp",
        "AGENT_GVISOR_NIX=1",
        "NIX_CONFIG=sandbox = false\nsubstituters = https://cache.nixos.org",
    ]
    .iter()
    .flat_map(|e| ["--env".to_string(), e.to_string()])
    .collect();
    assert_eq!(&args[wt + 1..wt + 1 + expected.len()], &expected[..]);

    // Counts: 3 fixed binds + 1 volume; 6 fixed envs + 6 Nix envs.
    assert_eq!(args.iter().filter(|a| *a == "--mount").count(), 4);
    assert_eq!(args.iter().filter(|a| *a == "--env").count(), 12);
    // The init wrapper is the payload (no loopback forward), before the
    // default /bin/bash.
    assert_eq!(&args[args.len() - 2..args.len() - 1], &["/bin/agent-gvisor-init".to_string()]);
    assert_eq!(args.last().unwrap(), "/bin/bash");

    // An old-session meta (empty `nix` — pre-dating the field) gains NO
    // volume and no init wrapper on `run`, whatever the env defaults say
    // (they only feed `start`).
    env.nix_config = None;
    let args = build_run_args(&env, &test_meta(), &meta_dir, true, &[]);
    assert!(!args.iter().any(|a| a.starts_with("type=volume")));
    assert!(!args.contains(&"/bin/agent-gvisor-init".to_string()));
}

#[test]
fn start_nix_records_volume_and_destroy_removes_it() {
    let s = Scenario::new("start-nix-records-volume");
    s.run_ok(&["start", "s1", "--nix", "--detach"]);

    let repo = s.repo.canonicalize().unwrap();
    let repo_id = common::expected_repo_id(&s.repo);
    let container = format!("agent-{repo_id}-s1");
    let meta_dir = repo
        .parent()
        .unwrap()
        .join(format!("{}__agent-gvisor", repo.file_name().unwrap().to_string_lossy()))
        .join("__sessions")
        .join("s1");

    let run = s.recorded_starting_with("podman", "run")[0].clone();
    assert!(run.contains(&format!("type=volume,src={container}-nix,dst=/nix/store")));
    assert!(run.contains(&"NIX_REMOTE=local".to_string()));
    assert!(run.contains(&"AGENT_GVISOR_NIX=1".to_string()));
    assert!(run.contains(&"/bin/agent-gvisor-init".to_string()));
    // `--nix` is recorded in the meta (and `--no-nix` would be `nix=false`).
    let meta_text = fs::read_to_string(meta_dir.join("meta")).unwrap();
    assert!(meta_text.contains("\nnix=true\n"));

    // destroy removes the volume along with the session.
    s.run_ok(&["destroy", "s1", "--force", "--delete-branch"]);
    let volume_calls = s.recorded_starting_with("podman", "volume");
    let rm_call = volume_calls
        .iter()
        .find(|c| c.contains(&"rm".to_string()))
        .expect("podman volume rm");
    assert!(rm_call.contains(&format!("{container}-nix".to_string())));

    // A plain (non-nix) session never touches volumes.
    let s2 = Scenario::new("start-plain-has-no-volume");
    s2.run_ok(&["start", "s1", "--detach"]);
    let run = s2.recorded_starting_with("podman", "run")[0].clone();
    assert!(!run.iter().any(|a| a.starts_with("type=volume")));
    let s2_repo = s2.repo.canonicalize().unwrap();
    let s2_meta_dir = s2_repo
        .parent()
        .unwrap()
        .join(format!(
            "{}__agent-gvisor",
            s2_repo.file_name().unwrap().to_string_lossy()
        ))
        .join("__sessions")
        .join("s1");
    let meta_text = fs::read_to_string(s2_meta_dir.join("meta")).unwrap();
    assert!(meta_text.contains("\nnix=false\n"));
}

#[test]
fn build_run_args_full_vector() {
    let mut env = test_env();
    env.loopback_forward = Some("8080:127.0.0.1:8080".to_string());
    let meta = test_meta();
    let (meta_dir, _keep) = meta_dir_with("h1\td1\tro\nh2\td2\trw\n", "K1=V1\nK2=V2\n");

    let args = build_run_args(
        &env,
        &meta,
        &meta_dir,
        false,
        &["/bin/echo".to_string(), "hi".to_string()],
    );

    let expected: Vec<String> = vec![
        "podman",
        "--runtime=runsc",
        "--cgroup-manager=cgroupfs",
        "--runtime-flag=ignore-cgroups",
        "run",
        "--replace",
        "--interactive",
        "--tty",
        "--name",
        "agent-abc012-s1",
        "--hostname",
        "s1",
        "--userns=keep-id",
        "--read-only",
        "--read-only-tmpfs=true",
        "--cap-drop=ALL",
        "--security-opt=no-new-privileges",
        "--workdir",
        "/repo",
        "--mount",
        "type=bind,src=/w/s1,dst=/repo,rw",
        "--mount",
        "type=bind,src=/pools/abc012.git,dst=/pools/abc012.git,rw",
        "--mount",
        "type=bind,src=/meta/s1/home,dst=/home/agent,rw",
        "--env",
        "HOME=/home/agent",
        "--env",
        "XDG_CONFIG_HOME=/home/agent/.config",
        "--env",
        "XDG_CACHE_HOME=/home/agent/.cache",
        "--env",
        "XDG_STATE_HOME=/home/agent/.local/state",
        "--env",
        "AGENT_SESSION=s1",
        "--env",
        "AGENT_WORKTREE=/repo",
        "--env",
        "AGENT_GVISOR_LOOPBACK_FORWARD=8080:127.0.0.1:8080",
        // cgroups ignored -> no --pids-limit/--memory/--cpus here:
        "--network",
        "pasta:--map-guest-addr,10.0.2.2",
        "--security-opt=seccomp=unconfined",
        "--env-file",
        "/envs/s1.list",
        "--mount",
        "type=bind,src=h1,dst=d1,ro",
        "--mount",
        "type=bind,src=h2,dst=d2,rw",
        "--env",
        "K1=V1",
        "--env",
        "K2=V2",
        IMAGE,
        "/bin/agent-gvisor-init",
        "/bin/echo",
        "hi",
    ]
    .into_iter()
    .map(String::from)
    .collect();
    assert_eq!(args, expected);
}

#[test]
fn build_run_args_rootful_limits() {
    let mut env = test_env();
    env.cgroup_manager = String::new();
    env.runtime_flags = vec![];
    let mut meta = test_meta();
    meta.network = String::new();
    meta.seccomp_unconfined = "false".to_string();
    meta.env_file = String::new();
    let (meta_dir, _keep) = meta_dir_with("\n", "\n"); // empty, like start writes

    let args = build_run_args(&env, &meta, &meta_dir, true, &[]);

    assert!(args.contains(&"--detach".to_string()));
    assert!(!args.contains(&"--interactive".to_string()));
    // No loopback env and no init wrapper:
    assert!(!args.iter().any(|a| a.starts_with("AGENT_GVISOR_LOOPBACK_FORWARD")));
    assert!(!args.contains(&"/bin/agent-gvisor-init".to_string()));
    // Limits directly follow the fixed --env block:
    let wt = args
        .iter()
        .position(|a| a == &format!("AGENT_WORKTREE={}", meta.repo))
        .expect("AGENT_WORKTREE env");
    assert_eq!(
        args[wt + 1..wt + 7],
        vec![
            "--pids-limit".to_string(),
            "2048".to_string(),
            "--memory".to_string(),
            "8g".to_string(),
            "--cpus".to_string(),
            "4".to_string(),
        ]
    );
    // Empty network/env_file/seccomp omitted:
    assert!(!args.iter().any(|a| a.starts_with("--network")));
    assert!(!args.iter().any(|a| a.starts_with("--env-file")));
    assert!(!args.iter().any(|a| a.contains("seccomp=unconfined")));
    // Empty mounts.tsv/env.list contribute nothing beyond the 3 fixed binds
    // and the 6 fixed --envs (bash run_container always emits both blocks):
    assert_eq!(args.iter().filter(|a| *a == "--mount").count(), 3);
    assert_eq!(args.iter().filter(|a| *a == "--env").count(), 6);
    // No command given and no default -> /bin/bash:
    assert_eq!(args.last().unwrap(), "/bin/bash");
}

#[test]
fn build_run_args_default_command() {
    let mut env = test_env();
    env.default_command = Some("herder --flag".to_string());
    let meta = test_meta();
    let (meta_dir, _keep) = meta_dir_with("\n", "\n");
    let args = build_run_args(&env, &meta, &meta_dir, true, &[]);
    assert_eq!(&args[args.len() - 2..], &["herder".to_string(), "--flag".to_string()]);

    // An empty default command falls back to /bin/bash (bash `read -a`
    // yields zero words -> default_cmd=(/bin/bash)).
    env.default_command = Some("".to_string());
    let args = build_run_args(&env, &meta, &meta_dir, true, &[]);
    assert_eq!(args.last().unwrap(), "/bin/bash");
}

#[test]
fn start_records_exact_podman_argv() {
    let s = Scenario::new("start-records-exact-podman-argv");
    let conf = s.root.join("conf");
    fs::write(&conf, "config").unwrap();
    let env_file = s.root.join("envfile");
    fs::write(&env_file, "EXTRA=1\n").unwrap();

    s.run_ok(&[
        "start",
        "s1",
        "--config",
        &format!("{}:/work/config", conf.display()),
        "--env",
        "FOO=bar",
        "--env-file",
        &env_file.display().to_string(),
        "--network",
        "slirp4netns",
        "--seccomp-unconfined",
        "--memory",
        "4g",
        "--cpus",
        "2",
        "--pids-limit",
        "512",
        "--detach",
        "--",
        "/bin/echo",
        "hello world",
    ]);

    let repo = s.repo.canonicalize().unwrap();
    let repo_id = common::expected_repo_id(&s.repo);
    let agent_root = repo.parent().unwrap().join(format!(
        "{}__agent-gvisor",
        repo.file_name().unwrap().to_string_lossy()
    ));
    let worktree = agent_root.join("s1");
    let pool = agent_root.join("__pools").join(format!("{repo_id}.git"));
    let home = agent_root.join("__sessions").join("s1").join("home");
    let container = format!("agent-{repo_id}-s1");
    let runtime = format!("--runtime={}", s.stub_bin.join("runsc").display());

    let runs = s.recorded_starting_with("podman", "run");
    assert_eq!(runs.len(), 1, "exactly one podman run");
    let expected: Vec<String> = vec![
        "podman".into(),
        runtime,
        "--cgroup-manager=cgroupfs".into(),
        "--runtime-flag=ignore-cgroups".into(),
        "run".into(),
        "--replace".into(),
        "--detach".into(),
        "--name".into(),
        container.clone(),
        "--hostname".into(),
        "s1".into(),
        "--userns=keep-id".into(),
        "--read-only".into(),
        "--read-only-tmpfs=true".into(),
        "--cap-drop=ALL".into(),
        "--security-opt=no-new-privileges".into(),
        "--workdir".into(),
        repo.display().to_string(),
        "--mount".into(),
        format!("type=bind,src={},dst={},rw", worktree.display(), repo.display()),
        "--mount".into(),
        format!("type=bind,src={},dst={},rw", pool.display(), pool.display()),
        "--mount".into(),
        format!("type=bind,src={},dst=/home/agent,rw", home.display()),
        "--env".into(),
        "HOME=/home/agent".into(),
        "--env".into(),
        "XDG_CONFIG_HOME=/home/agent/.config".into(),
        "--env".into(),
        "XDG_CACHE_HOME=/home/agent/.cache".into(),
        "--env".into(),
        "XDG_STATE_HOME=/home/agent/.local/state".into(),
        "--env".into(),
        "AGENT_SESSION=s1".into(),
        "--env".into(),
        format!("AGENT_WORKTREE={}", repo.display()),
        // rootless (ignore-cgroups) -> no --pids-limit/--memory/--cpus:
        "--network".into(),
        "slirp4netns".into(),
        "--security-opt=seccomp=unconfined".into(),
        "--env-file".into(),
        env_file.display().to_string(),
        "--mount".into(),
        format!("type=bind,src={},dst=/work/config,ro", conf.display()),
        "--env".into(),
        "FOO=bar".into(),
        IMAGE.into(),
        "/bin/echo".into(),
        "hello world".into(),
    ];
    assert_eq!(runs[0], expected);

    // last-command is the %q-quoted argv with a trailing space before the
    // newline (bash `printf '%q ' "${cmd[@]}"`).
    let last_command =
        fs::read_to_string(agent_root.join("__sessions").join("s1").join("last-command")).unwrap();
    let expected_last: String = runs[0]
        .iter()
        .map(|a| quote(a))
        .collect::<Vec<_>>()
        .join(" ")
        + " \n";
    assert_eq!(last_command, expected_last);
}

#[test]
fn detach_vs_tty() {
    let detached = Scenario::new("detach-vs-tty-detached");
    detached.run_ok(&["start", "a1", "--detach"]);
    let run = detached.recorded_starting_with("podman", "run")[0].clone();
    assert!(run.contains(&"--detach".to_string()));
    assert!(!run.contains(&"--interactive".to_string()));
    assert!(!run.contains(&"--tty".to_string()));

    let interactive = Scenario::new("detach-vs-tty-interactive");
    interactive.run_ok(&["start", "a1"]);
    let run = interactive.recorded_starting_with("podman", "run")[0].clone();
    assert!(run.contains(&"--interactive".to_string()));
    assert!(run.contains(&"--tty".to_string()));
    assert!(!run.contains(&"--detach".to_string()));
}
