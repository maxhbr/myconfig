// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Session-state layout: the registry symlink, the repo-adjacent
//! `__pools`/`__sessions` tree, the exact `meta` bytes, `list`/`status`
//! output, and `destroy` cleanup (docs/spec.md §8, §9).

mod common;

use std::fs;
use std::os::unix::fs::PermissionsExt;

use agent_gvisor::state::Meta;
use common::{expected_repo_id, Scenario, IMAGE};

fn start_simple(s: &Scenario, name: &str) {
    s.run_ok(&["start", name, "--detach"]);
}

fn agent_root_of(s: &Scenario) -> std::path::PathBuf {
    let repo = s.repo.canonicalize().unwrap();
    repo.parent().unwrap().join(format!(
        "{}_agent-gvisor",
        repo.file_name().unwrap().to_string_lossy()
    ))
}

#[test]
fn start_creates_expected_tree() {
    let s = Scenario::new("start-creates-expected-tree");
    start_simple(&s, "s1");

    let repo = s.repo.canonicalize().unwrap();
    let repo_id = expected_repo_id(&s.repo);
    let agent_root = agent_root_of(&s);
    let meta_dir = agent_root.join("__sessions").join("s1");
    let home = meta_dir.join("home");
    let pool = agent_root.join("__pools").join(format!("{repo_id}.git"));
    let worktree = agent_root.join("s1");

    // Registry: symlink to the repo-adjacent session dir.
    let reg = s.state.join("sessions").join("s1");
    assert!(reg.is_symlink());
    assert_eq!(fs::read_link(&reg).unwrap(), meta_dir);

    // The disposable bare pool and the worktree exist.
    assert!(pool.is_dir());
    assert!(worktree.is_dir());

    // Session dir: 0700, XDG dirs pre-created in the home.
    assert_eq!(fs::metadata(&meta_dir).unwrap().permissions().mode() & 0o777, 0o700);
    assert_eq!(fs::metadata(&home).unwrap().permissions().mode() & 0o777, 0o700);
    for d in [".cache", ".config", ".local/state"] {
        assert!(home.join(d).is_dir(), "{d} missing in session home");
    }

    // meta: fixed field order, %q values, trailing newline.
    let meta_text = fs::read_to_string(meta_dir.join("meta")).unwrap();
    let expected_meta = format!(
        "name=s1\n\
         repo={}\n\
         repo_id={repo_id}\n\
         pool={}\n\
         worktree={}\n\
         home={}\n\
         container=agent-{repo_id}-s1\n\
         branch=agent/gvisor/s1\n\
         image={IMAGE}\n\
         memory=8g\n\
         cpus=4\n\
         pids_limit=2048\n\
         network=''\n\
         seccomp_unconfined=false\n\
         env_file=''\n",
        repo.display(),
        pool.display(),
        worktree.display(),
        home.display(),
    );
    assert_eq!(meta_text, expected_meta);

    // mounts.tsv / env.list: always a trailing newline, empty when unset.
    assert_eq!(fs::read(meta_dir.join("mounts.tsv")).unwrap(), b"\n");
    assert_eq!(fs::read(meta_dir.join("env.list")).unwrap(), b"\n");
    assert!(meta_dir.join("last-command").is_file());
}

#[test]
fn mounts_and_envs_written_in_order() {
    let s = Scenario::new("mounts-and-envs-written-in-order");
    let c1 = s.root.join("c1");
    let c2 = s.root.join("c2");
    fs::write(&c1, "x").unwrap();
    fs::write(&c2, "x").unwrap();
    s.run_ok(&[
        "start",
        "s1",
        "--detach",
        "--config",
        &format!("{}:/a:ro", c1.display()),
        "--mount",
        &format!("{}:/b", c2.display()),
        "--env",
        "A=1",
        "--env",
        "B=2",
    ]);

    let meta_dir = agent_root_of(&s).join("__sessions").join("s1");
    assert_eq!(
        fs::read_to_string(meta_dir.join("mounts.tsv")).unwrap(),
        format!("{}\t/a\tro\n{}\t/b\trw\n", c1.canonicalize().unwrap().display(), c2.canonicalize().unwrap().display()),
    );
    assert_eq!(
        fs::read_to_string(meta_dir.join("env.list")).unwrap(),
        "A=1\nB=2\n"
    );
}

#[test]
fn list_sorted_and_states() {
    let s = Scenario::new("list-sorted-and-states");
    start_simple(&s, "b1");
    start_simple(&s, "a1");

    // An incomplete entry: registry symlink to a dir without meta.
    let inc_target = s.root.join("incomplete-dir");
    fs::create_dir_all(&inc_target).unwrap();
    std::os::unix::fs::symlink(&inc_target, s.state.join("sessions").join("inc")).unwrap();
    // A pre-rewrite entry: a real directory.
    fs::create_dir_all(s.state.join("sessions").join("old")).unwrap();

    let out = s.run_ok(&["list"]);
    let text = String::from_utf8(out.stdout).unwrap();
    let a1 = agent_root_of(&s).join("a1");
    let b1 = agent_root_of(&s).join("b1");
    let expected = format!(
        "{:<24} {:<12} {:<28} {}\n\
         {:<24} {:<12} {:<28} {}\n\
         {:<24} {:<12} {:<28} {}\n\
         {:<24} {:<12} {:<28} {}\n\
         {:<24} {:<12} {:<28} {}\n",
        "SESSION", "STATUS", "BRANCH", "WORKTREE",
        "a1", "running", "agent/gvisor/a1", a1.display(),
        "b1", "running", "agent/gvisor/b1", b1.display(),
        "inc", "incomplete", "-", s.state.join("sessions/inc").display(),
        "old", "incompatible (pre-rewrite layout)", "-", s.state.join("sessions/old").display(),
    );
    assert_eq!(text, expected);
}

#[test]
fn list_stopped_when_container_absent() {
    let s = Scenario::new("list-stopped-when-container-absent");
    start_simple(&s, "s1");
    // Simulate a removed container: no record dir for the container name.
    let repo_id = expected_repo_id(&s.repo);
    fs::remove_dir_all(s.record.join("containers").join(format!("agent-{repo_id}-s1"))).unwrap();

    let out = s.run_ok(&["list"]);
    let text = String::from_utf8(out.stdout).unwrap();
    assert!(text.contains(&format!("{:<12}", "stopped")), "{text}");
}

#[test]
fn status_output() {
    let s = Scenario::new("status-output");
    start_simple(&s, "s1");
    // The git status trailer is marker-driven in the stub.
    s.marker("status-branch", "## main");

    let out = s.run_ok(&["status", "s1"]);
    let repo = s.repo.canonicalize().unwrap();
    let repo_id = expected_repo_id(&s.repo);
    let agent_root = agent_root_of(&s);
    let worktree = agent_root.join("s1");
    let pool = agent_root.join("__pools").join(format!("{repo_id}.git"));
    let expected = format!(
        "session:   s1\n\
         repo:      {}\n\
         branch:    agent/gvisor/s1\n\
         worktree:  {}\n\
         pool:      {}\n\
         container: agent-{repo_id}-s1\n\
         image:     {IMAGE}\n\
         status:    running\\npid:       42\\nstarted:   2025-01-01T00:00:00Z\n\
         ## main\n",
        repo.display(),
        worktree.display(),
        pool.display(),
    );
    assert_eq!(String::from_utf8(out.stdout).unwrap(), expected);
}

#[test]
fn meta_parses_bash_fixture() {
    // A meta file exactly as the bash CLI could have written it (including
    // $'…' and quoted forms) must load identically.
    let fixture = "name='s 1'\n\
                   repo=$'/tmp/re\\tpo'\n\
                   repo_id=abc012\n\
                   pool=/x/pool.git\n\
                   worktree='w t'\n\
                   home=/h\n\
                   container=agent-abc012-s_1\n\
                   branch='agent/gvisor/s 1'\n\
                   image=img\n\
                   memory=8g\n\
                   cpus=4\n\
                   pids_limit=2048\n\
                   network=''\n\
                   seccomp_unconfined=true\n\
                   env_file='/e 1'\n\
                   unknown_key=ignored\n";
    let meta = Meta::parse(fixture).expect("parse");
    assert_eq!(
        meta,
        Meta {
            name: "s 1".to_string(),
            repo: "/tmp/re\tpo".to_string(),
            repo_id: "abc012".to_string(),
            pool: "/x/pool.git".to_string(),
            worktree: "w t".to_string(),
            home: "/h".to_string(),
            container: "agent-abc012-s_1".to_string(),
            branch: "agent/gvisor/s 1".to_string(),
            image: "img".to_string(),
            memory: "8g".to_string(),
            cpus: "4".to_string(),
            pids_limit: "2048".to_string(),
            network: String::new(),
            seccomp_unconfined: "true".to_string(),
            env_file: "/e 1".to_string(),
        }
    );
    // Round-trip: to_text produces the canonical quoted form.
    let expected = "name=s\\ 1\n\
                    repo=$'/tmp/re\\tpo'\n\
                    repo_id=abc012\n\
                    pool=/x/pool.git\n\
                    worktree=w\\ t\n\
                    home=/h\n\
                    container=agent-abc012-s_1\n\
                    branch=agent/gvisor/s\\ 1\n\
                    image=img\n\
                    memory=8g\n\
                    cpus=4\n\
                    pids_limit=2048\n\
                    network=''\n\
                    seccomp_unconfined=true\n\
                    env_file=/e\\ 1\n";
    assert_eq!(meta.to_text(), expected);
}

#[test]
fn destroy_removes_everything() {
    let s = Scenario::new("destroy-removes-everything");
    start_simple(&s, "s1");
    let agent_root = agent_root_of(&s);
    let meta_dir = agent_root.join("__sessions").join("s1");
    let worktree = agent_root.join("s1");
    let repo_id = expected_repo_id(&s.repo);
    let container = format!("agent-{repo_id}-s1");

    s.run_ok(&["destroy", "s1"]);

    assert!(!s.state.join("sessions").join("s1").exists());
    assert!(!meta_dir.exists());
    assert!(!worktree.exists());
    // The pool is disposable but kept; the container record is gone.
    assert!(agent_root.join("__pools").join(format!("{repo_id}.git")).is_dir());
    assert!(!s.record.join("containers").join(&container).exists());

    // git was told to remove the worktree (without --delete-branch):
    let git = s.recorded("git");
    let removed: Vec<Vec<String>> = git
        .into_iter()
        .filter(|c| c.contains(&"worktree".to_string()) && c.contains(&"remove".to_string()))
        .collect();
    assert_eq!(removed.len(), 1);
    assert!(!removed[0].contains(&"--force".to_string()));
    assert!(!s
        .recorded("git")
        .iter()
        .any(|c| c.contains(&"branch".to_string()) && c.contains(&"-D".to_string())));

    // Re-destroy is not an error path in bash (load_meta of a gone name),
    // it must fail with unknown session:
    s.run_fail(
        &["destroy", "s1"],
        1,
        "agent-gvisor: error: unknown session: s1\n",
    );
}

#[test]
fn destroy_delete_branch() {
    let s = Scenario::new("destroy-delete-branch");
    start_simple(&s, "s1");
    s.run_ok(&["destroy", "s1", "--delete-branch"]);
    let pool = agent_root_of(&s)
        .join("__pools")
        .join(format!("{}.git", expected_repo_id(&s.repo)));
    assert!(s.recorded("git").iter().any(|c| {
        *c == vec![
            format!("--git-dir={}", pool.display()),
            "branch".to_string(),
            "-D".to_string(),
            "agent/gvisor/s1".to_string(),
        ]
    }));
}

#[test]
fn incomplete_session_is_reset_by_start() {
    let s = Scenario::new("incomplete-session-is-reset-by-start");
    let meta_dir = agent_root_of(&s).join("__sessions").join("s1");
    fs::create_dir_all(&meta_dir).unwrap();
    std::os::unix::fs::symlink(&meta_dir, s.state.join("sessions").join("s1")).unwrap();

    // status reports the debris with a helpful message...
    s.run_fail(
        &["status", "s1"],
        1,
        &format!(
            "agent-gvisor: error: session s1 is incomplete: an earlier start was interrupted before it\n\
             registered the session. Re-run the start (it cleans the leftovers up),\n\
             or remove {} by hand.\n",
            meta_dir.display()
        ),
    );

    // ...and start recovers from it.
    let out = s.run_ok(&["start", "s1", "--detach"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(&format!(
            "agent-gvisor: session s1 is incomplete (interrupted start); removing {}",
            meta_dir.display()
        )),
        "{stderr}"
    );
    assert!(meta_dir.join("meta").is_file());
}
