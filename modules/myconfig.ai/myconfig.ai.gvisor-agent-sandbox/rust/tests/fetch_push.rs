// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `fetch` and `push` (docs/spec.md §9): the shared pool fetch, the
//! current-repository default, the `--repo` override and the implicit
//! fetch of `push` — against the recording git stub.

mod common;

use common::Scenario;

fn started(s: &Scenario) {
    s.run_ok(&["start", "s1", "--detach"]);
}

/// The pool path of session `s1` started from this scenario's repo, as
/// recorded in its meta.
fn pool_of(s: &Scenario) -> String {
    let repo = s.repo.canonicalize().unwrap();
    let repo_id = common::expected_repo_id(&s.repo);
    repo.parent()
        .unwrap()
        .join(format!(
            "{}__agent-gvisor",
            repo.file_name().unwrap().to_string_lossy()
        ))
        .join("__pools")
        .join(format!("{repo_id}.git"))
        .display()
        .to_string()
}

/// The recorded `git -C <repo> fetch --no-tags <pool> +<branch>:…` call
/// every fetch/merge/push performs (the shared `try_fetch_branch_from_pool`).
fn expect_pool_fetch(s: &Scenario, repo: &str) {
    let expected: Vec<String> = [
        "git".to_string(),
        "-C".to_string(),
        repo.to_string(),
        "fetch".to_string(),
        "--no-tags".to_string(),
        pool_of(s),
        "+agent/gvisor/s1:refs/heads/agent/gvisor/s1".to_string(),
    ]
    .to_vec();
    assert!(
        s.recorded("git").iter().any(|c| *c == expected),
        "no recorded pool fetch: {:?}",
        s.recorded("git")
    );
}

#[test]
fn fetch_into_the_current_repo() {
    let s = Scenario::new("fetch-into-current-repo");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    let out = s.run_ok(&["fetch", "s1"]);
    // Scenario::cmd runs in the scenario repo, so that is the "current
    // repository" the branch is fetched into.
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: fetching branch agent/gvisor/s1 from pool {pool} into {repo}\n\
             agent-gvisor: fetched agent/gvisor/s1 into {repo}; merge it with 'agent-gvisor merge s1'\n",
            pool = pool_of(&s)
        )
    );
    expect_pool_fetch(&s, &repo);
}

#[test]
fn fetch_repo_override() {
    let s = Scenario::new("fetch-repo-override");
    started(&s);
    // Run from OUTSIDE any Git work tree; only --repo points at the target.
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    let mut c = s.cmd(&["fetch", "s1", "--repo", &s.repo.display().to_string()]);
    c.current_dir(&s.home);
    let out = c.output().expect("spawn agent-gvisor");
    assert!(
        out.status.success(),
        "expected success, got {}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr),
    );
    expect_pool_fetch(&s, &repo);
}

#[test]
fn fetch_outside_a_git_work_tree() {
    let s = Scenario::new("fetch-outside-a-git-work-tree");
    started(&s);
    let mut c = s.cmd(&["fetch", "s1"]);
    c.current_dir(&s.home);
    let out = c.output().expect("spawn agent-gvisor");
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: error: not a Git working tree: {}\n",
            s.home.canonicalize().unwrap().display()
        )
    );
}

#[test]
fn fetch_failure_message() {
    let s = Scenario::new("fetch-failure-message");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    s.marker("fetch-fail", "1");
    s.run_fail(
        &["fetch", "s1"],
        1,
        &format!(
            "agent-gvisor: fetching branch agent/gvisor/s1 from pool {pool} into {repo}\n\
             agent-gvisor: error: fetch from pool failed; is the session pool still present?\n",
            pool = pool_of(&s)
        ),
    );
}

#[test]
fn fetch_repo_override_errors() {
    let s = Scenario::new("fetch-repo-override-errors");
    started(&s);
    // realpath's own RAW diagnostic, then the --repo die.
    let missing = s.root.join("nope").display().to_string();
    s.run_fail(
        &["fetch", "s1", "--repo", &missing],
        1,
        &format!(
            "realpath: {missing}: No such file or directory\n\
             agent-gvisor: error: --repo: not a path: {missing}\n"
        ),
    );
    let plain = s.root.join("plain");
    std::fs::create_dir_all(&plain).unwrap();
    s.run_fail(
        &["fetch", "s1", "--repo", &plain.display().to_string()],
        1,
        &format!(
            "agent-gvisor: error: --repo: not a Git work tree: {}\n",
            plain.canonicalize().unwrap().display()
        ),
    );
    s.run_fail(
        &["push", "s1", "--repo", &plain.display().to_string()],
        1,
        &format!(
            "agent-gvisor: error: --repo: not a Git work tree: {}\n",
            plain.canonicalize().unwrap().display()
        ),
    );
}

#[test]
fn push_defaults_to_origin() {
    let s = Scenario::new("push-defaults-to-origin");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    let out = s.run_ok(&["push", "s1"]);
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: fetching branch agent/gvisor/s1 from pool {pool} into {repo}\n\
             agent-gvisor: pushing agent/gvisor/s1 to origin of {repo}\n",
            pool = pool_of(&s)
        )
    );
    // The implicit fetch runs BEFORE the push.
    expect_pool_fetch(&s, &repo);
    let calls = s.recorded("git");
    let fetch_pos = calls
        .iter()
        .position(|c| c.contains(&"fetch".to_string()) && c.contains(&repo))
        .expect("pool fetch recorded");
    let push: Vec<String> = [
        "git".to_string(),
        "-C".to_string(),
        repo.to_string(),
        "push".to_string(),
        "origin".to_string(),
        "agent/gvisor/s1".to_string(),
    ]
    .to_vec();
    let push_pos = calls.iter().position(|c| *c == push).expect("push recorded");
    assert!(fetch_pos < push_pos, "the implicit fetch must run first");
}

#[test]
fn push_explicit_remote() {
    let s = Scenario::new("push-explicit-remote");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    s.run_ok(&["push", "s1", "upstream"]);
    let push: Vec<String> = [
        "git".to_string(),
        "-C".to_string(),
        repo,
        "push".to_string(),
        "upstream".to_string(),
        "agent/gvisor/s1".to_string(),
    ]
    .to_vec();
    assert!(
        s.recorded("git").iter().any(|c| *c == push),
        "no recorded push to upstream"
    );
}

#[test]
fn push_repo_override_and_remote() {
    let s = Scenario::new("push-repo-override-and-remote");
    started(&s);
    // Run from OUTSIDE any Git work tree; --repo points at the target and
    // the remote is positional after the NAME.
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    let mut c = s.cmd(&["push", "s1", "--repo", &s.repo.display().to_string(), "review"]);
    c.current_dir(&s.home);
    let out = c.output().expect("spawn agent-gvisor");
    assert!(
        out.status.success(),
        "expected success, got {}\nstderr: {}",
        out.status,
        String::from_utf8_lossy(&out.stderr),
    );
    let push: Vec<String> = [
        "git".to_string(),
        "-C".to_string(),
        repo,
        "push".to_string(),
        "review".to_string(),
        "agent/gvisor/s1".to_string(),
    ]
    .to_vec();
    assert!(
        s.recorded("git").iter().any(|c| *c == push),
        "no recorded push to review"
    );
}

#[test]
fn push_second_remote_dies() {
    let s = Scenario::new("push-second-remote-dies");
    started(&s);
    s.run_fail(
        &["push", "s1", "origin", "upstream"],
        1,
        "agent-gvisor: error: push accepts at most one remote: upstream\n",
    );
}

#[test]
fn push_failure_exits_with_gits_code() {
    let s = Scenario::new("push-failure-exits-with-gits-code");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    s.marker("push-fail", "1");
    // `set -e` semantics: the exit code is git push's own (the stub's 3).
    let out = s.run(&["push", "s1"]);
    assert_eq!(out.status.code(), Some(3));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: fetching branch agent/gvisor/s1 from pool {pool} into {repo}\n\
             agent-gvisor: pushing agent/gvisor/s1 to origin of {repo}\n",
            pool = pool_of(&s)
        )
    );
}
