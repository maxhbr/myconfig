// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Every fatal error path with its exact message and exit code
//! (docs/spec.md §3, §14).

mod common;

use std::fs;

use common::Scenario;

fn started(s: &Scenario) {
    s.run_ok(&["start", "s1", "--detach"]);
}

/// The worktree (session clone) path of session `s1` started from this
/// scenario's repo, as recorded in its meta.
fn worktree_of(s: &Scenario) -> String {
    let repo = s.repo.canonicalize().unwrap();
    repo.parent()
        .unwrap()
        .join(format!(
            "{}__agent-gvisor",
            repo.file_name().unwrap().to_string_lossy()
        ))
        .join("s1")
        .display()
        .to_string()
}

#[test]
fn help_and_usage() {
    let s = Scenario::new("help-and-usage");
    for args in [&["--help"][..], &["-h"][..], &["help"][..], &[][..]] {
        let out = s.run_ok(args);
        assert_eq!(out.stdout, common::USAGE_BYTES);
    }
    // start's own -h/--help
    for flag in ["-h", "--help"] {
        let out = s.run_ok(&["start", flag]);
        assert_eq!(out.stdout, common::USAGE_BYTES);
    }
}

#[test]
fn invalid_session_name() {
    let s = Scenario::new("invalid-session-name");
    s.run_fail(
        &["start", "b@d"],
        1,
        "agent-gvisor: error: invalid session name 'b@d' (allowed: letters, digits, dot, underscore, hyphen)\n",
    );
    // Also via --name:
    s.run_fail(
        &["start", "--name", "b@d"],
        1,
        "agent-gvisor: error: invalid session name 'b@d' (allowed: letters, digits, dot, underscore, hyphen)\n",
    );
}

#[test]
fn session_name_required() {
    let s = Scenario::new("session-name-required");
    s.run_fail(
        &["start"],
        1,
        "agent-gvisor: error: session name is required (NAME or --name)\n",
    );
    s.run_fail(&["status"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["run"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["logs"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["shell"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["stop"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["merge"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["fetch"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["push"], 1, "agent-gvisor: error: session name required\n");
    s.run_fail(&["destroy"], 1, "agent-gvisor: error: session name required\n");
}

#[test]
fn session_name_given_twice() {
    let s = Scenario::new("session-name-given-twice");
    s.run_fail(
        &["start", "s1", "--name", "s1"],
        1,
        "agent-gvisor: error: session name given twice (--name and positional)\n",
    );
}

#[test]
fn unknown_subcommand_and_option() {
    let s = Scenario::new("unknown-subcommand-and-option");
    s.run_fail(&["--bad"], 1, "agent-gvisor: error: unknown subcommand: --bad\n");
    // The positional-NAME shorthand still validates the name:
    s.run_fail(
        &["frob!nicate"],
        1,
        "agent-gvisor: error: invalid session name 'frob!nicate' (allowed: letters, digits, dot, underscore, hyphen)\n",
    );
    s.run_fail(
        &["start", "s1", "--bad"],
        1,
        "agent-gvisor: error: unknown start option: --bad\n",
    );
    s.run_fail(
        &["destroy", "s1", "--bad"],
        1,
        "agent-gvisor: error: unknown destroy option: --bad\n",
    );
    s.run_fail(
        &["fetch", "s1", "--bad"],
        1,
        "agent-gvisor: error: unknown fetch option: --bad\n",
    );
    s.run_fail(
        &["push", "s1", "--bad"],
        1,
        "agent-gvisor: error: unknown push option: --bad\n",
    );
}

#[test]
fn missing_flag_value() {
    let s = Scenario::new("missing-flag-value");
    s.run_fail(
        &["start", "s1", "--image"],
        1,
        "agent-gvisor: error: option requires a value: --image\n",
    );
    s.run_fail(
        &["start", "s1", "--repo"],
        1,
        "agent-gvisor: error: option requires a value: --repo\n",
    );
    s.run_fail(
        &["merge", "s1", "--repo"],
        1,
        "agent-gvisor: error: option requires a value: --repo\n",
    );
    s.run_fail(
        &["fetch", "s1", "--repo"],
        1,
        "agent-gvisor: error: option requires a value: --repo\n",
    );
    s.run_fail(
        &["push", "s1", "--repo"],
        1,
        "agent-gvisor: error: option requires a value: --repo\n",
    );
}

#[test]
fn mount_parsing_errors() {
    let s = Scenario::new("mount-parsing-errors");
    let existing = s.root.join("exists");
    fs::write(&existing, "x").unwrap();

    s.run_fail(
        &["start", "s1", "--mount", "onlyhost"],
        1,
        "agent-gvisor: error: invalid mount 'onlyhost'; expected HOST:DEST[:ro|rw]\n",
    );
    s.run_fail(
        &["start", "s1", "--config", "a:b:c:d"],
        1,
        "agent-gvisor: error: invalid mount 'a:b:c:d'; expected HOST:DEST[:ro|rw]\n",
    );
    s.run_fail(
        &["start", "s1", "--mount", "relative:/dest"],
        1,
        "agent-gvisor: error: mount source does not exist: relative\n",
    );
    s.run_fail(
        &["start", "s1", "--mount", &format!("{}:relative", existing.display())],
        1,
        "agent-gvisor: error: container mount destination must be absolute: relative\n",
    );
    let mode_spec = format!("{}:/dest:bad", existing.display());
    s.run_fail(
        &["start", "s1", "--mount", &mode_spec],
        1,
        &format!("agent-gvisor: error: mount mode must be ro or rw: {mode_spec}\n"),
    );
}

#[test]
fn missing_paths_and_repos() {
    let s = Scenario::new("missing-paths-and-repos");
    let missing = s.root.join("nope");
    // A failing `realpath -e` dies on its own diagnostic (no agent-gvisor prefix):
    s.run_fail(
        &["start", "s1", "--repo", &missing.display().to_string()],
        1,
        &format!("realpath: {}: No such file or directory\n", missing.display()),
    );
    s.run_fail(
        &["start", "s1", "--env-file", &missing.display().to_string()],
        1,
        &format!("realpath: {}: No such file or directory\n", missing.display()),
    );
    s.run_fail(
        &["start", "s1", "--home-seed", &missing.display().to_string()],
        1,
        &format!("realpath: {}: No such file or directory\n", missing.display()),
    );

    // A plain directory that is not a Git work tree:
    s.marker("not-git", &s.repo.canonicalize().unwrap().display().to_string());
    s.run_fail(
        &["start", "s1", "--repo", &s.repo.display().to_string()],
        1,
        &format!(
            "agent-gvisor: error: not a Git working tree: {}\n",
            s.repo.canonicalize().unwrap().display()
        ),
    );
}

#[test]
fn image_and_runtime_checks() {
    let s = Scenario::new("image-and-runtime-checks");
    s.marker("image-missing", "1");
    s.run_fail(
        &["start", "s1"],
        1,
        "agent-gvisor: error: container image localhost/agent-gvisor-test:latest is not in the local Podman store.\n\
         Build and load it with: agent-gvisor-load-image\n\
         (or: nix run .#load-image), or pass --image with another reference.\n",
    );

    // An absolute runtime that is not executable:
    let nope = s.stub_bin.join("not-a-runtime");
    let mut cmd = s.cmd(&["start", "s1"]);
    cmd.env("AGENT_GVISOR_PODMAN_RUNTIME", &nope);
    let out = cmd.output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("agent-gvisor: error: OCI runtime is not executable: {}\n", nope.display())
    );

    // A named runtime podman does not know:
    s.marker("info-fail", "1");
    let mut cmd = s.cmd(&["start", "s1"]);
    cmd.env("AGENT_GVISOR_PODMAN_RUNTIME", "notregistered");
    let out = cmd.output().unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        "agent-gvisor: error: Podman OCI runtime notregistered is not registered.\n\
         Register it in containers.conf (on NixOS:\n\
         virtualisation.containers.containersConf.settings.engine.runtimes), or\n\
         set AGENT_GVISOR_PODMAN_RUNTIME to the absolute path of a runsc binary.\n"
    );
}

#[test]
fn existing_session_without_force() {
    let s = Scenario::new("existing-session-without-force");
    started(&s);
    s.run_fail(
        &["start", "s1"],
        1,
        "agent-gvisor: error: session already exists: s1 (pass --force, or remove it with 'agent-gvisor destroy s1 --force --delete-branch')\n",
    );
    // --force destroys and recreates:
    s.run_ok(&["start", "s1", "--force", "--detach"]);
}

#[test]
fn unknown_session() {
    let s = Scenario::new("unknown-session");
    for sub in ["status", "run", "logs", "shell", "stop", "merge", "fetch", "push", "destroy"] {
        s.run_fail(&[sub, "nope"], 1, "agent-gvisor: error: unknown session: nope\n");
    }
}

#[test]
fn pre_rewrite_registry_entries() {
    let s = Scenario::new("pre-rewrite-registry-entries");
    // The pre-rewrite layout: the registry entry is the session directory
    // itself (a real directory, not a symlink).
    let old = s.state.join("sessions").join("old");
    fs::create_dir_all(&old).unwrap();
    fs::write(old.join("meta"), "name=old\n").unwrap();

    let msg = format!(
        "agent-gvisor: error: session old is from the pre-rewrite layout; remove it by hand with:\n\
         rm -rf {}\n",
        old.display()
    );
    for sub in ["status", "run", "logs", "shell", "stop", "merge", "fetch", "push", "destroy"] {
        s.run_fail(&[sub, "old"], 1, &msg);
    }
    // start refuses it too (like any existing session)...
    s.run_fail(
        &["start", "old"],
        1,
        "agent-gvisor: error: session already exists: old (pass --force, or remove it with 'agent-gvisor destroy old --force --delete-branch')\n",
    );
    // ...and --force cannot rescue it either: the destroy fails with the
    // pre-rewrite error, which start wraps (§9):
    s.run_fail(
        &["start", "old", "--force", "--detach"],
        1,
        "agent-gvisor: error: could not destroy the existing session: old\n",
    );
}

#[test]
fn run_refuses_running_container() {
    let s = Scenario::new("run-refuses-running-container");
    started(&s);
    s.run_fail(
        &["run", "s1"],
        1,
        "agent-gvisor: error: container is already running: s1\n",
    );
}

#[test]
fn logs_and_shell_on_absent_container() {
    let s = Scenario::new("logs-and-shell-on-absent-container");
    started(&s);
    let repo_id = common::expected_repo_id(&s.repo);
    fs::remove_dir_all(s.record.join("containers").join(format!("agent-{repo_id}-s1"))).unwrap();

    s.run_fail(
        &["logs", "s1"],
        1,
        "agent-gvisor: error: container is absent: s1\n",
    );
    s.run_fail(
        &["shell", "s1"],
        1,
        "agent-gvisor: error: container is not running: s1\n",
    );
}

#[test]
fn merge_guards() {
    let s = Scenario::new("merge-guards");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();

    s.marker("detached", "1");
    s.run_fail(
        &["merge", "s1"],
        1,
        &format!(
            "agent-gvisor: error: the repository at {repo} is in detached HEAD state; switch to the branch you want to merge into first\n"
        ),
    );

    // realpath prints its own diagnostic (raw, no prefix),
    // then the `|| die` fires with the --repo message.
    s.run_fail(
        &["merge", "s1", "--repo", &s.root.join("nope").display().to_string()],
        1,
        &format!(
            "realpath: {}: No such file or directory\nagent-gvisor: error: --repo: not a path: {}\n",
            s.root.join("nope").display(),
            s.root.join("nope").display()
        ),
    );
    let plain = s.root.join("plain");
    fs::create_dir_all(&plain).unwrap();
    s.run_fail(
        &["merge", "s1", "--repo", &plain.display().to_string()],
        1,
        &format!("agent-gvisor: error: --repo: not a Git work tree: {}\n", plain.display()),
    );
}

#[test]
fn merge_dirty_worktree() {
    let s = Scenario::new("merge-dirty-worktree");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    s.marker("dirty", &repo);
    s.run_fail(
        &["merge", "s1"],
        1,
        &format!(
            "agent-gvisor: error: working tree of {repo} is dirty; commit or stash before merging\n"
        ),
    );
}

#[test]
fn destroy_dirty_worktree() {
    let s = Scenario::new("destroy-dirty-worktree");
    s.run_ok(&["start", "s1", "--nix", "--detach"]);
    let worktree = s
        .root
        .join("repo__agent-gvisor")
        .join("s1")
        .canonicalize()
        .unwrap();
    s.marker("dirty", &worktree.display().to_string());
    s.run_fail(
        &["destroy", "s1"],
        1,
        "agent-gvisor: error: worktree has uncommitted changes; commit them or use --force\n",
    );
    // Refusing a non-force destroy must leave every session resource intact,
    // including the writable Nix store volume.
    assert!(s.recorded_starting_with("podman", "rm").is_empty());
    assert!(s.recorded_starting_with("podman", "volume").is_empty());
    // --force overrides:
    s.run_ok(&["destroy", "s1", "--force"]);
    assert!(s
        .recorded_starting_with("podman", "volume")
        .iter()
        .any(|call| call.contains(&"rm".to_string())));
}

#[test]
fn merge_failure_message() {
    let s = Scenario::new("merge-failure-message");
    started(&s);
    let repo = s.repo.canonicalize().unwrap().display().to_string();
    s.marker("merge-fail", "1");
    s.run_fail(
        &["merge", "s1"],
        1,
        &format!(
            "agent-gvisor: fetching branch agent/gvisor/s1 from worktree {worktree} into {repo}\n\
             agent-gvisor: merging agent/gvisor/s1 into main of {repo}\n\
             agent-gvisor: error: merge failed; resolve conflicts in {repo}, then delete the leftover ref with 'git -C \"{repo}\" branch -D agent/gvisor/s1'\n",
            worktree = worktree_of(&s)
        ),
    );
}

#[test]
fn base_ref_must_resolve() {
    let s = Scenario::new("base-ref-must-resolve");
    s.run_fail(
        &["start", "s1", "--base", "nope"],
        1,
        "agent-gvisor: error: cannot resolve base ref: nope\n",
    );
}
