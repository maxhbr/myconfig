// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Home seeding and the endpoint rewrite rules (docs/spec.md §11),
//! exercised through `start` with `AGENT_GVISOR_HOME_SEED*` set.

mod common;

use std::fs;
use std::os::unix::fs::PermissionsExt;

use common::Scenario;

/// Build a seed tree and start a session seeded from it.
struct SeedFixture {
    s: Scenario,
    seed: std::path::PathBuf,
}

impl SeedFixture {
    fn new(test: &str, paths: &str, rewrite: Option<&str>) -> SeedFixture {
        let s = Scenario::new(test);
        let seed = s.root.join("seed");
        fs::create_dir_all(seed.join("a")).unwrap();
        fs::write(seed.join("a/b.txt"), "endpoint=OLD\n").unwrap();
        fs::write(seed.join("c.txt"), b"bin\0ary OLD\n").unwrap();
        fs::write(seed.join("ok.txt"), "plain\n").unwrap();
        // A symlink to an existing file: `cp -RL` copies it dereferenced.
        std::os::unix::fs::symlink("ok.txt", seed.join("slink")).unwrap();
        // A dangling symlink inside a copied subtree -> partial copy.
        fs::create_dir_all(seed.join("sub")).unwrap();
        fs::write(seed.join("sub/real.txt"), "real\n").unwrap();
        std::os::unix::fs::symlink("../gone", seed.join("sub/link")).unwrap();
        // A file whose trailing newlines must collapse to one on rewrite.
        fs::write(seed.join("e.txt"), "OLD\n\n\n").unwrap();
        // A top-level dangling symlink -> skipped with a warning.
        std::os::unix::fs::symlink("../gone", seed.join("gone-link")).unwrap();

        let mut envs = vec![
            ("AGENT_GVISOR_HOME_SEED", seed.display().to_string()),
            ("AGENT_GVISOR_HOME_SEED_PATHS", paths.to_string()),
        ];
        if let Some(r) = rewrite {
            envs.push(("AGENT_GVISOR_HOME_SEED_REWRITE", r.to_string()));
        }
        let out = s
            .cmd_with_env(&["start", "s1", "--detach"], &envs)
            .output()
            .expect("spawn");
        assert!(
            out.status.success(),
            "fixture start failed:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
        SeedFixture { s, seed }
    }

    fn home(&self) -> std::path::PathBuf {
        self.s
            .root
            .join("repo_agent-gvisor")
            .join("__sessions")
            .join("s1")
            .join("home")
    }
}

#[test]
fn seed_home_copies_allowlist() {
    let f = SeedFixture::new("seed-home-copies-allowlist", "a/b.txt c.txt missing.txt gone-link slink", None);
    let home = f.home();

    // Copied dereferenced, contents intact, writable for the agent:
    assert_eq!(fs::read_to_string(home.join("a/b.txt")).unwrap(), "endpoint=OLD\n");
    assert_eq!(fs::read_to_string(home.join("slink")).unwrap(), "plain\n");
    assert_eq!(fs::read(home.join("c.txt")).unwrap(), b"bin\0ary OLD\n");
    assert!(home.join("slink").is_file(), "symlink must be copied dereferenced");
    assert!(fs::metadata(home.join("a/b.txt")).unwrap().permissions().mode() & 0o200 != 0);
    // missing.txt: absent is normal, silently skipped:
    assert!(!home.join("missing.txt").exists());
}

#[test]
fn seed_home_warnings_and_summary() {
    let f = SeedFixture::new(
        "seed-home-warnings-and-summary",
        "a/b.txt c.txt missing.txt gone-link slink",
        None,
    );
    let seed = f.seed.display().to_string();
    let wt = f.s.root.join("repo_agent-gvisor").join("s1").display().to_string();
    // (re-run to capture stderr; the fixture already started the session)
    let out = f.s.cmd_with_env(
        &["start", "s1", "--force", "--detach"],
        &[
            ("AGENT_GVISOR_HOME_SEED", seed.clone()),
            (
                "AGENT_GVISOR_HOME_SEED_PATHS",
                "a/b.txt c.txt missing.txt gone-link slink".to_string(),
            ),
        ],
    ).output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: --force: destroying existing session s1 and deleting branch agent/gvisor/s1\n\
             agent-gvisor: destroyed session s1\n\
             agent-gvisor: warning: skipping dangling seed path gone-link -> ../gone\n\
             agent-gvisor: seeded /home/agent with 3 path(s) from {seed}\n\
             agent-gvisor: created worktree {wt} on branch agent/gvisor/s1\n\
             agent-gvisor: if the container fails to start the session is kept; retry with 'agent-gvisor run s1', diagnose with 'agent-gvisor doctor', or clean up with 'agent-gvisor destroy s1'\n\
             agent-gvisor: warning: memory/cpu/pids limits not enforced, the runtime ignores cgroups\n"
        )
    );
}

#[test]
fn seed_home_partial_copy() {
    let f = SeedFixture::new("seed-home-partial-copy", "sub", None);
    let home = f.home();
    assert_eq!(fs::read_to_string(home.join("sub/real.txt")).unwrap(), "real\n");
    assert!(!home.join("sub/link").exists());

    let out = f.s.cmd_with_env(
        &["start", "s1", "--force", "--detach"],
        &[
            ("AGENT_GVISOR_HOME_SEED", f.seed.display().to_string()),
            ("AGENT_GVISOR_HOME_SEED_PATHS", "sub".to_string()),
        ],
    ).output().unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr).to_string();
    assert!(
        stderr.contains(&format!(
            "agent-gvisor: warning: seed path sub copied incompletely (broken links in the home-manager generation):\n  cp: cannot stat '{}': No such file or directory\n",
            f.seed.join("sub/link").display()
        )),
        "{stderr}"
    );
    assert!(
        stderr.contains(&format!(
            "agent-gvisor: seeded /home/agent with 1 path(s) from {} (1 of them incomplete)\n",
            f.seed.display()
        )),
        "{stderr}"
    );
}

#[test]
fn rewrite_rules() {
    // Trailing newlines collapse to exactly one (bash $(<file) + printf).
    let f = SeedFixture::new("rewrite-rules", "a/b.txt c.txt e.txt", Some("OLD=http://127.0.0.1:8080"));
    let home = f.home();
    assert_eq!(
        fs::read_to_string(home.join("a/b.txt")).unwrap(),
        "endpoint=http://127.0.0.1:8080\n"
    );
    // Binary file untouched (NUL byte):
    assert_eq!(fs::read(home.join("c.txt")).unwrap(), b"bin\0ary OLD\n");
    // Trailing newlines collapse to exactly one (bash $(<file) + printf):
    assert_eq!(
        fs::read_to_string(home.join("e.txt")).unwrap(),
        "http://127.0.0.1:8080\n"
    );
}

#[test]
fn no_seeding_by_default() {
    let s = Scenario::new("no-seeding-by-default");
    let out = s.run_ok(&["start", "s1", "--detach"]);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(!stderr.contains("seeded"), "{stderr}");
    assert!(!stderr.contains("applied"), "{stderr}");

    // --no-home-seed disables even an explicit seed:
    let seed = s.root.join("seed");
    fs::create_dir_all(&seed).unwrap();
    fs::write(seed.join("ok.txt"), "x\n").unwrap();
    let out = s
        .cmd_with_env(
            &["start", "s1", "--force", "--detach", "--no-home-seed"],
            &[
                ("AGENT_GVISOR_HOME_SEED", seed.display().to_string()),
                ("AGENT_GVISOR_HOME_SEED_PATHS", "ok.txt".to_string()),
            ],
        )
        .output()
        .unwrap();
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{stderr}");
    assert!(!stderr.contains("seeded"), "{stderr}");
}

#[test]
fn seed_must_be_a_directory() {
    let s = Scenario::new("seed-must-be-a-directory");
    let notdir = s.root.join("file.txt");
    fs::write(&notdir, "x\n").unwrap();
    let out = s
        .cmd_with_env(
            &["start", "s1", "--detach"],
            &[
                ("AGENT_GVISOR_HOME_SEED", notdir.display().to_string()),
                ("AGENT_GVISOR_HOME_SEED_PATHS", "ok.txt".to_string()),
            ],
        )
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("agent-gvisor: error: home seed is not a directory: {}\n", notdir.display())
    );
}

#[test]
fn invalid_rewrite_rule() {
    let s = Scenario::new("invalid-rewrite-rule");
    let seed = s.root.join("seed");
    fs::create_dir_all(&seed).unwrap();
    fs::write(seed.join("ok.txt"), "x\n").unwrap();
    let out = s
        .cmd_with_env(
            &["start", "s1", "--detach"],
            &[
                ("AGENT_GVISOR_HOME_SEED", seed.display().to_string()),
                ("AGENT_GVISOR_HOME_SEED_PATHS", "ok.txt".to_string()),
                ("AGENT_GVISOR_HOME_SEED_REWRITE", "noequals".to_string()),
            ],
        )
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!(
            "agent-gvisor: seeded /home/agent with 1 path(s) from {}\n\
             agent-gvisor: error: invalid home-seed rewrite rule (expected OLD=NEW): noequals\n",
            seed.display()
        )
    );
}
