// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Shared scenario scaffolding for the integration tests: temp dirs, the
//! recording `git`/`podman` stubs (POSIX `sh`, no tooling assumptions), and
//! helpers to run the built `agent-gvisor` binary against them.

#![allow(dead_code)]

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};
use std::sync::atomic::{AtomicU64, Ordering};

/// The built binary under test.
pub const BIN: &str = env!("CARGO_BIN_EXE_agent-gvisor");

pub const BASE_SHA: &str = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef";
pub const IMAGE: &str = "localhost/agent-gvisor-test:latest";

/// The usage text as raw bytes, to compare `--help` output byte-for-byte.
pub const USAGE_BYTES: &[u8] = include_bytes!("../../src/usage.txt");

// --- the stubs -----------------------------------------------------------

pub const GIT_STUB: &str = include_str!("../stubs/git.sh");
pub const PODMAN_STUB: &str = include_str!("../stubs/podman.sh");

// --- scenario ------------------------------------------------------------

/// One isolated test scenario.
pub struct Scenario {
    /// Scenario root (tempdir path, kept alive via `dir`).
    pub root: PathBuf,
    /// Stub-call recordings land here (`<tool>-<n>.argv`, NUL-separated).
    pub record: PathBuf,
    /// The PATH-prepended directory with the `git`/`podman` stubs and a
    /// dummy executable `runsc`.
    pub stub_bin: PathBuf,
    /// A plain directory standing in for the Git working tree.
    pub repo: PathBuf,
    /// `AGENT_GVISOR_STATE` for the binary under test.
    pub state: PathBuf,
    /// Isolated `HOME`.
    pub home: PathBuf,
    dir: TempDir,
}

/// Minimal tempdir without external crates (the crate is zero-dependency by
/// design, and that extends to the test suite).
pub struct TempDir {
    pub path: PathBuf,
}

impl TempDir {
    pub fn new(prefix: &str) -> TempDir {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let n = COUNTER.fetch_add(1, Ordering::SeqCst);
        for attempt in 0..1000u64 {
            let path = std::env::temp_dir().join(format!(
                "agent-gvisor-test-{prefix}-{}-{n}-{attempt}",
                std::process::id()
            ));
            if fs::create_dir(&path).is_ok() {
                return TempDir { path };
            }
        }
        panic!("cannot create temp dir");
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.path);
    }
}

impl Scenario {
    /// Create the directory tree, write and chmod the stubs.
    pub fn new(test_name: &str) -> Scenario {
        let dir = TempDir::new(test_name);
        let root = dir.path.clone();
        let s = Scenario {
            record: root.join("record"),
            stub_bin: root.join("stub-bin"),
            repo: root.join("repo"),
            state: root.join("state"),
            home: root.join("home"),
            root,
            dir,
        };
        fs::create_dir_all(&s.record).unwrap();
        fs::create_dir_all(&s.stub_bin).unwrap();
        fs::create_dir_all(&s.repo).unwrap();
        fs::create_dir_all(s.state.join("sessions")).unwrap();
        fs::create_dir_all(&s.home).unwrap();
        s.write_stub("git", GIT_STUB);
        s.write_stub("podman", PODMAN_STUB);
        // AGENT_GVISOR_PODMAN_RUNTIME: an absolute path only needs to be
        // executable for `need`.
        let runtime = s.stub_bin.join("runsc");
        fs::write(&runtime, "#!/bin/sh\nexit 0\n").unwrap();
        make_executable(&runtime);
        Scenario { ..s }
    }

    fn write_stub(&self, name: &str, body: &str) {
        let p = self.stub_bin.join(name);
        fs::write(&p, body).unwrap();
        make_executable(&p);
    }

    /// Create a marker file (stub-behaviour switch) with one line.
    pub fn marker(&self, name: &str, line: &str) {
        fs::write(self.record.join(name), format!("{line}\n")).unwrap();
    }

    /// A `Command` for the binary with the scenario environment applied.
    pub fn cmd(&self, args: &[&str]) -> Command {
        let mut c = Command::new(BIN);
        c.args(args);
        self.apply_env(&mut c);
        c
    }

    /// The scenario environment, applied to a `Command`.
    pub fn apply_env(&self, c: &mut Command) {
        let path = format!("{}:{}", self.stub_bin.display(), std::env::var("PATH").unwrap_or_default());
        c.env("PATH", &path)
            .env("HOME", &self.home)
            .env("RECORD", &self.record)
            .env("AGENT_GVISOR_STATE", &self.state)
            .env("AGENT_GVISOR_DEFAULT_IMAGE", IMAGE)
            .env("AGENT_GVISOR_PODMAN_RUNTIME", self.stub_bin.join("runsc"))
            .env("AGENT_GVISOR_HOME_SEED_PATHS", "");
    }

    /// A `Command` for the binary with the scenario environment plus extra
    /// variables (later `.env` calls win, so these override the defaults).
    pub fn cmd_with_env(&self, args: &[&str], envs: &[(&str, String)]) -> Command {
        let mut c = self.cmd(args);
        for (k, v) in envs {
            c.env(k, v);
        }
        c
    }

    /// Run the binary, capturing stdout/stderr.
    pub fn run(&self, args: &[&str]) -> Output {
        self.cmd(args).output().expect("spawn agent-gvisor")
    }

    /// Run the binary and assert exit code 0.
    pub fn run_ok(&self, args: &[&str]) -> Output {
        let out = self.run(args);
        assert!(
            out.status.success(),
            "expected success for {args:?}, got {}\nstdout:\n{}\nstderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );
        out
    }

    /// Run the binary and assert the exit code and (lossy) stderr.
    pub fn run_fail(&self, args: &[&str], code: i32, stderr: &str) -> Output {
        let out = self.run(args);
        assert_eq!(
            out.status.code(),
            Some(code),
            "for {args:?}\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );
        assert_eq!(
            String::from_utf8_lossy(&out.stderr),
            stderr,
            "stderr mismatch for {args:?}"
        );
        out
    }

    /// All recorded calls of `tool`, in order.
    pub fn recorded(&self, tool: &str) -> Vec<Vec<String>> {
        let mut calls = Vec::new();
        for entry in fs::read_dir(&self.record).unwrap() {
            let p = entry.unwrap().path();
            let name = p.file_name().unwrap().to_string_lossy().to_string();
            if let Some(n) = name.strip_prefix(&format!("{tool}-")).and_then(|s| s.strip_suffix(".argv")) {
                let n: u64 = n.parse().unwrap();
                calls.push((n, p));
            }
        }
        calls.sort();
        calls
            .into_iter()
            .map(|(_, p)| read_argv(&p))
            .collect()
    }

    /// Only the recorded calls of `tool` whose first argument equals `head`.
    pub fn recorded_starting_with(&self, tool: &str, head: &str) -> Vec<Vec<String>> {
        self.recorded(tool)
            .into_iter()
            .filter(|c| c.first().map(String::as_str) == Some(head))
            .collect()
    }
}

/// Read one NUL-separated argv recording.
pub fn read_argv(path: &std::path::Path) -> Vec<String> {
    let bytes = fs::read(path).unwrap();
    bytes
        .split(|b| *b == 0)
        .filter(|chunk| !chunk.is_empty())
        .map(|chunk| String::from_utf8_lossy(chunk).to_string())
        .collect()
}

fn make_executable(path: &std::path::Path) {
    use std::os::unix::fs::PermissionsExt;
    let mut perm = fs::metadata(path).unwrap().permissions();
    perm.set_mode(0o755);
    fs::set_permissions(path, perm).unwrap();
}


/// Normalize recorded podman argv: strip argv[0] (`podman`) and the global
/// `--runtime=*` / `--cgroup-manager=*` / `--runtime-flag=*` arguments.
pub fn strip_podman_globals(argv: &[String]) -> Vec<String> {
    argv.iter()
        .skip(1)
        .filter(|a| {
            !(a.starts_with("--runtime=")
                || a.starts_with("--cgroup-manager=")
                || a.starts_with("--runtime-flag="))
        })
        .cloned()
        .collect()
}

/// Compute the repo ID exactly like `state::repo_id` does (first 16 hex
/// chars of `sha256(realpath repo)`), via `sha256sum` — independent of the
/// code under test.
pub fn expected_repo_id(repo: &std::path::Path) -> String {
    let real = repo.canonicalize().unwrap();
    let out = Command::new("sha256sum")
        .arg(&real)
        .output()
        .expect("sha256sum (tests rely on coreutils being present)");
    let text = String::from_utf8(out.stdout).unwrap();
    text.trim()
        .split_whitespace()
        .next()
        .unwrap()
        .chars()
        .take(16)
        .collect()
}

/// A simple line-oriented diff for readable assertion failures.
pub fn assert_lines(actual: &str, expected: &[String]) {
    let expected_text = expected
        .iter()
        .map(|l| format!("{l}\n"))
        .collect::<String>();
    if actual != expected_text {
        panic!(
            "output mismatch.\n--- expected ---\n{expected_text}--- actual ---\n{actual}---------"
        );
    }
}
