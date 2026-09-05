// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! End-to-end CLI tests (docs/TODOs/mvp-5-cli-and-dry-run.md).
//!
//! These drive the *built binary* (`env!("CARGO_BIN_EXE_mysbx")`) as a
//! subprocess, with `HOME`, `XDG_CONFIG_HOME` and the `MYSBX_*` parameters
//! pointed at temporary directories and the working directory set to a
//! synthetic repo — so the tests never depend on the machine they run on.
//!
//! Sandbox note: the nix build sandbox has no network and may lack a
//! runnable `bwrap`; the two real-execution tests detect that and skip
//! gracefully instead of failing.

use std::path::{Path, PathBuf};
use std::process::Command;

/// A fresh temporary directory per test; hand-rolled, the crate has no
/// dependencies.
fn tmpdir(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("mysbx-cli-test-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// One binary invocation: fixed arguments, a working directory and a fully
/// controlled environment (only the variables set here survive).
struct Invocation {
    args: Vec<&'static str>,
    cwd: PathBuf,
    home: PathBuf,
    xdg: PathBuf,
}

fn spawn(inv: &Invocation) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mysbx"));
    cmd.args(&inv.args)
        .current_dir(&inv.cwd)
        .env("HOME", &inv.home)
        .env("XDG_CONFIG_HOME", &inv.xdg)
        .env("MYSBX_SHELL", "/synth/bin/bash")
        .env("MYSBX_TOOLS_PATH", "/synth/bin")
        .env_remove("MYSBX_BWRAP")
        // Keep the host's TERM & co. out of the result: the forwarded set
        // must come only from variables the test actually sets. The list
        // is the same constant the pipeline reads — not a hand copy that
        // could drift.
        ;
    for name in mysbx::FORWARDED_ENV_VARS {
        cmd.env_remove(name);
    }
    cmd
}

fn run_binary(inv: &Invocation) -> (i32, String, String) {
    let out = spawn(inv)
        .output()
        .expect("failed to spawn the mysbx binary");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// A synthetic repo `base/<name>` with a `sub` directory, and its sidecar
/// `base/<name>.mysbx` (already created, config left to the caller).
fn make_repo(base: &Path, name: &str) -> (PathBuf, PathBuf) {
    let repo = base.join(name);
    std::fs::create_dir_all(repo.join("sub")).unwrap();
    let sidecar = base.join(format!("{name}.mysbx"));
    std::fs::create_dir_all(&sidecar).unwrap();
    (repo, sidecar)
}

/// The minimal golden fixture (tests/assets/argv/minimal.txt) with the
/// synthetic repo path substituted — the expected `--dry-run` output of
/// the smallest real invocation.
fn expected_minimal_argv(repo: &Path) -> String {
    let golden = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/assets/argv/minimal.txt");
    std::fs::read_to_string(&golden)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", golden.display()))
        .replace("/synth/repo", &repo.to_string_lossy())
}

/// A standard fixture: a repo with sidecar at `base/repo`, empty home and
/// XDG dirs, `args` to run from inside the repo.
fn fixture(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let base = tmpdir(name);
    let (repo, sidecar) = make_repo(&base, "repo");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: args.to_vec(),
        cwd: repo.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    (inv, repo, sidecar)
}

/// [`fixture`] with `backend = "bubblewrap"` in the *user* config, so the
/// sidecar directory stays absent and the pipeline still reaches the argv
/// stage (D7: whichever layer names the backend decides).
fn fixture_user_backend(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    (inv, repo, sidecar)
}

/// [`fixture`] with `backend = "bubblewrap"` in the sidecar config.
fn fixture_with_backend(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    (inv, repo, sidecar)
}

fn is_bwrap_available() -> bool {
    Command::new("bwrap")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// ---- the core acceptance: --dry-run prints the argv ------------------------

#[test]
fn dry_run_bare_form_matches_the_minimal_golden() {
    // Byte-for-byte against the mvp-4 golden: --dry-run prints exactly the
    // argv format of tests/argv.rs, one argument per line, no prefix. The
    // backend comes from the user config; the sidecar directory does not
    // even exist.
    let (inv, repo, _) = fixture_user_backend("dry-run-minimal", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(stdout, expected_minimal_argv(&repo));
    assert!(stderr.is_empty(), "stderr: {stderr}");
    // Dry run is side-effect-free: no implicit init — the sidecar stays
    // without a config.toml (the directory itself comes from the fixture).
    assert!(!inv
        .cwd
        .parent()
        .unwrap()
        .join("repo.mysbx/config.toml")
        .exists());
}

#[test]
fn dry_run_command_form_differs_only_in_the_payload() {
    // Spec "Watch out": bare and `run` share one code path; the argvs are
    // identical apart from the payload after the single `--`.
    let (inv, repo, _) =
        fixture_user_backend("dry-run-cmd", &["run", "--dry-run", "--", "echo", "hi"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");

    let expected_lines: Vec<String> = expected_minimal_argv(&repo)
        .lines()
        .map(String::from)
        .collect();
    // The minimal argv ends with `-- /synth/bin/bash`; `run` replaces the
    // shell with the command vector.
    assert_eq!(
        &expected_lines[expected_lines.len() - 2..],
        &["--", "/synth/bin/bash"]
    );
    let actual_lines: Vec<&str> = stdout.lines().collect();
    let n = expected_lines.len();
    assert_eq!(actual_lines.len(), n + 1, "stdout: {stdout}");
    assert_eq!(&actual_lines[..n - 2], &expected_lines[..n - 2]);
    assert_eq!(&actual_lines[n - 2..], &["--", "echo", "hi"]);
}

#[test]
fn dry_run_without_sidecar_config_creates_nothing() {
    // --dry-run must be side-effect-free: a missing sidecar is NOT created
    // (an empty sidecar layer is used instead) and the run still succeeds.
    let base = tmpdir("dry-run-no-sidecar");
    let repo = base.join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: repo.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(stdout, expected_minimal_argv(&repo));
    let sidecar = base.join("repo.mysbx");
    assert!(!sidecar.exists(), "--dry-run must not create the sidecar");
}

#[test]
fn dry_run_after_dashdash_is_payload() {
    // cli.md D4: after `--`, `--dry-run` is verbatim payload content,
    // never parsed as a flag — so no argv may be printed. The payload
    // cannot actually start (no binary named `--dry-run`), which is a
    // failure either way.
    let (inv, _, _) = fixture_user_backend("dry-run-after-dd", &["run", "--", "--dry-run"]);
    let (code, stdout, _stderr) = run_binary(&inv);
    assert_ne!(code, 0);
    // The implicit-init chatter is fine; the bwrap argv must not appear.
    assert!(!stdout.contains("--clearenv"), "argv printed: {stdout}");
}

// ---- validation still runs under --dry-run ---------------------------------

#[test]
fn dry_run_sidecar_widening_fails() {
    // A sidecar mount with no user-config grant is a hard error even in a
    // dry run: a dry run that skipped validation would exercise the wrong
    // function. The user config is absent here, so it grants nothing.
    let (inv, repo, sidecar) = fixture("dry-run-widening", &["--dry-run"]);
    std::fs::create_dir_all(repo.join("secret")).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "[[mounts]]\npath = \"{}/secret\"\nmode = \"ro\"\n",
            repo.display()
        ),
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stdout: {stdout}");
    assert!(stdout.is_empty(), "no argv on failure: {stdout}");
    assert!(stderr.contains("mysbx: "), "stderr: {stderr}");
    assert!(stderr.contains("not at or below"), "stderr: {stderr}");
    assert!(stderr.contains("user"), "stderr: {stderr}");
}

#[test]
fn no_backend_configured_fails() {
    // cli.md D7: the backend is explicit, never auto-detected; neither
    // layer named one, so the run is refused.
    let (inv, _, _) = fixture("no-backend", &["--dry-run"]);
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1);
    assert!(stderr.contains("no backend configured"), "stderr: {stderr}");
}

#[test]
fn unknown_backend_fails() {
    // The MVP accepts exactly `bubblewrap`.
    let (inv, _, sidecar) = fixture("unknown-backend", &["--dry-run"]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"qemu\"\n").unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1);
    assert!(stderr.contains("qemu"), "stderr: {stderr}");
}

#[test]
fn backend_bubblewrap_is_accepted() {
    let (inv, _, _) = fixture_with_backend("backend-ok", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.starts_with("--clearenv\n"), "stdout: {stdout}");
}

// ---- environment forwarding and payload handling ---------------------------

#[test]
fn only_set_host_variables_are_forwarded() {
    // docs/plan.md "Environment": exactly TERM COLORTERM LANG LC_ALL
    // EDITOR VISUAL, each only when actually set.
    let (inv, _, _) = fixture_user_backend("forward-env", &["--dry-run"]);
    let mut cmd = spawn(&inv);
    cmd.env("TERM", "xterm-test").env("VISUAL", "nvim-test");
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();

    // TERM and VISUAL are set; the other four are not.
    let term = lines.iter().position(|x| *x == "TERM").unwrap();
    let visual = lines.iter().position(|x| *x == "VISUAL").unwrap();
    assert_eq!(lines[term + 1], "xterm-test");
    assert_eq!(lines[visual + 1], "nvim-test");
    for absent in ["COLORTERM", "LANG", "LC_ALL", "EDITOR"] {
        assert!(!lines.contains(&absent), "{absent} must not be forwarded");
    }
    // Forwarded variables precede PATH, which is always last of the
    // --setenv section.
    let path = lines.iter().position(|x| *x == "PATH").unwrap();
    assert!(term < path && visual < path);
}

#[test]
fn run_payload_after_dashdash_is_verbatim() {
    // cli.md D4: flag-looking arguments after `--` stay payload content.
    let (inv, _, _) = fixture_user_backend(
        "verbatim-payload",
        &["run", "--dry-run", "--", "agent", "--help", "--dry-run"],
    );
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    let dd = lines.iter().position(|x| *x == "--").unwrap();
    assert_eq!(
        &lines[dd + 1..],
        &["agent", "--help", "--dry-run"],
        "stdout: {stdout}"
    );
}

#[test]
fn run_dry_run_without_dashdash_is_still_usage_error() {
    // `run` accepts its own `--dry-run` before `--`, but without a `--` and
    // a command it stays a usage error — never an empty sandbox.
    let (inv, _, _) = fixture_user_backend("dry-run-late", &["run", "--dry-run"]);
    // Consumed as run's own flag; but without a `--` it stays a usage error.
    let (code, _stdout, _stderr) = run_binary(&inv);
    assert_eq!(code, 2);
}

// ---- exit codes (cli.md D8) -------------------------------------------------

#[test]
fn usage_errors_exit_2() {
    let cases: &[&[&str]] = &[
        &["nope"],
        &["--dry-run", "nope"],
        &["run"],
        &["run", "--"],
        &["run", "ls"],
        &["run", "--dry-run"],
        &["--dry-run", "--dry-run"],
        &["run", "extra", "--", "ls"],
        &["init", "extra"],
    ];
    for (i, args) in cases.iter().enumerate() {
        let (inv, _, _) = fixture_user_backend(&format!("usage-{i}"), args);
        let (code, _stdout, stderr) = run_binary(&inv);
        assert_eq!(code, 2, "args {args:?}: stderr: {stderr}");
        assert!(
            stderr.starts_with("mysbx"),
            "args {args:?}: stderr: {stderr}"
        );
    }
}

#[test]
fn help_and_version_exit_0() {
    for args in [
        vec!["help"],
        vec!["--help"],
        vec!["-h"],
        vec!["version"],
        vec!["--version"],
        vec!["-V"],
    ] {
        let (inv, _, _) = fixture("help-version", &args);
        let (code, stdout, _stderr) = run_binary(&inv);
        assert_eq!(code, 0, "args {args:?}");
        assert!(stdout.contains("mysbx"), "args {args:?}: stdout: {stdout}");
    }
}

#[test]
fn bare_plain_directory_falls_back_to_cwd() {
    // Discovery step 3 (mvp-2): no sidecar ancestor, no `.git` — the
    // starting directory is the repo, and --chdir targets it.
    let base = tmpdir("plain-dir");
    let dir = base.join("plain");
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let home = base.join("home");
    std::fs::create_dir_all(&home).unwrap();
    // A user config that names the backend, so the only thing under test
    // is the discovery step-3 fallback.
    let xdg_cfg = base.join("xdg").join("mysbx");
    std::fs::create_dir_all(&xdg_cfg).unwrap();
    std::fs::write(xdg_cfg.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: dir.clone(),
        home,
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    let chdir = lines.iter().position(|x| *x == "--chdir").unwrap();
    assert_eq!(lines[chdir + 1], dir.to_string_lossy());
}

#[test]
fn home_directory_is_refused_exit_1() {
    // The mvp-2 guard, reachable through the CLI: a repo resolved to $HOME
    // fails with exit 1 and a `mysbx: ` message.
    let base = tmpdir("home-guard");
    let home = base.join("home");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: home.clone(),
        home: home.clone(),
        xdg: base.join("xdg"),
    };
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1);
    assert!(stderr.contains("mysbx: "), "stderr: {stderr}");
}

// ---- init stays what it was --------------------------------------------------

#[test]
fn init_creates_sidecar_config_and_is_idempotent() {
    let (inv, _, sidecar) = fixture("init", &["init"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("## created"), "stdout: {stdout}");
    let config = sidecar.join("config.toml");
    assert!(config.exists());

    // Idempotent (D12): the second run reports `exists`, not `created`,
    // and does not touch the file.
    let (code, stdout, _) = run_binary(&inv);
    assert_eq!(code, 0);
    assert!(stdout.contains("## exists"), "stdout: {stdout}");
    // The generated config is comment-only: it names no backend and no
    // mount — init never decides policy.
    let text = std::fs::read_to_string(&config).unwrap();
    assert!(text.lines().all(|l| l.trim_start().starts_with('#')));
}

// ---- the real execution (skipped without a runnable bwrap) -------------------

#[test]
fn run_executes_payload_when_bwrap_exists() {
    // Item 6 pins the binary via MYSBX_BWRAP; unwrapped, the fallback is a
    // PATH lookup. This test needs a real, runnable bwrap — skip when the
    // environment has none (e.g. the nix build sandbox).
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }

    let (inv, _, _) = fixture_user_backend("real-run", &["run", "--", "/usr/bin/env"]);
    let mut cmd = spawn(&inv);
    // PATH inside the sandbox is MYSBX_TOOLS_PATH; the payload must
    // live under a base-bound path, so /usr/bin/env is the portable
    // choice.
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin").env("TERM", "dumb");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "exit {:?}\nstdout: {stdout}\nstderr: {stderr}",
        out.status.code()
    );
    // --clearenv ran: the payload sees exactly the forwarded + [env] set.
    assert!(stdout.contains("TERM=dumb"), "stdout: {stdout}");
    assert!(stdout.contains("PATH=/usr/bin"), "stdout: {stdout}");
}

#[test]
fn failing_payload_propagates_exit_code() {
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }

    let (inv, _, _) = fixture_user_backend(
        "payload-exit-code",
        &["run", "--", "/usr/bin/env", "no-such-binary-xyz"],
    );
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().unwrap();
    // `env` exits 127 for a missing command — propagated unchanged (D8).
    assert_eq!(out.status.code(), Some(127), "payload code must propagate");
}
