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
    spawn_with_args(inv, &inv.args)
}

/// [`spawn`] with an argument list computed at runtime (the fixture's
/// `args` are `&'static str`, which a payload containing a discovered
/// host path cannot be).
fn spawn_with_args<S: AsRef<std::ffi::OsStr>>(inv: &Invocation, args: &[S]) -> Command {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mysbx"));
    cmd.args(args)
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
/// the smallest real invocation. argv[0] (the backend executable,
/// review-1 finding 7) is `bwrap`: the tests run without the Nix
/// wrapper's `MYSBX_BWRAP` pin, so the fallback applies.
fn expected_minimal_argv(repo: &Path) -> String {
    let golden = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/assets/argv/minimal.txt");
    let argv = std::fs::read_to_string(&golden)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", golden.display()))
        .replace("/synth/repo", &repo.to_string_lossy());
    format!("bwrap\n{argv}")
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

// ---- --verbose (cli.md D10) -------------------------------------------------

/// The report block of a stdout stream: every `## `-prefixed line.
fn report_lines(stdout: &str) -> Vec<&str> {
    stdout.lines().filter(|l| l.starts_with("## ")).collect()
}

/// Everything that is NOT a report line, i.e. the `--dry-run` argv block,
/// reassembled with its trailing newline.
fn argv_block(stdout: &str) -> String {
    stdout
        .lines()
        .filter(|l| !l.starts_with("## "))
        .map(|l| format!("{l}\n"))
        .collect()
}

#[test]
fn verbose_dry_run_keeps_the_argv_byte_identical() {
    // The D10 compatibility promise: the report is `## `-prefixed and
    // comes first, so stripping it leaves exactly the plain --dry-run
    // output — compared against the same mvp-4 golden.
    let (inv, repo, _) = fixture_user_backend("verbose-dry-run", &["--verbose", "--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stderr.is_empty(), "stderr: {stderr}");
    assert_eq!(argv_block(&stdout), expected_minimal_argv(&repo));
    // The report precedes the argv: the last report line comes before the
    // first argv line.
    let first_argv = stdout.find("--clearenv").unwrap();
    let last_report = stdout.rfind("## ").unwrap();
    assert!(last_report < first_argv, "stdout: {stdout}");

    // The reverse flag order is the same run.
    let (inv2, _, _) = fixture_user_backend("verbose-dry-run", &["--dry-run", "--verbose"]);
    let (code2, stdout2, _) = run_binary(&inv2);
    assert_eq!(code2, 0);
    assert_eq!(stdout2, stdout);
}

#[test]
fn verbose_report_covers_the_run_configuration() {
    // A repo whose user config mounts a directory rw and whose sidecar
    // mounts part of it ro with an explicit dest — so the report has
    // something from both layers to attribute.
    let base = tmpdir("verbose-report");
    let (repo, sidecar) = make_repo(&base, "repo");
    let home = base.join("home");
    let xdg = base.join("xdg");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    let granted = base.join("granted");
    std::fs::create_dir_all(granted.join("sub")).unwrap();
    std::fs::write(
        xdg.join("mysbx").join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\nnetwork = false\n\
             [[mounts]]\npath = \"{}\"\ndest = \"/granted\"\nmode = \"rw\"\n\
             [env]\nUSER_VAR = \"u\"\n",
            granted.display()
        ),
    )
    .unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "network = false\n\
             [[mounts]]\npath = \"{}/sub\"\ndest = \"/inside\"\nmode = \"ro\"\n\
             [env]\nSIDECAR_VAR = \"s\"\n",
            granted.display()
        ),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["--verbose", "--dry-run"],
        cwd: repo.join("sub"),
        home,
        xdg: xdg.clone(),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");

    // repo root and sidecar path
    assert!(
        report.contains(&format!("repo root:      {}", repo.display())),
        "{report}"
    );
    assert!(
        report.contains(&format!("{} (exists)", sidecar.display())),
        "{report}"
    );
    // both config paths, with their loaded/absent state
    assert!(
        report.contains(&format!(
            "user config:    {} (loaded)",
            xdg.join("mysbx").join("config.toml").display()
        )),
        "{report}"
    );
    assert!(
        report.contains(&format!(
            "sidecar config: {} (loaded)",
            sidecar.join("config.toml").display()
        )),
        "{report}"
    );
    // backend and network sense
    assert!(report.contains("backend:        bubblewrap"), "{report}");
    assert!(report.contains("network:        denied"), "{report}");
    // mounts: the implicit repo bind, the user mount and the sidecar
    // mount with its explicit dest — with modes and layers.
    assert!(
        report.contains(&format!(
            "  rw {} -> {}  [repo, implicit]",
            repo.display(),
            repo.display()
        )),
        "{report}"
    );
    // (both configured mounts carry an explicit `dest`: the temporary
    // fixture lives under /tmp, and binding a source path back onto /tmp
    // is refused by the argv builder's protected-dest check.)
    assert!(
        report.contains(&format!(
            "  rw {} -> /granted  [user config]",
            granted.display()
        )),
        "{report}"
    );
    assert!(
        report.contains(&format!(
            "  ro {}/sub -> /inside  [sidecar config]",
            granted.display()
        )),
        "{report}"
    );
    // env entries from both layers, plus the tools PATH
    assert!(report.contains("USER_VAR=u  [config]"), "{report}");
    assert!(report.contains("SIDECAR_VAR=s  [config]"), "{report}");
    assert!(report.contains("PATH=/synth/bin  [tools]"), "{report}");
    // the runtime parameters and the payload
    assert!(
        report.contains("shell:          /synth/bin/bash"),
        "{report}"
    );
    assert!(report.contains("bwrap:          bwrap"), "{report}");
    assert!(
        report.contains("payload:        shell /synth/bin/bash"),
        "{report}"
    );
    assert!(report.contains("dry run"), "{report}");
}

#[test]
fn verbose_run_form_reports_the_command_payload() {
    let (inv, _, _) = fixture_user_backend(
        "verbose-run-form",
        &["run", "--verbose", "--dry-run", "--", "echo", "hi"],
    );
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains("payload:        command echo hi"),
        "{report}"
    );
    // The argv is still there, unprefixed and last, argv[0] first
    // (review-1 finding 7).
    assert!(argv_block(&stdout).starts_with("bwrap\n--clearenv\n"), "{stdout}");
}

#[test]
fn verbose_after_dashdash_is_payload() {
    // cli.md D4 verbatim rule: after `--`, `--verbose` is payload content,
    // never a flag — so no report may be printed.
    let (inv, _, _) = fixture_user_backend("verbose-after-dd", &["run", "--", "--verbose"]);
    let (code, stdout, _stderr) = run_binary(&inv);
    assert_ne!(code, 0);
    assert!(
        !stdout.contains("run configuration"),
        "report printed: {stdout}"
    );
}

#[test]
fn verbose_without_dry_run_reports_and_still_executes() {
    // Without --dry-run the report is printed and the run proceeds to the
    // real exec; needs a runnable bwrap, so it self-skips like the other
    // execution tests.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let (inv, _, _) =
        fixture_user_backend("verbose-exec", &["run", "--verbose", "--", "/usr/bin/env"]);
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin").env("TERM", "dumb");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(out.status.success(), "stdout: {stdout}");
    let report = report_lines(&stdout).join("\n");
    assert!(report.contains("run configuration"), "{stdout}");
    assert!(report.contains("mode:           executing"), "{stdout}");
    assert!(report.contains("TERM=dumb  [host]"), "{stdout}");
    // The exec really happened: the payload's own output is there.
    assert!(stdout.contains("PATH=/usr/bin"), "stdout: {stdout}");
}

#[test]
fn verbose_bare_form_reports_and_still_executes() {
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    // The bare form's payload is the shell; point it at a command that
    // exits on its own, so the test does not hang on an interactive
    // one. `true` must be reachable INSIDE the sandbox, i.e. its real
    // path under a base-bound directory — the same discovery
    // `sandbox_bash()` uses, for a host whose `/usr/bin` holds only
    // `env` (plain NixOS).
    // NOTE: keep the ORIGINAL /nix/store/.../bin/true path, not the
    // canonicalized one — coreutils ships `true` as a symlink to the
    // multi-call `coreutils` binary, and canonicalize() would resolve
    // it to a binary that no longer behaves like `true`.
    let true_path = std::env::var_os("PATH").and_then(|path| {
        std::env::split_paths(&path).find_map(|dir| {
            // The candidate itself must be base-bound (sandbox mounts
            // /nix/store and /usr/bin only) — a profile symlink like
            // /run/current-system/sw/bin/true canonicalizes INTO the
            // store, but the profile path itself is not mounted, so
            // profile paths must be skipped, not used. In a nix shell
            // the PATH entry IS the store path, so `dir` qualifies.
            if !(dir.starts_with("/nix/store") || dir.starts_with("/usr/bin")) {
                return None;
            }
            let candidate = dir.join("true");
            candidate.is_file().then_some(candidate)
        })
    });
    let Some(true_path) = true_path else {
        eprintln!("skipping: no sandbox-reachable true");
        return;
    };
    let true_str = true_path.to_string_lossy().into_owned();
    let (inv, _, _) = fixture_user_backend("verbose-bare-exec", &["--verbose"]);
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_SHELL", &true_str)
        .env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "stdout: {stdout}\nstderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains(&format!("payload:        shell {true_str}")),
        "{stdout}"
    );
    assert!(report.contains("mode:           executing"), "{stdout}");
}

// ---- validation still runs under --dry-run ---------------------------------

#[test]
fn dry_run_sidecar_mount_without_user_config_succeeds() {
    // config.md D7: the sidecar is trusted. With NO user config at all,
    // a sidecar `[[mounts]]` entry mounts what it names — this used to
    // be the "widening" hard error.
    let (inv, repo, sidecar) = fixture("dry-run-sidecar-mount", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::create_dir_all(repo.join("data")).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n[[mounts]]\npath = \"{}/data\"\ndest = \"/data\"\nmode = \"ro\"\n",
            repo.display()
        ),
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains(&format!("--ro-bind\n{}/data\n/data\n", repo.display())),
        "stdout: {stdout}"
    );
}

#[test]
fn dry_run_sidecar_rw_mount_without_user_config_succeeds() {
    // The same for `rw`: no mode is derived from a user entry any more.
    let (inv, repo, sidecar) = fixture("dry-run-sidecar-rw", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::create_dir_all(repo.join("data")).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n[[mounts]]\npath = \"{}/data\"\ndest = \"/data\"\nmode = \"rw\"\n",
            repo.display()
        ),
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains(&format!("--bind\n{}/data\n/data\n", repo.display())),
        "stdout: {stdout}"
    );
}

#[test]
fn user_network_deny_survives_a_fresh_sidecar() {
    // Review-1 P1: an omitted sidecar `network` used to count as an
    // explicit `true`, so a user-config deny plus a freshly `init`ed
    // (comment-only) sidecar tripped the NetworkUpgrade hard error and
    // made every newly initialized sandbox fail. The dry run must
    // succeed with `--unshare-all` and WITHOUT `--share-net`.
    let (inv, _, sidecar) = fixture("deny-fresh-sidecar", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nnetwork = false\n",
    )
    .unwrap();
    // Exactly what `mysbx init` writes: comments only.
    std::fs::write(sidecar.join("config.toml"), "# mysbx sidecar config\n").unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("--unshare-all\n"), "stdout: {stdout}");
    assert!(!stdout.contains("--share-net"), "stdout: {stdout}");
}

#[test]
fn sidecar_network_true_still_cannot_reenable() {
    // The guard itself is unchanged: an EXPLICIT sidecar `network = true`
    // against a user deny stays the hard error of docs/design/config.md
    // D7 — the tri-state only stops ABSENT values from counting as true.
    let (inv, _, sidecar) = fixture("explicit-reenable", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nnetwork = false\n",
    )
    .unwrap();
    std::fs::write(sidecar.join("config.toml"), "network = true\n").unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stdout: {stdout}");
    assert!(stderr.contains("never re-enable it"), "stderr: {stderr}");
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
    assert!(stdout.starts_with("bwrap\n--clearenv\n"), "stdout: {stdout}");
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
        &["run", "--verbose"],
        &["--dry-run", "--dry-run"],
        &["--verbose", "--verbose"],
        &["--verbose", "nope"],
        &["--verbose", "init"],
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

// ---- state dirs (docs/design/config.md D15) -------------------------------

#[test]
fn dry_run_binds_state_dirs_from_both_layers() {
    // The argv shows one rw bind per declared entry — sidecar-backed
    // source, sandbox-home dest — after the repo bind, before any
    // configured mount. `--dry-run` must not create the backing dirs.
    let (inv, repo, sidecar) = fixture("state-dirs-dry-run", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share/opencode\"]\n",
    )
    .unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        "state-dirs = [\".local/state/opencode\"]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    assert!(
        stdout.contains(&format!(
            "--bind\n{}/state/.local/share/opencode\n/mysbx-home/.local/share/opencode\n",
            side.display()
        )),
        "user layer bind missing: {stdout}"
    );
    assert!(
        stdout.contains(&format!(
            "--bind\n{}/state/.local/state/opencode\n/mysbx-home/.local/state/opencode\n",
            side.display()
        )),
        "sidecar layer bind missing: {stdout}"
    );
    // Side-effect-free: neither backing directory was created.
    assert!(!side.join("state").exists(), "dry run created state dirs");
}

#[test]
fn a_real_run_creates_the_backing_dirs_and_persists_writes() {
    // The end-to-end property (D15): the payload writes into the sandbox
    // home below a declared state entry; after the run the file is in
    // `<sidecar>/state/<entry>` — the state survives the sandbox.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    let (inv, repo, sidecar) = fixture("state-dirs-persist", &[]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share/opencode\"]\n",
    )
    .unwrap();
    std::fs::write(sidecar.join("config.toml"), "").unwrap();
    let args = vec![
        "run".to_owned(),
        "--".to_owned(),
        bash.to_string_lossy().into_owned(),
        "-c".to_owned(),
        // No `mkdir -p`: the payload PATH is the tools dir only, and
        // bubblewrap (0.11) creates missing dest parents itself, so
        // the redirect below can land directly in the bound dir.
        "echo persisted > \"$HOME/.local/share/opencode/sessions.txt\"".to_owned(),
    ];
    let mut cmd = spawn_with_args(&inv, &args);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "exit {:?}\nstdout: {stdout}\nstderr: {stderr}",
        out.status.code()
    );
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    let persisted = side.join("state/.local/share/opencode/sessions.txt");
    let text = std::fs::read_to_string(&persisted)
        .unwrap_or_else(|e| panic!("{persisted:?}: {e}"));
    assert_eq!(text.trim(), "persisted");
}

#[test]
fn nested_state_dirs_fail_with_a_mysbx_error() {
    // D15's nesting refusal, through the real CLI: exit 1, `mysbx: `
    // prefix, never a panic.
    let (inv, _, _sidecar) = fixture("state-dirs-nested", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share\", \".local/share/opencode\"]\n",
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1);
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(stderr.contains("state-dirs entries nest"), "stderr: {stderr}");
    assert!(!stderr.contains("panicked"), "stderr: {stderr}");
}

#[test]
fn verbose_report_lists_state_dirs() {
    let (inv, repo, _) = fixture("state-dirs-verbose", &[]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share/opencode\"]\n",
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["--verbose", "--dry-run"],
        ..inv
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    let side = repo
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .join("repo.mysbx");
    assert!(report.contains("state dirs:     1"), "{report}");
    assert!(
        report.contains(&format!(
            "  /mysbx-home/.local/share/opencode <-> {}/state/.local/share/opencode  [state]",
            side.display()
        )),
        "{report}"
    );
}

#[test]
fn init_template_mentions_state_dirs() {
    // The init template documents every schema key; `state-dirs` must
    // appear in it (commented), or a new operator would never learn the
    // feature exists.
    let (inv, _, sidecar) = fixture("state-dirs-init", &["init"]);
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let text = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(text.contains("state-dirs"), "{text}");
    // And it stays comment-only: init never decides policy.
    assert!(text.lines().all(|l| l.trim_start().starts_with('#')));
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
fn ripgrep_config_mount_is_activated_through_the_variable() {
    // Review-3 item 6, execution-level: mounting the ripgrep config
    // directory is inert by itself — the activation mechanism is the
    // `RIPGREP_CONFIG_PATH` variable, which `--clearenv` kills. A run
    // whose user layer mirrors the generated `baselineEnv` (default.nix)
    // must hand the payload BOTH the mounted file and the variable
    // pointing at its in-sandbox path. The payload prints the variable
    // and the file, proving the mount and the setenv landed together.
    // A REAL `rg` in the tools closure is not assumed (the nix build
    // sandbox has none): reading the variable and the file is the
    // observable contract between mysbx and whatever tool consumes it.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    let base = tmpdir("ripgrep-activation");
    let home = base.join("home");
    let xdg = base.join("xdg");
    let repo = base.join("repo");
    std::fs::create_dir_all(repo.join("sub")).unwrap();
    std::fs::create_dir_all(base.join("repo.mysbx")).unwrap();
    std::fs::create_dir_all(home.join(".config").join("ripgrep")).unwrap();
    std::fs::write(
        home.join(".config").join("ripgrep").join("ripgreprc"),
        "--max-columns-preview\n",
    )
    .unwrap();
    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    std::fs::write(
        xdg.join("mysbx").join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n\n[[mounts]]\npath = \"~/.config/ripgrep\"\ndest = \"/mysbx-home/.config/ripgrep\"\nmode = \"ro\"\n\n[env]\nRIPGREP_CONFIG_PATH = \"/mysbx-home/.config/ripgrep/ripgreprc\"\n"
        ),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["run"], // the real argv is passed to spawn_with_args below
        cwd: repo,
        home,
        xdg,
    };
    // The payload bash is the same sandbox-reachable one
    // `sandbox_bash()` discovers; it prints the variable AND the file
    // content, so one exec proves both the setenv and the mount
    // landed. The [env] entry below mirrors default.nix `baselineEnv`
    // by hand — keep the two in sync (the golden test
    // golden_ripgrep_config_path_activation pins the same shape).
    let args = vec![
        "run".to_owned(),
        "--".to_owned(),
        bash.to_string_lossy().into_owned(),
        "-c".to_owned(),
        // Shell builtins only (PATH inside the sandbox is the bare
        // /usr/bin of a NixOS host): $(< file) reads the mounted file
        // without `cat`.
        "printf \"%s|\" \"$(< \"$RIPGREP_CONFIG_PATH\")\"; printf \"%s\" \"$RIPGREP_CONFIG_PATH\"".to_owned(),
    ];
    let mut cmd = spawn_with_args(&inv, &args);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "exit {:?}\nstdout: {stdout}\nstderr: {stderr}",
        out.status.code()
    );
    assert!(
        stdout.trim().ends_with("|/mysbx-home/.config/ripgrep/ripgreprc"),
        "the variable must name the in-sandbox path: {stdout}"
    );
    assert!(
        stdout.contains("--max-columns-preview"),
        "the variable must resolve to the MOUNTED file's content: {stdout}"
    );
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

// ---- the sandbox's own $HOME (config.md D14) --------------------------------

/// A `bash` that is reachable *inside* the sandbox, i.e. one whose real
/// path lies under a base-bound directory (`/nix/store`, `/usr/bin`).
/// `/run/current-system/sw/bin/bash` qualifies after canonicalization —
/// `/run` itself is never mounted, but its target in the store is.
fn sandbox_bash() -> Option<PathBuf> {
    let path = std::env::var_os("PATH")?;
    for dir in std::env::split_paths(&path) {
        let candidate = dir.join("bash");
        if !candidate.is_file() {
            continue;
        }
        let real = std::fs::canonicalize(&candidate).ok()?;
        if real.starts_with("/nix/store") || real.starts_with("/usr/bin") {
            return Some(real);
        }
    }
    None
}

#[test]
fn dry_run_sets_home_to_the_sandbox_home_not_the_host_one() {
    // config.md D14: `HOME` names the in-sandbox tmpfs. The host's HOME
    // value is not forwarded and its directory is not mounted, so neither
    // may appear anywhere in the argv.
    let (inv, _, _) = fixture_user_backend("home-dry-run", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    let i = lines
        .iter()
        .position(|l| *l == "HOME")
        .unwrap_or_else(|| panic!("no HOME in the argv:\n{stdout}"));
    assert_eq!(lines[i - 1], "--setenv");
    assert_eq!(lines[i + 1], mysbx::bwrap::SANDBOX_HOME);
    // The tmpfs that backs it is there, and nothing is bound onto it.
    assert!(
        stdout.contains(&format!("--tmpfs\n{}\n", mysbx::bwrap::SANDBOX_HOME)),
        "{stdout}"
    );
    // The host home value never leaks: neither the process HOME the test
    // set, nor the machine's real one.
    let host_home = inv.home.to_string_lossy().into_owned();
    assert!(!stdout.contains(&host_home), "host HOME leaked: {stdout}");
    assert_ne!(mysbx::bwrap::SANDBOX_HOME, host_home);
    assert!(!stdout.contains("/home/"), "host home path: {stdout}");
}

#[test]
fn env_home_in_the_config_does_not_win() {
    // The last `--setenv HOME` wins in bubblewrap, and mysbx sets it
    // after `[env]` (config.md D14) — a config that names HOME is inert.
    let (inv, _, sidecar) = fixture_with_backend("home-env-override", &["--dry-run"]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"bubblewrap\"\n[env]\nHOME = \"/synth/evil-home\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    let last = lines
        .iter()
        .enumerate()
        .filter(|(_, l)| **l == "HOME")
        .map(|(i, _)| i)
        .next_back()
        .unwrap();
    assert_eq!(lines[last + 1], mysbx::bwrap::SANDBOX_HOME);
    assert!(stdout.contains("/synth/evil-home"), "{stdout}");
}

#[test]
fn cd_tilde_works_inside_the_sandbox() {
    // The bug this decision fixes: without `HOME`, `cd ~` fails with
    // `bash: cd: HOME not set`. Needs a runnable bwrap and a bash that is
    // reachable inside the sandbox — self-skip otherwise.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    let (inv, _, _) = fixture_user_backend("home-cd-tilde", &[]);
    let args = vec![
        "run".to_owned(),
        "--".to_owned(),
        bash.to_string_lossy().into_owned(),
        "-c".to_owned(),
        // Shell builtins only: PATH inside the sandbox is /usr/bin here,
        // which holds `env` and nothing else on a NixOS host.
        concat!(
            "cd ~ && pwd && : > .probe && [ -f \"$HOME/.probe\" ] ",
            "&& echo probe-written ",
            "&& { [ -e \"$HOME/.ssh\" ] && echo ssh-present || echo no-ssh; }"
        )
        .to_owned(),
    ];
    let mut cmd = spawn_with_args(&inv, &args);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "exit {:?}\nstdout: {stdout}\nstderr: {stderr}",
        out.status.code()
    );
    // `cd ~` landed in the sandbox home, which is writable …
    assert!(
        stdout.lines().any(|l| l == mysbx::bwrap::SANDBOX_HOME),
        "stdout: {stdout}"
    );
    assert!(
        stdout.contains("probe-written"),
        "home not writable: {stdout}"
    );
    // … and it is empty apart from what the payload just created, i.e.
    // it is not the host home.
    assert!(stdout.contains("no-ssh"), "host home leaked: {stdout}");
}

#[test]
fn dry_run_prints_the_pinned_backend_as_argv0() {
    // Review-1 finding 7: --dry-run audited only the bwrap ARGUMENTS —
    // argv[0] (the MYSBX_BWRAP the Nix wrapper pins, i.e. the wrapped
    // store path) was invisible because the early return came before
    // the variable was read. It must be the FIRST line of the argv
    // block, so the pinned backend is verifiable.
    let (inv, repo, _) = fixture_user_backend("dry-run-argv0", &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    // A synthetic (never executed) store path in Nix's placeholder
    // style: 32 zero characters instead of a real hash.
    cmd.env("MYSBX_BWRAP", "/nix/store/0000000000000000000000000000000-mysbx-bwrap/bin/bwrap");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.starts_with("/nix/store/0000000000000000000000000000000-mysbx-bwrap/bin/bwrap\n"),
        "argv[0] must be the pinned backend: {stdout}"
    );
    // And the rest is the ordinary argv block.
    let rest: String = stdout.lines().skip(1).map(|l| format!("{l}\n")).collect();
    assert_eq!(rest, expected_minimal_argv(&repo).strip_prefix("bwrap\n").unwrap());
}

#[test]
fn backend_failure_still_leaves_the_created_state_dirs() {
    // D15 ordering: the backing dirs are created BEFORE the backend
    // starts, so a run whose bwrap fails (bad pin, missing binary)
    // exits 1 but the sidecar's state tree exists — the next run's
    // bind sources are ready, and nothing about the failure undoes
    // the preparation.
    let (inv, repo, _) = fixture("state-dirs-backend-fail", &[]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share/opencode\"]\n",
    )
    .unwrap();
    let mut cmd = spawn_with_args(
        &inv,
        &["run", "--", "/nonexistent/mysbx-bwrap", "payload"],
    );
    cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(out.status.code(), Some(1));
    let side = repo
        .canonicalize()
        .unwrap()
        .parent()
        .unwrap()
        .join("repo.mysbx");
    assert!(side.join("state/.local/share/opencode").exists());
}

#[test]
fn a_symlink_in_the_state_tree_is_refused_not_followed() {
    // D15 ("Trust"): the state tree is the one part of the sidecar the
    // PAYLOAD can write, so it can plant a symlink there between two
    // runs. Following it would make the next run create directories
    // outside the sidecar and bind them rw into the sandbox home — a
    // host-home path re-entering the sandbox through the back door,
    // which D14 forbids. Every level of a backing path must therefore
    // be a real directory; a symlink is a hard error naming the path,
    // and nothing is created through it.
    let (inv, repo, sidecar) = fixture("state-dirs-symlink", &[]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nstate-dirs = [\".local/share/opencode\"]\n",
    )
    .unwrap();
    // A first run prepares the backing tree (the backend pin is
    // deliberately broken: state dirs are created before it runs).
    let mut cmd = spawn_with_args(&inv, &["run", "--", "/nonexistent/mysbx-bwrap"]);
    cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
    assert_eq!(
        cmd.output().expect("failed to spawn mysbx").status.code(),
        Some(1)
    );
    assert!(sidecar.join("state/.local/share/opencode").is_dir());

    // The payload's move: swap an intermediate level for a symlink
    // pointing outside the sidecar (here a stand-in for the host home).
    let outside = repo.parent().unwrap().join("outside");
    std::fs::create_dir_all(&outside).unwrap();
    std::fs::remove_dir_all(sidecar.join("state/.local")).unwrap();
    std::os::unix::fs::symlink(&outside, sidecar.join("state/.local")).unwrap();

    let mut cmd = spawn_with_args(&inv, &["run", "--", "/nonexistent/mysbx-bwrap"]);
    cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(1), "stdout: {stdout}");
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(stderr.contains("is a symlink"), "stderr: {stderr}");
    assert!(!stderr.contains("panicked"), "stderr: {stderr}");
    // The escape did not happen: no directory was created through the
    // symlink, and no bind of it was printed.
    assert!(
        !outside.join("share").exists(),
        "created a directory through the planted symlink"
    );
    assert!(!stdout.contains("outside"), "stdout: {stdout}");
}

#[test]
fn invalid_layout_is_an_error_not_a_panic() {
    // Review-2 item 4: a user-reachable invalid configuration (a mount
    // dest onto a protected path) must exit 1 with a `mysbx: `-prefixed
    // message on stderr (cli.md D8/D9) — not abort as a Rust panic.
    let (inv, _, _) = fixture("invalid-dest", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    // The mount source itself is fine, but its dest lands on the
    // protected /tmp — the argv builder must refuse.
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n\n[[mounts]]\npath = \"/etc/hosts\"\nmode = \"ro\"\ndest = \"/tmp\"\n",
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(
        stderr.starts_with("mysbx: "),
        "must carry the mysbx prefix: {stderr}"
    );
    assert!(
        stderr.contains("would shadow or overwrite the protected"),
        "stderr: {stderr}"
    );
    assert!(
        !stderr.contains("panicked"),
        "a panic leaked through: {stderr}"
    );
}

#[test]
fn hidden_mount_is_an_error_not_a_panic() {
    // Review-2 item 4, the second variant: a later mount whose dest
    // hides an earlier one is equally user-reachable, so it must also
    // exit 1 with `mysbx: ` — never a panic.
    let (inv, repo, _) = fixture("invalid-hidden", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    // Two user-layer mounts, narrow first then wide, with dests OUTSIDE
    // the test tmpdir (which sits under the protected /tmp): the wide
    // dest hides the narrow one (review-1 finding 3's scenario, .ssh
    // under /home/u, replayed on ordinary dest paths).
    std::fs::create_dir_all(repo.join("u/.ssh")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        &format!(
            "backend = \"bubblewrap\"\n\n\
             [[mounts]]\npath = \"{}/u/.ssh\"\ndest = \"/workspace/.ssh\"\nmode = \"ro\"\n\n\
             [[mounts]]\npath = \"{}/u\"\ndest = \"/workspace\"\nmode = \"rw\"\n",
            repo.display(),
            repo.display()
        ),
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(stderr.contains("would hide earlier mount"), "stderr: {stderr}");
    assert!(!stderr.contains("panicked"), "a panic leaked: {stderr}");
}

// ---- git metadata approval, end to end (review-2 item 1) -------------------

/// A fixture root OUTSIDE `/tmp`. Git metadata under `/tmp` is refused
/// on principle — `/tmp` is a protected sandbox path (the base table
/// gives the sandbox its own tmpfs), so a git dir there could never be
/// bound. `CARGO_TARGET_TMPDIR` lives under `target/`, which is an
/// ordinary path, and makes these fixtures represent the real case.
fn target_tmpdir(name: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR"))
        .join(format!("mysbx-git-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// A linked-worktree fixture: a main checkout whose `.git` directory
/// holds the per-worktree gitdir, and a worktree whose `.git` is a FILE
/// pointing at it. Returns (worktree, gitdir).
fn make_worktree_fixture(base: &Path, name: &str) -> (PathBuf, PathBuf) {
    let gitdir = base.join("main").join(".git").join("worktrees").join(name);
    std::fs::create_dir_all(gitdir.join("refs")).unwrap();
    std::fs::write(gitdir.join("HEAD"), "ref: refs/heads/main\n").unwrap();
    let worktree = base.join(name);
    std::fs::create_dir_all(&worktree).unwrap();
    std::fs::write(
        worktree.join(".git"),
        format!("gitdir: {}\n", gitdir.display()),
    )
    .unwrap();
    // The sidecar sibling of the worktree, empty on purpose: repo
    // resolution prefers the NEAREST sidecar, so this pins the
    // resolution to the fixture instead of letting it walk up into
    // whatever real repository the test tree happens to live in
    // (CARGO_TARGET_TMPDIR is inside a worktree here — without this,
    // a sidecar created at that outer repo by any earlier run would
    // hijack every make_worktree_fixture test).
    std::fs::create_dir_all(base.join(format!("{name}.mysbx"))).unwrap();
    (worktree, gitdir)
}

#[test]
fn unapproved_worktree_git_metadata_is_refused() {
    // The regression review-2 item 1 reports: the `.git` FILE lives in
    // the repo and is therefore untrusted content. Without an approval
    // in a trusted layer the bind must be refused — loudly, with the
    // `mysbx: ` prefix and exit 1.
    let base = target_tmpdir("gitdir-unapproved");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    assert!(
        !stdout.contains(&gitdir.display().to_string()),
        "no bind may be printed: {stdout}"
    );
}

#[test]
fn approved_worktree_git_metadata_is_bound_rw() {
    // With the approval in the sidecar — where `mysbx init` records it —
    // the same repo builds an argv that binds the git dir rw.
    let base = target_tmpdir("gitdir-approved");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let sidecar = base.join("wt.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!("git-dirs = [\"{}\"]\n", gitdir.display()),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = std::fs::canonicalize(&gitdir).unwrap();
    let lines: Vec<&str> = stdout.lines().collect();
    let at = lines
        .iter()
        .position(|l| *l == canon.display().to_string())
        .unwrap_or_else(|| panic!("git dir not bound: {stdout}"));
    assert_eq!(lines[at - 1], "--bind", "git metadata must be bound rw");
}

#[test]
fn init_records_the_discovered_git_metadata() {
    // `mysbx init` snapshots what it found into the fresh sidecar: the
    // trust decision happens once, in a file outside the repo, instead
    // of on every run from a repo-writable pointer.
    let base = target_tmpdir("gitdir-init-snapshot");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["init"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("created"), "stdout: {stdout}");
    let written =
        std::fs::read_to_string(base.join("wt.mysbx").join("config.toml")).unwrap();
    let canon = std::fs::canonicalize(&gitdir).unwrap();
    assert!(
        written.contains(&format!("\"{}\"", canon.display())),
        "the snapshot must list the discovered git dir: {written}"
    );
    assert!(written.contains("git-dirs = ["), "{written}");
}

#[test]
fn a_git_pointer_edited_after_init_cannot_widen_the_snapshot() {
    // The property the snapshot buys: the repo may rewrite its own
    // `.git` file at any time (it is inside the sandbox, rw), but the
    // new target is not approved, so nothing new is bound.
    let base = target_tmpdir("gitdir-tampered");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let sidecar = base.join("wt.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\ngit-dirs = [\"{}\"]\n",
            std::fs::canonicalize(&gitdir).unwrap().display()
        ),
    )
    .unwrap();
    // The repo now points its `.git` file at a DIFFERENT, git-shaped
    // directory that nobody approved.
    let evil = base.join("evil");
    std::fs::create_dir_all(evil.join("refs")).unwrap();
    std::fs::write(evil.join("HEAD"), "ref: refs/heads/main\n").unwrap();
    std::fs::write(
        worktree.join(".git"),
        format!("gitdir: {}\n", evil.display()),
    )
    .unwrap();

    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    assert!(!stdout.contains("evil"), "stdout: {stdout}");
}

#[test]
fn the_implicit_init_approves_nothing() {
    // Review-2 item 1, the trust boundary: a first bare run in a freshly
    // cloned hostile worktree must NOT turn the repo's own `.git`
    // pointer into an approval. The implicit init creates the sidecar
    // without a `git-dirs` list, and the run refuses the bind.
    let base = target_tmpdir("gitdir-implicit-init");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["run", "--", "true"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    let written =
        std::fs::read_to_string(base.join("wt.mysbx").join("config.toml")).unwrap();
    assert!(
        !written.contains("git-dirs"),
        "the implicit init must not approve: {written}"
    );
    assert!(
        !written.contains(&gitdir.display().to_string()),
        "the implicit init must not approve: {written}"
    );
}

// ---- the review-3 item 5 recovery -------------------------------------------

#[test]
fn approve_git_dirs_recovers_after_an_implicit_init() {
    // The exact scenario review-3 item 5 describes: the user's first
    // contact with a linked-worktree repo was the bare form, so the
    // sidecar exists WITHOUT approvals (the implicit init never
    // snapshots). Plain `init` would just say `exists`. The flag
    // takes the trust decision explicitly, after the fact: the config
    // must now carry the discovered git dir, and a following `run`
    // must accept it.
    let base = target_tmpdir("approve-git-dirs-recovery");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let sidecar = base.join("wt.mysbx");
    let inv = |args: Vec<&'static str>| Invocation {
        args,
        cwd: worktree.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };

    // First contact: the bare form creates the sidecar without
    // approvals and refuses the bind (the_implicit_init_approves_nothing
    // pins that half).
    let (code, _, stderr) = run_binary(&inv(vec!["run", "--", "true"]));
    assert_eq!(code, 1, "stderr: {stderr}");
    let written = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(!written.contains("git-dirs"), "{written}");

    // The recovery: explicit init with the flag.
    let (code, stdout, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("approved git metadata"), "{stdout}");
    let written = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(written.contains("git-dirs = ["), "{written}");
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "the recovery must record the discovered git dir: {written}"
    );

    // The recorded approval satisfies the next run's bind.
    let (code, _, stderr) = run_binary(&inv(vec!["--dry-run", "run", "--", "true"]));
    assert_eq!(code, 0, "stderr: {stderr}");
}

#[test]
fn approve_git_dirs_is_idempotent_and_additive_only() {
    // A second invocation with the flag must be a no-op (nothing
    // missing), and an entry an operator deliberately REMOVED is not
    // resurrected by a later `init --approve-git-dirs`... it IS
    // rediscovered, so the flag re-approves it — that is the explicit
    // word the flag speaks. What must hold: the rest of the file —
    // comments, mounts, hand-written entries — survives byte-for-byte
    // except for the added lines, and already-approved entries are
    // never duplicated.
    let base = target_tmpdir("approve-git-dirs-idempotent");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    let sidecar = base.join("wt.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    // An operator-written config: a comment, a mount, a hand-approved
    // git dir (in a `~`-free absolute spelling), one entry removed.
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "# operator notes\nbackend = \"bubblewrap\"\n\ngit-dirs = [\n  \"{}\",\n]\n\n[[mounts]]\npath = \"/etc/hosts\"\nmode = \"ro\"\n",
            gitdir.display()
        ),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["init", "--approve-git-dirs"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };

    let before = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("already lists everything"),
        "nothing was missing, so nothing may be written: {stdout}"
    );
    let after = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert_eq!(before, after, "the config must be untouched");
}

#[test]
fn approve_git_dirs_splices_into_an_existing_list_without_duplicates() {
    // The existing list names the git dir in a DIFFERENT spelling
    // (with a redundant trailing component pattern): the resolved
    // comparison must recognize it as approved and not duplicate it.
    let base = target_tmpdir("approve-git-dirs-splice");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    let sidecar = base.join("wt.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!("git-dirs = [\n  \"{}\",\n]\n# trailing comment\n", gitdir.display()),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["init", "--approve-git-dirs"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("already lists everything"),
        "the resolved spelling must count as approved: {stdout}"
    );
    let written = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert_eq!(
        written.matches(gitdir.display().to_string().as_str()).count(),
        1,
        "no duplicates: {written}"
    );
    assert!(written.contains("# trailing comment"), "{written}");
}

#[test]
fn init_rejects_unknown_arguments() {
    // The flag surface stays minimal: anything else on `init` is a
    // usage error (exit 2).
    let base = target_tmpdir("init-unknown-arg");
    let (worktree, _gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["init", "--approve-git", "extra"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unexpected argument"), "{stderr}");
}

#[test]
fn an_unapproved_common_dir_is_refused_even_when_the_gitdir_is_approved() {
    // `commondir` is a second repo-controlled pointer: approving the
    // per-worktree gitdir must not implicitly approve whatever the
    // commondir file names.
    let base = target_tmpdir("commondir-unapproved");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    // The common dir the gitdir names is elsewhere, git-shaped, and
    // NOT covered by the approval below.
    let common = base.join("elsewhere");
    std::fs::create_dir_all(common.join("refs")).unwrap();
    std::fs::write(common.join("HEAD"), "ref: refs/heads/main\n").unwrap();
    std::fs::write(gitdir.join("commondir"), format!("{}\n", common.display())).unwrap();
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let sidecar = base.join("wt.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "git-dirs = [\"{}\"]\n",
            std::fs::canonicalize(&gitdir).unwrap().display()
        ),
    )
    .unwrap();
    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: worktree,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    assert!(!stdout.contains("elsewhere"), "stdout: {stdout}");
}

#[test]
fn the_pinned_nix_conf_reaches_the_argv_and_the_report() {
    // MYSBX_NIX_CONF is a pin like MYSBX_SHELL: end-to-end, a set
    // value must appear as the source of the /etc/nix/nix.conf bind,
    // and the report must say which file the sandbox's nix reads
    // (review-2 item 3).
    let (inv, _, _) = fixture_user_backend("nix-conf-pin", &["--verbose", "--dry-run"]);
    let conf = inv.home.join("sanitized-nix.conf");
    std::fs::write(&conf, "experimental-features = nix-command flakes\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    cmd.env("MYSBX_NIX_CONF", &conf);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    let at = lines
        .iter()
        .position(|l| *l == conf.display().to_string())
        .unwrap_or_else(|| panic!("the pinned nix.conf is not bound: {stdout}"));
    assert_eq!(lines[at - 1], "--ro-bind");
    assert_eq!(lines[at + 1], "/etc/nix/nix.conf");
    assert!(
        stdout.contains(&format!("## nix.conf:       {}", conf.display())),
        "the report must name it: {stdout}"
    );
}

#[test]
fn without_the_pin_no_nix_conf_is_bound() {
    // Unset means "no nix configuration", never "the host's": that
    // file may carry access-tokens.
    let (inv, _, _) = fixture_user_backend("nix-conf-unset", &["--verbose", "--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        !stdout.contains("/etc/nix/nix.conf"),
        "no nix.conf bind: {stdout}"
    );
    assert!(stdout.contains("## nix.conf:       (none"), "{stdout}");
}

#[test]
fn a_writable_mount_of_the_home_with_the_sidecar_is_refused_end_to_end() {
    // Review-3 item 3, as a real run sees it: the sidecar config
    // exists, and the user config grants `rw` on a directory that
    // contains it. The run must fail with the policy-file error — a
    // writable sidecar steers the next run (git-dirs approvals, .git
    // rewrites) — and must NOT fall back to executing anything.
    //
    // The mounted tree must NOT contain the invocation's home: that
    // is the review-3 item 4 guard, which fires first by design (a
    // home exposure is the sharper diagnosis). The fixture therefore
    // lays the repo+sidecar tree out beside the home, not around it.
    let base = tmpdir("policy-writable");
    let trees = base.join("trees");
    let repo = trees.join("repo");
    let sidecar = trees.join("repo.mysbx");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::create_dir_all(&sidecar).unwrap();
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    let home = base.join("home");
    let xdg = base.join("xdg");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    let inv = Invocation {
        args: Vec::new(),
        cwd: repo,
        home: home.clone(),
        xdg: xdg.clone(),
    };
    let user_cfg = format!(
        "backend = \"bubblewrap\"\n\n[[mounts]]\npath = {:?}\nmode = \"rw\"\ndest = \"/all\"\n",
        trees.canonicalize().unwrap()
    );
    std::fs::write(xdg.join("mysbx").join("config.toml"), user_cfg).unwrap();

    let mut cmd = spawn_with_args(&inv, &[] as &[&str]);
    cmd.env("MYSBX_BWRAP", "/nonexistent-bwrap");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(1), "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
}

// ---- the policy PATHNAME is protected end to end (review-4 item 1) ----

/// Run the bare form with a backend that cannot be executed: the guard
/// must refuse BEFORE the payload starts, so the missing binary is
/// never reached. Returns (exit code, stdout, stderr).
fn run_refusing_launch(inv: &Invocation) -> (Option<i32>, String, String) {
    let mut cmd = spawn_with_args(inv, &[] as &[&str]);
    cmd.env("MYSBX_BWRAP", "/nonexistent-bwrap");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// The Home-Manager shape of the user config: `<xdg>/mysbx/config.toml`
/// is a SYMLINK to an immutable store-like file holding `contents`.
/// Returns the symlink path and its target.
fn hm_style_user_config(base: &Path, xdg: &Path, contents: &str) -> (PathBuf, PathBuf) {
    let store = base.join("store");
    std::fs::create_dir_all(&store).unwrap();
    let target = store.join("mysbx-config.toml");
    std::fs::write(&target, contents).unwrap();
    let dir = xdg.join("mysbx");
    std::fs::create_dir_all(&dir).unwrap();
    let link = dir.join("config.toml");
    std::os::unix::fs::symlink(&target, &link).unwrap();
    (link, target)
}

/// The layout every test in this section shares: `base/home` (HOME,
/// deliberately NOT inside the mounted tree — the review-3 item 4 home
/// guard would fire first), `base/xdg` (XDG_CONFIG_HOME), and
/// `base/trees/repo` + its sidecar.
fn policy_pathname_fixture(name: &str) -> (PathBuf, PathBuf, PathBuf, PathBuf, PathBuf) {
    let base = tmpdir(name);
    let trees = base.join("trees");
    let repo = trees.join("repo");
    let sidecar = trees.join("repo.mysbx");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::create_dir_all(&sidecar).unwrap();
    let home = base.join("home");
    let xdg = base.join("xdg");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(&xdg).unwrap();
    (base, repo, sidecar, home, xdg)
}

fn rw_mount_toml(path: &Path, dest: &str) -> String {
    format!(
        "\n[[mounts]]\npath = {:?}\nmode = \"rw\"\ndest = {dest:?}\n",
        std::fs::canonicalize(path).unwrap()
    )
}

#[test]
fn a_writable_mount_over_the_generated_user_config_symlink_is_refused() {
    // Review-4 item 1, the exact exploit: the user config is a
    // Home-Manager symlink into the store, so the RESOLVED target is
    // unwritable — but an rw mount of the directory holding the
    // symlink lets the payload unlink it and drop its own policy
    // there, which the NEXT run would trust. The run must be refused
    // before anything executes, and the symlink must be untouched.
    let (base, repo, sidecar, home, xdg) = policy_pathname_fixture("policy-symlink-user");
    std::fs::write(sidecar.join("config.toml"), "# sidecar\n").unwrap();
    // The mount source must exist to be canonicalized (D8), and it is
    // the directory the symlink will live in.
    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    let contents = format!(
        "backend = \"bubblewrap\"\n{}",
        rw_mount_toml(&xdg.join("mysbx"), "/policy")
    );
    let (link, target) = hm_style_user_config(&base, &xdg, &contents);

    let inv = Invocation {
        args: Vec::new(),
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_refusing_launch(&inv);
    assert_eq!(code, Some(1), "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    // Nothing ran, so nothing could have replaced the entry.
    assert!(
        std::fs::symlink_metadata(&link).unwrap().file_type().is_symlink(),
        "the policy symlink was replaced"
    );
    assert_eq!(std::fs::read_link(&link).unwrap(), target);
}

#[test]
fn a_writable_mount_over_a_symlinked_sidecar_config_is_refused() {
    // Same shape for the sidecar layer: its `config.toml` is a symlink
    // to a file elsewhere, and an rw mount of the sidecar directory
    // would let the payload replace the entry.
    let (base, repo, sidecar, home, xdg) = policy_pathname_fixture("policy-symlink-sidecar");
    let store = base.join("sidecar-store");
    std::fs::create_dir_all(&store).unwrap();
    let target = store.join("config.toml");
    std::fs::write(&target, "# sidecar policy\n").unwrap();
    let link = sidecar.join("config.toml");
    std::os::unix::fs::symlink(&target, &link).unwrap();

    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    std::fs::write(
        xdg.join("mysbx").join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n{}",
            rw_mount_toml(&sidecar, "/policy")
        ),
    )
    .unwrap();

    let inv = Invocation {
        args: Vec::new(),
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_refusing_launch(&inv);
    assert_eq!(code, Some(1), "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        std::fs::symlink_metadata(&link).unwrap().file_type().is_symlink(),
        "the sidecar policy symlink was replaced"
    );
    assert_eq!(std::fs::read_link(&link).unwrap(), target);
}

#[test]
fn a_writable_mount_over_an_intermediate_symlink_component_is_refused() {
    // The symlink need not be the final entry: `<xdg>/mysbx` itself is
    // a link to a directory elsewhere, and an rw mount of `<xdg>`
    // makes THAT entry replaceable — the next run's `config.toml`
    // would then be looked up in a directory the payload chose.
    let (base, repo, sidecar, home, xdg) = policy_pathname_fixture("policy-symlink-intermediate");
    std::fs::write(sidecar.join("config.toml"), "# sidecar\n").unwrap();
    let real = base.join("real-mysbx");
    std::fs::create_dir_all(&real).unwrap();
    let link = xdg.join("mysbx");
    std::os::unix::fs::symlink(&real, &link).unwrap();
    std::fs::write(
        real.join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n{}",
            rw_mount_toml(&xdg, "/xdg")
        ),
    )
    .unwrap();

    let inv = Invocation {
        args: Vec::new(),
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_refusing_launch(&inv);
    assert_eq!(code, Some(1), "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        std::fs::symlink_metadata(&link).unwrap().file_type().is_symlink(),
        "the intermediate symlink was replaced"
    );
    assert_eq!(std::fs::read_link(&link).unwrap(), real);
}

#[test]
fn an_unrelated_writable_mount_still_runs_with_a_symlinked_user_config() {
    // The guard must not swallow ordinary rw grants: a source that
    // touches neither the pathname chain nor the target is fine.
    let (base, repo, sidecar, home, xdg) = policy_pathname_fixture("policy-symlink-unrelated");
    std::fs::write(sidecar.join("config.toml"), "# sidecar\n").unwrap();
    let work = base.join("work");
    std::fs::create_dir_all(&work).unwrap();
    let contents = format!(
        "backend = \"bubblewrap\"\n{}",
        rw_mount_toml(&work, "/work")
    );
    hm_style_user_config(&base, &xdg, &contents);

    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("/work"), "the mount is built: {stdout}");
}

#[test]
fn a_read_only_view_of_the_policy_directory_still_runs() {
    // `ro` cannot replace a directory entry, so reviewing the
    // generated config from inside the sandbox stays possible.
    let (base, repo, sidecar, home, xdg) = policy_pathname_fixture("policy-symlink-ro");
    std::fs::write(sidecar.join("config.toml"), "# sidecar\n").unwrap();
    std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
    let contents = format!(
        "backend = \"bubblewrap\"\n\n[[mounts]]\npath = {:?}\nmode = \"ro\"\ndest = \"/policy\"\n",
        std::fs::canonicalize(xdg.join("mysbx")).unwrap()
    );
    hm_style_user_config(&base, &xdg, &contents);

    let inv = Invocation {
        args: vec!["--dry-run"],
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("/policy"), "the ro mount is built: {stdout}");
}

// ---- a repo root above the home is refused end to end (review-4 item 2) ----

#[test]
fn a_repo_root_containing_the_home_is_refused_before_anything_is_created() {
    // `HOME=<base>/tree/users/alice` below a `.git` marker at
    // `<base>/tree`: discovery used to accept `<base>/tree` as the repo
    // (only EQUALITY with the home was refused) and the implicit rw
    // repo bind then exposed the whole subtree — home, `.ssh` and all.
    // The guard runs before the implicit init, so no sidecar may
    // appear either.
    let base = tmpdir("repo-root-above-home");
    let tree = base.join("tree");
    let home = tree.join("users").join("alice");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(tree.join(".git")).unwrap();
    let cwd = home.join("project").join("sub");
    std::fs::create_dir_all(&cwd).unwrap();
    let xdg = base.join("xdg");
    std::fs::create_dir_all(&xdg).unwrap();

    let inv = Invocation {
        args: Vec::new(),
        cwd,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_refusing_launch(&inv);
    assert_eq!(code, Some(1), "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("contains the home directory"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        !base.join("tree.mysbx").exists(),
        "the guard must run before the implicit init"
    );
}

// ---- the approval is a table-aware TOML edit (review-4 item 3) ----------

/// The shared shape of the approval tests: a linked-worktree repo whose
/// sidecar carries `contents`, plus a user config naming the backend.
/// Returns the invocation factory and the sidecar config path.
fn approval_fixture(
    name: &'static str,
    contents: &str,
) -> (
    impl Fn(Vec<&'static str>) -> Invocation,
    PathBuf,
    PathBuf,
) {
    let base = target_tmpdir(name);
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let config = base.join("wt.mysbx").join("config.toml");
    std::fs::write(&config, contents).unwrap();
    let inv = move |args: Vec<&'static str>| Invocation {
        args,
        cwd: worktree.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    (inv, config, gitdir)
}

#[test]
fn approving_into_a_config_ending_in_a_table_stays_top_level() {
    // The review-4 bug: the approval appended `git-dirs` at EOF, and
    // TOML never returns to the root table — so in a config ending in
    // `[env]` the new key became `env.git-dirs`, which the strict
    // parser rejects. The command reported success and left a config
    // mysbx could not read.
    let (inv, config, gitdir) = approval_fixture(
        "approve-ends-in-env",
        "backend = \"bubblewrap\"\n\n[env]\nEDITOR = \"nvim\"\n",
    );

    let (code, stdout, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("approved git metadata"), "{stdout}");
    let written = std::fs::read_to_string(&config).unwrap();
    assert!(
        written.find("git-dirs").unwrap() < written.find("[env]").unwrap(),
        "the key must sit in the root table: {written}"
    );
    assert!(written.contains("EDITOR = \"nvim\""), "{written}");
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "{written}"
    );

    // The proof that matters: the rewritten config parses and the run
    // it configures succeeds.
    let (code, _, stderr) = run_binary(&inv(vec!["--dry-run", "run", "--", "true"]));
    assert_eq!(code, 0, "stderr: {stderr}");

    // And it is idempotent: nothing added twice, nothing rewritten.
    let before = std::fs::read_to_string(&config).unwrap();
    let (code, stdout, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("already lists everything"), "{stdout}");
    assert_eq!(before, std::fs::read_to_string(&config).unwrap());
}

#[test]
fn approving_into_a_config_ending_in_an_array_of_tables_stays_top_level() {
    // The `[[mounts]]` half of the same bug: the appended key became a
    // field of the last mount.
    let (inv, config, gitdir) = approval_fixture(
        "approve-ends-in-mounts",
        "backend = \"bubblewrap\"\n",
    );
    // A mount source inside the fixture: every path under /etc is
    // either protected or (on NixOS) a symlink into /nix/store, which
    // the dest rules refuse for unrelated reasons.
    let base = config.parent().unwrap().parent().unwrap().to_path_buf();
    let data = base.join("data");
    std::fs::create_dir_all(&data).unwrap();
    std::fs::write(
        &config,
        format!(
            "backend = \"bubblewrap\"\n\n[[mounts]]\npath = {:?}\nmode = \"ro\"\ndest = \"/mysbx-home/data\"\n",
            std::fs::canonicalize(&data).unwrap()
        ),
    )
    .unwrap();

    let (code, _, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    let written = std::fs::read_to_string(&config).unwrap();
    assert!(
        written.find("git-dirs").unwrap() < written.find("[[mounts]]").unwrap(),
        "the key must sit in the root table: {written}"
    );
    assert!(written.contains("/mysbx-home/data"), "{written}");
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "{written}"
    );

    let (code, stdout, stderr) = run_binary(&inv(vec!["--dry-run", "run", "--", "true"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("/mysbx-home/data"),
        "the mount survives: {stdout}"
    );
}

#[test]
fn approving_extends_an_existing_quoted_key_without_duplicating_it() {
    // `"git-dirs"` is the same key as `git-dirs`: a second definition
    // would be a duplicate-key parse error. The old line-prefix
    // locator did not recognise the quoted spelling.
    let (inv, config, gitdir) = approval_fixture(
        "approve-quoted-key",
        "backend = \"bubblewrap\"\n\"git-dirs\" = [\"/nonexistent-but-unused\"]\n",
    );
    // The pre-existing entry must not exist on disk (it is only there
    // to prove the quoted key is found); a dangling entry is a runtime
    // error for `run`, so this test stops at the file.
    let (code, stdout, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("approved git metadata"), "{stdout}");
    let written = std::fs::read_to_string(&config).unwrap();
    assert_eq!(
        written.matches("git-dirs").count(),
        1,
        "the quoted key must be extended, not duplicated: {written}"
    );
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "{written}"
    );
}

#[test]
fn approving_preserves_comments_and_a_same_named_key_in_a_table() {
    // Everything the operator wrote stays: the leading comment, the
    // `[env]` table and its (unrelated) same-named key.
    let (inv, config, gitdir) = approval_fixture(
        "approve-preserves",
        "# operator notes\nbackend = \"bubblewrap\"\n\n# about the environment\n[env]\n\"git-dirs\" = \"a value, not a path\"\n",
    );

    let (code, _, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    let written = std::fs::read_to_string(&config).unwrap();
    assert!(written.contains("# operator notes"), "{written}");
    assert!(written.contains("# about the environment"), "{written}");
    assert!(
        written.contains("\"git-dirs\" = \"a value, not a path\""),
        "the table's own key is untouched: {written}"
    );
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "{written}"
    );
    // The comment documenting `[env]` still sits on `[env]`.
    assert!(
        written.find("# about the environment").unwrap() < written.find("[env]").unwrap(),
        "{written}"
    );

    let (code, _, stderr) = run_binary(&inv(vec!["--dry-run", "run", "--", "true"]));
    assert_eq!(code, 0, "stderr: {stderr}");
}

#[test]
fn approving_a_path_with_brackets_and_hashes_round_trips() {
    // The old locator scanned for `]` and `#` without knowing about
    // strings: either character in a path corrupted the edit. The
    // fixture puts them in the REPO name, so the discovered git dir
    // carries them.
    let base = target_tmpdir("approve-weird-path");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt#1]x");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let config = base.join("wt#1]x.mysbx").join("config.toml");
    std::fs::write(&config, "backend = \"bubblewrap\"\n\n[env]\nEDITOR = \"nvim\"\n").unwrap();
    let inv = |args: Vec<&'static str>| Invocation {
        args,
        cwd: worktree.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };

    let (code, _, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    let written = std::fs::read_to_string(&config).unwrap();
    assert!(
        written.contains(&format!("\"{}\"", gitdir.display())),
        "{written}"
    );
    // A second approval finds it already listed — the round trip
    // through the parser recognised the escaped path.
    let (code, stdout, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("already lists everything"), "{stdout}");

    let (code, _, stderr) = run_binary(&inv(vec!["--dry-run", "run", "--", "true"]));
    assert_eq!(code, 0, "stderr: {stderr}");
}

#[test]
fn an_unparsable_sidecar_config_is_never_rewritten() {
    // The edit validates with the real parser before it replaces
    // anything — and a config that does not parse in the first place
    // fails before that, with the file untouched.
    let (inv, config, _gitdir) =
        approval_fixture("approve-unparsable", "git-dirs = [\"/a\n");
    let before = std::fs::read_to_string(&config).unwrap();
    let (code, _, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, 1, "stderr: {stderr}");
    assert!(stderr.starts_with("mysbx: "), "{stderr}");
    assert_eq!(before, std::fs::read_to_string(&config).unwrap());
}
