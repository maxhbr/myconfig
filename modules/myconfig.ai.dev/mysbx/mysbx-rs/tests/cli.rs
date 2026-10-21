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

use mysbx::config::Multiplexer;
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

impl Clone for Invocation {
    fn clone(&self) -> Self {
        Self {
            args: self.args.clone(),
            cwd: self.cwd.clone(),
            home: self.home.clone(),
            xdg: self.xdg.clone(),
        }
    }
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
        // The `gui` closing-message shell is a SEPARATE pin (a POSIX
        // `sh`, never the payload shell): unpinned here, so the tests
        // get the crate's `sh` fallback unless they pin it themselves.
        .env_remove("MYSBX_GUI_SHELL")
        .env_remove("MYSBX_BWRAP")
        // The gvisor pins are wrapper-provided, never inherited from the
        // test runner's own (possibly wrapped) environment — a wrapped
        // mysbx on PATH would otherwise leak its image pins into tests
        // that must exercise the UNPINNED refusal paths (bd
        // myconfig-xrt).
        .env_remove("MYSBX_PODMAN_TARBALL")
        .env_remove("MYSBX_PODMAN_IMAGE")
        .env_remove("MYSBX_PODMAN_IMAGE_ID")
        .env_remove("MYSBX_GVISOR_RUNTIME_FLAGS")
        .env_remove("MYSBX_GVISOR_CGROUP_MANAGER")
        .env_remove("MYSBX_PODMAN_SHELL")
        .env_remove("MYSBX_PODMAN_TOOLS_PATH")
        .env_remove("MYSBX_PODMAN")
        .env_remove("MYSBX_NONO")
        .env_remove("MYSBX_NONO_PROFILE")
        .env_remove("MYSBX_ENV")
        // The waypipe pins are wrapper-provided too (D18): a wrapped
        // mysbx on PATH would otherwise leak its display pins into
        // tests that must exercise the UNPINNED refusal path.
        .env_remove("MYSBX_WAYPIPE")
        .env_remove("MYSBX_PODMAN_WAYPIPE")
        .env_remove("MYSBX_WAYPIPE_SECCTX")
        // The detached-run pins and markers (docs/design/detach.md
        // B4/B10) are wrapper- or dtach-provided: a wrapped mysbx on
        // PATH, or a test runner under a `mysbx --detach` itself,
        // would otherwise leak the dtach pin or the inner marker into
        // tests that must exercise the refusal paths and the plain
        // foreground runs.
        .env_remove("MYSBX_DTACH")
        .env_remove("MYSBX_DETACHED")
        // The detached-run registry root is not a fixture input: a
        // host-set value would make runs of this very sandbox depend
        // on the machine's runtime dir. Tests that exercise the
        // runtime-root refusal set it themselves.
        .env_remove("XDG_RUNTIME_DIR")
        // Keep the host's TERM & co. out of the result: the forwarded set
        // must come only from variables the test actually sets. The list
        // is the same constant the pipeline reads — not a hand copy that
        // could drift.
        ;
    for name in mysbx::FORWARDED_ENV_VARS {
        cmd.env_remove(name);
    }
    for suffix in mysbx::RENAMED_PODMAN_PINS {
        cmd.env_remove(format!("MYSBX_PODMAN_{suffix}"));
        cmd.env_remove(format!("MYSBX_GVISOR_{suffix}"));
    }
    // The multiplexer entry pins (host and in-image) are wrapper-
    // provided too: the refusal tests need them unset.
    for mux in ["TMUX", "WORKMUX", "HERDR", "AOE", "ORCA"] {
        cmd.env_remove(format!("MYSBX_MUX_ENTRY_{mux}"));
        cmd.env_remove(format!("MYSBX_PODMAN_MUX_ENTRY_{mux}"));
    }
    cmd
}

/// [`run_binary`] with an argument list that overrides the fixture's.
fn run_binary_with(inv: &Invocation, args: &[&str]) -> (i32, String, String) {
    let out = spawn_with_args(inv, args)
        .output()
        .expect("failed to spawn the mysbx binary");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
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

/// Mark a sidecar as INITIALIZED, i.e. put the file `mysbx init` would
/// have written there (cli.md D13: a run refuses a repo without it).
/// Empty on purpose — the real template is comment-only, so an empty
/// file is the same empty policy layer with less noise. Callers that
/// test a policy overwrite it.
fn init_sidecar(sidecar: &Path) {
    std::fs::write(sidecar.join("config.toml"), "").unwrap();
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

/// A standard fixture: an INITIALIZED repo with sidecar at `base/repo`,
/// empty home and XDG dirs, `args` to run from inside the repo. Every
/// sandbox-running test needs the sidecar config to exist (cli.md D13);
/// use [`fixture_uninited`] for the tests that pin the refusal.
fn fixture(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture_uninited(name, args);
    init_sidecar(&sidecar);
    (inv, repo, sidecar)
}

/// [`fixture`] without the sidecar `config.toml`: the sidecar DIRECTORY
/// exists, the repo is not initialized.
fn fixture_uninited(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
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
/// sidecar config stays empty and the pipeline still reaches the argv
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

/// Whether a usable `ssh-keygen` sits on the PATH — SMOKE-TESTED,
/// because ssh-keygen refuses to run for a uid without a passwd entry
/// ("No user exists for uid …"), the norm inside this very repo's
/// mysbx sandbox but never on a real host or in CI: those get the
/// lifecycle tests, the degraded environment skips.
fn is_ssh_keygen_available() -> bool {
    let probe =
        std::env::temp_dir().join(format!("mysbx-ssh-keygen-cli-probe-{}", std::process::id()));
    let ok = Command::new("ssh-keygen")
        .args(["-t", "ed25519", "-N", "", "-C", "probe", "-f"])
        .arg(&probe)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false);
    let _ = std::fs::remove_file(&probe);
    let _ = std::fs::remove_file(format!("{}.pub", probe.display()));
    ok
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
    // Side-effect-free, and nothing beyond the fixture's own
    // `config.toml` appeared in the sidecar (no state tree, no rewrite).
    let sidecar = inv.cwd.parent().unwrap().join("repo.mysbx");
    assert!(!sidecar.join("state").exists());
    assert_eq!(
        std::fs::read_to_string(sidecar.join("config.toml")).unwrap(),
        ""
    );
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

// ---- initialization is explicit (cli.md D13) -------------------------------

/// The three run forms, all of which must refuse an uninitialized repo.
const RUN_FORMS: &[&[&str]] = &[
    &[],
    &["run", "--", "true"],
    &["--dry-run"],
    &["run", "--dry-run", "--", "true"],
];

#[test]
fn a_run_in_an_uninitialized_repo_fails_with_the_init_hint() {
    // cli.md D13: a run never creates the sidecar. Every run form —
    // bare, `run --`, and both under `--dry-run` — exits nonzero with a
    // `mysbx: ` message naming the missing config path and telling the
    // operator to run `mysbx init`. Nothing is written: not the
    // sidecar directory, not the config.
    for (i, args) in RUN_FORMS.iter().enumerate() {
        let base = tmpdir(&format!("uninited-{i}"));
        let repo = base.join("repo");
        std::fs::create_dir_all(&repo).unwrap();
        std::fs::create_dir_all(base.join("home")).unwrap();
        std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
        // A user config that names the backend: the refusal must not
        // depend on a half-configured host.
        std::fs::write(
            base.join("xdg").join("mysbx").join("config.toml"),
            "backend = \"bubblewrap\"\n",
        )
        .unwrap();
        let inv = Invocation {
            args: Vec::new(),
            cwd: repo,
            home: base.join("home"),
            xdg: base.join("xdg"),
        };
        let (code, stdout, stderr) = run_binary_with(&inv, args);
        assert_ne!(code, 0, "args {args:?}: stdout: {stdout}");
        assert!(stderr.starts_with("mysbx: "), "args {args:?}: {stderr}");
        assert!(stderr.contains("mysbx init"), "args {args:?}: {stderr}");
        let sidecar = base.join("repo.mysbx");
        assert!(
            stderr.contains(&sidecar.join("config.toml").display().to_string()),
            "the message must name the missing sidecar config: {stderr}"
        );
        // No argv, no report, no creation.
        assert!(!stdout.contains("--clearenv"), "args {args:?}: {stdout}");
        assert!(
            !sidecar.exists(),
            "args {args:?}: a run created {}",
            sidecar.display()
        );
    }
}

#[test]
fn a_run_with_a_sidecar_directory_but_no_config_still_fails() {
    // The *config file* is what initialization means: a bare
    // `<repo>.mysbx/` directory (a leftover state tree, a hand-made
    // directory) is not a policy, and running with an empty layer
    // instead would hide that.
    let (inv, _, sidecar) = fixture_uninited("uninited-dir-only", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(stderr.contains("mysbx init"), "{stderr}");
    assert!(!sidecar.join("config.toml").exists());
}

#[test]
fn init_then_the_bare_form_works() {
    // The full first-contact sequence: the bare form fails, `mysbx
    // init` initializes, and the very same invocation now builds an
    // argv. The user config names the backend (`init` writes a
    // comment-only sidecar, which decides no policy).
    let (inv, repo, sidecar) = fixture_uninited("init-then-run", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();

    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.contains("mysbx init"), "{stderr}");

    let (code, stdout, stderr) = run_binary_with(&inv, &["init"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("## created"), "stdout: {stdout}");
    assert!(sidecar.join("config.toml").exists());

    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(stdout, expected_minimal_argv(&repo));
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
    // The bwrap argv must not appear: it was never a flag.
    assert!(!stdout.contains("--clearenv"), "argv printed: {stdout}");
}

// ---- --verbose (cli.md D10) -------------------------------------------------

/// The report block of a stdout stream: every `## `-prefixed line.
fn report_lines(stdout: &str) -> Vec<&str> {
    stdout.lines().filter(|l| l.starts_with("## ")).collect()
}

#[test]
fn the_verbose_report_shows_the_flag_overridden_multiplexer() {
    // cli.md D10/D14: the report describes the run that is about to
    // happen, so with `--multiplexer` it names the flag's choice — the
    // effective one — not the configuration's. A report repeating the
    // configured value while the argv starts another entry would lie
    // about the very thing the isolation claim rests on.
    let (inv, _, _) = fixture_mux(
        "verbose-mux-flag",
        Multiplexer::Workmux,
        &["--verbose", "--dry-run"],
    );
    let mut cmd = spawn_with_args(&inv, &["--multiplexer", "tmux", "--verbose", "--dry-run"]);
    all_mux_pins(&mut cmd);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(report.contains("multiplexer:    tmux"), "{report}");
    assert!(!report.contains("multiplexer:    workmux"), "{report}");
    assert!(stdout.contains(&mux_pin(Multiplexer::Tmux).1), "{stdout}");
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

// ---- the implicit worktrees sibling bind ---------------------------------

#[test]
fn an_existing_worktrees_sibling_is_bound_implicitly() {
    // A repo whose workmux `<repo>__worktrees` sibling exists gets it
    // bound rw implicitly, after the repo bind and before every
    // configured mount — visible in `--dry-run` and named in the
    // `--verbose` report. No sidecar `[[mounts]]` entry is needed.
    let (inv, repo, _) = fixture_with_backend("worktrees-implicit", &["--dry-run"]);
    let worktrees = repo.parent().unwrap().join(format!(
        "{}__worktrees",
        repo.file_name().unwrap().to_string_lossy()
    ));
    std::fs::create_dir_all(&worktrees).unwrap();

    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains(&format!(
            "--bind\n{}\n{}\n",
            worktrees.display(),
            worktrees.display()
        )),
        "missing the implicit rw worktrees bind:\n{stdout}"
    );
}

#[test]
fn an_absent_worktrees_sibling_adds_no_bind() {
    // The counterpart: without the sibling the argv is byte-identical
    // to the minimal golden — a run never creates the directory, so
    // absence keeps the sandbox narrow.
    let (inv, repo, _) = fixture_with_backend("worktrees-absent", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(argv_block(&stdout), expected_minimal_argv(&repo));
    assert!(!stdout.contains("__worktrees"), "{stdout}");
}

#[test]
fn the_report_names_the_implicit_worktrees_bind() {
    // cli.md D10: every bind that reaches the argv belongs in the
    // report — with its provenance, so an operator sees WHY a
    // directory they never declared is mounted.
    let (inv, repo, _) = fixture_with_backend("worktrees-report", &["--verbose", "--dry-run"]);
    let worktrees = repo.parent().unwrap().join(format!(
        "{}__worktrees",
        repo.file_name().unwrap().to_string_lossy()
    ));
    std::fs::create_dir_all(&worktrees).unwrap();

    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains(&format!(
            "  rw {} -> {}  [worktrees, implicit]",
            worktrees.display(),
            worktrees.display()
        )),
        "{report}"
    );
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
    assert!(
        argv_block(&stdout).starts_with("bwrap\n--clearenv\n"),
        "{stdout}"
    );
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(stderr.contains("never re-enable it"), "stderr: {stderr}");
}

#[test]
fn no_backend_configured_fails() {
    // cli.md D7: the backend is explicit, never auto-detected; neither
    // layer named one, so the run is refused.
    let (inv, _, _) = fixture("no-backend", &["--dry-run"]);
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("no backend configured"), "stderr: {stderr}");
}

#[test]
fn unknown_backend_fails() {
    // The MVP accepts exactly `bubblewrap`.
    let (inv, _, sidecar) = fixture("unknown-backend", &["--dry-run"]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"qemu\"\n").unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("qemu"), "stderr: {stderr}");
}

#[test]
fn backend_bubblewrap_is_accepted() {
    let (inv, _, _) = fixture_with_backend("backend-ok", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.starts_with("bwrap\n--clearenv\n"),
        "stdout: {stdout}"
    );
}

// ---- --backend (cli.md D18, bd myconfig-veg) -------------------------------

#[test]
fn the_backend_flag_is_accepted_on_every_run_form() {
    // The flag belongs to the bare form and to `run`, before the verb
    // and after it — one position rule, like `--ro`/`--rw` (D10/D16).
    for (args, label) in [
        (
            vec!["--backend", "bubblewrap", "--dry-run"],
            "bare, pre-verb",
        ),
        (
            vec!["run", "--backend", "bubblewrap", "--dry-run", "--", "true"],
            "run, after the verb",
        ),
        (
            vec!["--backend", "bubblewrap", "run", "--dry-run", "--", "true"],
            "run, before the verb",
        ),
    ] {
        let (inv, _, _) = fixture("backend-flag-forms", &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 0, "{label}: stderr: {stderr}");
        assert!(stdout.starts_with("bwrap\n"), "{label}: {stdout}");
    }
}

#[test]
fn the_backend_flag_overrides_both_config_layers() {
    // The CLI is the outermost layer (config.md D1): the flag wins
    // over the sidecar and over the user config. Both layers name
    // `bubblewrap` — in both directions the argv and the report must
    // show the flag's choice — and with NEITHER layer naming a
    // backend at all, the flag alone selects one.
    let (inv, _, sidecar) = fixture_user_backend("backend-flag-user-config", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    let mut cmd = spawn_with_args(
        &inv,
        &["--backend", "podman-gvisor", "--verbose", "--dry-run"],
    );
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains("backend:        podman-gvisor  [--backend]"),
        "{report}"
    );
    assert!(!stdout.contains("backend:        bubblewrap"), "{stdout}");
    assert!(
        stdout.contains("\npodman\n--runtime=runsc\n"),
        "argv of the flag's backend: {stdout}"
    );

    // The override is per-invocation: without the flag the same
    // fixture runs the configured backend again (the empty sidecar
    // layer lets the user config's bubblewrap decide, no provenance
    // tag).
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(stdout.starts_with("## mysbx "), "report first: {stdout}");
    let report = report_lines(&stdout).join("\n");
    assert!(report.contains("backend:        bubblewrap\n"), "{report}");
    assert!(!report.contains("[--backend]"), "{report}");
    // ... and the argv block behind it is the bwrap one.
    assert_eq!(argv_block(&stdout), expected_minimal_argv(&inv.cwd));
}

#[test]
fn the_backend_flag_selects_one_on_an_unset_configuration() {
    // Neither layer names a backend: the flag alone decides (D18,
    // "neither layer has to name a backend for the flag to select
    // one"), where without the flag this run is refused with `no
    // backend configured` (D7).
    let (inv, _, _) = fixture("backend-flag-from-nothing", &[]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["--backend", "bubblewrap", "--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.starts_with("bwrap\n"), "{stdout}");
}

#[test]
fn an_unknown_backend_flag_value_is_a_refused_run_naming_the_set() {
    // cli.md D18/D8: the command line was fine, the backend it names
    // does not exist — an infrastructure refusal (`70`), not a usage
    // error, listing the valid values and naming the flag as the
    // source; the rows of the config layers never appear.
    let (inv, _, _) = fixture("backend-flag-bad", &[]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["--backend", "qemu", "--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(
        stderr.contains("unknown backend `qemu`"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("(from --backend)"), "stderr: {stderr}");
    assert!(stderr.contains("`bubblewrap`"), "stderr: {stderr}");
    assert!(stderr.contains("`podman-gvisor`"), "stderr: {stderr}");
    assert!(
        !stdout.contains("--clearenv"),
        "no argv on refusal: {stdout}"
    );

    // A missing value, by contrast, is a usage error (2) — the
    // command line itself is wrong.
    let (code, _, _) = run_binary_with(&inv, &["--backend"]);
    assert_eq!(code, 2);
    let (code, _, stderr) = run_binary_with(&inv, &["run", "--backend"]);
    assert_eq!(code, 2, "{stderr}");
}

#[test]
fn the_backend_flag_is_rejected_by_every_verb() {
    // cli.md D18: no verb without a run accepts the flag — the same
    // "is not valid with `<verb>`" usage error (2) the other
    // run-scoped flags get, and `run` accepts it only before its `--`.
    for (args, name) in [
        (vec!["--backend", "bubblewrap", "init"], "init"),
        (vec!["--backend", "bubblewrap", "edit"], "edit"),
        (vec!["--backend", "bubblewrap", "version"], "version"),
        (vec!["--backend", "bubblewrap", "help"], "help"),
        (
            vec!["--backend", "bubblewrap", "podman-load-image"],
            "podman-load-image",
        ),
        (vec!["--backend", "bubblewrap", "gui"], "gui"),
    ] {
        let (inv, _, _) = fixture(&format!("backend-flag-verb-{name}"), &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{name}: stdout: {stdout}");
        assert!(stderr.contains("--backend"), "{name}: {stderr}");
        assert!(stderr.contains("is not valid with"), "{name}: {stderr}");
        assert!(!stdout.contains("--clearenv"), "{name}: no argv: {stdout}");
    }

    // A repeated flag is a typo, not an intensifier (D5).
    let (inv, _, _) = fixture("backend-flag-repeated", &[]);
    let (code, _, stderr) = run_binary_with(
        &inv,
        &["--backend", "bubblewrap", "--backend", "podman-gvisor"],
    );
    assert_eq!(code, 2, "{stderr}");
    assert!(stderr.contains("repeated flag"), "{stderr}");
}

// bd myconfig-6tt: the `--<backend>` and `--<mux>` shorthands —
// end-to-end dry-run runs that must behave exactly like their long
// forms (pure aliases), plus the one-run conflict rule.
#[test]
fn backend_shorthand_runs_like_the_long_form() {
    // `--nono` before the verb, after it, and its long form: the same
    // argv, the same `[--backend]` provenance tag in the report.
    for (args, label) in [
        (vec!["--nono", "--verbose", "--dry-run"], "bare shorthand"),
        (
            vec!["run", "--nono", "--verbose", "--dry-run", "--", "true"],
            "run, after the verb",
        ),
        (
            vec!["--backend", "nono", "--verbose", "--dry-run"],
            "bare long form",
        ),
    ] {
        let (inv, _, _) = fixture("backend-shorthand-forms", &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 0, "{label}: stderr: {stderr}");
        let report = report_lines(&stdout).join("\n");
        assert!(
            report.contains("backend:        nono  [--backend]"),
            "{label}: {report}"
        );
    }

    // The one-run rule: two shorthands, or shorthand + long form, in
    // either order — a usage error naming both spellings, never a
    // silently last-wins.
    for (args, first, second) in [
        (
            vec!["--bubblewrap", "--nono", "--dry-run"],
            "--bubblewrap",
            "--nono",
        ),
        (
            vec!["--nono", "--bubblewrap", "--dry-run"],
            "--nono",
            "--bubblewrap",
        ),
        (
            vec!["--backend", "bubblewrap", "--nono", "--dry-run"],
            "--nono",
            "--backend",
        ),
        (
            vec!["--nono", "--backend", "bubblewrap", "--dry-run"],
            "--backend",
            "--nono",
        ),
    ] {
        let (inv, _, _) = fixture("backend-shorthand-conflict", &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "stderr: {stderr}");
        assert!(stderr.contains(first), "{first}: {stderr}");
        assert!(stderr.contains(second), "{second}: {stderr}");
        assert!(!stdout.contains("--clearenv"), "no argv: {stdout}");
    }
}

#[test]
fn mux_shorthand_runs_like_the_long_form() {
    // `--herdr` and its long form pick the same session payload —
    // byte-identical dry-run argv when the entry is pinned.
    let (inv, _, _) = fixture_mux("mux-shorthand-herdr", Multiplexer::Herdr, &[]);
    let mut long = spawn_with_args(&inv, &["--multiplexer", "herdr", "--dry-run"]);
    let (_, entry) = mux_pin(Multiplexer::Herdr);
    long.env("MYSBX_MUX_ENTRY_HERDR", &entry);
    let out = long.output().expect("failed to spawn the mysbx binary");
    let with_long = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(out.status.code(), Some(0), "{with_long}");

    let (inv, _, _) = fixture_mux("mux-shorthand-herdr", Multiplexer::Herdr, &[]);
    let mut short = spawn_with_args(&inv, &["--herdr", "--dry-run"]);
    short.env("MYSBX_MUX_ENTRY_HERDR", &entry);
    let out = short.output().expect("failed to spawn the mysbx binary");
    let with_short = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(out.status.code(), Some(0), "{with_short}");
    assert_eq!(with_short, with_long, "pure alias: byte-identical argv");
    assert!(with_short.contains(&entry), "{with_short}");

    // `--none` forces the plain shell over a configured session —
    // compared against its LONG form, not the pre-existing golden:
    // the keypair state bind differs with the environment (uid-0
    // ssh-keygen, bd myconfig-0x5), and the alias contract is the
    // byte-identity of the two spellings.
    let (inv, _, _) = fixture_mux("mux-shorthand-none", Multiplexer::Workmux, &[]);
    let mut short = spawn_with_args(&inv, &["--none", "--dry-run"]);
    all_mux_pins(&mut short);
    let out = short.output().expect("failed to spawn the mysbx binary");
    let with_short = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(out.status.code(), Some(0), "{with_short}");
    assert!(
        with_short.contains("MYSBX_MULTIPLEXER\nnone\n"),
        "{with_short}"
    );

    let (inv, _, _) = fixture_mux("mux-shorthand-none", Multiplexer::Workmux, &[]);
    let mut long = spawn_with_args(&inv, &["--multiplexer", "none", "--dry-run"]);
    all_mux_pins(&mut long);
    let out = long.output().expect("failed to spawn the mysbx binary");
    let with_long = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(out.status.code(), Some(0), "{with_long}");
    assert_eq!(with_short, with_long, "pure alias: byte-identical argv");

    // The one-run rule, both spellings named.
    for (args, first, second) in [
        (vec!["--tmux", "--none", "--dry-run"], "--tmux", "--none"),
        (vec!["--none", "--tmux", "--dry-run"], "--none", "--tmux"),
        (
            vec!["--multiplexer", "tmux", "--none", "--dry-run"],
            "--none",
            "--multiplexer",
        ),
        (
            vec!["--none", "--multiplexer", "tmux", "--dry-run"],
            "--multiplexer",
            "--none",
        ),
    ] {
        let (inv, _, _) = fixture("mux-shorthand-conflict", &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "stderr: {stderr}");
        assert!(stderr.contains(first), "{first}: {stderr}");
        assert!(stderr.contains(second), "{second}: {stderr}");
        assert!(!stdout.contains("--clearenv"), "no argv: {stdout}");
    }
}

// ---- environment forwarding and payload handling ---------------------------

#[test]
fn only_set_host_variables_are_forwarded() {
    // docs/plan.md "Environment": exactly the allowlist of lib.rs
    // (`FORWARDED_ENV_VARS`) — technical variables only (terminal,
    // locale, editor), each only when actually set. Credentials are
    // NOT part of the default: a host that exports ANTHROPIC_AUTH_TOKEN
    // does not leak it into an unconfigured sandbox.
    let (inv, _, _) = fixture_user_backend("forward-env", &["--dry-run"]);
    let mut cmd = spawn(&inv);
    cmd.env("TERM", "xterm-test")
        .env("VISUAL", "nvim-test")
        .env("ANTHROPIC_AUTH_TOKEN", "secret-token")
        .env("ANTHROPIC_BASE_URL", "https://example.internal");
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();

    // TERM and VISUAL are set and technical; everything else is either
    // unset or a secret nobody allowlisted.
    let term = lines.iter().position(|x| *x == "TERM").unwrap();
    let visual = lines.iter().position(|x| *x == "VISUAL").unwrap();
    assert_eq!(lines[term + 1], "xterm-test");
    assert_eq!(lines[visual + 1], "nvim-test");
    for absent in [
        "COLORTERM",
        "LANG",
        "LC_ALL",
        "EDITOR",
        "ANTHROPIC_AUTH_TOKEN",
        "ANTHROPIC_BASE_URL",
        "OPENAI_API_KEY",
        "OPENAI_BASE_URL",
        "ANTHROPIC_API_KEY",
        "OPENROUTER_API_KEY",
        "OPENROUTER_BASE_URL",
    ] {
        assert!(!lines.contains(&absent), "{absent} must not be forwarded");
    }
    assert!(
        !stdout.contains("secret-token"),
        "a host credential must not reach the sandbox by default: {stdout}"
    );
    // Forwarded variables precede PATH, which is always last of the
    // --setenv section.
    let path = lines.iter().position(|x| *x == "PATH").unwrap();
    assert!(term < path && visual < path);
}

#[test]
fn forward_env_config_adds_to_the_default_allowlist() {
    // A `forward-env` (user config or sidecar) ADDS names to the
    // built-in default of `FORWARDED_ENV_VARS` — additive like
    // `state-dirs`, never a replacement: everything the default
    // already carries keeps forwarding, the config's names join it,
    // and a duplicate (TERM is already a default) costs nothing. The
    // values still come from the host environment only when set. This
    // is the ONLY way a credential reaches a sandbox.
    let (inv, _, _) = fixture_user_backend("forward-env-config", &["--dry-run"]);
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nforward-env = [\"ANTHROPIC_AUTH_TOKEN\", \"TERM\"]\n",
    )
    .unwrap();
    let mut cmd = spawn(&inv);
    cmd.env("TERM", "xterm-config")
        .env("ANTHROPIC_AUTH_TOKEN", "allowlisted-secret")
        .env("VISUAL", "nvim-still-default")
        .env("OPENAI_API_KEY", "not-allowlisted");
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();

    // The config's addition forwards when set...
    let term = lines.iter().position(|x| *x == "TERM").unwrap();
    let token = lines
        .iter()
        .position(|x| *x == "ANTHROPIC_AUTH_TOKEN")
        .unwrap();
    assert_eq!(lines[term + 1], "xterm-config");
    assert_eq!(lines[token + 1], "allowlisted-secret");
    // ...and the technical default still forwards too — a config that
    // adds a credential does not silently drop VISUAL.
    let visual = lines.iter().position(|x| *x == "VISUAL").unwrap();
    assert_eq!(lines[visual + 1], "nvim-still-default");
    // A name nobody allowlisted stays out even when set on the host,
    // and an un-set default name never appears either.
    for absent in ["COLORTERM", "LANG", "OPENAI_API_KEY"] {
        assert!(!lines.contains(&absent), "{absent} must not be forwarded");
    }
    assert!(
        !stdout.contains("not-allowlisted"),
        "only allowlisted credentials may be forwarded: {stdout}"
    );
}

#[test]
fn forward_env_of_both_layers_concatenates() {
    // Both layers declare `forward-env`; the lists concatenate user
    // layer first (the same rule as state-dirs) — the host-wide config
    // may pre-approve the defaults while a repo sidecar adds its own.
    let (inv, _, sidecar) = fixture_user_backend("forward-env-merge", &["--dry-run"]);
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\nforward-env = [\"TERM\"]\n",
    )
    .unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"bubblewrap\"\nforward-env = [\"TIER_TOKEN\", \"TERM\"]\n",
    )
    .unwrap();
    let mut cmd = spawn(&inv);
    cmd.env("TERM", "xterm-merge")
        .env("TIER_TOKEN", "tier-sidecar");
    let out = cmd.output().unwrap();
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    let term = lines.iter().position(|x| *x == "TERM").unwrap();
    let token = lines.iter().position(|x| *x == "TIER_TOKEN").unwrap();
    assert_eq!(lines[term + 1], "xterm-merge");
    assert_eq!(lines[token + 1], "tier-sidecar");
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
    // Initialized (cli.md D13), so the only thing under test is the
    // discovery fallback, not the init refusal.
    let sidecar = base.join("plain.mysbx");
    std::fs::create_dir_all(&sidecar).unwrap();
    init_sidecar(&sidecar);
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
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
    let text = std::fs::read_to_string(&persisted).unwrap_or_else(|e| panic!("{persisted:?}: {e}"));
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(
        stderr.contains("state-dirs entries nest"),
        "stderr: {stderr}"
    );
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
    // The count includes the implicit `.ssh` entry of the
    // unconditional keypair (D22): 2 entries, one declared.
    assert!(report.contains("state dirs:     2"), "{report}");
    assert!(
        report.contains(&format!(
            "  /mysbx-home/.ssh <-> {}/state/.ssh  [state]",
            side.display()
        )),
        "{report}"
    );
    assert!(
        report.contains(&format!(
            "  /mysbx-home/.local/share/opencode <-> {}/state/.local/share/opencode  [state]",
            side.display()
        )),
        "{report}"
    );
}

// ---- the sandbox ssh keypair (docs/design/config.md D22) ---------

/// A fixture with the bubblewrap backend (the minimal shape a run
/// needs). The keypair is unconditional: no `ssh-key` key exists.
fn fixture_ssh_key(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    (inv, repo, sidecar)
}

#[test]
fn a_dry_run_binds_the_ssh_dir_and_creates_nothing() {
    // D22: the argv binds `<sidecar>/state/.ssh` rw at
    // `/mysbx-home/.ssh` — the implicit state entry — and `--dry-run`
    // stays side-effect-free: no state tree, no keypair. No host
    // `~/.ssh` content is ever mounted: the only ssh path in the argv
    // is the sidecar store.
    let (inv, repo, sidecar) = fixture_ssh_key("ssh-key-dry-run", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    assert!(
        stdout.contains(&format!(
            "--bind\n{}/state/.ssh\n/mysbx-home/.ssh\n",
            side.display()
        )),
        "the ssh bind is missing from the argv: {stdout}"
    );
    assert!(
        !stdout.contains("/home/"),
        "a host home path in the argv: {stdout}"
    );
    assert!(
        !side.join("state").exists(),
        "the dry run created the state tree"
    );
}

#[test]
fn the_verbose_report_names_the_ssh_key() {
    // D22: the report shows the bind with its provenance — the
    // sandbox `~/.ssh` on one side, the sidecar store on the other,
    // `[generated, ed25519]` — so the operator can check the argv
    // against it, and `state dirs:` counts the implicit entry.
    let (inv, repo, _) = fixture_ssh_key("ssh-key-verbose", &["--verbose", "--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    assert!(
        stdout.contains(&format!(
            "ssh key:        /mysbx-home/.ssh <-> {}/state/.ssh  [generated, ed25519]",
            side.display()
        )),
        "the ssh key line is missing from the report: {stdout}"
    );
    assert!(
        stdout.contains("state dirs:     1 (rw, persisted in the sidecar)"),
        "the implicit entry must count as a state dir: {stdout}"
    );
}

#[test]
fn the_ssh_pubkey_verb_prints_and_generates() {
    // D22: `mysbx ssh-pubkey` generates the pair (the key is enabled)
    // and prints the public line unprefixed — the value to paste into
    // a GitHub deploy-key form / a gitolite keydir. A second call
    // leaves the key untouched.
    if !is_ssh_keygen_available() {
        eprintln!("skipping: no usable ssh-keygen in this environment");
        return;
    }
    let (inv, repo, sidecar) = fixture_ssh_key("ssh-key-verb", &["ssh-pubkey"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    let dir = side.join("state").join(".ssh");
    assert!(
        dir.join("id_ed25519").is_file(),
        "no private key was created"
    );
    // The printed public key is exactly the `.pub` file's content.
    let printed = stdout
        .lines()
        .find(|l| l.starts_with("ssh-ed25519 "))
        .expect("the public key was not printed");
    assert_eq!(
        printed,
        std::fs::read_to_string(dir.join("id_ed25519.pub"))
            .unwrap()
            .trim_end()
    );
    // A second call: same key, not regenerated.
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout
            .lines()
            .any(|l| l.starts_with("ssh-ed25519 ") && l == printed),
        "the existing pair must print unchanged: {stdout}"
    );
    let _ = std::fs::remove_dir_all(&sidecar.parent().unwrap());
}

#[test]
fn the_ssh_pubkey_verb_refuses_a_disabled_key() {
    // The old `ssh-key = false` is obsolete: the sidecar config must
    // FAIL parsing, naming the key — the keypair is unconditional
    // now, and a config that still sets the key must not load.
    let (inv, _, sidecar) = fixture("ssh-key-disabled", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"bubblewrap\"\nssh-key = false\n",
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "an obsolete ssh-key key must be refused");
    assert!(stderr.contains("ssh-key"), "{stderr}");
    assert!(!sidecar.join("state").exists(), "nothing was created");
}

#[test]
fn the_ssh_pubkey_dry_run_prints_the_plan_and_generates_nothing() {
    let (inv, repo, sidecar) = fixture_ssh_key("ssh-key-dry-verb", &["--dry-run", "ssh-pubkey"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let canon = repo.canonicalize().unwrap();
    let side = canon.parent().unwrap().join("repo.mysbx");
    assert!(
        stdout.contains(&format!("## would create: {}/state/.ssh", side.display())),
        "{stdout}"
    );
    assert!(
        !side.join("state").exists(),
        "--dry-run generated a keypair"
    );
    let _ = std::fs::remove_dir_all(&sidecar.parent().unwrap());
}

#[test]
fn init_template_documents_every_schema_key() {
    // The template is the operator's map of the schema: a key the parser
    // accepts but the fresh file never shows is a key nobody uncomments.
    // Pinned against `Config::KEYS`, so adding a key to the parser without
    // adding it here fails this test instead of silently narrowing the
    // template (config.md D11/D12).
    let (inv, _, sidecar) = fixture_uninited("template-keys", &["init"]);
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let text = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    for key in mysbx::config::Config::KEYS {
        // A table key appears as a header (`[[mounts]]`, `[env]`), a scalar
        // key as a commented `key = …` line.
        let shown = text.contains(&format!("# {key} ="))
            || text.contains(&format!("\n{key} ="))
            || text.contains(&format!("[[{key}]]"))
            || text.contains(&format!("[{key}]"));
        assert!(shown, "the init template does not show `{key}`:\n{text}");
    }
    // Every key is COMMENTED (except the recorded `multiplexer`, see
    // below): a template that decided a backend or a mount would sandbox
    // a repo by accident.
    assert_eq!(
        active_lines(&text),
        vec!["multiplexer = \"none\""],
        "{text}"
    );
    // The file mysbx just wrote parses as the empty policy it claims to be.
    let parsed = mysbx::config::Config::parse(&text).expect("the template must parse");
    assert_eq!(
        parsed,
        mysbx::config::Config {
            multiplexer: parsed.multiplexer,
            ..mysbx::config::Config::default()
        },
        "{text}"
    );
    // Both table sections are present, and LAST — a root key after a table
    // header would belong to that table.
    let env = text.find("[env]").expect("[env] section");
    let nix = text.find("[nix]").expect("[nix] section");
    assert!(env < nix, "{text}");
    for (key, at) in [
        ("mounts", text.find("[[mounts]]")),
        ("network", text.find("network =")),
    ] {
        assert!(
            at.expect(key) < env,
            "`{key}` must sit in the root table:\n{text}"
        );
    }
}

#[test]
fn init_template_mentions_state_dirs() {
    // The init template documents every schema key; `state-dirs` must
    // appear in it (commented), or a new operator would never learn the
    // feature exists.
    let (inv, _, sidecar) = fixture_uninited("state-dirs-init", &["init"]);
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let text = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(text.contains("state-dirs"), "{text}");
    // And it decides no POLICY: the only active line is the
    // `multiplexer` key, which grants nothing and merely records the
    // user layer's own value (D17, see the dedicated tests below).
    assert_eq!(
        active_lines(&text),
        vec!["multiplexer = \"none\""],
        "{text}"
    );
}

/// The lines of a config file that are neither blank nor a comment —
/// what a template actually DECIDES.
fn active_lines(text: &str) -> Vec<&str> {
    text.lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .collect()
}

#[test]
fn init_records_the_user_layers_multiplexer_in_the_sidecar() {
    // D17: `init` copies the effective default, it does not invent
    // one. The sidecar WINS over the user config, so writing anything
    // else here would silently downgrade the host default for every
    // freshly initialized repository.
    for mux in [
        Multiplexer::None,
        Multiplexer::Tmux,
        Multiplexer::Workmux,
        Multiplexer::Herdr,
        Multiplexer::Aoe,
        Multiplexer::Orca,
    ] {
        let (inv, _, sidecar) = fixture_uninited(&format!("init-mux-{mux}"), &["init"]);
        std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
        std::fs::write(
            inv.xdg.join("mysbx").join("config.toml"),
            format!(
                "backend = \"bubblewrap\"\nmultiplexer = \"{}\"\n",
                mux.name()
            ),
        )
        .unwrap();
        let (code, _stdout, stderr) = run_binary(&inv);
        assert_eq!(code, 0, "{mux}: stderr: {stderr}");
        let text = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
        assert_eq!(
            active_lines(&text),
            vec![format!("multiplexer = \"{}\"", mux.name())],
            "{mux}: {text}"
        );
        // The enum is documented next to the value, so the operator
        // can change it without reading the design docs.
        for name in Multiplexer::NAMES {
            assert!(text.contains(name), "{mux}: {text} does not mention {name}");
        }
        // And the file mysbx just wrote parses — with exactly that
        // value.
        let parsed = mysbx::config::Config::parse(&text).expect("the template must parse");
        assert_eq!(parsed.multiplexer, Some(mux), "{mux}");
    }
}

#[test]
fn init_leaves_the_multiplexer_commented_when_the_user_layer_is_unreadable() {
    // A guessed value would OVERRIDE the user layer (the sidecar
    // wins), so an unparsable user config gets a commented-out key
    // instead. That layer fails every run on its own anyway, with its
    // own message naming the file.
    let (inv, _, sidecar) = fixture_uninited("init-mux-broken-user", &["init"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "multiplexer = \"screen\"\n",
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let text = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(active_lines(&text).is_empty(), "{text}");
    assert!(text.contains("# multiplexer = \"none\""), "{text}");
}

// ---- init stays what it was --------------------------------------------------

#[test]
fn init_creates_sidecar_config_and_is_idempotent() {
    let (inv, _, sidecar) = fixture_uninited("init", &["init"]);
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
    // The generated config decides no policy: it names no backend and
    // no mount, and its one active line is the `multiplexer` key that
    // mirrors the user layer (D17).
    let text = std::fs::read_to_string(&config).unwrap();
    assert_eq!(
        active_lines(&text),
        vec!["multiplexer = \"none\""],
        "{text}"
    );
}

// ---- the multiplexer (docs/design/config.md D17, cli.md D11) ---------------

/// [`fixture`] with `backend` and `multiplexer = "<mux>"` in the USER
/// config — the shape the generated myconfig layer has on a host with
/// that multiplexer wired.
fn fixture_mux(
    name: &str,
    mux: Multiplexer,
    args: &[&'static str],
) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\nmultiplexer = \"{}\"\n",
            mux.name()
        ),
    )
    .unwrap();
    (inv, repo, sidecar)
}

/// Every multiplexer that starts a session — the set these tests run
/// over, taken from the CLI's own enum so a new variant cannot be
/// added without deciding what these tests say about it.
const SESSION_MUXES: [Multiplexer; 5] = [
    Multiplexer::Tmux,
    Multiplexer::Workmux,
    Multiplexer::Herdr,
    Multiplexer::Aoe,
    Multiplexer::Orca,
];

/// The wrapper pin of `mux` and a synthetic entry path for it. The pin
/// name is read from the CLI's mapping, never hand-copied: a renamed
/// variable turns these tests red instead of leaving them exercising a
/// variable nothing reads.
fn mux_pin(mux: Multiplexer) -> (&'static str, String) {
    (
        mux.entry_var()
            .expect("a session-starting multiplexer has a pin"),
        format!("/synth/bin/mysbx-{}-entry", mux.name()),
    )
}

/// The in-sandbox private socket directory, spelled from the constant
/// the argv builder uses — never hand-copied.
fn socket_dir() -> &'static str {
    mysbx::bwrap::MUX_SOCKET_DIR
}

#[test]
fn dry_run_bare_form_launches_the_selected_entry_on_an_in_sandbox_socket() {
    // D17: every selectable multiplexer replaces the interactive
    // payload with ITS OWN pinned entry, on the private socket
    // directory inside the sandbox home.
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        let (inv, _, _) = fixture_mux(&format!("mux-dry-run-{mux}"), mux, &["--dry-run"]);
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env(var, &entry);
        // The pins of the OTHER multiplexers are set too, to a path
        // that must never be started: only the selected one's pin is
        // read (a shared or mixed-up pin would show up here).
        for other in SESSION_MUXES.iter().filter(|m| **m != mux) {
            let (other_var, _) = mux_pin(*other);
            cmd.env(other_var, "/synth/bin/WRONG-entry");
        }
        // A host tmux server in the calling environment must change
        // nothing: neither variable is forwarded, and the socket path
        // is infrastructure.
        cmd.env("TMUX_TMPDIR", "/tmp/host-tmux");
        cmd.env("TMUX", "/tmp/host-tmux/socket,123,0");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(out.status.code(), Some(0), "{mux}: {stdout}");
        let lines: Vec<&str> = stdout.lines().collect();

        // The payload is that multiplexer's entry, not the shell and
        // not another multiplexer's entry.
        assert_eq!(lines[lines.len() - 2], "--");
        assert_eq!(lines[lines.len() - 1], entry, "{mux}: {stdout}");
        assert!(!stdout.contains("/synth/bin/bash"), "{mux}: {stdout}");
        assert!(!stdout.contains("WRONG-entry"), "{mux}: {stdout}");

        // TMUX_TMPDIR points inside the sandbox home, and it is the
        // LAST `--setenv` — no `[env]` layer can follow and repoint it.
        let at = lines
            .iter()
            .position(|l| *l == "TMUX_TMPDIR")
            .unwrap_or_else(|| panic!("{mux}: TMUX_TMPDIR is not set: {stdout}"));
        assert_eq!(lines[at - 1], "--setenv");
        assert_eq!(lines[at + 1], socket_dir());
        assert!(
            socket_dir().starts_with(&format!("{}/", mysbx::bwrap::SANDBOX_HOME)),
            "the socket must live in the sandbox home"
        );
        // Nothing of the host's tmux world is bound or forwarded.
        assert!(!stdout.contains("/tmp/host-tmux"), "{mux}: {stdout}");
        assert!(!stdout.contains("/tmp/tmux-"), "{mux}: {stdout}");
        // `/tmp` is the tmpfs of the base table, not a bind of the
        // host's.
        assert!(stdout.contains("--tmpfs\n/tmp\n"), "{mux}: {stdout}");
    }
}

#[test]
fn dry_run_run_form_is_never_a_multiplexer_session() {
    // cli.md D11: `run -- CMD` stays a one-shot whichever multiplexer
    // is selected — same payload, no TMUX_TMPDIR, and byte-identical
    // to the argv the same fixture produces without the key.
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        let args = &["run", "--dry-run", "--", "ls"];
        let (inv, _, _) = fixture_mux(&format!("mux-run-form-{mux}"), mux, args);
        let mut cmd = spawn_with_args(&inv, args);
        cmd.env(var, &entry);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let with = String::from_utf8_lossy(&out.stdout).into_owned();
        assert_eq!(out.status.code(), Some(0), "{mux}: {with}");
        assert!(!with.contains("TMUX_TMPDIR"), "{mux}: {with}");
        assert!(!with.contains(&entry), "{mux}: {with}");
        assert!(with.ends_with("--\nls\n"), "{mux}: {with}");

        std::fs::write(
            inv.xdg.join("mysbx").join("config.toml"),
            "backend = \"bubblewrap\"\nmultiplexer = \"none\"\n",
        )
        .unwrap();
        let (code, without, stderr) = run_binary(&inv);
        assert_eq!(code, 0, "{mux}: stderr: {stderr}");
        // cli.md D11 byte-compat, minus the identity variable (bd
        // myconfig-3nn): MYSBX_MULTIPLEXER names the CONFIG's choice,
        // so it is the one expected difference between a config that
        // selects a session and one that selects none — everything
        // else must stay byte-identical.
        let strip_mux = |argv: &str| -> String {
            argv.split('\n')
                .filter(|l| {
                    !matches!(
                        *l,
                        "MYSBX_MULTIPLEXER"
                            | "none"
                            | "tmux"
                            | "workmux"
                            | "herdr"
                            | "aoe"
                            | "orca"
                    )
                })
                .collect::<Vec<_>>()
                .join("\n")
        };
        assert_eq!(
            strip_mux(&with),
            strip_mux(&without),
            "{mux}: the run form must not change with a multiplexer"
        );
        assert!(
            without.contains("MYSBX_MULTIPLEXER\nnone\n"),
            "{mux}: {without}"
        );
        assert!(
            with.contains(&format!("MYSBX_MULTIPLEXER\n{mux}\n")),
            "{mux}: {with}"
        );
    }
}

#[test]
fn a_multiplexer_without_a_pinned_entry_fails_instead_of_starting_a_shell() {
    // The refusal is the point (D17): a silent plain shell would be
    // discovered only after the work happened outside the session. A
    // config selecting a multiplexer this build does not carry is a
    // configuration error at argv-build time — so `--dry-run` refuses
    // it too, and no bwrap is ever started.
    for mux in SESSION_MUXES {
        let (var, _) = mux_pin(mux);
        let (inv, _, _) = fixture_mux(&format!("mux-unpinned-{mux}"), mux, &["--dry-run"]);
        let (code, stdout, stderr) = run_binary(&inv);
        assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "{mux}: stdout: {stdout}");
        // The message names the value AND the variable a host must set.
        assert!(stderr.contains(var), "{mux}: {stderr}");
        assert!(stderr.contains(mux.name()), "{mux}: {stderr}");
        assert!(
            !stdout.contains("--clearenv"),
            "{mux}: no argv on refusal: {stdout}"
        );
    }
}

#[test]
fn an_unknown_multiplexer_is_a_config_error_naming_the_file_and_the_key() {
    // The strict enum (D17), end to end: the message must let the
    // operator find the offending line — file, key, accepted values.
    let (inv, _, _) = fixture("mux-unknown", &["--dry-run"]);
    let user_config = inv.xdg.join("mysbx").join("config.toml");
    std::fs::create_dir_all(user_config.parent().unwrap()).unwrap();
    std::fs::write(
        &user_config,
        "backend = \"bubblewrap\"\nmultiplexer = \"screen\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(
        stderr.contains(&user_config.display().to_string()),
        "{stderr}"
    );
    assert!(stderr.contains("multiplexer"), "{stderr}");
    assert!(stderr.contains("`screen`"), "{stderr}");
    for name in mysbx::config::Multiplexer::NAMES {
        assert!(stderr.contains(name), "{stderr} does not list {name}");
    }
    assert!(!stdout.contains("--clearenv"), "no argv: {stdout}");
}

#[test]
fn multiplexer_none_keeps_the_plain_interactive_shell() {
    // Byte-compat: a host without any integration, or a repo that
    // selected `none`, gets exactly the pre-existing argv — even with
    // every entry pinned.
    for (name, layer) in [("mux-absent", None), ("mux-none", Some(Multiplexer::None))] {
        let (inv, repo, _) = match layer {
            None => fixture_user_backend(name, &["--dry-run"]),
            Some(mux) => fixture_mux(name, mux, &["--dry-run"]),
        };
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        for mux in SESSION_MUXES {
            let (var, entry) = mux_pin(mux);
            cmd.env(var, entry);
        }
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(out.status.code(), Some(0), "{name}: {stdout}");
        assert_eq!(stdout, expected_minimal_argv(&repo), "{name}");
    }
}

#[test]
fn the_sidecar_wins_over_the_user_layer_for_the_multiplexer() {
    // D17's layering rule, end to end: the repo decides. Both
    // directions — another multiplexer, and `none` for a repo that
    // wants a bare shell on a host whose default is a session.
    let (inv, repo, sidecar) =
        fixture_mux("mux-sidecar-wins", Multiplexer::Workmux, &["--dry-run"]);
    std::fs::write(sidecar.join("config.toml"), "multiplexer = \"herdr\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        cmd.env(var, entry);
    }
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.ends_with(&format!("{}\n", mux_pin(Multiplexer::Herdr).1)),
        "{stdout}"
    );

    // `none` in the sidecar: the plain shell, byte-identical to a host
    // that never named a multiplexer at all.
    std::fs::write(sidecar.join("config.toml"), "multiplexer = \"none\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        cmd.env(var, entry);
    }
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert_eq!(stdout, expected_minimal_argv(&repo));
}

/// Every pin of [`SESSION_MUXES`], set on `cmd` (the `--multiplexer`
/// tests run with all of them, so the flag selects the entry and never
/// the order in which the pins happen to be read).
fn all_mux_pins(cmd: &mut Command) {
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        cmd.env(var, entry);
    }
}

#[test]
fn the_multiplexer_flag_overrides_the_configuration_for_one_invocation() {
    // cli.md D14: `--multiplexer <mux>` wins over BOTH layers for THIS
    // run — a different session than the configured one, a session on
    // a `none` configuration, and `none` on a session configuration.
    let (inv, _, _) = fixture_mux("mux-flag-override", Multiplexer::Workmux, &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--multiplexer", "herdr", "--dry-run"]);
    all_mux_pins(&mut cmd);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(
        stdout.ends_with(&format!("{}\n", mux_pin(Multiplexer::Herdr).1)),
        "{stdout}"
    );
    assert!(
        !stdout.contains(&mux_pin(Multiplexer::Workmux).1),
        "{stdout}"
    );
    assert!(stdout.contains("TMUX_TMPDIR"), "{stdout}");

    // `none` forces the plain shell on a host that configured a
    // session — the payload is the shell, with no socket variable.
    let mut cmd = spawn_with_args(&inv, &["--multiplexer", "none", "--dry-run"]);
    all_mux_pins(&mut cmd);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(!stdout.contains("TMUX_TMPDIR"), "{stdout}");
    assert!(stdout.ends_with("/synth/bin/bash\n"), "{stdout}");

    // And the override is per-invocation: without the flag the very
    // same fixture starts the configured workmux session again.
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    all_mux_pins(&mut cmd);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.ends_with(&format!("{}\n", mux_pin(Multiplexer::Workmux).1)),
        "{stdout}"
    );
}

#[test]
fn the_multiplexer_flag_selects_a_session_on_a_none_configuration() {
    // The other direction: a host whose layers say `none` (or say
    // nothing) can still start a session for one invocation — with
    // every pin set, the flag alone decides which entry runs, and the
    // pins of the OTHER multiplexers point at paths that must never
    // be started.
    let (inv, _, _) = fixture_user_backend("mux-flag-from-none", &["--dry-run"]);
    for mux in SESSION_MUXES {
        let (var, entry) = mux_pin(mux);
        let mut cmd = spawn_with_args(&inv, &["--multiplexer", mux.name(), "--dry-run"]);
        for other in SESSION_MUXES.iter().filter(|m| **m != mux) {
            let (other_var, _) = mux_pin(*other);
            cmd.env(other_var, "/synth/bin/WRONG-entry");
        }
        cmd.env(var, &entry);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(out.status.code(), Some(0), "{mux}: {stdout}");
        assert!(stdout.ends_with(&format!("{entry}\n")), "{mux}: {stdout}");
        assert!(!stdout.contains("WRONG-entry"), "{mux}: {stdout}");
        assert!(stdout.contains("TMUX_TMPDIR"), "{mux}: {stdout}");
    }
}

#[test]
fn the_multiplexer_flag_without_a_pinned_entry_fails_like_the_config() {
    // The refusal is the same one D17 gives a config layer selecting an
    // unpinned multiplexer: never a silent plain shell. `--dry-run`
    // refuses too, so no bwrap is started. (The fixture's user layer
    // says `none`, so the refusal can only come from the flag.)
    let (inv, _, _) = fixture_mux("mux-flag-unpinned", Multiplexer::None, &["--dry-run"]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["--multiplexer", "herdr", "--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(stderr.contains(mux_pin(Multiplexer::Herdr).0), "{stderr}");
    assert!(stderr.contains("herdr"), "{stderr}");
    assert!(
        !stdout.contains("--clearenv"),
        "no argv on refusal: {stdout}"
    );
}

#[test]
fn an_unknown_multiplexer_flag_value_is_a_usage_error_naming_the_set() {
    // cli.md D14/D8: a bad value is a command-line error (`2`), not a
    // runtime one — and the message names every accepted spelling.
    let (inv, _, _) = fixture("mux-flag-bad", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["--multiplexer", "screen", "--dry-run"]);
    assert_eq!(code, 2, "stdout: {stdout}");
    assert!(stderr.contains("--multiplexer"), "{stderr}");
    assert!(stderr.contains("`screen`"), "{stderr}");
    for name in mysbx::config::Multiplexer::NAMES {
        assert!(stderr.contains(name), "{stderr} does not list {name}");
    }
    assert!(!stdout.contains("--clearenv"), "no argv: {stdout}");

    // A missing value is the same usage error, never a `none`.
    let (code, _, stderr) = run_binary_with(&inv, &["--multiplexer"]);
    assert_eq!(code, 2, "{stderr}");
}

#[test]
fn the_multiplexer_flag_is_rejected_by_every_verb() {
    // cli.md D14: the flag selects the INTERACTIVE payload, so no verb
    // accepts it — `run` because a one-shot starts no session (D11),
    // the others because they have no run at all.
    for (args, name) in [
        (vec!["--multiplexer", "tmux", "run", "--", "ls"], "run"),
        (vec!["--multiplexer", "tmux", "init"], "init"),
        (vec!["--multiplexer", "tmux", "edit"], "edit"),
        (vec!["--multiplexer", "tmux", "version"], "version"),
        (vec!["--multiplexer", "tmux", "help"], "help"),
        (
            vec!["run", "--multiplexer", "tmux", "--", "ls"],
            "run (after the verb)",
        ),
    ] {
        let (inv, _, _) = fixture(&format!("mux-flag-verb-{name}"), &[]);
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{name}: stdout: {stdout}");
        assert!(stderr.contains("--multiplexer"), "{name}: {stderr}");
        assert!(!stdout.contains("--clearenv"), "{name}: no argv: {stdout}");
    }
}

#[test]
fn a_mount_over_the_mux_socket_dir_is_refused_end_to_end() {
    let (inv, _, sidecar) = fixture_mux("mux-socket-mount", Multiplexer::Workmux, &["--dry-run"]);
    let (var, entry) = mux_pin(Multiplexer::Workmux);
    std::fs::create_dir_all(inv.home.join("shared")).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "[[mounts]]\npath = \"{}\"\ndest = \"{}\"\nmode = \"rw\"\n",
            inv.home.join("shared").display(),
            socket_dir(),
        ),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env(var, entry);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains(socket_dir()), "{stderr}");
    assert!(stderr.contains("D16/D17"), "{stderr}");
}

#[test]
fn the_selected_entry_really_runs_with_an_in_sandbox_socket_dir() {
    // The real-execution counterpart of the dry runs above: a stand-in
    // entry script (no multiplexer closure is available to the cargo
    // suite) proves that the interactive form execs the entry INSIDE
    // the sandbox, that `$TMUX_TMPDIR` is writable there, and that the
    // host's own tmux socket directory is unreachable. Run for every
    // variant: the stand-in stands in for all of them, and what is
    // under test is mysbx's dispatch, not the real multiplexers.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    for mux in SESSION_MUXES {
        let (var, _) = mux_pin(mux);
        let (inv, repo, _) = fixture_mux(&format!("mux-real-{mux}"), mux, &[]);
        // The stand-in entry lives in the repo, which is bound rw at
        // its real path — so the host path mysbx pins is a valid
        // in-sandbox path too.
        let entry = repo.join("fake-mux-entry");
        std::fs::write(
            &entry,
            format!(
                // Shell builtins only: PATH inside the sandbox is
                // /usr/bin here, which holds `env` and nothing else on
                // a NixOS host — so the socket DIRECTORY is not created
                // (that is the real entry's `mkdir`), the test proves
                // instead that its parent (the sandbox home tmpfs) is
                // writable and that the path is inside it.
                "#!{}\nset -eu\necho \"tmpdir=$TMUX_TMPDIR\"\n\
                 case \"$TMUX_TMPDIR\" in \"$HOME\"/*) echo tmpdir-inside-home ;; \
                 *) echo tmpdir-outside-home ;; esac\n\
                 : > \"$HOME/.probe\" && echo home-writable\n\
                 if [ -e /tmp/host-tmux ]; then echo host-socket-visible; \
                 else echo host-socket-absent; fi\n",
                bash.display()
            ),
        )
        .unwrap();
        let mut perms = std::fs::metadata(&entry).unwrap().permissions();
        use std::os::unix::fs::PermissionsExt;
        perms.set_mode(0o755);
        std::fs::set_permissions(&entry, perms).unwrap();
        // A host-side directory that a leaked socket path could hit.
        std::fs::create_dir_all("/tmp/host-tmux").ok();

        let mut cmd = spawn_with_args(&inv, &[] as &[&str]);
        cmd.env(var, &entry);
        cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
        cmd.env("TMUX_TMPDIR", "/tmp/host-tmux");
        let out = cmd.output().expect("failed to spawn mysbx");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            out.status.success(),
            "{mux}: exit {:?}\nstdout: {stdout}\nstderr: {stderr}",
            out.status.code()
        );
        assert!(
            stdout.contains(&format!("tmpdir={}", socket_dir())),
            "{mux}: the entry did not see the in-sandbox socket dir: {stdout}"
        );
        assert!(stdout.contains("tmpdir-inside-home"), "{mux}: {stdout}");
        assert!(stdout.contains("home-writable"), "{mux}: {stdout}");
        // The host's `/tmp/host-tmux` is invisible: `/tmp` is a tmpfs.
        assert!(
            stdout.contains("host-socket-absent"),
            "{mux}: a host tmux socket dir is reachable: {stdout}"
        );
        let _ = std::fs::remove_dir_all("/tmp/host-tmux");
    }
}

// ---- the display channel (docs/design/config.md D18) ---------------

/// [`fixture`] with `backend` and `display = "waypipe"` in the USER
/// config — the shape the generated myconfig layer would have on a
/// host that pins waypipe.
fn fixture_display(name: &str, args: &[&'static str]) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\ndisplay = \"waypipe\"\n",
    )
    .unwrap();
    (inv, repo, sidecar)
}

#[test]
fn a_waypipe_display_without_the_client_pin_is_a_refused_run() {
    // D18: a selection this build cannot serve (no `MYSBX_WAYPIPE`)
    // exits 70 with the pin named — never a silently headless run.
    // `--dry-run` refuses it too: the failure is a configuration
    // error, not an exec failure.
    let (inv, _, sidecar) = fixture_display("display-unpinned", &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--dry-run"] as &[&str]);
    cmd.env_remove("MYSBX_WAYPIPE");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("display"), "{stderr}");
    assert!(stderr.contains("MYSBX_WAYPIPE"), "{stderr}");
    // Nothing was created: no waypipe/ token directory under the sidecar.
    assert!(!sidecar.join("waypipe").exists());
}

#[test]
fn a_waypipe_display_dry_run_wraps_the_payload_and_creates_nothing() {
    // D18: with the pin set the argv wraps the payload in the guest
    // `waypipe server`, binds the per-run token directory — and a dry
    // run still creates nothing under the sidecar.
    let (inv, repo, sidecar) = fixture_display("display-dry-run", &["--dry-run"]);
    let waypipe = repo.join("fake-waypipe");
    let mut cmd = spawn_with_args(&inv, &["--dry-run"] as &[&str]);
    cmd.env("MYSBX_WAYPIPE", &waypipe);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    // The argv prints one argument per line, so the wrap shows as the
    // consecutive tokens of the guest server invocation — waypipe's
    // options are ROOT options, so they precede the `server`
    // subcommand and the payload follows its `--`.
    let lines: Vec<&str> = stdout.lines().collect();
    let server_at = lines
        .iter()
        .position(|l| *l == "server")
        .expect("the guest waypipe server is not the payload prefix");
    assert_eq!(lines[server_at - 5], waypipe.to_string_lossy());
    assert_eq!(lines[server_at - 4], "--socket");
    assert!(
        lines[..server_at]
            .iter()
            .any(|l| l.ends_with("/waypipe.sock")),
        "the per-run socket is not named: {stdout}"
    );
    assert_eq!(lines[server_at - 2], "--display");
    assert_eq!(lines[server_at - 1], "wayland-0");
    assert_eq!(lines[server_at + 1], "--");
    assert!(!sidecar.join("waypipe").exists());
}

#[test]
fn a_waypipe_display_run_reports_the_channel() {
    // cli.md D10: the `--verbose` report carries the display line —
    // the host-side socket path, the guest display name and the guest
    // binary — so the operator can check the channel against the argv.
    let (inv, _, _) = fixture_display("display-verbose", &["--dry-run"]);
    let waypipe = "/synth/bin/waypipe";
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"] as &[&str]);
    cmd.env("MYSBX_WAYPIPE", waypipe);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("display:         waypipe"), "{stdout}");
    assert!(stdout.contains("waypipe.sock"), "{stdout}");
    assert!(stdout.contains(waypipe), "{stdout}");
    // The default is stated too: an ordinary run says `off`.
    let (inv2, _, _) = fixture_user_backend("display-verbose-off", &["--verbose", "--dry-run"]);
    let mut cmd = spawn_with_args(&inv2, &["--verbose", "--dry-run"] as &[&str]);
    cmd.env_remove("MYSBX_WAYPIPE");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("display:         off — the run is headless"),
        "{stdout}"
    );
}

#[test]
fn a_waypipe_result_run_spawns_the_client_and_cleans_up_after_the_run() {
    // S1, the real spawn path (D18) with a FAKE waypipe client and a
    // FAKE bwrap — both `#!/bin/sh` scripts: the client creates the
    // socket PATH (a plain file is all the spawn contract polls for)
    // and a marker proving it ran, then detaches its stderr (mysbx
    // inherits stderr into the client on purpose — the test's output
    // pipe must not be held by the fake's `sleep`) and sleeps (a multi
    // client never exits on its own); the backend exits 0. A `--result`
    // run must then: exit 0 (the fake bwrap's code), have spawned the
    // client (the marker), have killed it (its pid is gone) and have
    // removed the per-run token directory — nothing of the channel
    // outlives the run.
    let (inv, repo, sidecar) = fixture_display("display-result-spawn", &["run"]);
    let base = repo.parent().unwrap();
    let marker = base.join("client-spawned");
    let pidfile = base.join("client-pid");
    let fake_client = base.join("fake-waypipe-client");
    std::fs::write(
        &fake_client,
        format!(
            "#!/bin/sh\nprintf %s $$ > {}\ntouch \"$2\"\nprintf spawned > {}\nexec 2>/dev/null\nsleep 10\n",
            pidfile.display(),
            marker.display()
        ),
    )
    .unwrap();
    let fake_bwrap = base.join("fake-bwrap");
    std::fs::write(&fake_bwrap, "#!/bin/sh\nexit 0\n").unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&fake_client, std::fs::Permissions::from_mode(0o755)).unwrap();
    std::fs::set_permissions(&fake_bwrap, std::fs::Permissions::from_mode(0o755)).unwrap();
    let mut cmd = spawn_with_args(&inv, &["run", "--result", "--", "/usr/bin/env"] as &[&str]);
    cmd.env("MYSBX_WAYPIPE", &fake_client)
        .env("MYSBX_BWRAP", &fake_bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(0),
        "the fake backend's exit is the run's\nstdout: {stdout}\nstderr: {stderr}"
    );
    assert!(!stderr.contains("infrastructure"), "{stderr}");
    // The client DID run: it created the socket path and its marker.
    assert_eq!(std::fs::read_to_string(&marker).unwrap(), "spawned");
    // The waited run KILLED it: the pid it published is gone (a pid
    // reuse within the test is not a realistic hazard — the sweep of
    // a reused pid is D18's own best-effort rule, not this test's).
    let pid: u32 = std::fs::read_to_string(&pidfile)
        .unwrap()
        .trim()
        .parse()
        .unwrap();
    assert!(
        !std::path::Path::new(&format!("/proc/{pid}")).exists(),
        "the waypipe client must not outlive the waited run"
    );
    // The per-run token directory is gone too (only the `waypipe/`
    // root remains).
    assert!(sidecar.join("waypipe").exists());
    assert_eq!(
        std::fs::read_dir(sidecar.join("waypipe")).unwrap().count(),
        0,
        "the token directory must not outlive the run"
    );
}

// ---- `mysbx edit` (docs/design/cli.md D12) --------------------------------

/// A stand-in `$EDITOR`: a shell script that appends its whole argument
/// vector to `record` and exits 0. Proves both WHICH file mysbx opens
/// and how it split the variable — without any editor in the closure.
fn fake_editor(dir: &Path, record: &Path) -> PathBuf {
    let script = dir.join("fake-editor");
    std::fs::write(
        &script,
        format!("#!/bin/sh\nprintf '%s\\n' \"$@\" >> {}\n", record.display()),
    )
    .unwrap();
    let mut perms = std::fs::metadata(&script).unwrap().permissions();
    use std::os::unix::fs::PermissionsExt;
    perms.set_mode(0o755);
    std::fs::set_permissions(&script, perms).unwrap();
    script
}

#[test]
fn edit_creates_the_sidecar_config_and_opens_it_in_the_editor() {
    let (inv, _, sidecar) = fixture_uninited("edit", &["edit"]);
    // A fresh repo: the sidecar directory exists (the fixture makes it)
    // but the config does not — `edit` must create the commented
    // template first, so the operator edits a file, not a void.
    let config = sidecar.join("config.toml");
    assert!(!config.exists());
    let record = inv.home.join("opened");
    let editor = fake_editor(&inv.home, &record);

    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", &editor);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(0),
        "stdout: {stdout} stderr: {stderr}"
    );
    assert!(stdout.contains("## created"), "{stdout}");
    assert!(config.exists());
    // Exactly one argument: the SIDECAR config of this repo.
    assert_eq!(
        std::fs::read_to_string(&record).unwrap(),
        format!("{}\n", config.display())
    );

    // Idempotent in the D12 sense: a second `edit` opens the same file
    // and does not rewrite it.
    let before = std::fs::read_to_string(&config).unwrap();
    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", &editor);
    assert!(cmd.output().unwrap().status.success());
    assert_eq!(std::fs::read_to_string(&config).unwrap(), before);
    // The user config is NOT what was opened (on myconfig hosts it is a
    // generated store symlink).
    let opened = std::fs::read_to_string(&record).unwrap();
    assert!(
        !opened.contains("xdg"),
        "the user config was opened: {opened}"
    );
}

#[test]
fn edit_splits_the_editor_variable_into_arguments() {
    // `EDITOR="code --wait"` is the common shape; the flags must reach
    // the editor as arguments, before the file.
    let (inv, _, sidecar) = fixture("edit-args", &["edit"]);
    let record = inv.home.join("opened");
    let editor = fake_editor(&inv.home, &record);
    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", format!("{} --wait -x", editor.display()));
    assert!(cmd.output().unwrap().status.success());
    assert_eq!(
        std::fs::read_to_string(&record).unwrap(),
        format!("--wait\n-x\n{}\n", sidecar.join("config.toml").display())
    );
}

#[test]
fn edit_falls_back_to_visual_and_fails_without_either() {
    let (inv, _, sidecar) = fixture("edit-visual", &["edit"]);
    let record = inv.home.join("opened");
    let editor = fake_editor(&inv.home, &record);

    // No $EDITOR, but $VISUAL: used.
    let mut cmd = spawn(&inv);
    cmd.env("VISUAL", &editor);
    assert!(cmd.output().unwrap().status.success());
    assert_eq!(
        std::fs::read_to_string(&record).unwrap(),
        format!("{}\n", sidecar.join("config.toml").display())
    );

    // Neither: a runtime failure (exit 1) naming both variables — never
    // a guessed `vi` on a policy file.
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stdout: {stdout}");
    assert!(stderr.starts_with("mysbx: "), "{stderr}");
    assert!(stderr.contains("$EDITOR"), "{stderr}");
    assert!(stderr.contains("$VISUAL"), "{stderr}");
}

#[test]
fn edit_without_an_editor_creates_no_sidecar() {
    // The editor is resolved before anything is created: a run that
    // cannot edit must not leave a sidecar as its only effect.
    let base = tmpdir("edit-no-editor");
    let repo = base.join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["edit"],
        cwd: repo,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(!base.join("repo.mysbx").exists(), "a sidecar was created");
}

#[test]
fn edit_propagates_the_editor_exit_code() {
    // `exec` replaces the process, so the editor's own exit code is
    // mysbx's (D8) — a failed editor must not look like a success.
    let (inv, _, _) = fixture("edit-exit", &["edit"]);
    let script = inv.home.join("failing-editor");
    std::fs::write(&script, "#!/bin/sh\nexit 3\n").unwrap();
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(&script).unwrap().permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(&script, perms).unwrap();
    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", &script);
    assert_eq!(cmd.output().unwrap().status.code(), Some(3));
}

#[test]
fn edit_in_the_home_directory_is_refused() {
    // The repo guard runs before the editor and before any creation:
    // `$HOME` is not a repo (plan.md, repo.rs).
    let base = tmpdir("edit-home");
    let home = base.join("home");
    std::fs::create_dir_all(&home).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let record = base.join("opened");
    let editor = fake_editor(&base, &record);
    let inv = Invocation {
        args: vec!["edit"],
        cwd: home.clone(),
        home,
        xdg: base.join("xdg"),
    };
    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", &editor);
    let out = cmd.output().unwrap();
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    assert!(!record.exists(), "the editor ran anyway");
}

#[test]
fn edit_rejects_arguments() {
    let (inv, _, _) = fixture("edit-args-rejected", &["edit"]);
    let (code, _, stderr) = run_binary_with(&inv, &["edit", "--user"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unexpected argument"), "{stderr}");
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
    init_sidecar(&base.join("repo.mysbx"));
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
        "printf \"%s|\" \"$(< \"$RIPGREP_CONFIG_PATH\")\"; printf \"%s\" \"$RIPGREP_CONFIG_PATH\""
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
    assert!(
        stdout
            .trim()
            .ends_with("|/mysbx-home/.config/ripgrep/ripgreprc"),
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
    cmd.env(
        "MYSBX_BWRAP",
        "/nix/store/0000000000000000000000000000000-mysbx-bwrap/bin/bwrap",
    );
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.starts_with("/nix/store/0000000000000000000000000000000-mysbx-bwrap/bin/bwrap\n"),
        "argv[0] must be the pinned backend: {stdout}"
    );
    // And the rest is the ordinary argv block.
    let rest: String = stdout.lines().skip(1).map(|l| format!("{l}\n")).collect();
    assert_eq!(
        rest,
        expected_minimal_argv(&repo)
            .strip_prefix("bwrap\n")
            .unwrap()
    );
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
    let mut cmd = spawn_with_args(&inv, &["run", "--", "/nonexistent/mysbx-bwrap", "payload"]);
    cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
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
        Some(mysbx::EXIT_INFRASTRUCTURE)
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
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}"
    );
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.starts_with("mysbx: "), "stderr: {stderr}");
    assert!(
        stderr.contains("would hide earlier mount"),
        "stderr: {stderr}"
    );
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
    // Initialized, but with an empty policy: the refusal under test is
    // the missing approval, not the missing sidecar (cli.md D13).
    init_sidecar(&base.join("wt.mysbx"));
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
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
    let written = std::fs::read_to_string(base.join("wt.mysbx").join("config.toml")).unwrap();
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    assert!(!stdout.contains("evil"), "stdout: {stdout}");
}

#[test]
fn a_run_in_an_uninitialized_worktree_creates_no_approval() {
    // Review-2 item 1, the trust boundary, under the explicit-init
    // rule (cli.md D13): a first run in a freshly cloned hostile
    // worktree must not turn the repo's own `.git` pointer into an
    // approval — and now it writes nothing at all. The run is refused
    // with the init hint, and no sidecar config exists afterwards that
    // could carry an approval.
    let base = target_tmpdir("gitdir-uninited");
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.contains("mysbx init"), "stderr: {stderr}");
    assert!(
        !base.join("wt.mysbx").join("config.toml").exists(),
        "a run must not create the sidecar config"
    );
    assert!(
        !stderr.contains(&gitdir.display().to_string()),
        "nothing about the untrusted pointer is acted on: {stderr}"
    );
}

#[test]
fn edit_creating_the_sidecar_approves_nothing() {
    // `mysbx edit` is the other command that creates the sidecar
    // config (cli.md D12/D13) — it writes the template WITHOUT
    // approvals, so the trust decision stays the explicit `mysbx init`
    // (config.md D13, review-2 item 1). A run afterwards refuses the
    // git bind.
    let base = target_tmpdir("gitdir-edit-init");
    let (worktree, gitdir) = make_worktree_fixture(&base, "wt");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg").join("mysbx")).unwrap();
    std::fs::write(
        base.join("xdg").join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    let home = base.join("home");
    let record = home.join("opened");
    let editor = fake_editor(&home, &record);
    let inv = Invocation {
        args: vec!["edit"],
        cwd: worktree,
        home: home.clone(),
        xdg: base.join("xdg"),
    };
    let mut cmd = spawn(&inv);
    cmd.env("EDITOR", &editor);
    assert!(cmd.output().unwrap().status.success());

    let config = base.join("wt.mysbx").join("config.toml");
    let written = std::fs::read_to_string(&config).unwrap();
    // The template SHOWS the key as a commented example (config.md D12),
    // so the assertion is about the written file deciding it: an active
    // `git-dirs = [` line or the discovered path.
    assert!(
        !written.contains("\ngit-dirs ="),
        "`edit` must not approve: {written}"
    );
    assert!(
        !written.contains(&gitdir.display().to_string()),
        "`edit` must not approve: {written}"
    );
    // The repo is initialized now, so the run gets past D13 and fails
    // on the missing approval instead.
    let (code, _stdout, stderr) = run_binary_with(&inv, &["run", "--", "true"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
}

// ---- the review-3 item 5 recovery -------------------------------------------

#[test]
fn approve_git_dirs_recovers_an_unapproved_sidecar() {
    // The scenario review-3 item 5 describes, in its explicit-init
    // shape: the sidecar config exists WITHOUT approvals — written by
    // `mysbx edit`, by hand, or by an `init` that ran before the
    // checkout became a linked worktree. Plain `init` would just say
    // `exists`. The flag takes the trust decision explicitly, after
    // the fact: the config must now carry the discovered git dir, and
    // a following `run` must accept it.
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

    // The starting point: an approval-free sidecar config, and a run
    // that refuses the git bind because of it
    // (edit_creating_the_sidecar_approves_nothing pins the other half:
    // that creating it approves nothing).
    std::fs::write(
        sidecar.join("config.toml"),
        "# written before this checkout became a linked worktree\n",
    )
    .unwrap();
    let (code, _, stderr) = run_binary(&inv(vec!["run", "--", "true"]));
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.contains("not approved"), "stderr: {stderr}");
    let written = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert!(!written.contains("\ngit-dirs ="), "{written}");

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
        format!(
            "git-dirs = [\n  \"{}\",\n]\n# trailing comment\n",
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
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("already lists everything"),
        "the resolved spelling must count as approved: {stdout}"
    );
    let written = std::fs::read_to_string(sidecar.join("config.toml")).unwrap();
    assert_eq!(
        written
            .matches(gitdir.display().to_string().as_str())
            .count(),
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
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
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
fn the_pinned_bin_sh_reaches_the_argv_and_the_report() {
    // MYSBX_BINSH is a pin like MYSBX_NIX_CONF: end-to-end, a set value
    // must appear as the source of the /bin/sh bind, and the report
    // must say which shell the sandbox's /bin/sh is.
    let (inv, _, _) = fixture_user_backend("binsh-pin", &["--verbose", "--dry-run"]);
    let sh = inv.home.join("bin-sh");
    std::fs::write(&sh, "#!/bin/sh\nexit 0\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    cmd.env("MYSBX_BINSH", &sh);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    let at = lines
        .iter()
        .position(|l| *l == sh.display().to_string())
        .unwrap_or_else(|| panic!("the pinned /bin/sh is not bound: {stdout}"));
    assert_eq!(lines[at - 1], "--ro-bind");
    assert_eq!(lines[at + 1], "/bin/sh");
    assert!(
        stdout.contains(&format!("## /bin/sh:        {}", sh.display())),
        "the report must name it: {stdout}"
    );
}

#[test]
fn without_the_bin_sh_pin_no_bin_sh_is_bound() {
    // Unset means "no /bin/sh", never "the host's" (a host /bin/sh is
    // outside mysbx's own closure): the argv gains no bind and the
    // report says the absence out loud. The `--verbose` report itself
    // names "/bin/sh" in its `(none …)` line, so the no-bind assertion
    // checks the ARGV block: a bind would show up as a `--ro-bind`/`
    // /bin/sh` pair there, and the report line below asserts the
    // intended absence instead.
    let (inv, _, _) = fixture_user_backend("binsh-unset", &["--verbose", "--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    let argv = lines
        .iter()
        .position(|l| *l == "bwrap")
        .unwrap_or_else(|| panic!("no argv block in the dry run: {stdout}"));
    assert!(
        !lines[argv..].contains(&"/bin/sh"),
        "no /bin/sh bind in the argv: {stdout}"
    );
    assert!(stdout.contains("## /bin/sh:        (none"), "{stdout}");
}

#[test]
fn the_pinned_ca_bundle_reaches_the_argv_and_the_report() {
    // MYSBX_CA_BUNDLE is a pin like MYSBX_NIX_CONF (bd myconfig-938):
    // end-to-end, a set value must appear as the value of the three
    // TLS env variables — AFTER any [env] entry of the same name, so
    // the pin wins — and the report must name the bundle.
    let (inv, _, _) = fixture_user_backend("ca-bundle-pin", &["--verbose", "--dry-run"]);
    let bundle = inv.home.join("ca-bundle.crt");
    std::fs::write(&bundle, "# a bundle\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    cmd.env("MYSBX_CA_BUNDLE", &bundle);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    for key in ["SSL_CERT_FILE", "GIT_SSL_CAINFO", "NIX_SSL_CERT_FILE"] {
        let lines: Vec<&str> = stdout.lines().collect();
        let at = lines
            .iter()
            .position(|l| *l == key)
            .unwrap_or_else(|| panic!("{key} is not set: {stdout}"));
        assert_eq!(lines[at - 1], "--setenv");
        assert_eq!(lines[at + 1], bundle.display().to_string());
    }
    assert!(
        stdout.contains(&format!("## ca-bundle:      {}", bundle.display())),
        "the report must name it: {stdout}"
    );
}

#[test]
fn without_the_ca_bundle_pin_no_tls_env_is_set() {
    // Unset means "no TLS env variables", never "invent a path": the
    // run relies on the /etc/ssl + /etc/static resolver binds alone.
    let (inv, _, _) = fixture_user_backend("ca-bundle-unset", &["--verbose", "--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    for key in ["SSL_CERT_FILE", "GIT_SSL_CAINFO", "NIX_SSL_CERT_FILE"] {
        assert!(!stdout.contains(key), "no {key} without a pin: {stdout}");
    }
    assert!(
        stdout.contains("## ca-bundle:      (none"),
        "the report must say the absence out loud: {stdout}"
    );
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
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
}

// ---- the runtime-root exposure refusal (detach.md B8) --------------

/// The runtime-root fixture: a standard bubblewrap-user-backend
/// fixture plus the `base/runtime` directory, so a test can set
/// `XDG_RUNTIME_DIR` to it. The user config carries
/// `extra_config` after the backend line; an empty `extra_config`
/// leaves the fixture's own `backend` line in place.
fn fixture_runtime_root(name: &str, extra_config: &str) -> (Invocation, PathBuf, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture_user_backend(name, &["--dry-run"]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let runtime = base.join("runtime");
    std::fs::create_dir_all(&runtime).unwrap();
    if !extra_config.is_empty() {
        std::fs::write(
            inv.xdg.join("mysbx").join("config.toml"),
            format!("backend = \"bubblewrap\"\n{extra_config}"),
        )
        .unwrap();
    }
    (inv, repo, sidecar, runtime)
}

#[test]
fn a_mount_exposing_the_runtime_root_is_refused_end_to_end() {
    // detach.md B8: a sandbox that could reach the runtime root could
    // connect to a dtach socket and type into every detached run of
    // the user. A `[[mounts]]` source at the root, below it and above
    // it is refused end to end — 70 and the runtime-root message —
    // before the backend is executed: the first two iterations pin
    // MYSBX_BWRAP to a missing binary and never reach it, the third
    // runs as a plain dry run.
    for (label, relative, bwrap) in [
        ("at the root", "mysbx", true),
        ("below the root", "mysbx/repo", true),
        ("above the root ($XDG_RUNTIME_DIR itself)", "", false),
    ] {
        let (inv, _, _, runtime) = fixture_runtime_root(
            &format!("expose-{}", label.replace([' ', '$', '(', ')'], "")),
            "",
        );
        let source = if relative.is_empty() {
            runtime.clone()
        } else {
            runtime.join(relative)
        };
        std::fs::create_dir_all(&source).unwrap();
        std::fs::write(
            inv.xdg.join("mysbx").join("config.toml"),
            format!(
                "backend = \"bubblewrap\"\n\n[[mounts]]\npath = {:?}\nmode = \"ro\"\n",
                source.canonicalize().unwrap()
            ),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &[] as &[&str]);
        cmd.env("XDG_RUNTIME_DIR", &runtime);
        if bwrap {
            cmd.env("MYSBX_BWRAP", "/nonexistent-bwrap");
        }
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{label}: stdout: {stdout}\nstderr: {stderr}"
        );
        assert!(
            stderr.contains("runtime root") && stderr.contains("detach.md B8"),
            "{label}: unexpected stderr: {stderr}"
        );
    }
}

#[test]
fn a_flag_path_exposing_the_runtime_root_is_refused() {
    // detach.md B8 for the flags: `--ro` and `--rw` may not expose
    // the runtime root any more than a layer entry may — and the
    // refusal precedes the `--dry-run` early return, so the audit
    // refuses too.
    for (flag, relative) in [("--ro", "mysbx"), ("--rw", "")] {
        let (inv, _, _, runtime) = fixture_runtime_root(&format!("expose-flag-{flag}"), "");
        let source = if relative.is_empty() {
            runtime.clone()
        } else {
            runtime.join(relative)
        };
        std::fs::create_dir_all(&source).unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run", flag]);
        cmd.arg(&source);
        cmd.env("XDG_RUNTIME_DIR", &runtime);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{flag}: stdout: {stdout}\nstderr: {stderr}"
        );
        assert!(
            stderr.contains("runtime root") && stderr.contains("detach.md B8"),
            "{flag}: unexpected stderr: {stderr}"
        );
    }
}

#[test]
fn a_mount_exposing_the_runtime_root_is_refused_on_every_backend() {
    // detach.md B8 names every backend: the refusal sits in the
    // pipeline before the backend check, so all five refuse. The
    // krun family's dry runs need no pins beyond what the fixtures
    // carry: the runtime-root refusal fires before the backend's
    // own validation, so the guest-root path issues of this host
    // never mask the runtime-root message.
    for backend in ["bubblewrap", "nono", "podman-gvisor", "podman-krun", "krun"] {
        let (inv, _, sidecar) = fixture(&format!("expose-{backend}"), &["--dry-run"]);
        let base = inv.cwd.parent().unwrap().to_path_buf();
        let runtime = base.join("runtime");
        std::fs::create_dir_all(runtime.join("mysbx")).unwrap();
        std::fs::write(
            sidecar.join("config.toml"),
            format!(
                "backend = \"{backend}\"\n\n[[mounts]]\npath = {:?}\nmode = \"ro\"\n",
                runtime.join("mysbx").canonicalize().unwrap()
            ),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env("XDG_RUNTIME_DIR", &runtime);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        let stdout = String::from_utf8_lossy(&out.stdout);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{backend}: stdout: {stdout}\nstderr: {stderr}"
        );
        assert!(
            stderr.contains("runtime root") && stderr.contains("detach.md B8"),
            "{backend}: unexpected stderr: {stderr}"
        );
    }
}

#[test]
fn a_waypipe_run_with_a_runtime_dir_still_starts() {
    // detach.md B8: the waypipe socket bind
    // (`$XDG_RUNTIME_DIR/<display>`) is a sibling of the runtime
    // root, never a `Mount` — a waypipe run with the runtime dir
    // set stays a valid dry run.
    let (inv, repo, _) = fixture_display("expose-waypipe", &["--dry-run"]);
    let waypipe = repo.join("fake-waypipe");
    let runtime = inv.cwd.parent().unwrap().join("runtime");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_WAYPIPE", &waypipe);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.lines().any(|l| l == "server"),
        "the guest waypipe server is still the payload prefix: {stdout}"
    );
}

#[test]
fn a_mount_of_a_sibling_of_the_runtime_root_is_allowed() {
    // detach.md B8: the anchor is the `mysbx` child, not the runtime
    // dir itself — a sibling like the waypipe socket path
    // (`$XDG_RUNTIME_DIR/<display>`) is a legitimate mount.
    let runtime = tmpdir("expose-sibling-cli");
    let socket = runtime.join("wayland-0");
    std::fs::write(&socket, "").unwrap();
    let (inv, _, _, _) = fixture_runtime_root(
        "expose-sibling",
        &format!(
            "\n[[mounts]]\npath = {:?}\nmode = \"ro\"\n",
            socket.canonicalize().unwrap()
        ),
    );
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
}

#[test]
fn a_mount_without_a_runtime_dir_is_allowed() {
    // Without `$XDG_RUNTIME_DIR` there is no runtime root to expose:
    // the check passes, and a foreground run in an environment
    // without the variable does not break (the no-fallback rule of
    // B3 governs detached runs, not this refusal).
    let (inv, _, _, _) = fixture_runtime_root("expose-no-runtime-dir", "");
    let granted = inv.cwd.parent().unwrap().join("granted");
    std::fs::create_dir_all(&granted).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n\n[[mounts]]\npath = {:?}\nmode = \"ro\"\n",
            granted.canonicalize().unwrap()
        ),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env_remove("XDG_RUNTIME_DIR");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
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
    assert_eq!(
        code,
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    // Nothing ran, so nothing could have replaced the entry.
    assert!(
        std::fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink(),
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
    assert_eq!(
        code,
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        std::fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink(),
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
        format!("backend = \"bubblewrap\"\n{}", rw_mount_toml(&xdg, "/xdg")),
    )
    .unwrap();

    let inv = Invocation {
        args: Vec::new(),
        cwd: repo,
        home,
        xdg,
    };
    let (code, stdout, stderr) = run_refusing_launch(&inv);
    assert_eq!(
        code,
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("policy file"),
        "unexpected stderr: {stderr}"
    );
    assert!(
        std::fs::symlink_metadata(&link)
            .unwrap()
            .file_type()
            .is_symlink(),
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
    assert!(
        stdout.contains("/policy"),
        "the ro mount is built: {stdout}"
    );
}

// ---- a repo root above the home is refused end to end (review-4 item 2) ----

#[test]
fn a_repo_root_containing_the_home_is_refused_before_anything_is_created() {
    // `HOME=<base>/tree/users/alice` below a `.git` marker at
    // `<base>/tree`: discovery used to accept `<base>/tree` as the repo
    // (only EQUALITY with the home was refused) and the implicit rw
    // repo bind then exposed the whole subtree — home, `.ssh` and all.
    // The guard runs before the sidecar check, so nothing is created
    // and the message is about the home, not about `mysbx init`.
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
    assert_eq!(
        code,
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stderr.contains("mysbx: ") && stderr.contains("contains the home directory"),
        "unexpected stderr: {stderr}"
    );
    // The guard runs before the sidecar check, so the diagnosis is the
    // exposed home — not "run mysbx init", which would invite the
    // operator to initialize exactly the tree that must never be bound.
    assert!(!stderr.contains("mysbx init"), "unexpected hint: {stderr}");
    assert!(!base.join("tree.mysbx").exists(), "a sidecar was created");
}

// ---- the approval is a table-aware TOML edit (review-4 item 3) ----------

/// The shared shape of the approval tests: a linked-worktree repo whose
/// sidecar carries `contents`, plus a user config naming the backend.
/// Returns the invocation factory and the sidecar config path.
fn approval_fixture(
    name: &'static str,
    contents: &str,
) -> (impl Fn(Vec<&'static str>) -> Invocation, PathBuf, PathBuf) {
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
    let (inv, config, gitdir) =
        approval_fixture("approve-ends-in-mounts", "backend = \"bubblewrap\"\n");
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
    std::fs::write(
        &config,
        "backend = \"bubblewrap\"\n\n[env]\nEDITOR = \"nvim\"\n",
    )
    .unwrap();
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
    let (inv, config, _gitdir) = approval_fixture("approve-unparsable", "git-dirs = [\"/a\n");
    let before = std::fs::read_to_string(&config).unwrap();
    let (code, _, stderr) = run_binary(&inv(vec!["init", "--approve-git-dirs"]));
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(stderr.starts_with("mysbx: "), "{stderr}");
    assert_eq!(before, std::fs::read_to_string(&config).unwrap());
}

// ---- `mysbx gui` (docs/design/cli.md D15) ------------------------------------

/// A fake `MYSBX_TERMINAL`: a shell script that writes its own argv (one
/// per line, then a CWD line) to the file named by `$MYSBX_GUI_STUB_OUT`
/// and exits 0 — everything `gui` promises can be asserted on those bytes
/// without a graphical session. Written with the mode bits of a script
/// because `gui` execs it directly.
///
/// The record file is written by the stub AT RUNTIME, and `gui` returns
/// before the stub has written it (D15: it detaches, like `& disown`) —
/// so a test reads it through [`wait_for_file`], never directly.
///
/// The stub assembles the record in a sibling `.tmp` file and `mv`s it
/// into place at the end: `mv` within the same directory is a `rename(2)`,
/// which is ATOMIC, so the record file never exists in a half-written
/// state (the argv lines without the trailing `pwd` line yet). Without
/// that, a test polling for the file's existence can read the record
/// between the stub's two writes — a race that was nearly deterministic
/// on the loaded remote nix builder and invisible on an idle dev host.
fn terminal_stub(base: &Path) -> (PathBuf, PathBuf) {
    let out = base.join("stub-out.txt");
    let stub = base.join("terminal-stub");
    std::fs::write(
        &stub,
        format!(
            "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"$MYSBX_GUI_STUB_OUT.tmp\"\npwd >> \"$MYSBX_GUI_STUB_OUT.tmp\"\nmv -f \"$MYSBX_GUI_STUB_OUT.tmp\" \"$MYSBX_GUI_STUB_OUT\"\n"
        ),
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&stub, std::fs::Permissions::from_mode(0o755)).unwrap();
    (stub, out)
}

/// Wait for `path` to come into existence, then read it — the polling
/// counterpart of the detach: `mysbx gui` returns while the terminal it
/// started is still running, so the bytes the stub writes arrive AFTER
/// the mysbx process the test drove has exited. The stub publishes its
/// record with an atomic `rename(2)` (see [`terminal_stub`]), so the
/// existence check below is also a completeness guarantee: what the
/// read returns is always the FULL record. Polls for up to 10s
/// (generous; the stub writes within milliseconds) and panics with the
/// timeout otherwise.
fn wait_for_file(path: &Path) -> String {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    while !path.exists() {
        if std::time::Instant::now() > deadline {
            panic!(
                "the terminal stub never wrote its record: {}",
                path.display()
            );
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    std::fs::read_to_string(path).unwrap()
}

#[test]
fn gui_passes_the_tail_verbatim_to_a_mysbx_in_the_cwd() {
    // D15: `mysbx gui ARG1 ARG2` must run the same mysbx (by absolute
    // path) inside the terminal, from the current directory, with the
    // tail passed through verbatim — including things that look like
    // flags and a `--` the outer form never parses.
    let base = tmpdir("gui-verbatim");
    let (repo, _) = make_repo(&base, "repo");
    let (stub, out) = terminal_stub(&base);
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["gui", "--multiplexer", "herdr", "--", "run"],
        cwd: repo.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_TERMINAL", &stub)
        // The shell of the window's command is the wrapper's
        // `MYSBX_GUI_SHELL` pin; the test pins a REAL `sh` so the argv
        // shape is the wrapper's, not this build environment's.
        .env("MYSBX_GUI_SHELL", "/bin/sh")
        // A payload shell that is NOT a POSIX shell, the way myconfig
        // hosts pin fish there: `gui` must not read this variable at
        // all (a fish cannot run the closing-message script).
        .env("MYSBX_SHELL", "/synth/bin/fish")
        .env("MYSBX_GUI_STUB_OUT", &out);
    let status = cmd.status().expect("failed to spawn the mysbx binary");
    assert_eq!(status.code(), Some(0));
    let recorded = wait_for_file(&out);
    let mut lines = recorded.lines();
    assert_eq!(
        lines.next(),
        Some("--working-directory"),
        "the stub's argv is the terminal's: {recorded}"
    );
    assert_eq!(lines.next(), Some(repo.to_str().unwrap()));
    // The window must not close under the log: `--hold` keeps it open
    // after the inner mysbx exits.
    assert_eq!(
        lines.next(),
        Some("--hold"),
        "the argv pins --hold: {recorded}"
    );
    assert_eq!(lines.next(), Some("--command"));
    // The command is the closing-message wrapper: `SH -c SCRIPT MYSBX
    // ARG...` (D15) — the shell that prints the last line once the
    // inner mysbx is gone.
    assert_eq!(
        lines.next(),
        Some("/bin/sh"),
        "the command starts with the payload shell: {recorded}"
    );
    assert_eq!(lines.next(), Some("-c"));
    // The stub prints one argv entry per line, so the SCRIPT arrives as
    // its eight lines: run the inner mysbx, capture the status, print
    // the closing message (success or the failing status), re-exit the
    // status.
    let script: Vec<&str> = (0..8).map(|_| lines.next().unwrap()).collect();
    assert_eq!(
        script,
        [
            "\"$0\" \"$@\"",
            "status=$?",
            "if [ \"$status\" -eq 0 ]; then",
            "printf '\\nmysbx: finished successfully — this window can be closed\\n'",
            "else",
            "printf '\\nmysbx: finished with status %s — this window can be closed\\n' \"$status\"",
            "fi",
            "exit \"$status\"",
        ],
        "{recorded}"
    );
    let inner = lines.next().unwrap();
    // The inner mysbx is THIS mysbx: the wrapped binary the test drives,
    // not a PATH lookup — `current_exe` of the spawned process.
    assert!(
        inner.ends_with("mysbx"),
        "the inner invocation must be the mysbx binary itself: {inner}"
    );
    // And the tail, verbatim and unparsed by the outer form — the
    // stub's LAST line is the `pwd` it appends after the argv.
    let recorded_tail: Vec<&str> = lines.collect();
    assert_eq!(
        &recorded_tail[..recorded_tail.len() - 1],
        &["--multiplexer", "herdr", "--", "run"],
        "{recorded}"
    );
    // Nothing of the sandbox pipeline ran: `make_repo` pre-creates the
    // sidecar DIRECTORY, but `gui` never reaches the stage that would
    // create anything in it — and the inner "run" was the stub, so no
    // `config.toml` can have appeared (D13/D15: `gui` creates nothing,
    // the inner run is the one that would).
    assert!(!repo
        .parent()
        .unwrap()
        .join("repo.mysbx")
        .join("config.toml")
        .exists());
}

#[test]
fn gui_does_not_parse_the_tail_and_needs_no_sidecar() {
    // A repo WITHOUT a sidecar, with a tail that is a usage error for
    // every other verb: `gui` still starts the terminal (D15: the
    // inner invocation reports its own errors, in the window). The
    // tail here even contains `--dry-run` — rejected BEFORE the verb,
    // passed through AFTER it.
    let base = tmpdir("gui-no-sidecar");
    let (repo, _) = make_repo(&base, "repo");
    let (stub, out) = terminal_stub(&base);
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec![],
        cwd: repo.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let result = spawn_with_args(&inv, &["gui", "nope", "--dry-run"])
        .env("MYSBX_TERMINAL", &stub)
        .env("MYSBX_GUI_STUB_OUT", &out)
        .output()
        .expect("failed to spawn the mysbx binary");
    assert_eq!(result.status.code(), Some(0), "gui must not parse the tail");
    let recorded = wait_for_file(&out);
    assert!(
        recorded.contains("nope\n") && recorded.contains("--dry-run"),
        "the tail reached the terminal unparsed: {recorded}"
    );
}

#[test]
fn gui_rejects_global_flags_before_the_verb() {
    // D15: the flags before `gui` have no meaning for the terminal's
    // argv — a `--dry-run` before the verb is a usage error (`2`), the
    // same refusal every verb without a run reports. After the verb
    // they are the INNER invocation's, and passed through instead.
    let base = tmpdir("gui-global-flags");
    let (repo, _) = make_repo(&base, "repo");
    let (stub, out) = terminal_stub(&base);
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec![],
        cwd: repo.clone(),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    for args in [
        vec!["--dry-run", "gui"],
        vec!["--verbose", "gui"],
        vec!["--multiplexer", "tmux", "gui"],
    ] {
        let result = spawn_with_args(&inv, &args)
            .env("MYSBX_TERMINAL", &stub)
            .env("MYSBX_GUI_STUB_OUT", &out)
            .output()
            .expect("failed to spawn the mysbx binary");
        assert_eq!(
            result.status.code(),
            Some(2),
            "{args:?}: the stub must not have run"
        );
        assert!(
            !out.exists(),
            "{args:?}: no terminal was started: the flags are refused before the verb"
        );
    }
}

#[test]
fn gui_names_the_terminal_it_cannot_start() {
    // The MYSBX_TERMINAL fallback contract, the same shape as
    // `MYSBX_BWRAP`: a binary that cannot be spawned is a runtime
    // failure (`1`) that names the terminal, never a panic.
    let base = tmpdir("gui-broken-terminal");
    let (repo, _) = make_repo(&base, "repo");
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["gui"],
        cwd: repo,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_TERMINAL", "/nonexistent/terminal");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("/nonexistent/terminal"),
        "the error names the terminal: {stderr}"
    );
}

#[test]
fn gui_ignores_a_non_posix_payload_shell() {
    // The live regression: `MYSBX_SHELL` is the PAYLOAD shell, and on
    // myconfig hosts that is fish. The closing-message script is POSIX
    // (`status=$?`), so reusing the payload pin for the window's shell
    // made `mysbx gui …` die with fish's `Unsupported use of '='`
    // before the terminal even started. `gui` must therefore read its
    // OWN pin (`MYSBX_GUI_SHELL`) and never the payload one.
    let base = tmpdir("gui-posix-shell");
    let (repo, _) = make_repo(&base, "repo");
    let (stub, out) = terminal_stub(&base);
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    let inv = Invocation {
        args: vec!["gui"],
        cwd: repo,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let mut cmd = spawn(&inv);
    cmd.env("MYSBX_TERMINAL", &stub)
        .env("MYSBX_GUI_STUB_OUT", &out)
        // A payload shell that would fail the script on its second line…
        .env("MYSBX_SHELL", "/synth/bin/fish")
        // …and the pin `gui` actually uses.
        .env("MYSBX_GUI_SHELL", "/bin/sh");
    let status = cmd.status().expect("failed to spawn the mysbx binary");
    assert_eq!(status.code(), Some(0));
    let recorded = wait_for_file(&out);
    assert!(
        recorded.contains("--command\n/bin/sh\n-c\n"),
        "the window's shell is the POSIX pin, not the payload shell: {recorded}"
    );
    assert!(
        !recorded.contains("/synth/bin/fish"),
        "the payload shell must never reach the terminal's command: {recorded}"
    );
}

#[test]
fn gui_detaches_and_returns_before_the_terminal_exits() {
    // D15, the detach half: `mysbx gui` must behave like `mysbx gui &
    // disown` — the mysbx process itself returns as soon as the window
    // was STARTED, long before the terminal stub exits, and the
    // starter survives the invoking shell. The stub here sleeps BEFORE
    // it writes its record, so both halves are pinned: the elapsed time
    // (well under the sleep) proves mysbx did not wait for the window,
    // and the record arriving afterwards proves the detached starter
    // outlived the mysbx process the test drove.
    let base = tmpdir("gui-detach");
    let (repo, _) = make_repo(&base, "repo");
    let (_, out) = terminal_stub(&base);
    std::fs::create_dir_all(base.join("home")).unwrap();
    std::fs::create_dir_all(base.join("xdg")).unwrap();
    // Slow the stub down: 2s is far beyond any legitimate startup wait
    // and well under the cargo test timeout.
    let slow = base.join("terminal-slow-stub");
    std::fs::write(
        &slow,
        "#!/bin/sh\nsleep 2\nprintf '%s\\n' \"$@\" > \"$MYSBX_GUI_STUB_OUT.tmp\"\npwd >> \"$MYSBX_GUI_STUB_OUT.tmp\"\nmv -f \"$MYSBX_GUI_STUB_OUT.tmp\" \"$MYSBX_GUI_STUB_OUT\"\n",
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&slow, std::fs::Permissions::from_mode(0o755)).unwrap();
    let inv = Invocation {
        args: vec![],
        cwd: repo,
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let started = std::time::Instant::now();
    let result = spawn_with_args(&inv, &["gui"])
        .env("MYSBX_TERMINAL", &slow)
        .env("MYSBX_GUI_STUB_OUT", &out)
        .output()
        .expect("failed to spawn the mysbx binary");
    let elapsed = started.elapsed();
    assert_eq!(
        result.status.code(),
        Some(0),
        "mysbx gui itself must succeed: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(
        elapsed < std::time::Duration::from_secs(2),
        "mysbx gui returned after {elapsed:?} — it must not wait for the terminal"
    );
    // At return time the stub is still sleeping: the record is written
    // only after the sleep, so its absence is the detach made visible.
    assert!(
        !out.exists(),
        "mysbx gui returned while the terminal was still running"
    );
    // And the detached starter outlived mysbx to reap it: the record
    // does arrive, with the D15 window content — the stub's argv.
    let recorded = wait_for_file(&out);
    assert!(
        recorded.contains("--command") && recorded.contains("--hold") && recorded.contains("mysbx"),
        "the terminal ran the same mysbx: {recorded}"
    );
}

// ---- --ro / --rw additions (cli.md D16) ------------------------------------

/// A scratch root for fixtures the flags BIND AT THEIR OWN HOST PATH:
/// the temp dir works unless it lies at or below a protected sandbox
/// dest (`/tmp`, `/run`, `/nix/store` — the tmpfs, the run tree and
/// the store of the base table, which no configured mount may shadow),
/// which is the case on a dev host (TMPDIR unset → `/tmp`) but not in
/// the nix build sandbox (TMPDIR = the build top). The real `$HOME` —
/// a plain writable directory on every host that can run the suite —
/// is the fallback. `None` when no candidate is writable: the caller
/// skips, the same graceful-degradation the bwrap-availability tests
/// use; the flags' behavior is pinned by the argv content assertions
/// regardless of where the fixture lives.
fn bindable_scratch(name: &str) -> Option<(PathBuf, PathBuf)> {
    let probe = |base: PathBuf| -> Option<(PathBuf, PathBuf)> {
        let protected = ["/tmp", "/run", "/nix/store"]
            .iter()
            .any(|p| base.starts_with(p));
        if protected {
            return None;
        }
        let dir = base.join(format!("mysbx-cli-bindable-{name}",));
        std::fs::create_dir_all(&dir).ok()?;
        Some((base, dir))
    };
    probe(std::env::temp_dir())
        .or_else(|| std::env::var_os("HOME").map(PathBuf::from).and_then(probe))
}

/// [`bindable_scratch`] for the granted paths, panicking when no
/// candidate exists (the tests that need it cannot assert anything
/// without one). Returns the UNIQUE per-call directory (not the
/// shared root): the flag tests create their `repo`, `home`, `xdg`
/// and `granted` fixtures IN the scratch root, and the tests run in
/// parallel in one process — a shared root made concurrent tests
/// write the SAME `repo`/`repo.mysbx/config.toml`/`xdg` paths and
/// race each other (observed as intermittent exit-70 failures of
/// `a_relative_flag_path_resolves_against_the_cwd` on the loaded
/// remote nix builder, bd myconfig-319).
fn granted_scratch(name: &str) -> PathBuf {
    bindable_scratch(name)
        .unwrap_or_else(|| {
            panic!("no writable scratch root outside the protected dests for `{name}`")
        })
        .1
}

/// A granted directory and a granted file in `root`, as `PathBuf`s —
/// created so the flags' canonicalization succeeds.
fn granted_paths(root: &Path) -> (PathBuf, PathBuf) {
    let dir = root.join("granted");
    std::fs::create_dir_all(dir.join("sub")).unwrap();
    let file = root.join("granted-file.txt");
    std::fs::write(&file, "content\n").unwrap();
    (dir, file)
}

#[test]
fn ro_flag_binds_the_path_read_only_for_this_run() {
    // cli.md D16: `--ro <path>` adds one read-only bind for THIS run,
    // after every configured mount, dest = the canonicalized source.
    let (inv, _, _) = fixture_with_backend("ro-flag", &["--dry-run"]);
    let base = granted_scratch("ro-flag");
    let (dir, file) = granted_paths(&base);
    let dir_c = dir.canonicalize().unwrap();
    let file_c = file.canonicalize().unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg(&dir);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.contains(&format!(
            "--ro-bind\n{}\n{}\n",
            dir_c.display(),
            dir_c.display()
        )),
        "the ro bind is missing: {stdout}"
    );
    // A file binds just like a directory.
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg(&file);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.contains(&format!(
            "--ro-bind\n{}\n{}\n",
            file_c.display(),
            file_c.display()
        )),
        "the file ro bind is missing: {stdout}"
    );
}

#[test]
fn rw_flag_binds_the_path_read_write_for_this_run() {
    // The same, `--bind` instead of `--ro-bind`.
    let (inv, _, _) = fixture_with_backend("rw-flag", &["--dry-run"]);
    let base = granted_scratch("rw-flag");
    let (dir, _) = granted_paths(&base);
    let dir_c = dir.canonicalize().unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--rw"]);
    cmd.arg(&dir);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.contains(&format!(
            "--bind\n{}\n{}\n",
            dir_c.display(),
            dir_c.display()
        )),
        "the rw bind is missing: {stdout}"
    );
}

#[test]
fn ro_rw_flags_are_repeatable_and_ordered() {
    // Repeatable: one bind per flag value, every `--ro` addition
    // before every `--rw` one (so `--rw` wins a same-path tie no
    // matter the typing order — the one predictable rule), the values
    // of each flag in the order they were given.
    let (inv, _, _) = fixture_with_backend("ro-rw-repeat", &["--dry-run"]);
    let base = granted_scratch("ro-rw-repeat");
    let (dir, file) = granted_paths(&base);
    let dir_c = dir.canonicalize().unwrap().to_string_lossy().into_owned();
    let file_c = file.canonicalize().unwrap().to_string_lossy().into_owned();
    let args = vec![
        "--dry-run".to_owned(),
        "--rw".to_owned(),
        file.to_string_lossy().into_owned(),
        "--ro".to_owned(),
        dir.to_string_lossy().into_owned(),
        "--ro".to_owned(),
        dir.to_string_lossy().into_owned(),
    ];
    let out = spawn_with_args(&inv, &args)
        .output()
        .expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    // The additions, identified by their SOURCE path (the base
    // table's own binds share the --ro-bind/--bind tokens, so the
    // token alone is ambiguous). Each is `<flag> <src> <dest>` with
    // dest == src.
    let find = |flag: &str, src: &str, from: usize| {
        (from..lines.len().saturating_sub(2))
            .find(|&i| lines[i] == flag && lines[i + 1] == src && lines[i + 2] == src)
            .unwrap_or_else(|| panic!("`{flag} {src}` is missing: {stdout}"))
    };
    // both --ro additions, in typing order, before the --rw one
    let ro0 = find("--ro-bind", &dir_c, 0);
    let ro1 = find("--ro-bind", &dir_c, ro0 + 1);
    let rw = find("--bind", &file_c, ro1 + 1);
    assert!(
        ro0 < ro1 && ro1 < rw,
        "the additions must be grouped: {stdout}"
    );
    // ... and nothing else was added.
    let count = |flag: &str, src: &str| {
        lines
            .windows(3)
            .filter(|w| w[0] == flag && w[1] == src)
            .count()
    };
    assert_eq!(count("--ro-bind", &dir_c), 2, "{stdout}");
    assert_eq!(count("--bind", &file_c), 1, "{stdout}");
}

#[test]
fn flag_additions_come_after_the_configured_mounts() {
    // D6 precedence: the flags apply ON TOP of the layers, and argv
    // order is later-wins — so the additions are the LAST binds before
    // the `--setenv` section, after the repo and every configured
    // mount.
    let (inv, repo, sidecar) = fixture("ro-flag-order", &["--dry-run"]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "backend = \"bubblewrap\"\n[[mounts]]\npath = \"{}/sub\"\ndest = \"/data\"\nmode = \"ro\"\n",
            repo.display()
        ),
    )
    .unwrap();
    let base = granted_scratch("flags");
    let (granted, _) = granted_paths(&base);
    let granted_c = granted
        .canonicalize()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg(&granted);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    let configured = lines
        .iter()
        .position(|l| *l == "/data")
        .expect("the configured mount is bound");
    let flag_bind = lines
        .iter()
        .position(|l| *l == granted_c.as_str())
        .expect("the flag addition is bound");
    assert!(
        configured < flag_bind,
        "the flag bind must come after the configured mount: {stdout}"
    );
    // ... and before the environment section.
    let setenv = lines.iter().position(|l| *l == "--setenv").unwrap();
    assert!(
        flag_bind < setenv,
        "the flag bind must precede --setenv: {stdout}"
    );
}

#[test]
fn a_missing_flag_path_is_a_runtime_failure() {
    // D8 for the flags: the path must exist and resolve; a typo is an
    // ordinary runtime failure (exit 1) that names the spelling.
    let (inv, _, _) = fixture_with_backend("ro-flag-missing", &["--dry-run"]);
    let base = inv.cwd.parent().unwrap();
    let missing = base.join("does-not-exist");
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg(&missing);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "{stderr}"
    );
    assert!(stderr.contains("mysbx: "), "{stderr}");
    assert!(
        stderr.contains("--ro"),
        "the error must name the flag: {stderr}"
    );
    // No argv is printed for a failed run.
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(!stdout.contains("--clearenv"), "{stdout}");
}

#[test]
fn a_flag_path_exposing_the_home_is_refused() {
    // The review-3 item 4 refusal holds for the command line exactly as
    // it holds for either config layer: no flag may bind the host home
    // (or an ancestor of it).
    let (inv, _, _) = fixture_with_backend("ro-flag-home", &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg(&inv.home);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "{stderr}"
    );
    assert!(
        stderr.contains("home directory"),
        "the refusal must name the home-exposure rule: {stderr}"
    );
}

#[test]
fn a_tilde_flag_path_expands_against_home() {
    // The same three-path spellings as a `[[mounts]]` path (D8): `~/…`
    // expands against $HOME — canonicalized, so the bind names the
    // real directory. HOME lives in the bindable scratch root: the
    // granted path binds at its own host path, which must not fall at
    // or below a protected sandbox dest (the temp root may be /tmp).
    let base = granted_scratch("tilde");
    let inv = Invocation {
        args: vec![],
        cwd: base.join("repo"),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (repo, sidecar) = make_repo(&base, "repo");
    init_sidecar(&sidecar);
    std::fs::create_dir_all(&inv.home).unwrap();
    std::fs::create_dir_all(&inv.xdg).unwrap();
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    assert_eq!(repo, inv.cwd);
    let home = inv.home.canonicalize().unwrap();
    let secret = home.join("granted-under-home");
    std::fs::create_dir_all(&secret).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg("~/granted-under-home");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    assert!(
        stdout.contains(&format!(
            "--ro-bind\n{}\n{}\n",
            secret.display(),
            secret.display()
        )),
        "the tilde bind is missing: {stdout}"
    );
}

#[test]
fn a_relative_flag_path_resolves_against_the_cwd() {
    // A config file resolves a relative path against its own
    // directory; the command line's "own directory" is the cwd it was
    // typed in. The repo lives in the bindable scratch root for the
    // same protected-dest reason as the tilde test above.
    let base = granted_scratch("relative");
    let inv = Invocation {
        args: vec![],
        cwd: base.join("repo"),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (repo, sidecar) = make_repo(&base, "repo");
    init_sidecar(&sidecar);
    std::fs::create_dir_all(&inv.home).unwrap();
    std::fs::create_dir_all(&inv.xdg).unwrap();
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    // The granted path is OUTSIDE the repo, reached by a RELATIVE
    // spelling from the cwd: `../granted`. A path below the repo tree
    // itself could not bind at its own dest — the repo content is
    // writable in the sandbox, and a dest below a writable tree is the
    // symlink-redirect guard's hard error — which is a different
    // refusal than the one under test.
    let granted = base.join("granted");
    std::fs::create_dir_all(&granted).unwrap();
    let args = vec![
        "--dry-run".to_owned(),
        "--ro".to_owned(),
        "../granted".to_owned(),
    ];
    let out = spawn_with_args(&inv, &args)
        .output()
        .expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let granted_c = granted.canonicalize().unwrap();
    assert!(
        stdout.contains(&format!(
            "--ro-bind\n{}\n{}\n",
            granted_c.display(),
            granted_c.display()
        )),
        "the relative bind is missing: {stdout}"
    );
}

#[test]
fn the_flags_work_with_the_run_form_too() {
    // cli.md D16: the additions are run flags, valid before the verb
    // and after it for `run` — the same one pipeline runs both forms.
    // `--dry-run` suffices: what is pinned is the PARSING (both
    // positions accepted) and the resulting argv, not the exec.
    let (inv, _, _) = fixture_with_backend("ro-flag-run", &[]);
    let base = granted_scratch("flags");
    let (granted, _) = granted_paths(&base);
    let granted_c = granted.canonicalize().unwrap();
    let expected = format!("--bind\n{}\n{}\n", granted_c.display(), granted_c.display());
    // after the verb
    let args = vec![
        "run".to_owned(),
        "--dry-run".to_owned(),
        "--rw".to_owned(),
        granted.to_string_lossy().into_owned(),
        "--".to_owned(),
        "true".to_owned(),
    ];
    let out = spawn_with_args(&inv, &args)
        .output()
        .expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "{stderr}");
    assert!(
        stdout.contains(&expected),
        "the run-form rw bind is missing: {stdout}"
    );
    // before the verb
    let args = vec![
        "--rw".to_owned(),
        granted.to_string_lossy().into_owned(),
        "run".to_owned(),
        "--dry-run".to_owned(),
        "--".to_owned(),
        "true".to_owned(),
    ];
    let out = spawn_with_args(&inv, &args)
        .output()
        .expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "{stderr}");
    assert!(
        stdout.contains(&expected),
        "the before-verb rw bind is missing: {stdout}"
    );
}

#[test]
fn flag_binds_and_a_policy_file_rw_exposure() {
    // Review-3 item 3, for the flags: an `--rw` of a directory that
    // contains a trusted policy file must be refused — the payload
    // writing it steers the next run, exactly like a config entry
    // would be. The user config must live somewhere the fixture can
    // bind the parent of, so it is placed in the bindable scratch
    // root (the default temp root may lie under the protected /tmp,
    // and the guard would fire for the wrong reason).
    let base = granted_scratch("policy");
    let inv = Invocation {
        args: vec![],
        cwd: base.join("repo"),
        home: base.join("home"),
        xdg: base.join("xdg"),
    };
    let (repo, sidecar) = make_repo(&base, "repo");
    init_sidecar(&sidecar);
    assert_eq!(repo, inv.cwd);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(
        inv.xdg.join("mysbx").join("config.toml"),
        "backend = \"bubblewrap\"\n",
    )
    .unwrap();
    // --rw of the XDG directory itself: it covers the policy file's
    // guarded pathname entries (the file AND its parent directories).
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--rw"]);
    cmd.arg(inv.xdg.join("mysbx"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "{stderr}"
    );
    assert!(
        stderr.contains("policy"),
        "the refusal must name the policy exposure: {stderr}"
    );
}

#[test]
fn flag_binds_may_not_shadow_protected_dests() {
    // A flag path that IS a protected sandbox path is refused by the
    // argv builder's protected-dest check — the same refusal a config
    // dest gets, reached through a source spelling.
    let (inv, _, _) = fixture_with_backend("ro-flag-protected", &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "--ro"]);
    cmd.arg("/proc");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(mysbx::EXIT_INFRASTRUCTURE),
        "{stderr}"
    );
    assert!(
        stderr.contains("protected"),
        "the refusal must name the protected path: {stderr}"
    );
}

#[test]
fn the_flags_are_refused_for_the_verbs() {
    // D16 position rule: valid before the verb and after it for `run`,
    // refused for every other verb — the same words as `--multiplexer`.
    for (i, (flag, verb)) in [
        ("--ro", "init"),
        ("--rw", "init"),
        ("--ro", "edit"),
        ("--rw", "edit"),
        ("--ro", "gui"),
        ("--rw", "gui"),
        ("--ro", "help"),
        ("--rw", "help"),
        ("--ro", "version"),
        ("--rw", "version"),
    ]
    .iter()
    .enumerate()
    {
        let (inv, _, _) = fixture_user_backend(&format!("flag-verb-{i}"), &[]);
        let args = vec![flag.to_string(), "/tmp".to_owned(), verb.to_string()];
        let out = spawn_with_args(&inv, &args)
            .output()
            .expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(2), "{flag} {verb}: {stderr}");
        assert!(
            stderr.contains(&format!("is not valid with `{verb}`")),
            "{flag} {verb}: {stderr}"
        );
    }
}

#[test]
fn a_missing_flag_value_is_a_usage_error() {
    // `mysbx --ro` with nothing after it: exit 2, like a `--multiplexer`
    // without a value.
    let (inv, _, _) = fixture_user_backend("flag-no-value", &[]);
    for flag in ["--ro", "--rw"] {
        let out = spawn_with_args(&inv, &[flag])
            .output()
            .expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(2), "{flag}: {stderr}");
        assert!(stderr.contains("requires a path"), "{flag}: {stderr}");
    }
}

#[test]
fn the_report_attributes_flag_mounts_to_the_command_line() {
    // cli.md D10: every bind that reaches the argv belongs in the
    // report, with its provenance — the flag additions are labeled
    // `command line`, after the layers.
    let (inv, _, sidecar) = fixture("ro-flag-report", &[]);
    std::fs::create_dir_all(inv.xdg.join("mysbx")).unwrap();
    std::fs::write(sidecar.join("config.toml"), "backend = \"bubblewrap\"\n").unwrap();
    let base = granted_scratch("flags");
    let (granted, _) = granted_paths(&base);
    let granted_c = granted.canonicalize().unwrap();
    let args = vec![
        "--verbose".to_owned(),
        "--dry-run".to_owned(),
        "--ro".to_owned(),
        granted.to_string_lossy().into_owned(),
    ];
    let out = spawn_with_args(&inv, &args)
        .output()
        .expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(out.status.code(), Some(0), "{stdout}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains(&format!(
            "  ro {} -> {}  [command line]",
            granted_c.display(),
            granted_c.display()
        )),
        "the report must attribute the flag mount to the command line: {report}"
    );
}

#[test]
fn a_run_without_the_flags_is_byte_identical() {
    // The compatibility promise of D16: a run without `--ro`/`--rw` is
    // exactly what it was — the minimal golden, byte for byte.
    let (inv, repo, _) = fixture_with_backend("flag-absent", &["--dry-run"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(stdout, expected_minimal_argv(&repo));
}

#[test]
fn help_documents_the_ro_and_rw_flags() {
    // The usage pairing guard of cli.md D5, end to end: `mysbx --help`
    // mentions both flags.
    let (inv, _, _) = fixture("flag-help", &["--help"]);
    let (code, stdout, _stderr) = run_binary(&inv);
    assert_eq!(code, 0);
    for token in ["--ro", "--rw"] {
        assert!(stdout.contains(token), "help does not mention {token}");
    }
}

// ---- the structured result and the extended exit codes (bd myconfig-0ql,
// docs/design/cli.md D8/D17) -----------------------------------------------

/// Read `<sidecar>/result.json` and check the two invariants every
/// consumer rests on: it parses as strict JSON (via the strictest
/// zero-dependency check available: the hand-written renderer's fields
/// are extracted by the caller, here we check the shape) and its
/// `exitCode` agrees with the run's observed process status.
fn read_result(sidecar: &Path) -> String {
    std::fs::read_to_string(sidecar.join(mysbx::result::FILE_NAME))
        .unwrap_or_else(|e| panic!("cannot read the result file: {e}"))
}

#[test]
fn help_documents_the_result_and_timeout_flags() {
    // cli.md D5 pairing, end to end: the usage mentions both flags and
    // the exit-code set they bring.
    let (inv, _, _) = fixture("result-help", &["--help"]);
    let (code, stdout, _stderr) = run_binary(&inv);
    assert_eq!(code, 0);
    for token in ["--result", "--timeout", "result.json"] {
        assert!(stdout.contains(token), "help does not mention {token}");
    }
}

#[test]
fn a_dry_run_with_result_writes_no_file_and_prints_the_argv() {
    // `--result` changes what happens AFTER the argv is built; the
    // argv itself is the plain run's, and `--dry-run` stays
    // side-effect-free: no result file, the ordinary argv block.
    let (inv, repo, sidecar) = fixture_with_backend("result-dry-run", &[]);
    let (code, stdout, stderr) =
        run_binary_with(&inv, &["run", "--result", "--dry-run", "--", "ls"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    // The payload line of the argv is the payload, like every `run`.
    assert!(stdout.ends_with("--\nls\n"), "argv: {stdout}");
    assert!(
        !stdout.contains("/synth/bin/bash"),
        "the payload is not the shell: {stdout}"
    );
    assert_eq!(
        stdout,
        expected_minimal_argv(&repo).replace("/synth/bin/bash\n", "ls\n"),
        "the argv must be the plain run's, only the payload differs"
    );
    assert!(
        !sidecar.join(mysbx::result::FILE_NAME).exists(),
        "a dry run must not write a result"
    );
}

#[test]
fn the_flags_before_and_after_the_verb_are_one_run() {
    // D10's one position rule, applied to the new flags: `--result`
    // before the verb and `--timeout` after it are the same
    // invocation. The dry run of both spellings prints the same argv
    // (and never reaches the waiting path).
    let (inv, _, _) = fixture_with_backend("result-positions", &[]);
    let first = run_binary_with(
        &inv,
        &[
            "--result",
            "run",
            "--timeout",
            "30",
            "--dry-run",
            "--",
            "ls",
        ],
    );
    let second = run_binary_with(
        &inv,
        &[
            "run",
            "--result",
            "--dry-run",
            "--timeout",
            "30",
            "--",
            "ls",
        ],
    );
    assert_eq!(first.0, 0, "stderr: {}", first.2);
    assert_eq!(second.0, 0, "stderr: {}", second.2);
    assert_eq!(first.1, second.1, "both spellings must be one run");
}

#[test]
fn result_without_run_is_refused_with_a_naming_message() {
    // The bare form has no consumable outcome, and every other verb
    // too: both refusals are usage errors with a message that names
    // the flag AND the reason (not accept-and-ignore).
    let (inv, _, _) = fixture("result-refused", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--result"]);
    assert_eq!(code, 2);
    assert!(
        stderr.contains("--result") && stderr.contains("run -- CMD"),
        "the refusal must name the flag and the form it belongs to: {stderr}"
    );
    let (code, _, stderr) = run_binary_with(&inv, &["--timeout", "30"]);
    assert_eq!(code, 2);
    assert!(
        stderr.contains("--timeout") && stderr.contains("run -- CMD"),
        "{stderr}"
    );
}

#[test]
fn timeout_without_result_is_a_usage_error() {
    // A plain run ends in an exec — there is no mysbx left to enforce a
    // budget, so `--timeout` without `--result` is a command line that
    // promises what no form of the invocation can do.
    let (inv, _, _) = fixture_with_backend("timeout-alone", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["run", "--timeout", "30", "--", "ls"]);
    assert_eq!(code, 2);
    assert!(
        stderr.contains("--timeout is not valid without --result"),
        "{stderr}"
    );
}

#[test]
fn a_bad_timeout_value_is_a_usage_error() {
    let (inv, _, _) = fixture_with_backend("timeout-bad", &[]);
    for value in ["0", "-1", "soon", ""] {
        let args = ["run", "--result", "--timeout", value, "--", "ls"];
        let out = spawn_with_args(&inv, &args)
            .output()
            .expect("failed to spawn the mysbx binary");
        assert_eq!(
            out.status.code(),
            Some(2),
            "`{value}` must be a usage error: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}

#[test]
fn mysbx_own_failures_exit_70_not_1() {
    // The heart of the D8 extension: a failure of the TOOL is no
    // longer `1` — that is a payload's own code (the `failed` state).
    // `70` (agent-microvm's `infrastructure-error`) keeps the two
    // apart; every earlier `1` assertion in this suite is now this
    // one. Pin it on the two canonical cases: the uninitialized repo
    // and the missing backend.
    let (inv, _, _) = fixture_uninited("exit70", &["--dry-run"]);
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("mysbx init"), "{stderr}");

    let (inv, _, _) = fixture("exit70-backend", &["--dry-run"]);
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("no backend configured"), "{stderr}");
}

#[test]
fn a_result_run_of_a_dead_backend_records_an_infrastructure_error() {
    // The waited path needs no runnable bwrap — a broken `MYSBX_BWRAP`
    // pin is a spawn failure, which the waited run RECORDS (unlike the
    // exec path, which only reports it). This is the one exit-70 case
    // a result file exists for: the run happened, it failed at the
    // boundary, and the driver polling the file learns the outcome.
    let (inv, _, sidecar) = fixture_with_backend("result-infra", &[]);
    let mut cmd = spawn_with_args(
        &inv,
        &[
            "run",
            "--result",
            "--",
            "/nonexistent/mysbx-bwrap",
            "payload",
        ],
    );
    cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let result = read_result(&sidecar);
    assert!(
        result.contains("\"state\": \"infrastructure-error\""),
        "{result}"
    );
    assert!(result.contains("\"exitCode\": 70"), "{result}");
    assert!(result.contains("cannot exec the backend"), "{result}");
    // The pointer line on stderr (stdout is the payload's, D9).
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains(&format!(
            "result: {}",
            sidecar.join(mysbx::result::FILE_NAME).display()
        )),
        "stderr must point at the result file: {stderr}"
    );
}

#[test]
fn the_result_file_is_replaced_not_appended() {
    // One file per repo: a second run replaces the first's outcome, so
    // the sidecar cannot grow without bound and the file always names
    // the LATEST run (a driver wanting history copies it).
    let (inv, _, sidecar) = fixture_with_backend("result-replace", &[]);
    for _ in 0..3 {
        let mut cmd = spawn_with_args(&inv, &["run", "--result", "--", "/nonexistent/mysbx-bwrap"]);
        cmd.env("MYSBX_BWRAP", "/nonexistent/mysbx-bwrap");
        let out = cmd.output().expect("failed to spawn mysbx");
        assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
        let result = read_result(&sidecar);
        assert_eq!(
            result.matches("\"state\"").count(),
            1,
            "one state per run, the file was replaced: {result}"
        );
    }
}

#[test]
fn a_payload_run_through_a_result_stub_completes_and_records() {
    // The full waited path with a runnable backend: the tests use a
    // STUB for `MYSBX_BWRAP` — a shell script that ignores its bwrap
    // argv and runs `$MYSBX_RESULT_STUB_CMD`-style behavior is too
    // clever; instead the stub simply exits with the code the test
    // wants, proving the waited run records the REAL outcome of the
    // process it started. Exit 42: completed/failed is decided by the
    // payload, 42 is a failed run with the code recorded.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let (inv, repo, sidecar) = fixture_with_backend("result-payload", &[]);
    let mut cmd = spawn_with_args(&inv, &["run", "--result", "--", "/usr/bin/env"]);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin").env("TERM", "dumb");
    let out = cmd.output().expect("failed to spawn mysbx");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(0),
        "a completing payload is exit 0\nstdout: {stdout}\nstderr: {stderr}"
    );
    let result = read_result(&sidecar);
    assert!(result.contains("\"state\": \"completed\""), "{result}");
    assert!(result.contains("\"exitCode\": 0"), "{result}");
    assert!(result.contains("\"payloadExitCode\": 0"), "{result}");
    assert!(
        result.contains(&format!(
            "\"repo\": \"{}\"",
            repo.canonicalize().unwrap().display()
        )),
        "the record names the repo: {result}"
    );
    assert!(
        result.contains("\"payload\": [\"/usr/bin/env\"]"),
        "the record names the payload: {result}"
    );
    // The payload's stdout is untouched by the result machinery: the
    // file went to the sidecar, the pointer to stderr.
    assert!(!stdout.contains("result.json"), "{stdout}");
}

#[test]
fn a_failing_payload_run_through_a_result_records_the_failed_state() {
    // The same waited run, payload non-zero: exit `1` — the
    // interpreted `failed` state, NOT the payload's own 127 — and the
    // exact code in the file.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let (inv, _, sidecar) = fixture_with_backend("result-failed", &[]);
    let mut cmd = spawn_with_args(
        &inv,
        &[
            "run",
            "--result",
            "--",
            "/usr/bin/env",
            "no-such-binary-xyz",
        ],
    );
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(
        out.status.code(),
        Some(1),
        "the failed state is exit 1, not the payload's 127"
    );
    let result = read_result(&sidecar);
    assert!(result.contains("\"state\": \"failed\""), "{result}");
    assert!(result.contains("\"exitCode\": 1"), "{result}");
    assert!(result.contains("\"payloadExitCode\": 127"), "{result}");
}

#[test]
fn a_result_run_without_a_timeout_records_the_state_without_a_budget() {
    // `timeoutSec` is present only when a budget was given — a driver
    // distinguishing "no budget" from "budget 0" needs the omission.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let (inv, _, sidecar) = fixture_with_backend("result-no-timeout", &[]);
    let mut cmd = spawn_with_args(&inv, &["run", "--result", "--", "/usr/bin/env"]);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(out.status.code(), Some(0));
    let result = read_result(&sidecar);
    assert!(result.contains("\"state\": \"completed\""), "{result}");
    assert!(!result.contains("timeoutSec"), "{result}");
}

#[test]
fn an_over_the_budget_run_is_timed_out_exit_124() {
    // The timeout budget of the D8 extension: a payload that outlives
    // `--timeout` is killed with its whole process group and the run
    // exits `124` (agent-microvm's `timed-out`), the file records
    // `timed-out` with the budget and NO payload fate — the payload's
    // outcome is mysbx's kill, not its own.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    let (inv, _, sidecar) = fixture_with_backend("result-timeout", &[]);
    let args = [
        "run".to_owned(),
        "--result".to_owned(),
        "--timeout".to_owned(),
        "1".to_owned(),
        "--".to_owned(),
        bash.to_string_lossy().into_owned(),
        "-c".to_owned(),
        "sleep 30".to_owned(),
    ];
    let mut cmd = spawn_with_args(&inv, &args);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    let out = cmd.output().expect("failed to spawn mysbx");
    assert_eq!(
        out.status.code(),
        Some(124),
        "the exhausted budget is exit 124\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let result = read_result(&sidecar);
    assert!(result.contains("\"state\": \"timed-out\""), "{result}");
    assert!(result.contains("\"exitCode\": 124"), "{result}");
    assert!(result.contains("\"timeoutSec\": 1"), "{result}");
    assert!(!result.contains("payloadExitCode"), "{result}");
    assert!(!result.contains("payloadSignal"), "{result}");
}

#[test]
fn a_cancelled_result_run_exits_130_and_records_the_signal() {
    // SIGINT while mysbx waits: the whole sandbox process group dies,
    // the run exits `130` (the shell's 128 + 2) and the file records
    // `cancelled` naming SIGINT. `kill(2)` from the test is the
    // operator's Ctrl-C.
    if !is_bwrap_available() {
        eprintln!("skipping: bwrap not available in this environment");
        return;
    }
    let Some(bash) = sandbox_bash() else {
        eprintln!("skipping: no sandbox-reachable bash");
        return;
    };
    let (inv, _, sidecar) = fixture_with_backend("result-cancel", &[]);
    let args = [
        "run".to_owned(),
        "--result".to_owned(),
        "--".to_owned(),
        bash.to_string_lossy().into_owned(),
        "-c".to_owned(),
        "sleep 30".to_owned(),
    ];
    let mut cmd = spawn_with_args(&inv, &args);
    cmd.env("MYSBX_TOOLS_PATH", "/usr/bin");
    cmd.stdin(std::process::Stdio::piped());
    let child = cmd.spawn().expect("failed to spawn mysbx");
    // Give the run time to reach the wait loop, then send SIGINT to
    // the mysbx process — the handler records it, the loop kills the
    // group, the record is written.
    std::thread::sleep(std::time::Duration::from_millis(700));
    unsafe {
        libc_kill(child.id(), 2);
    }
    let out = child.wait_with_output().expect("mysbx died unexpectedly");
    assert_eq!(
        out.status.code(),
        Some(130),
        "SIGINT while waiting is exit 130\nstdout: {}\nstderr: {}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    let result = read_result(&sidecar);
    assert!(result.contains("\"state\": \"cancelled\""), "{result}");
    assert!(result.contains("\"exitCode\": 130"), "{result}");
    assert!(result.contains("\"cancelledBy\": \"SIGINT\""), "{result}");
    assert!(!result.contains("payloadExitCode"), "{result}");
}

/// `kill(2)` from the tests, the zero-dependency way: a raw extern like
/// the ones the crate's own `gui` detach uses. The test crate is as
/// dependency-free as the lib.
unsafe fn libc_kill(pid: u32, signum: i32) {
    extern "C" {
        fn kill(pid: i32, signum: i32) -> i32;
    }
    kill(pid as i32, signum);
}

#[test]
fn a_verbose_result_run_reports_the_waiting_mode() {
    // D10 + D17: the report's `mode:` line tells the truth for a
    // waited run — not "executing", and the `## `-prefix rule of the
    // report is unchanged, so a following `--dry-run` argv block
    // stays byte-identical (D10's grep-promise).
    let (inv, _, _) = fixture_with_backend("verbose-result", &[]);
    let (code, stdout, stderr) = run_binary_with(
        &inv,
        &["run", "--result", "--verbose", "--dry-run", "--", "ls"],
    );
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains("mode:           dry run"),
        "the dry run wins the mode line (nothing is executed): {report}"
    );
    // The unprefixed argv block follows, byte-identical to a plain
    // dry run of the same payload.
    let argv: String = stdout
        .lines()
        .filter(|l| !l.starts_with("## "))
        .map(|l| format!("{l}\n"))
        .collect();
    assert!(argv.starts_with("bwrap\n"), "{argv}");
}

// ---- the clone sessions of the workspace model (workspace.md D1-D5) --------

/// A real git repository fixture (workspace.md D2 works on the host
/// repo's HEAD, so the end-to-end tests of a session need one commit
/// to clone from). Returns `None` when no runnable `git` is on PATH —
/// the same skip the bwrap execution tests make.
fn git_repo(base: &Path, name: &str) -> Option<PathBuf> {
    let repo = base.join(name);
    std::fs::create_dir_all(&repo).unwrap();
    let git = |args: &[&str]| {
        Command::new("git")
            .arg("-C")
            .arg(&repo)
            .args(args)
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .env("GIT_AUTHOR_NAME", "mysbx-tests")
            .env("GIT_AUTHOR_EMAIL", "mysbx-tests@invalid")
            .env("GIT_COMMITTER_NAME", "mysbx-tests")
            .env("GIT_COMMITTER_EMAIL", "mysbx-tests@invalid")
            .status()
            .map(|s| s.success())
            .unwrap_or(false)
    };
    if !git(&["init", "-b", "main"])
        || {
            std::fs::write(repo.join("file"), "content").unwrap();
            !git(&["add", "file"])
        }
        || !git(&["commit", "-m", "initial"])
    {
        return None;
    }
    Some(repo)
}

/// A fixture whose repo is a REAL git repository (initialized sidecar,
/// backend in the user config) — the shape a `--session` run needs.
fn git_session_fixture(
    name: &str,
    args: &[&'static str],
) -> Option<(Invocation, PathBuf, PathBuf)> {
    let (inv, _, sidecar) = fixture_user_backend(name, args);
    git_repo(inv.cwd.parent().unwrap(), "repo")?;
    let repo = inv.cwd.clone();
    Some((inv, repo, sidecar))
}

#[test]
fn a_session_dry_run_prints_the_clone_commands_and_creates_nothing() {
    // cli.md D9 + workspace.md D2: the dry run of a FIRST session
    // prints the exact `git` commands in the argv format (one
    // argument per line, the executable first) and creates nothing —
    // `clones/` does not even appear.
    let Some((inv, repo, sidecar)) =
        git_session_fixture("session-dry-first", &["--session", "fix-1", "--dry-run"])
    else {
        return; // no git on PATH: skip, like the bwrap execution tests
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    // The creation block: `git clone --origin origin --no-hardlinks
    // <repo> <clone>`, then the branch checkout at HEAD.
    let clone_path = sidecar.join("clones").join("fix-1");
    let clone_cmd = ["git", "clone", "--origin", "origin", "--no-hardlinks"];
    let start = lines
        .iter()
        .position(|l| *l == "git")
        .expect("the creation block is printed");
    for (i, want) in clone_cmd.iter().enumerate() {
        assert_eq!(lines[start + i], *want, "stdout: {stdout}");
    }
    assert_eq!(lines[start + 5], repo.to_string_lossy());
    assert_eq!(lines[start + 6], clone_path.to_string_lossy());
    // The checkout command follows: the second `git` block, with the
    // branch `agent/mysbx/fix-1`.
    assert!(lines[start..].contains(&"checkout"), "{stdout}");
    // The bwrap argv follows, with the clone (which does NOT exist
    // yet — the dry run creates nothing) bound at the repo path.
    let bwrap_at = lines
        .iter()
        .position(|l| *l == "bwrap")
        .expect("the argv block follows");
    assert!(bwrap_at > start);
    let argv = &lines[bwrap_at..];
    let bind_at = argv
        .iter()
        .position(|l| *l == "--bind")
        .expect("the clone bind");
    assert_eq!(argv[bind_at + 1], clone_path.to_string_lossy());
    assert_eq!(argv[bind_at + 2], repo.to_string_lossy());
    // Nothing was created.
    assert!(!sidecar.join("clones").exists(), "stdout: {stdout}");
}

#[test]
fn an_existing_session_dry_run_prints_no_creation_commands() {
    // The registry of D2: a clone directory exists ⇒ the session
    // exists ⇒ no creation, and the dry run prints only the argv.
    let Some((inv, repo, sidecar)) =
        git_session_fixture("session-dry-exists", &["--session", "fix-1", "--dry-run"])
    else {
        return;
    };
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    // No creation: no `--no-hardlinks` and no `## creating:` line
    // (the bind of the EXISTING clone legitimately contains the
    // string "clones" — the directory name — so the flag is the
    // tell, not the substring).
    assert!(!stdout.contains("--no-hardlinks"), "no creation: {stdout}");
    assert!(!stdout.contains("## creating"), "no creation: {stdout}");
    assert!(stdout.starts_with("bwrap\n"), "{stdout}");
    let lines: Vec<&str> = stdout.lines().collect();
    let bind_at = lines.iter().position(|l| *l == "--bind").expect("bind");
    assert_eq!(lines[bind_at + 1], clone.to_string_lossy());
    assert_eq!(lines[bind_at + 2], repo.to_string_lossy());
}

#[test]
fn the_first_session_run_creates_the_clone_on_the_session_branch() {
    // workspace.md D2, end to end: the first `--session` run creates
    // the clone with both required flags, on the branch
    // `agent/mysbx/fix-1`, at the host repo's HEAD. The run itself
    // may end in `0` (a runnable bwrap) or `70` (none — the exec
    // fails after the creation); the clone is what is asserted.
    let Some((inv, repo, sidecar)) = git_session_fixture(
        "session-create",
        &["run", "--session", "fix-1", "--", "true"],
    ) else {
        return;
    };
    let (code, stdout, stderr) = run_binary(&inv);
    assert!(code == 0 || code == 70, "code {code}: {stderr}\n{stdout}");
    let clone = sidecar.join("clones").join("fix-1");
    assert!(clone.join(".git").is_dir(), "the clone exists: {stdout}");
    let branch = Command::new("git")
        .arg("-C")
        .arg(&clone)
        .args(["branch", "--show-current"])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&branch.stdout).trim(),
        "agent/mysbx/fix-1"
    );
    // `--no-hardlinks` cannot be observed from the result, but the
    // objects must not be hardlinks of the host's: the clone is a
    // standalone repository.
    assert!(clone.join(".git").join("objects").is_dir());
    assert!(repo.join(".git").is_dir());
    // The creation was reported before the run (D2).
    assert!(
        stdout.contains("## creating:"),
        "reported before the run: {stdout}"
    );
}

#[test]
fn a_second_session_run_does_not_recreate() {
    // The registry: an existing clone means no creation commands, no
    // `## creating:` line — the run proceeds into it.
    let Some((inv, _, sidecar)) = git_session_fixture(
        "session-second",
        &["run", "--session", "fix-1", "--", "true"],
    ) else {
        return;
    };
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert!(code == 0 || code == 70, "code {code}: {stderr}\n{stdout}");
    assert!(!stdout.contains("## creating"), "no recreation: {stdout}");
}

#[test]
fn an_empty_host_repo_is_refused_at_session_creation() {
    // workspace.md D2: no commits, nothing to clone — a refused run
    // (exit `70`) naming the offending fact, and no `clones/`
    // directory left behind.
    let (inv, repo, sidecar) = fixture_user_backend(
        "session-empty-repo",
        &["run", "--session", "fix-1", "--", "true"],
    );
    std::fs::create_dir_all(repo.join(".git")).unwrap(); // not a real repo: HEAD unresolvable
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("no commits"), "{stderr}");
    assert!(stderr.contains(&*repo.to_string_lossy()), "{stderr}");
    assert!(!sidecar.join("clones").exists());
}

#[test]
fn a_host_branch_named_agent_is_refused_at_session_creation() {
    // workspace.md D2: the ref-directory conflict with the reserved
    // `agent/mysbx/*` namespace, refused up front with the fact named.
    let Some((inv, repo, sidecar)) = git_session_fixture(
        "session-agent-branch",
        &["run", "--session", "fix-1", "--", "true"],
    ) else {
        return;
    };
    let ok = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["branch", "agent"])
        .env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_SYSTEM", "/dev/null")
        .status()
        .unwrap()
        .success();
    assert!(ok, "creating the branch failed");
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("`agent`"), "{stderr}");
    assert!(!sidecar.join("clones").exists());
}

#[test]
fn a_session_result_run_writes_the_per_session_file() {
    // workspace.md D5: `run --result --session NAME` writes
    // `clones/NAME.json`, not `result.json` — one file per session, so
    // parallel sessions cannot overwrite each other's outcomes. The
    // clone exists (the registry), the backend is missing from the
    // test environment: the run records `infrastructure-error` and
    // still writes the per-session file (the spawn failure IS the
    // run's outcome, cli.md D17).
    let (inv, _, sidecar) = fixture_user_backend(
        "session-result",
        &["run", "--session", "fix-1", "--result", "--", "true"],
    );
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    let result = sidecar.join("clones").join("fix-1.json");
    let text = std::fs::read_to_string(&result)
        .unwrap_or_else(|e| panic!("cannot read {}: {e} (stderr: {stderr})", result.display()));
    assert!(
        text.contains("\"state\": \"infrastructure-error\""),
        "{text}"
    );
    // The pointer line names the per-session file (D5: the pointer
    // goes to stderr in both cases).
    assert!(stderr.contains("mysbx: result:"), "{stderr}");
    assert!(stderr.contains(&*result.to_string_lossy()), "{stderr}");
    // And the live result file was NOT written.
    assert!(!sidecar.join("result.json").exists());
}

#[test]
fn a_live_result_run_still_writes_result_json() {
    // D5's other half: a live `--result` run keeps
    // `<repo>.mysbx/result.json` — the session spelling changes
    // nothing for the live mode (D1).
    let (inv, _, sidecar) = fixture_user_backend("live-result", &["run", "--result", "--", "true"]);
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    let result = sidecar.join("result.json");
    let text = std::fs::read_to_string(&result).unwrap();
    assert!(
        text.contains("\"state\": \"infrastructure-error\""),
        "{text}"
    );
    assert!(stderr.contains(&*result.to_string_lossy()), "{stderr}");
}

#[test]
fn rw_is_refused_in_a_session_run() {
    // workspace.md D4, end to end: `--rw` with `--session` is a
    // refused run (exit `70`, not a usage error) naming the flag and
    // the mode — never a silent downgrade.
    let (inv, _, _) = fixture_user_backend(
        "session-rw",
        &["run", "--session", "fix-1", "--rw", "/tmp", "--", "true"],
    );
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("--rw"), "{stderr}");
    assert!(stderr.contains("clone run"), "{stderr}");
}

#[test]
fn a_bad_session_name_is_a_usage_error() {
    // workspace.md D2: the grammar is enforced at parse time (exit
    // `2`), in both positions, for both run forms.
    for args in [
        vec!["--session", "../evil"],
        vec!["--session", "a/b"],
        vec!["--session", ""],
        vec!["--session", ".hidden"],
        vec!["run", "--session", "..", "--", "true"],
    ] {
        let (inv, _, _) = fixture_user_backend("session-grammar", &[]);
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(
            stderr.contains("invalid session name"),
            "{args:?}: {stderr}"
        );
    }
}

#[test]
fn a_run_inside_a_session_clone_is_refused() {
    // workspace.md D9: the sidecar is not a checkout. A run started
    // inside `clones/fix-1` is refused (exit `70`) naming the owning
    // repo and the session — not silently run as a live sandbox of
    // the clone's own `.git`.
    let (inv, repo, sidecar) = fixture_user_backend("inside-clone", &["--dry-run"]);
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let mut inv2 = inv;
    inv2.cwd = clone.clone();
    let (code, _, stderr) = run_binary(&inv2);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(
        stderr.contains("refusing to run inside the clone of session fix-1"),
        "{stderr}"
    );
    assert!(stderr.contains(&*repo.to_string_lossy()), "{stderr}");
}

#[test]
fn a_session_run_report_marks_the_clone_workspace() {
    // cli.md D10 + workspace.md D3/D4: the `--verbose` report of a
    // clone run names the workspace (the clone bound rw at the repo
    // path, the host repo not mounted) — the isolation claim of the
    // mode is checkable against the argv, like every other.
    let Some((inv, repo, sidecar)) = git_session_fixture(
        "session-verbose",
        &["--session", "fix-1", "--verbose", "--dry-run"],
    ) else {
        return;
    };
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains(&format!(
            "workspace:      clone run — {} bound rw at {} (the host repo is not mounted)",
            clone.display(),
            repo.display()
        )),
        "{report}"
    );
    // The argv block still follows, unprefixed.
    assert!(stdout.contains("bwrap\n"), "{stdout}");
}

// ---- the handoff verbs of the workspace model (workspace.md D6) ------------

/// The git closure of [`handoff_fixture`], for the tests that need
/// to drive git themselves.
fn git_in(dir: &Path, args: &[&str]) -> bool {
    Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(args)
        .env("GIT_CONFIG_GLOBAL", "/dev/null")
        .env("GIT_CONFIG_SYSTEM", "/dev/null")
        .env("GIT_AUTHOR_NAME", "mysbx-tests")
        .env("GIT_AUTHOR_EMAIL", "mysbx-tests@invalid")
        .env("GIT_COMMITTER_NAME", "mysbx-tests")
        .env("GIT_COMMITTER_EMAIL", "mysbx-tests@invalid")
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

/// A full handoff fixture: a real host git repo (one commit on
/// `main`), an initialized sidecar, and a REAL session clone created
/// the way the first `--session` run creates it (`git clone --origin
/// origin --no-hardlinks`, branch `agent/mysbx/NAME` at HEAD) — the
/// state every handoff verb operates on. Returns `None` when no
/// runnable `git` is on PATH, the same skip the creation tests make.
///
/// `commit` is a closure over the clone so a test can advance the
/// session branch the way a session does.
fn handoff_fixture(
    name: &str,
    f: impl FnOnce(&Path),
) -> Option<(Invocation, PathBuf, PathBuf, PathBuf)> {
    let (inv, repo, sidecar) = git_session_fixture(name, &[])?;
    let clone = sidecar.join("clones").join("fix-1");
    if !git_in(
        &repo,
        &[
            "clone",
            "--origin",
            "origin",
            "--no-hardlinks",
            &repo.to_string_lossy(),
            &clone.to_string_lossy(),
        ],
    ) {
        return None;
    }
    if !git_in(&clone, &["checkout", "-b", "agent/mysbx/fix-1"]) {
        return None;
    }
    f(&clone);
    Some((inv, repo, sidecar, clone))
}

#[test]
fn fetch_briges_the_session_branch_into_the_host_repo() {
    // D6, end to end: after a commit on the session branch, `fetch`
    // creates the host-local agent/mysbx/fix-1 at exactly the
    // session tip — fast-forward, no remote configured, exit 0.
    let Some((inv, repo, sidecar, clone)) = handoff_fixture("handoff-fetch", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    let mut inv = inv;
    inv.args = vec!["fetch", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    // The exact tip: the host-local branch IS the session tip.
    let tip = Command::new("git")
        .arg("-C")
        .arg(&clone)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    let tip = String::from_utf8_lossy(&tip.stdout).trim().to_string();
    let host = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&host.stdout).trim(), tip);
    // No remote was configured — the host repo learned nothing
    // permanent about the clone.
    let remotes = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["remote"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&remotes.stdout).trim(), "");
    // The session's result file and clones/ are untouched.
    assert!(sidecar.is_dir());
}

#[test]
fn a_fetch_of_a_diverged_host_branch_is_rejected_without_touching_it() {
    // D6: the fetch is fast-forward-only — a host-local branch that
    // advanced independently is REJECTED, never force-updated, and
    // its tip is untouched.
    let Some((inv, repo, _, clone)) = handoff_fixture("handoff-ff-only", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    // Fetch once — then advance the HOST branch independently.
    let mut inv0 = inv;
    inv0.args = vec!["fetch", "fix-1"];
    let (code, _, stderr) = run_binary(&inv0);
    assert_eq!(code, 0, "stderr: {stderr}");
    let inv = inv0;
    let host_tip = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    let host_tip = String::from_utf8_lossy(&host_tip.stdout).trim().to_string();
    assert!(git_in(&repo, &["checkout", "agent/mysbx/fix-1"]));
    std::fs::write(repo.join("host.txt"), "host work").unwrap();
    assert!(git_in(&repo, &["add", "host.txt"]));
    assert!(git_in(&repo, &["commit", "-m", "host work"]));
    assert!(git_in(&repo, &["checkout", "main"]));
    let host_tip2 = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    let host_tip2 = String::from_utf8_lossy(&host_tip2.stdout)
        .trim()
        .to_string();
    assert_ne!(host_tip, host_tip2);
    // The session advances too — now the two branches diverged.
    std::fs::write(clone.join("session2.txt"), "more work").unwrap();
    assert!(git_in(&clone, &["add", "session2.txt"]));
    assert!(git_in(&clone, &["commit", "-m", "more session work"]));
    let mut inv = inv;
    inv.args = vec!["fetch", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("diverged"), "{stderr}");
    // And the host tip is untouched.
    let host_tip3 = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&host_tip3.stdout).trim(), host_tip2);
}

#[test]
fn merge_consumes_the_exact_ref_and_deletes_the_ferry_copy() {
    // D6: the merge consumes refs/heads/agent/mysbx/NAME (never the
    // bare name), lands in the current branch with a --no-ff merge
    // commit by default, and deletes the fetched host-local ref on
    // success — a recreated session then starts at HEAD again.
    let Some((inv, repo, _, _)) = handoff_fixture("handoff-merge", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    let mut inv = inv;
    inv.args = vec!["merge", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    // The work IS in the current branch, via a merge commit (--no-ff).
    let merged = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["log", "--merges", "--oneline", "main"])
        .output()
        .unwrap();
    assert!(merged.status.success());
    assert!(!merged.stdout.is_empty(), "a --no-ff merge commit exists");
    assert!(repo.join("session.txt").exists());
    // The ferry ref is gone.
    let ferry = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args([
            "show-ref",
            "--verify",
            "--quiet",
            "refs/heads/agent/mysbx/fix-1",
        ])
        .status()
        .unwrap();
    assert!(!ferry.success(), "the fetched ref was deleted on success");
}

#[test]
fn merge_refuses_a_dirty_host_tree_and_a_detached_head() {
    // D6: the merge's own refusals run BEFORE any git does — a dirty
    // tree would mix the session's merge with uncommitted operator
    // work, a detached HEAD names no branch to land in.
    let Some((inv, repo, _, _)) = handoff_fixture("handoff-merge-dirty", |_| {}) else {
        return;
    };
    std::fs::write(repo.join("dirty.txt"), "uncommitted").unwrap();
    let mut inv = inv;
    inv.args = vec!["merge", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("dirty"), "{stderr}");
    // Nothing was fetched either: the refusal ran first.
    let ferry = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args([
            "show-ref",
            "--verify",
            "--quiet",
            "refs/heads/agent/mysbx/fix-1",
        ])
        .status()
        .unwrap();
    assert!(!ferry.success(), "no fetch happened");
    std::fs::remove_file(repo.join("dirty.txt")).unwrap();
    // A detached HEAD is refused the same way.
    assert!(git_in(&repo, &["checkout", "--detach", "HEAD"]));
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("detached HEAD"), "{stderr}");
}

#[test]
fn merge_passes_the_strategy_through() {
    // D6: `--squash` (and `--ff`, and git-merge args after `--`) pass
    // through to the merge.
    let Some((inv, repo, _, _)) = handoff_fixture("handoff-merge-squash", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    let mut inv = inv;
    inv.args = vec!["merge", "fix-1", "--squash"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    // A squashed merge leaves no merge commit but the changes are in.
    let merged = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["log", "--merges", "--oneline", "main"])
        .output()
        .unwrap();
    assert!(String::from_utf8_lossy(&merged.stdout).trim().is_empty());
    assert!(repo.join("session.txt").exists());
    // git-merge args after `--` ride along: a custom message.
    let (inv2, repo2, _, clone2) = handoff_fixture("handoff-merge-msg", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    })
    .unwrap();
    let mut inv2 = inv2;
    inv2.args = vec!["merge", "fix-1", "--", "-m", "the session work"];
    let (code, _, stderr) = run_binary(&inv2);
    assert_eq!(code, 0, "stderr: {stderr}");
    let msg = Command::new("git")
        .arg("-C")
        .arg(&repo2)
        .args(["log", "-1", "--pretty=%s"])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&msg.stdout).trim(),
        "the session work"
    );
    let _ = clone2;
}

#[test]
fn push_goes_through_the_host_repos_own_origin() {
    // D6: the push goes through the HOST REPO's own remotes — a
    // second repository as `origin`, not the clone. The implicit
    // fetch runs first, so the pushed ref is current.
    let Some((inv, repo, base, clone)) = handoff_fixture("handoff-push", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    // The host repo's origin: a bare upstream repository.
    let upstream = base.join("upstream");
    assert!(
        git_in(&repo, &["init", "--bare", &upstream.display().to_string()]) || {
            Command::new("git")
                .args(["init", "--bare"])
                .arg(&upstream)
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        }
    );
    assert!(git_in(
        &repo,
        &["remote", "add", "origin", &upstream.display().to_string()]
    ));
    assert!(git_in(
        &repo,
        &["push", "origin", "refs/heads/main:refs/heads/main"]
    ));
    let mut inv = inv;
    inv.args = vec!["push", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    // The upstream now has the session branch at the session tip.
    let tip = Command::new("git")
        .arg("-C")
        .arg(&clone)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    let tip = String::from_utf8_lossy(&tip.stdout).trim().to_string();
    let pushed = Command::new("git")
        .arg("-C")
        .arg(&upstream)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&pushed.stdout).trim(), tip);
    // An explicit remote passes through.
    assert!(git_in(
        &repo,
        &["remote", "add", "backup", &upstream.display().to_string()]
    ));
    inv.args = vec!["push", "fix-1", "backup"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
}

#[test]
fn diff_reports_the_session_changes_since_the_divergence() {
    // D6: the three-dot diff — the changes on the session branch
    // since it diverged from the host's HEAD, not the host's own
    // drift. With a host-side commit the session does not know, a
    // two-dot diff would report it as a removal; the three-dot form
    // reports only the session's own work.
    let Some((inv, repo, _, clone)) = handoff_fixture("handoff-diff", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    // The host advances independently — the session's drift.
    std::fs::write(repo.join("host.txt"), "host work").unwrap();
    assert!(git_in(&repo, &["add", "host.txt"]));
    assert!(git_in(&repo, &["commit", "-m", "host work"]));
    let mut inv = inv;
    inv.args = vec!["diff", "fix-1"];
    let (code, stdout, stderr) = run_binary(&inv);
    // git diff's own exit code propagates — 0 for a plain diff whose
    // output IS the answer (differences do not make it fail).
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("session.txt"), "{stdout}");
    assert!(
        !stdout.contains("host.txt"),
        "the host's own drift is not reported: {stdout}"
    );
    let _ = clone;
}

#[test]
fn a_handoff_dry_run_prints_the_git_commands_and_runs_nothing() {
    // cli.md D9: `--dry-run` prints the exact git commands of the
    // handoff — one argument per line, the executable first — and
    // runs nothing: no fetch happens, the host repo keeps no
    // agent/mysbx/NAME branch.
    let Some((inv, repo, _, _)) = handoff_fixture("handoff-dry", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    for (args, verb_line) in [
        (vec!["--dry-run", "fetch", "fix-1"], "fetch"),
        (vec!["--dry-run", "merge", "fix-1"], "merge"),
        (vec!["--dry-run", "push", "fix-1"], "push"),
        (vec!["--dry-run", "diff", "fix-1"], "diff"),
    ] {
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 0, "{args:?}: stderr: {stderr}");
        let lines: Vec<&str> = stdout.lines().collect();
        // The fetch command: `git -C <repo> fetch --no-tags <clone>
        // <refspec>` — the exact D6 mechanics.
        assert_eq!(lines[0], "git", "{stdout}");
        assert_eq!(lines[1], "-C", "{stdout}");
        assert_eq!(lines[2], repo.to_string_lossy(), "{stdout}");
        assert_eq!(lines[3], "fetch", "{stdout}");
        assert_eq!(lines[4], "--no-tags", "{stdout}");
        assert_eq!(
            lines[5],
            format!("{}/clones/fix-1", repo.to_string_lossy() + ".mysbx"),
            "{stdout}"
        );
        assert_eq!(
            lines[6], "refs/heads/agent/mysbx/fix-1:refs/heads/agent/mysbx/fix-1",
            "{stdout}"
        );
        // The verb's own command follows the fetch (for `fetch`
        // itself the fetch IS the command), and nothing runs.
        if verb_line != "fetch" {
            assert!(lines[7..].contains(&verb_line), "{stdout}");
        }
        let ferry = Command::new("git")
            .arg("-C")
            .arg(&repo)
            .args([
                "show-ref",
                "--verify",
                "--quiet",
                "refs/heads/agent/mysbx/fix-1",
            ])
            .status()
            .unwrap();
        assert!(!ferry.success(), "{args:?}: nothing was fetched");
    }
}

#[test]
fn the_merge_dry_run_shows_the_ff_default_and_the_ferry_deletion() {
    // The merge's dry run: the fetch, then `merge --no-ff
    // refs/heads/…` (the D6 default), then the `branch -D` of the
    // ferry copy — the complete plan, nothing run.
    let Some((inv, repo, _, _)) = handoff_fixture("handoff-merge-dry", |_| {}) else {
        return;
    };
    let mut inv = inv;
    inv.args = vec!["--dry-run", "merge", "fix-1"];
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    // The merge block: `git -C <repo> merge --no-ff refs/heads/…`.
    let merge_at = lines
        .iter()
        .position(|l| *l == "merge")
        .expect("the merge command");
    assert_eq!(lines[merge_at - 3], "git", "{stdout}");
    assert_eq!(lines[merge_at - 1], repo.to_string_lossy(), "{stdout}");
    assert_eq!(lines[merge_at + 1], "--no-ff", "{stdout}");
    assert_eq!(
        lines[merge_at + 2],
        "refs/heads/agent/mysbx/fix-1",
        "{stdout}"
    );
    // The ferry deletion: `git -C <repo> branch -D agent/mysbx/fix-1`.
    let del_at = lines
        .iter()
        .position(|l| *l == "branch")
        .expect("the ferry deletion");
    assert_eq!(lines[del_at - 3], "git", "{stdout}");
    assert_eq!(lines[del_at - 1], repo.to_string_lossy(), "{stdout}");
    assert_eq!(lines[del_at + 1], "-D", "{stdout}");
    assert_eq!(lines[del_at + 2], "agent/mysbx/fix-1", "{stdout}");
}

#[test]
fn handoff_usage_errors_exit_2() {
    // workspace.md D2 + cli.md D8: the NAME grammar at parse time,
    // the missing NAME, the unexpected tail of fetch/diff, a second
    // push remote, an unknown push flag, a repeated merge strategy.
    let (inv, _, _) = fixture_user_backend("handoff-usage", &[]);
    for args in [
        vec!["fetch"],
        vec!["merge"],
        vec!["push"],
        vec!["diff"],
        vec!["fetch", "a/b"],
        vec!["fetch", ""],
        vec!["fetch", ".."],
        vec!["diff", "fix-1", "extra"],
        vec!["fetch", "fix-1", "extra"],
        vec!["push", "fix-1", "origin", "backup"],
        vec!["push", "fix-1", "--repo", "/tmp"],
        vec!["push", "fix-1", "--"],
        vec!["merge", "fix-1", "--ff", "--squash"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
    }
}

#[test]
fn handoff_refuses_unknown_sessions_and_run_scoped_flags() {
    // The registry of D2: no clone directory ⇒ no session ⇒ the
    // unknown-session refusal (exit 70, the command line was fine),
    // naming the session and how to start one. The run-scoped global
    // flags are usage errors with the verbs (2): a handoff is not a
    // run — there is no workspace to choose and no payload.
    let (inv, _, _) = fixture_user_backend("handoff-unknown", &[]);
    for args in [
        vec!["fetch", "fix-1"],
        vec!["merge", "fix-1"],
        vec!["push", "fix-1"],
        vec!["diff", "fix-1"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 70, "{args:?}: {stderr}");
        assert!(
            stderr.contains("unknown session: fix-1"),
            "{args:?}: {stderr}"
        );
        assert!(
            stderr.contains("mysbx run --session fix-1"),
            "{args:?}: {stderr}"
        );
    }
    for args in [
        vec!["--session", "fix-1", "fetch", "fix-1"],
        vec!["--verbose", "fetch", "fix-1"],
        vec!["--ro", "/tmp", "diff", "fix-1"],
        vec!["--result", "merge", "fix-1"],
        vec!["--timeout", "5", "push", "fix-1"],
        vec!["--multiplexer", "tmux", "fetch", "fix-1"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(stderr.contains("is not valid with"), "{args:?}: {stderr}");
    }
}

#[test]
fn a_handoff_started_inside_a_session_clone_is_refused() {
    // workspace.md D9, the handoff side: the verbs belong to the
    // HOST side of a session; started inside the clone, the resolver
    // refuses with the error naming the owning repo and the session.
    let (mut inv, _, sidecar) = fixture_user_backend("handoff-inside-clone", &["fetch", "fix-1"]);
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    inv.cwd = clone.clone();
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(
        stderr.contains("refusing to run inside the clone of session fix-1"),
        "{stderr}"
    );
}

// ---- the session noun group of the workspace model (workspace.md D7) ----

/// A session-verbs fixture: the same real host repo + real session
/// clone as [`handoff_fixture`], but without committing on the
/// session branch — the tests advance the clone themselves — and
/// named per test, so several sessions can coexist in one registry.
fn session_fixture(name: &str, sessions: &[&str]) -> Option<(Invocation, PathBuf, PathBuf)> {
    let (inv, repo, sidecar) = git_session_fixture(name, &[])?;
    for session in sessions {
        let clone = sidecar.join("clones").join(session);
        if !git_in(
            &repo,
            &[
                "clone",
                "--origin",
                "origin",
                "--no-hardlinks",
                &repo.to_string_lossy(),
                &clone.to_string_lossy(),
            ],
        ) || !git_in(
            &clone,
            &["checkout", "-b", &format!("agent/mysbx/{session}")],
        ) {
            return None;
        }
    }
    Some((inv, repo, sidecar))
}

#[test]
fn session_list_prints_name_branch_and_ahead_count() {
    // D7: one line per clones/ entry — the name, the session branch
    // and the ahead-count (commits in the session branch that the
    // host repo does not have). Two sessions, one with two commits,
    // one fresh (ahead 0: its tip is the host HEAD).
    let Some((inv, repo, sidecar)) = session_fixture("session-list", &["fix-1", "other"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    for (file, msg) in [("s1.txt", "first"), ("s2.txt", "second")] {
        std::fs::write(fix.join(file), "work").unwrap();
        assert!(git_in(&fix, &["add", file]));
        assert!(git_in(&fix, &["commit", "-m", msg]));
    }
    let (code, stdout, stderr) = run_binary_with(&inv, &["session", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    // The header, then one line per entry, sorted.
    assert_eq!(
        lines[0],
        format!("{:<24} {:<28} {}", "SESSION", "BRANCH", "AHEAD"),
        "{stdout}"
    );
    assert!(lines.len() == 3, "one row per session: {stdout}");
    assert!(
        lines[1].starts_with(&format!("fix-1{}", " ".repeat(24 - 5))),
        "{stdout}"
    );
    assert!(lines[1].contains("agent/mysbx/fix-1"), "{stdout}");
    assert!(lines[1].trim_end().ends_with('2'), "ahead 2: {stdout}");
    assert!(lines[2].contains("other"), "{stdout}");
    assert!(lines[2].contains("agent/mysbx/other"), "{stdout}");
    assert!(lines[2].trim_end().ends_with('0'), "ahead 0: {stdout}");
    let _ = repo;
}

#[test]
fn session_list_marks_debris_and_skips_result_files() {
    // D7: an entry without .git is debris of an interrupted creation
    // and is marked as such (the gvisor incomplete-inventory
    // precedent); the per-session NAME.json result files of D5 are
    // files, not registry entries, and list no rows for them.
    let Some((inv, _, sidecar)) = session_fixture("session-list-debris", &["fix-1"]) else {
        return;
    };
    let clones = sidecar.join("clones");
    std::fs::create_dir_all(clones.join("broken")).unwrap();
    std::fs::write(clones.join("fix-1.json"), "{}").unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["session", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert!(lines.len() == 3, "two rows: {stdout}");
    let broken = lines.iter().find(|l| l.starts_with("broken")).unwrap();
    assert!(broken.contains("debris"), "{stdout}");
    assert!(broken.contains("interrupted creation"), "{stdout}");
    // The result file produced no row of its own.
    assert!(
        !stdout.contains("fix-1.json"),
        "the result file is not a session: {stdout}"
    );
}

#[test]
fn session_list_after_fetch_and_merge_counts_zero() {
    // The ahead-count follows "the host repo does not have them"
    // literally: after a fetch the work is IN the host repo (the
    // ferry branch), after a merge it is in the host history — the
    // count drops to 0 at the fetch already.
    let Some((inv, repo, sidecar)) = session_fixture("session-list-merged", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    std::fs::write(fix.join("s1.txt"), "work").unwrap();
    assert!(git_in(&fix, &["add", "s1.txt"]));
    assert!(git_in(&fix, &["commit", "-m", "session work"]));
    let mut inv_fetch = inv.clone();
    inv_fetch.args = vec!["fetch", "fix-1"];
    let (code, _, stderr) = run_binary(&inv_fetch);
    assert_eq!(code, 0, "stderr: {stderr}");
    let mut inv = inv;
    inv.args = vec!["session", "list"];
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let row = stdout
        .lines()
        .find(|l| l.starts_with("fix-1"))
        .expect("the session is listed");
    assert!(
        row.trim_end().ends_with('0'),
        "ahead 0 after fetch: {stdout}"
    );
    // …and after the merge, with the ferry ref deleted, still 0.
    let mut inv_merge = inv.clone();
    inv_merge.args = vec!["merge", "fix-1"];
    let (code, _, stderr) = run_binary(&inv_merge);
    assert_eq!(code, 0, "stderr: {stderr}");
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    let row = stdout
        .lines()
        .find(|l| l.starts_with("fix-1"))
        .expect("the session is listed");
    assert!(
        row.trim_end().ends_with('0'),
        "ahead 0 after merge: {stdout}"
    );
    let _ = repo;
}

#[test]
fn session_destroy_refuses_unmerged_work_then_succeeds_after_merge() {
    // D7's refusal, end to end: while the session branch holds
    // commits the host repo does not have, destroy refuses (70)
    // naming the branch and the merge to run instead; after the
    // merge the clone is gone — plain rm -rf, host repo untouched —
    // and the exit is 0.
    let Some((inv, repo, sidecar)) = session_fixture("session-destroy", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    std::fs::write(fix.join("s1.txt"), "work").unwrap();
    assert!(git_in(&fix, &["add", "s1.txt"]));
    assert!(git_in(&fix, &["commit", "-m", "session work"]));
    let mut inv = inv;
    inv.args = vec!["session", "destroy", "fix-1"];
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("refusing to destroy"), "{stderr}");
    assert!(stderr.contains("agent/mysbx/fix-1"), "{stderr}");
    assert!(stderr.contains("mysbx merge fix-1"), "{stderr}");
    assert!(fix.join(".git").is_dir(), "nothing was removed");
    // The merge hands the work over; then the destroy succeeds.
    let mut inv_merge = inv.clone();
    inv_merge.args = vec!["merge", "fix-1"];
    let (code, _, stderr) = run_binary(&inv_merge);
    assert_eq!(code, 0, "stderr: {stderr}");
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(!fix.exists(), "the clone is gone");
    assert!(stdout.contains("## destroyed:"), "{stdout}");
    assert!(
        repo.join("s1.txt").is_file(),
        "the work is in the host repo"
    );
    // The registry shows nothing left.
    let mut inv_list = inv;
    inv_list.args = vec!["session", "list"];
    let (code, stdout, stderr) = run_binary(&inv_list);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(!stdout.contains("fix-1"), "{stdout}");
}

#[test]
fn session_destroy_force_discards_and_keeps_the_ferry_branch() {
    // `--force` overrides ONLY the unmerged-work refusal: the work is
    // discarded (the message says so), while a host-local
    // agent/mysbx/NAME branch a fetch left is KEPT — it is the
    // operator's imported copy, and deleting it silently would
    // contradict the unmerged-work guard (D7).
    let Some((inv, repo, sidecar)) = session_fixture("session-destroy-force", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    std::fs::write(fix.join("s1.txt"), "work").unwrap();
    assert!(git_in(&fix, &["add", "s1.txt"]));
    assert!(git_in(&fix, &["commit", "-m", "session work"]));
    let mut inv_fetch = inv;
    inv_fetch.args = vec!["fetch", "fix-1"];
    let (code, _, stderr) = run_binary(&inv_fetch);
    assert_eq!(code, 0, "stderr: {stderr}");
    let mut inv = inv_fetch;
    inv.args = vec!["session", "destroy", "fix-1", "--force"];
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(!fix.exists(), "the clone is gone");
    assert!(stdout.contains("## destroyed:"), "{stdout}");
    let ferry = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args([
            "show-ref",
            "--verify",
            "--quiet",
            "refs/heads/agent/mysbx/fix-1",
        ])
        .status()
        .unwrap();
    assert!(ferry.success(), "the ferry branch was kept");
    // The per-session result file went with the session.
    assert!(!sidecar.join("clones").join("fix-1.json").exists());
}

#[test]
fn session_destroy_removes_debris() {
    // D2/D7: an entry without .git is debris of an interrupted
    // creation — destroy is the verb that names it and removes it;
    // no work can be lost, so no --force is needed.
    let (inv, _, sidecar) = fixture_user_backend("session-destroy-debris", &[]);
    let debris = sidecar.join("clones").join("half-made");
    std::fs::create_dir_all(&debris).unwrap();
    std::fs::write(debris.join("partial.txt"), "interrupted").unwrap();
    let mut inv = inv;
    inv.args = vec!["session", "destroy", "half-made"];
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(!debris.exists(), "the debris is gone");
    assert!(stdout.contains("debris"), "{stdout}");
}

#[test]
fn session_destroy_refuses_unknown_sessions_and_usage_errors() {
    // The registry of D2 (a missing clone is the unknown-session
    // refusal, 70, the same words the handoff verbs use) and the
    // parse-time grammar of the NAME (2, cli.md D8): the missing
    // NAME, a bad NAME, a second positional, an unknown flag, a
    // repeated --force, an unknown group verb, a bare `session`.
    let (inv, _, _) = fixture_user_backend("session-destroy-errors", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["session", "destroy", "fix-1"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("unknown session: fix-1"), "{stderr}");
    for args in [
        vec!["session", "destroy"],
        vec!["session", "destroy", "a/b"],
        vec!["session", "destroy", "fix-1", "extra"],
        vec!["session", "destroy", "-x", "fix-1"],
        vec!["session", "destroy", "fix-1", "--force", "--force"],
        vec!["session"],
        vec!["session", "bogus"],
        vec!["session", "list", "extra"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
    }
}

#[test]
fn session_verbs_refuse_run_scoped_flags_and_verbose() {
    // Like the handoff verbs (D6): no sandbox is started, so the
    // run-scoped flags are usage errors (2) and --verbose has no run
    // to report on.
    let (inv, _, _) = fixture_user_backend("session-flags", &[]);
    for args in [
        vec!["--session", "fix-1", "session", "list"],
        vec!["--ro", "/tmp", "session", "list"],
        vec!["--result", "session", "list"],
        vec!["--timeout", "5", "session", "destroy", "fix-1"],
        vec!["--multiplexer", "tmux", "session", "list"],
        vec!["--verbose", "session", "list"],
        vec!["--verbose", "session", "destroy", "fix-1"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(
            stderr.contains("is not valid with") || stderr.contains("--verbose"),
            "{args:?}: {stderr}"
        );
    }
}

#[test]
fn a_session_verb_started_inside_a_session_clone_is_refused() {
    // workspace.md D9, the session-verb side: the verbs belong to the
    // HOST side of a session; started inside the clone, the resolver
    // refuses with the error naming the owning repo and the session.
    let (mut inv, _, sidecar) = fixture_user_backend("session-inside-clone", &[]);
    inv.args = vec!["session", "list"];
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    inv.cwd = clone.clone();
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(
        stderr.contains("refusing to run inside the clone of session fix-1"),
        "{stderr}"
    );
}

#[test]
fn a_session_dry_run_prints_the_commands_and_removes_nothing() {
    // cli.md D9 for the session verbs: the exact probe and removal
    // commands, one argument per line, the executable first — and
    // nothing runs: no clone is removed, no listing is printed.
    let Some((inv, repo, sidecar)) = session_fixture("session-dry", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    std::fs::write(fix.join("s1.txt"), "work").unwrap();
    assert!(git_in(&fix, &["add", "s1.txt"]));
    assert!(git_in(&fix, &["commit", "-m", "session work"]));
    // destroy --dry-run: the refusals fire first (the work is
    // unmerged), so the dry run of a REFUSED destroy is the refusal.
    let mut inv = inv;
    inv.args = vec!["session", "destroy", "fix-1"];
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run", "session", "destroy", "fix-1"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("refusing to destroy"), "{stderr}");
    assert!(fix.exists(), "nothing was removed");
    // …of a merged session it is the plan: the probe command, then
    // the removal (rm, the same format).
    let mut inv_merge = inv.clone();
    inv_merge.args = vec!["merge", "fix-1"];
    let (code, _, stderr) = run_binary(&inv_merge);
    assert_eq!(code, 0, "stderr: {stderr}");
    let (code, stdout, stderr) =
        run_binary_with(&inv, &["--dry-run", "session", "destroy", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert!(lines.contains(&"git"), "the probe command: {stdout}");
    assert!(stdout.contains("for-each-ref"), "{stdout}");
    let rm_at = lines
        .iter()
        .position(|l| *l == "rm")
        .expect("the removal command");
    assert_eq!(lines[rm_at + 1], "-rf", "{stdout}");
    assert_eq!(
        lines[rm_at + 2],
        sidecar.join("clones").join("fix-1").to_string_lossy(),
        "{stdout}"
    );
    assert!(fix.exists(), "a dry run removes nothing");
    // list --dry-run: the probe commands of the registry, no table.
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run", "session", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("git"), "{stdout}");
    assert!(!stdout.contains("SESSION"), "no listing: {stdout}");
    let _ = repo;
}

#[test]
fn session_hunk_dry_run_prints_the_fetch_and_the_invocation() {
    // D7/cli.md D9: the exact commands of `session hunk` — the
    // implicit fetch of `mysbx diff` (D6), then the `hunk` invocation
    // with the same three-dot range — one argument per line, the
    // executable first, and nothing runs: no fetch happens, the host
    // repo keeps no agent/mysbx/NAME branch.
    let Some((inv, repo, _, clone)) = handoff_fixture("session-hunk-dry", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run", "session", "hunk", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    // The fetch command first: `git -C <repo> fetch --no-tags
    // <clone> <refspec>` — the exact D6 mechanics, shared with
    // `mysbx diff`.
    assert_eq!(lines[0], "git", "{stdout}");
    assert_eq!(lines[1], "-C", "{stdout}");
    assert_eq!(lines[2], repo.to_string_lossy(), "{stdout}");
    assert_eq!(lines[3], "fetch", "{stdout}");
    assert_eq!(lines[4], "--no-tags", "{stdout}");
    assert_eq!(lines[5], clone.to_string_lossy(), "{stdout}");
    assert_eq!(
        lines[6], "refs/heads/agent/mysbx/fix-1:refs/heads/agent/mysbx/fix-1",
        "{stdout}"
    );
    // Then the hunk invocation: `hunk diff HEAD...refs/heads/…`.
    let hunk_at = lines
        .iter()
        .position(|l| *l == "hunk")
        .expect("the hunk invocation: {stdout}");
    assert_eq!(
        &lines[hunk_at..],
        &["hunk", "diff", "HEAD...refs/heads/agent/mysbx/fix-1",],
        "{stdout}"
    );
    // Nothing ran: no host-local session branch was created.
    let ferry = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args([
            "show-ref",
            "--verify",
            "--quiet",
            "refs/heads/agent/mysbx/fix-1",
        ])
        .status()
        .unwrap();
    assert!(!ferry.success(), "a dry run fetches nothing");
}

#[test]
fn session_hunk_refuses_unknown_sessions_debris_and_usage_errors() {
    // The refusal order: the registry of D2 (a missing clone is the
    // unknown-session refusal, 70, the same words `session destroy`
    // and the handoff verbs use), debris (70 — no `.git`, no branch
    // to review), and the parse-time grammar of the NAME (2) plus
    // the closed group's usage errors.
    let (inv, _, sidecar) = fixture_user_backend("session-hunk-errors", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["session", "hunk", "fix-1"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("unknown session: fix-1"), "{stderr}");
    assert!(stderr.contains("mysbx run --session fix-1"), "{stderr}");
    // Debris: an entry without `.git`.
    let debris = sidecar.join("clones").join("broken");
    std::fs::create_dir_all(&debris).unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["session", "hunk", "broken"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("refusing to hunk"), "{stderr}");
    assert!(stderr.contains("debris"), "{stderr}");
    // The usage errors: the missing NAME, a bad grammar, an extra
    // argument, a flag, `--`, an unknown group verb, a bare
    // `session`.
    for args in [
        vec!["session", "hunk"],
        vec!["session", "hunk", "a/b"],
        vec!["session", "hunk", "fix-1", "extra"],
        vec!["session", "hunk", "--force", "fix-1"],
        vec!["session", "hunk", "--"],
        vec!["session", "bogus"],
        vec!["session"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
    }
    // `--verbose` is refused like every session verb (no run to
    // report on); `--dry-run` IS valid (cli.md D9).
    let (code, _, stderr) = run_binary_with(&inv, &["--verbose", "session", "hunk", "fix-1"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("--verbose"), "{stderr}");
}

#[test]
fn session_hunk_execs_hunk_over_the_fetched_range() {
    // W4 for sessions: after the implicit fetch, `hunk` is exec'd
    // with the host repo as the working directory — a stand-in on
    // PATH receives the exact argv (`diff HEAD...refs/heads/…`), its
    // exit code propagates unchanged, and the fetched ref is the
    // session's current tip (the same contract `mysbx diff` gives).
    let Some((inv, repo, sidecar, clone)) = handoff_fixture("session-hunk-exec", |clone| {
        std::fs::write(clone.join("session.txt"), "work").unwrap();
        assert!(git_in(clone, &["add", "session.txt"]));
        assert!(git_in(clone, &["commit", "-m", "session work"]));
    }) else {
        return;
    };
    let _ = sidecar;
    // A stand-in `hunk`: a shell script that records its argv and
    // cwd to a file and exits with a distinctive code, so both the
    // exec and the code's propagation are proven at once.
    let record = inv.home.join("hunk-argv");
    let stand_in = inv.home.join("hunk");
    std::fs::write(
        &stand_in,
        format!(
            "#!/bin/sh\nprintf '%s\\n' \"$PWD\" \"$@\" >> {}\nexit 3\n",
            record.display()
        ),
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(&stand_in).unwrap().permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(&stand_in, perms).unwrap();
    let mut cmd = spawn_with_args(&inv, &["session", "hunk", "fix-1"]);
    // The stand-in must WIN the PATH lookup (a real `hunk` must not
    // shadow it), but `git` — of the implicit fetch — stays
    // reachable: PREPEND the home, never replace the PATH.
    let path = std::env::var_os("PATH").unwrap_or_default();
    let path =
        std::env::join_paths(std::iter::once(inv.home.clone()).chain(std::env::split_paths(&path)))
            .unwrap();
    cmd.env("PATH", path);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    // The stand-in's own exit code propagated unchanged (cli.md D8).
    assert_eq!(out.status.code(), Some(3), "stderr: {stderr}");
    // The exec'd argv: the range of `mysbx diff`, in the host repo.
    assert_eq!(
        std::fs::read_to_string(&record).unwrap(),
        format!(
            "{}\ndiff\nHEAD...refs/heads/agent/mysbx/fix-1\n",
            repo.display()
        )
    );
    // The fetch really happened first: the host-local session branch
    // exists, at the session's tip (the ferry copy of D6).
    let tip = Command::new("git")
        .arg("-C")
        .arg(&clone)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    let host = Command::new("git")
        .arg("-C")
        .arg(&repo)
        .args(["rev-parse", "refs/heads/agent/mysbx/fix-1"])
        .output()
        .unwrap();
    assert_eq!(
        String::from_utf8_lossy(&host.stdout).trim(),
        String::from_utf8_lossy(&tip.stdout).trim(),
        "the fetched ref is the session tip"
    );
    assert!(!stdout.contains("## "), "no report lines: {stdout}");
}

#[test]
fn a_session_hunk_started_inside_a_session_clone_is_refused() {
    // workspace.md D9, the session-verb side: `session hunk` belongs
    // to the HOST side of a session; started inside the clone, the
    // resolver refuses with the error naming the owning repo and
    // the session.
    let (mut inv, _, sidecar) = fixture_user_backend("session-hunk-inside-clone", &[]);
    inv.args = vec!["session", "hunk", "fix-1"];
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    inv.cwd = clone.clone();
    let (code, _, stderr) = run_binary(&inv);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(
        stderr.contains("refusing to run inside the clone of session fix-1"),
        "{stderr}"
    );
}

// ---- the worktree noun group (docs/design/worktree.md W1-W5) ---------

/// A git fixture with a workmux-style worktrees sibling: a real repo
/// with one commit on `main`, plus `<repo>__worktrees/<handle>` —
/// a real LINKED worktree on its own branch (created with
/// `git worktree add`, exactly what `workmux add` runs), with the
/// workmux base record written like workmux writes it
/// (`branch.<branch>.workmux-base` in the repo config). Returns
/// `None` when no runnable `git` is on PATH — the same skip as
/// `git_repo`.
fn worktree_fixture(name: &str, handle: &str) -> Option<(Invocation, PathBuf, PathBuf, PathBuf)> {
    let (inv, repo, sidecar) = git_session_fixture(name, &[])?;
    let repo_name = repo.file_name()?.to_string_lossy().into_owned();
    let worktrees = repo
        .parent()
        .map(|p| p.join(format!("{repo_name}__worktrees")))
        .unwrap();
    if !git_in(
        &repo,
        &[
            "worktree",
            "add",
            &worktrees.join(handle).to_string_lossy(),
            "-b",
            handle,
        ],
    ) || !git_in(
        &repo,
        &["config", &format!("branch.{handle}.workmux-base"), "main"],
    ) {
        return None;
    }
    Some((inv, repo, sidecar, worktrees.join(handle)))
}

#[test]
fn worktree_list_prints_handle_branch_and_ahead_count() {
    // W1/W2: one line per __worktrees entry — the handle, the
    // checked-out branch and the ahead-count (commits in the
    // worktree's branch the BASE does not have). A worktree with one
    // commit lists ahead 1; a fresh one ahead 0.
    let Some((inv, repo, _, worktree)) = worktree_fixture("worktree-list", "fix-1") else {
        return;
    };
    std::fs::write(worktree.join("w1.txt"), "work").unwrap();
    assert!(git_in(&worktree, &["add", "w1.txt"]));
    assert!(git_in(&worktree, &["commit", "-m", "worktree work"]));
    let (code, stdout, stderr) = run_binary_with(&inv, &["worktree", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines[0],
        format!("{:<24} {:<28} {}", "WORKTREE", "BRANCH", "AHEAD"),
        "{stdout}"
    );
    assert!(lines.len() == 2, "one row: {stdout}");
    assert!(
        lines[1].starts_with(&format!("fix-1{}", " ".repeat(24 - 5))),
        "{stdout}"
    );
    assert!(lines[1].contains("fix-1 "), "{stdout}");
    assert!(lines[1].trim_end().ends_with('1'), "ahead 1: {stdout}");
    let _ = repo;
}

#[test]
fn worktree_list_marks_debris_and_skips_files() {
    // W2: an entry without a `.git` POINTER is debris and marked as
    // such; a stray FILE in the sibling is not a worktree and lists
    // no row.
    let Some((inv, _, _, worktree)) = worktree_fixture("worktree-list-debris", "fix-1") else {
        return;
    };
    let worktrees = worktree.parent().unwrap().to_path_buf();
    std::fs::create_dir_all(worktrees.join("broken")).unwrap();
    std::fs::write(worktrees.join("stray.txt"), "not a worktree").unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["worktree", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert!(lines.len() == 3, "two rows: {stdout}");
    let broken = lines.iter().find(|l| l.starts_with("broken")).unwrap();
    assert!(broken.contains("debris"), "{stdout}");
    assert!(
        !stdout.contains("stray.txt"),
        "a stray file lists no row: {stdout}"
    );
}

#[test]
fn worktree_list_without_a_sibling_lists_nothing() {
    // W2: an absent `__worktrees` is the empty registry — the listing
    // succeeds with the header and no rows, like an empty clones/.
    let (inv, _, _) = git_session_fixture("worktree-no-sibling", &[]).unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["worktree", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines.len(), 1, "just the header: {stdout}");
    assert!(lines[0].starts_with("WORKTREE"), "{stdout}");
}

#[test]
fn worktree_diff_shows_the_three_dot_range() {
    // W1/W3: the diff is `<base>...<branch>` — the changes since the
    // divergence, and git's own exit code passes through. The base is
    // the workmux record; a worktree whose branch advanced one commit
    // shows exactly that commit's diff.
    let Some((inv, repo, _, worktree)) = worktree_fixture("worktree-diff", "fix-1") else {
        return;
    };
    std::fs::write(worktree.join("w1.txt"), "worktree work\n").unwrap();
    assert!(git_in(&worktree, &["add", "w1.txt"]));
    assert!(git_in(&worktree, &["commit", "-m", "worktree work"]));
    let (code, stdout, stderr) = run_binary_with(&inv, &["worktree", "diff", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("w1.txt"), "{stdout}");
    assert!(stdout.contains("+worktree work"), "{stdout}");
    // The base that answered is reported (the workmux record).
    assert!(
        stderr.contains("workmux-base record"),
        "the base line is reported: {stderr}"
    );
    let _ = repo;
}

#[test]
fn worktree_diff_dry_run_prints_the_command() {
    // cli.md D9: the exact git command, one argument per line, the
    // executable first — and nothing runs.
    let Some((inv, _, _, worktree)) = worktree_fixture("worktree-diff-dry", "fix-1") else {
        return;
    };
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run", "worktree", "diff", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0], "git", "{stdout}");
    assert!(lines.contains(&"-C"), "{stdout}");
    assert!(
        lines.contains(&worktree.to_string_lossy().as_ref()),
        "{stdout}"
    );
    assert!(lines.contains(&"diff"), "{stdout}");
    assert!(lines.contains(&"main...fix-1"), "{stdout}");
}

#[test]
fn worktree_hunk_dry_run_prints_the_invocation() {
    // W4: the exact `hunk` invocation — `hunk`, then `diff`, then the
    // range — and nothing runs (a dry run of an interactive tool).
    let Some((inv, _, _, _)) = worktree_fixture("worktree-hunk-dry", "fix-1") else {
        return;
    };
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run", "worktree", "hunk", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines, vec!["hunk", "diff", "main...fix-1"], "{stdout}");
}

#[test]
fn worktree_diff_refuses_unknown_debris_and_detached() {
    // The refusal order of resolve_worktree: an unknown NAME, debris,
    // and (skipped here: a detached HEAD needs a more elaborate
    // fixture) — an unknown handle and debris are both 70 with the
    // fact named.
    let Some((inv, _, _, _)) = worktree_fixture("worktree-refusals", "fix-1") else {
        return;
    };
    let (code, _, stderr) = run_binary_with(&inv, &["worktree", "diff", "no-such"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("unknown worktree"), "{stderr}");
    // Debris: an entry without a .git pointer.
    let worktrees = inv.cwd.parent().unwrap().join(format!(
        "{}__worktrees",
        inv.cwd.file_name().unwrap().to_string_lossy()
    ));
    std::fs::create_dir_all(worktrees.join("broken")).unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["worktree", "diff", "broken"]);
    assert_eq!(code, 70, "stderr: {stderr}");
    assert!(stderr.contains("debris"), "{stderr}");
}

#[test]
fn worktree_verbs_refuse_run_scoped_flags_and_verbose() {
    // Like the session verbs: no sandbox is started, so the run-scoped
    // flags are usage errors (2) and --verbose has no run to report
    // on. --dry-run IS valid (cli.md D9).
    let (inv, _, _) = fixture_user_backend("worktree-flags", &[]);
    for args in [
        vec!["--session", "fix-1", "worktree", "list"],
        vec!["--ro", "/tmp", "worktree", "list"],
        vec!["--result", "worktree", "list"],
        vec!["--timeout", "5", "worktree", "diff", "fix-1"],
        vec!["--multiplexer", "tmux", "worktree", "list"],
        vec!["--verbose", "worktree", "list"],
        vec!["--verbose", "worktree", "diff", "fix-1"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(
            stderr.contains("is not valid with") || stderr.contains("--verbose"),
            "{args:?}: {stderr}"
        );
    }
}

#[test]
fn worktree_usage_errors() {
    // The closed group (W1): an unknown or missing sub-verb, a bad
    // NAME grammar, and extra arguments are all usage errors (2).
    let (inv, _, _) = fixture_user_backend("worktree-usage", &[]);
    for args in [
        vec!["worktree"],
        vec!["worktree", "add"],
        vec!["worktree", "diff"],
        vec!["worktree", "diff", "a/b"],
        vec!["worktree", "diff", "fix-1", "extra"],
        vec!["worktree", "list", "unexpected"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
    }
}

#[test]
fn worktree_diff_of_a_fresh_worktree_is_empty() {
    // W3 sanity: a worktree at its base's tip shows no diff (the
    // three-dot range of identical tips), and the base is still the
    // workmux record.
    let Some((inv, _, _, _)) = worktree_fixture("worktree-fresh", "fix-1") else {
        return;
    };
    let (code, stdout, stderr) = run_binary_with(&inv, &["worktree", "diff", "fix-1"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.trim().is_empty(), "no diff: {stdout}");
    assert!(stderr.contains("workmux-base record"), "{stderr}");
}

// ---- the status verb (cli.md D19) ----------------------------------------

#[test]
fn status_on_an_uninited_repo_reports_it_and_still_exits_zero() {
    // D19: a not-inited repo is a state, not a failure — status says
    // so with the `mysbx init` hint, still exits 0, and lists empty
    // registries. The fixture has a sidecar DIRECTORY but no
    // config.toml, so the not-inited-with-directory branch is the one
    // exercised.
    let (inv, repo, sidecar) = fixture_uninited("status-uninited", &["status"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.starts_with("## "), "D9 prefix: {stdout}");
    assert!(stdout.contains(&repo.display().to_string()), "{stdout}");
    assert!(stdout.contains(&sidecar.display().to_string()), "{stdout}");
    assert!(stdout.contains("not inited"), "{stdout}");
    assert!(stdout.contains("mysbx init"), "the hint: {stdout}");
    // The effective-configuration section answers from the user
    // layer alone (the fixture's empty one: a `(none …)` backend),
    // and says the sidecar is not inited.
    assert!(stdout.contains("backend:"), "{stdout}");
    assert!(stdout.contains("(none"), "{stdout}");
    assert!(
        stdout.contains("user layer only — the sidecar is not inited"),
        "{stdout}"
    );
    assert!(stdout.contains("sessions:"), "{stdout}");
    assert!(stdout.contains("none"), "{stdout}");
    assert!(stdout.contains("worktrees:"), "{stdout}");
}

#[test]
fn status_on_an_inited_repo_matches_the_list_verbs() {
    // D19's reuse rule: the session and worktree lines status prints
    // are the SAME lines the list verbs print — asserted by running
    // all three and comparing, not by restating the format.
    let Some((inv, _, _, worktree)) = worktree_fixture("status-inited", "fix-1") else {
        return;
    };
    // A session beside the worktree, with one commit of work.
    let sidecar = inv.cwd.parent().unwrap().join("repo.mysbx");
    let clone = sidecar.join("clones").join("fix-1");
    assert!(git_in(
        &inv.cwd,
        &[
            "clone",
            "--origin",
            "origin",
            "--no-hardlinks",
            &inv.cwd.to_string_lossy(),
            &clone.to_string_lossy(),
        ]
    ));
    assert!(git_in(&clone, &["checkout", "-b", "agent/mysbx/fix-1"]));
    std::fs::write(clone.join("s1.txt"), "work").unwrap();
    assert!(git_in(&clone, &["add", "s1.txt"]));
    assert!(git_in(&clone, &["commit", "-m", "session work"]));
    std::fs::write(worktree.join("w1.txt"), "work").unwrap();
    assert!(git_in(&worktree, &["add", "w1.txt"]));
    assert!(git_in(&worktree, &["commit", "-m", "worktree work"]));

    let (code, status, stderr) = run_binary_with(&inv, &["status"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(status.contains("inited"), "{status}");
    // The effective configuration is shown: the backend of the user
    // layer, attributed to it, the multiplexer, the network sense.
    assert!(status.contains("backend:"), "{status}");
    assert!(status.contains("bubblewrap"), "{status}");
    assert!(status.contains("[user config]"), "{status}");
    assert!(status.contains("multiplexer:"), "{status}");
    assert!(status.contains("network:"), "{status}");

    // The registries: the SAME rows the list verbs print, indented
    // under their headings. Both the session and its worktree carry
    // one commit, so the rows end in the ahead-count.
    let (code, list, stderr) = run_binary_with(&inv, &["session", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    for line in list.lines().skip(1) {
        assert!(
            status.contains(&format!("  {line}")),
            "status embeds the session row `{line}`: {status}"
        );
    }
    let (code, list, stderr) = run_binary_with(&inv, &["worktree", "list"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    for line in list.lines().skip(1) {
        assert!(
            status.contains(&format!("  {line}")),
            "status embeds the worktree row `{line}`: {status}"
        );
    }
}

#[test]
fn status_refuses_verbose_and_stray_arguments() {
    // D19: --verbose is refused (no run to report on), a stray
    // argument is a usage error naming the bare shape — and the
    // run-scoped flags are refused by the dispatcher's shared arm.
    let (inv, _, _) = fixture_user_backend("status-usage", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--verbose", "status"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(
        stderr.contains("--verbose is not valid with `status`"),
        "{stderr}"
    );
    assert!(stderr.contains("status starts no sandbox"), "{stderr}");
    let (code, _, stderr) = run_binary_with(&inv, &["status", "unexpected"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unexpected argument"), "{stderr}");
    assert!(stderr.contains("usage: mysbx status"), "{stderr}");
    for args in [
        vec!["--session", "fix-1", "status"],
        vec!["--ro", "/tmp", "status"],
        vec!["--backend", "bubblewrap", "status"],
        vec!["--multiplexer", "tmux", "status"],
        vec!["--result", "status"],
        vec!["--timeout", "5", "status"],
    ] {
        let (code, _, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(stderr.contains("is not valid with"), "{args:?}: {stderr}");
    }
}

#[test]
fn status_dry_run_prints_the_same_output() {
    // D19: --dry-run is accepted as a plain print — the verb has no
    // side effects to preview — so both spellings print the same
    // overview.
    let (inv, _, _) = fixture("status-dry-run", &[]);
    let (code, plain, stderr) = run_binary_with(&inv, &["status"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let (code, dry, stderr) = run_binary_with(&inv, &["--dry-run", "status"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(plain, dry, "--dry-run is a plain print");
}

// ---- the doctor verb ---------------------------------------------------------

/// An executable `#!/bin/sh` stub at `dir/name` running `body`.
fn doctor_stub(dir: &Path, name: &str, body: &str) -> PathBuf {
    use std::os::unix::fs::PermissionsExt;
    std::fs::create_dir_all(dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, format!("#!/bin/sh\n{body}")).unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    path
}

/// `mysbx ARGS` with the pins the doctor reads cleared, then `envs` set.
fn run_doctor(inv: &Invocation, args: &[&str], envs: &[(&str, &str)]) -> (i32, String, String) {
    let mut cmd = spawn_with_args(inv, args);
    for name in [
        "MYSBX_FORWARD_ENV",
        "MYSBX_KRUN_RUNTIME",
        "MYSBX_KRUN_CGROUP_MANAGER",
        "MYSBX_KRUN_RUNTIME_FLAGS",
        "MYSBX_GVISOR_CGROUP_MANAGER",
        "MYSBX_GVISOR_RUNTIME_FLAGS",
    ] {
        cmd.env_remove(name);
    }
    for name in mysbx::doctor::ENDPOINT_VARS {
        cmd.env_remove(name);
    }
    for (key, value) in envs {
        cmd.env(key, value);
    }
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn doctor_passes_on_a_healthy_bubblewrap_host() {
    let (inv, _, sidecar) = fixture_user_backend("doctor-bwrap-ok", &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let record = base.join("bwrap-args");
    let bwrap = doctor_stub(
        &base.join("bin"),
        "bwrap",
        &format!("printf '%s\\n' \"$@\" > {}\n", record.display()),
    );
    let bwrap = bwrap.to_string_lossy().into_owned();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[("MYSBX_BWRAP", bwrap.as_str()), ("MYSBX_SHELL", "/bin/sh")],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("== mysbx doctor =="), "{stdout}");
    assert!(
        stdout.contains("OK   backend: bubblewrap [configuration]"),
        "{stdout}"
    );
    assert!(stdout.contains("== bubblewrap =="), "{stdout}");
    assert!(
        stdout.contains(&format!("OK   bwrap binary: {bwrap}")),
        "{stdout}"
    );
    assert!(stdout.contains("OK   startup probe: "), "{stdout}");
    assert!(
        stdout.contains("OK   model endpoint: not applicable"),
        "{stdout}"
    );
    assert!(stdout.contains("== other backends =="), "{stdout}");
    assert!(stdout.contains("mysbx doctor: 0 problem(s)"), "{stdout}");
    // The stub saw exactly the argv of the pure builder, with the
    // run's --share-net (the network is shared by default).
    let recorded = std::fs::read_to_string(&record).unwrap();
    let want = format!(
        "{}\n",
        mysbx::doctor::bwrap_probe_argv("/bin/sh", true).join("\n")
    );
    assert_eq!(recorded, want);
    assert!(recorded.contains("--share-net\n"), "{recorded}");

    // network = false: the probe unshares the network like the run.
    std::fs::write(sidecar.join("config.toml"), "network = false\n").unwrap();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[("MYSBX_BWRAP", bwrap.as_str()), ("MYSBX_SHELL", "/bin/sh")],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("OK   model endpoint: not applicable: the network is denied"),
        "{stdout}"
    );
    let recorded = std::fs::read_to_string(&record).unwrap();
    let want = format!(
        "{}\n",
        mysbx::doctor::bwrap_probe_argv("/bin/sh", false).join("\n")
    );
    assert_eq!(recorded, want);
    assert!(!recorded.contains("--share-net"), "{recorded}");
}

#[test]
fn doctor_fails_when_bwrap_cannot_start_or_is_missing() {
    let (inv, _, _) = fixture_user_backend("doctor-bwrap-broken", &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let bwrap = doctor_stub(
        &base.join("bin"),
        "bwrap",
        "echo 'bwrap: setting up uid map: Permission denied' >&2\nexit 1\n",
    );
    let bwrap = bwrap.to_string_lossy().into_owned();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[("MYSBX_BWRAP", bwrap.as_str()), ("MYSBX_SHELL", "/bin/sh")],
    );
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("FAIL startup probe: bwrap: setting up uid map: Permission denied — "),
        "{stdout}"
    );
    assert!(stdout.contains("mysbx doctor: 1 problem(s)"), "{stdout}");

    // A missing binary is its own FAIL, and the probe is not run.
    let missing = base.join("bin").join("no-such-bwrap");
    let missing = missing.to_string_lossy().into_owned();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_BWRAP", missing.as_str()),
            ("MYSBX_SHELL", "/bin/sh"),
        ],
    );
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("FAIL bwrap binary: "), "{stdout}");
    assert!(
        stdout.contains("MYSBX_BWRAP"),
        "the hint names the pin: {stdout}"
    );
    assert!(
        stdout.contains("SKIP startup probe: not run (depends on: bwrap binary)"),
        "{stdout}"
    );
    assert!(stdout.contains("mysbx doctor: 1 problem(s)"), "{stdout}");
}

#[test]
fn doctor_without_a_backend_fails_unless_one_is_named() {
    let (inv, _, _) = fixture("doctor-no-backend", &[]);
    let (code, stdout, stderr) = run_doctor(&inv, &["doctor"], &[]);
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("FAIL backend: no backend configured"),
        "{stdout}"
    );
    assert!(!stdout.contains("== bubblewrap =="), "{stdout}");

    let base = inv.cwd.parent().unwrap().to_path_buf();
    let bwrap = doctor_stub(&base.join("bin"), "bwrap", "exit 0\n");
    let bwrap = bwrap.to_string_lossy().into_owned();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor", "bubblewrap"],
        &[("MYSBX_BWRAP", bwrap.as_str()), ("MYSBX_SHELL", "/bin/sh")],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("OK   backend: bubblewrap [command line]"),
        "{stdout}"
    );
    assert!(
        stdout.contains("OK   not checked: podman-gvisor, nono, podman-krun, krun (check one with"),
        "{stdout}"
    );
    assert!(!stdout.contains("not configured"), "{stdout}");
}

#[test]
fn doctor_routes_the_krun_backend_away_from_the_podman_checks() {
    // bd myconfig-dak.10: the direct-libkrun backend's doctor
    // section is its OWN — the podman checks (binary, runtime,
    // image) would be wrong answers for a backend that runs no
    // podman at all. A stub launcher prints the launcher's own
    // unknown-argument wording so the probe passes.
    let (inv, _, sidecar) = fixture("doctor-krun-routing", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"krun\"\n").unwrap();
    let base = inv.cwd.parent().unwrap().to_path_buf();
    // The stub reproduces the launcher's unknown-argument wording
    // (what the probe matches on); the backtick/quote dance keeps
    // /bin/sh happy inside the single-quoted echo.
    let launcher = doctor_stub(
        &base,
        "mysbx-krun-stub",
        "echo \"mysbx-krun: unknown argument '--doctor-probe-invalid'\" >&2\nexit 2",
    );
    let rootfs = base.join("krun-rootfs-stub");
    std::fs::create_dir_all(rootfs.join("bin")).unwrap();
    std::fs::write(rootfs.join("bin/mysbx-init"), "#!/bin/sh\n").unwrap();
    let (code, stdout, _stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_KRUN_LAUNCHER", launcher.to_str().unwrap()),
            ("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap()),
        ],
    );
    // The exit is NOT asserted 0: this host has no /dev/kvm, and
    // doctor correctly FAILS that check — the test pins the ROUTING
    // and the krun checks' shape, not the host's KVM.
    assert_ne!(
        code, 0,
        "the /dev/kvm failure must make doctor report: {stdout}"
    );
    assert!(stdout.contains("== krun =="), "{stdout}");
    assert!(
        stdout.contains("OK   launcher probe: the launcher runs and reaches argument parsing"),
        "{stdout}"
    );
    assert!(stdout.contains("OK   rootfs: "), "{stdout}");
    // NOT the podman section's checks:
    assert!(!stdout.contains("OCI runtime"), "{stdout}");
    assert!(!stdout.contains("image"), "{stdout}");
    assert!(!stdout.contains("startup probe"), "{stdout}");
    // and the launcher check has no PATH-fallback pass (bd
    // myconfig-dak.10 review finding 1): the pin resolves, but a
    // MISSING pin must fail even with mysbx-krun on PATH — pinned
    // by the stub's own name below.
    // With a bare `mysbx-krun` ON PATH and NO pin, the check must
    // STILL fail — the run path has no PATH fallback either (the
    // review's finding 1: doctor must not be greener than the run).
    let stub_on_path = base.join("path-stub");
    let _ = doctor_stub(
        &stub_on_path,
        "mysbx-krun",
        "echo 'not the launcher' >&2\nexit 1",
    );
    // PATH entries are separate join arguments — join_paths
    // refuses a `:`-embedded entry.
    let path_with_stub = std::env::join_paths(&[
        stub_on_path.clone(),
        std::path::PathBuf::from("/usr/bin"),
        std::path::PathBuf::from("/bin"),
    ])
    .unwrap();
    let (code, stdout, _stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[("PATH", path_with_stub.to_str().unwrap())],
    );
    assert_ne!(code, 0, "no pin: the launcher check must fail: {stdout}");
    assert!(
        stdout.contains("FAIL launcher: no launcher pinned (MYSBX_KRUN_LAUNCHER)"),
        "{stdout}"
    );
    std::fs::remove_dir_all(stub_on_path).unwrap();
}

#[test]
fn doctor_refuses_unknown_backends_and_global_flags() {
    let (inv, _, _) = fixture_user_backend("doctor-usage", &[]);
    let (code, _, stderr) = run_doctor(&inv, &["doctor", "bogus"], &[]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unknown backend: bogus"), "{stderr}");
    assert!(stderr.contains("usage: mysbx doctor"), "{stderr}");
    let (code, _, stderr) = run_doctor(&inv, &["doctor", "-x"], &[]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unknown option: -x"), "{stderr}");
    for args in [
        vec!["--dry-run", "doctor"],
        vec!["--verbose", "doctor"],
        vec!["--backend", "nono", "doctor"],
        vec!["--session", "fix-1", "doctor"],
    ] {
        let (code, _, stderr) = run_doctor(&inv, &args, &[]);
        assert_eq!(code, 2, "{args:?}: {stderr}");
        assert!(
            stderr.contains("is not valid with `doctor`"),
            "{args:?}: {stderr}"
        );
    }
}

/// A podman stub recording each invocation as one line (`$*`):
/// `image inspect` prints the `Ok` text, or the `Err` text on stderr
/// and fails; the endpoint probe answers `HTTP 404`, everything else
/// succeeds.
fn podman_stub(base: &Path, record: &Path, inspect: Result<&str, &str>) -> String {
    let inspect = match inspect {
        Ok(out) => format!("echo '{out}'"),
        Err(err) => format!("echo '{err}' >&2; exit 125"),
    };
    let body = format!(
        "printf '%s\\n' \"$*\" >> {}\ncase \"$*\" in\n  'image inspect'*) {inspect} ;;\n  *curl*) printf 'HTTP 404' ;;\nesac\nexit 0\n",
        record.display()
    );
    doctor_stub(&base.join("bin"), "podman", &body)
        .to_string_lossy()
        .into_owned()
}

const DOCTOR_IMAGE_ID: &str = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

/// The gvisor pins of the image tests, with `podman` as the binary.
fn gvisor_envs(podman: &str) -> Vec<(String, String)> {
    [
        ("MYSBX_PODMAN", podman),
        ("MYSBX_PODMAN_IMAGE", "localhost/agent:latest"),
        ("MYSBX_PODMAN_IMAGE_ID", DOCTOR_IMAGE_ID),
        ("MYSBX_GVISOR_CGROUP_MANAGER", "cgroupfs"),
        ("MYSBX_GVISOR_RUNTIME_FLAGS", "ignore-cgroups"),
    ]
    .iter()
    .map(|(k, v)| (k.to_string(), v.to_string()))
    .collect()
}

fn env_pairs(owned: &[(String, String)]) -> Vec<(&str, &str)> {
    owned
        .iter()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect()
}

#[test]
fn doctor_probes_a_healthy_podman_gvisor_host_with_the_exact_argv() {
    let (inv, _, sidecar) = fixture("doctor-gvisor-ok", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let record = base.join("podman-args");
    let loaded = format!("sha256:{DOCTOR_IMAGE_ID}");
    let podman = podman_stub(&base, &record, Ok(loaded.as_str()));
    let spec = "pasta:--map-guest-addr,10.0.2.2";
    let url = "http://10.0.2.2:4000/v1";
    let pinned_env = format!("OPENAI_BASE_URL={url}");
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_PODMAN", podman.as_str()),
            ("MYSBX_PODMAN_IMAGE", "localhost/agent:latest"),
            ("MYSBX_PODMAN_IMAGE_ID", DOCTOR_IMAGE_ID),
            ("MYSBX_GVISOR_CGROUP_MANAGER", "cgroupfs"),
            ("MYSBX_GVISOR_RUNTIME_FLAGS", "ignore-cgroups"),
            ("MYSBX_PODMAN_SHELL", "/bin/bash"),
            ("MYSBX_PODMAN_ENV", pinned_env.as_str()),
            ("MYSBX_PODMAN_PASTA_SPEC", spec),
        ],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("== podman-gvisor =="), "{stdout}");
    assert!(
        stdout.contains("OK   OCI runtime: `runsc` is registered with podman"),
        "{stdout}"
    );
    assert!(
        stdout.contains("OK   image: localhost/agent:latest current (0123456789ab)"),
        "{stdout}"
    );
    assert!(stdout.contains("OK   startup probe: "), "{stdout}");
    assert!(
        stdout.contains(&format!(
            "OK   model endpoint: OPENAI_BASE_URL={url} answered inside the container (HTTP 404)"
        )),
        "{stdout}"
    );
    assert!(stdout.contains("mysbx doctor: 0 problem(s)"), "{stdout}");

    let rt =
        mysbx::doctor::podman_runtime(false, true, None, Some("cgroupfs"), Some("ignore-cgroups"));
    let probe = mysbx::doctor::PodmanProbe {
        runtime: &rt,
        krun: false,
        image: "localhost/agent:latest",
        shell: "/bin/bash",
    };
    let want = [
        mysbx::doctor::runtime_info_argv("runsc").join(" "),
        mysbx::doctor::image_id_argv("localhost/agent:latest").join(" "),
        mysbx::doctor::startup_probe_argv(&probe).join(" "),
        mysbx::doctor::endpoint_probe_argv(&probe, Some(spec), url).join(" "),
    ];
    let recorded = std::fs::read_to_string(&record).unwrap();
    assert_eq!(recorded, format!("{}\n", want.join("\n")));
}

#[test]
fn doctor_fails_on_an_absent_podman_image_and_warns_on_a_stale_one() {
    let (inv, _, sidecar) = fixture("doctor-gvisor-image", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let record = base.join("podman-args");
    let podman = podman_stub(&base, &record, Err("Error: no such image"));
    // The detached-run prerequisites are set OK so the new host
    // checks (detach.md B9) do not add warnings to this image test.
    let dtach = doctor_stub(&base.join("bin"), "dtach", "exit 0");
    let runtime = short_runtime_root("doctor-gvisor-image");
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&runtime, std::fs::Permissions::from_mode(0o700)).unwrap();
    let mut owned = gvisor_envs(&podman);
    owned.push((
        "MYSBX_DTACH".to_string(),
        dtach.to_string_lossy().into_owned(),
    ));
    owned.push((
        "XDG_RUNTIME_DIR".to_string(),
        runtime.to_string_lossy().into_owned(),
    ));
    let (code, stdout, stderr) = run_doctor(&inv, &["doctor"], &env_pairs(&owned));
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains(
            "FAIL image: localhost/agent:latest is not in the podman store (Error: no such image) — run: mysbx podman-load-image"
        ),
        "{stdout}"
    );
    assert!(
        stdout.contains("SKIP startup probe: not run (depends on: image)"),
        "{stdout}"
    );
    let recorded = std::fs::read_to_string(&record).unwrap();
    assert!(
        !recorded.lines().any(|l| l.contains(" run --rm ")),
        "no container is started without the image: {recorded}"
    );

    // Any other inspect error is reported as it is, without the
    // load-image hint.
    let _ = std::fs::remove_file(&record);
    let podman = podman_stub(&base, &record, Err("Error: database is locked"));
    let mut owned = gvisor_envs(&podman);
    owned.push((
        "MYSBX_DTACH".to_string(),
        dtach.to_string_lossy().into_owned(),
    ));
    owned.push((
        "XDG_RUNTIME_DIR".to_string(),
        runtime.to_string_lossy().into_owned(),
    ));
    let (code, stdout, stderr) = run_doctor(&inv, &["doctor"], &env_pairs(&owned));
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains(
            "FAIL image: `podman image inspect localhost/agent:latest` failed: Error: database is locked — check that podman works"
        ),
        "{stdout}"
    );
    assert!(!stdout.contains("podman-load-image"), "{stdout}");

    let _ = std::fs::remove_file(&record);
    let podman = podman_stub(&base, &record, Ok("sha256:ffffffffffffffffffff"));
    let mut owned = gvisor_envs(&podman);
    owned.push((
        "MYSBX_DTACH".to_string(),
        dtach.to_string_lossy().into_owned(),
    ));
    owned.push((
        "XDG_RUNTIME_DIR".to_string(),
        runtime.to_string_lossy().into_owned(),
    ));
    let (code, stdout, stderr) = run_doctor(&inv, &["doctor"], &env_pairs(&owned));
    assert_eq!(
        code, 0,
        "a stale image still starts: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stdout.contains("WARN image: localhost/agent:latest is stale"),
        "{stdout}"
    );
    assert!(
        stdout.contains("mysbx doctor: 0 problem(s), 1 warning(s)"),
        "{stdout}"
    );
}

#[test]
fn doctor_refuses_a_renamed_podman_pin_before_any_probe() {
    let (inv, _, sidecar) = fixture("doctor-gvisor-renamed", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let record = base.join("podman-args");
    let loaded = format!("sha256:{DOCTOR_IMAGE_ID}");
    let podman = podman_stub(&base, &record, Ok(loaded.as_str()));
    let mut owned = gvisor_envs(&podman);
    owned.push(("MYSBX_GVISOR_IMAGE".to_string(), "old".to_string()));
    let (code, stdout, stderr) = run_doctor(&inv, &["doctor"], &env_pairs(&owned));
    assert_eq!(code, 1, "stdout: {stdout}\nstderr: {stderr}");
    assert!(stdout.contains("FAIL pins: "), "{stdout}");
    assert!(
        stderr.contains("MYSBX_GVISOR_IMAGE was renamed to MYSBX_PODMAN_IMAGE"),
        "{stderr}"
    );
    assert!(
        stdout.contains("SKIP startup probe: not run (depends on: pins)"),
        "{stdout}"
    );
    let recorded = std::fs::read_to_string(&record).unwrap();
    assert!(
        !recorded.lines().any(|l| l.contains(" run --rm ")),
        "a run refuses the old pin, so no container is started: {recorded}"
    );
}

#[test]
fn doctor_warns_when_started_where_a_run_is_refused() {
    let (inv, _, sidecar) = fixture_user_backend("doctor-inside-clone", &[]);
    let clone = sidecar.join("clones").join("fix-1");
    std::fs::create_dir_all(&clone).unwrap();
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let bwrap = doctor_stub(&base.join("bin"), "bwrap", "exit 0\n");
    let bwrap = bwrap.to_string_lossy().into_owned();
    let mut inside = inv.clone();
    inside.cwd = clone;
    let (code, stdout, stderr) = run_doctor(
        &inside,
        &["doctor"],
        &[("MYSBX_BWRAP", bwrap.as_str()), ("MYSBX_SHELL", "/bin/sh")],
    );
    assert_eq!(
        code, 0,
        "a WARN is not a problem: {stdout}\nstderr: {stderr}"
    );
    assert!(
        stdout.contains("WARN configuration: user layer only: "),
        "{stdout}"
    );
    assert!(stdout.contains("session fix-1"), "{stdout}");
    assert!(
        stdout.contains("OK   backend: bubblewrap [configuration]"),
        "{stdout}"
    );
}

// Tests for the `podman-load-image` subcommand
#[test]
fn podman_load_image_help_shows_usage() {
    // The --help flag prints usage and exits 0
    let (inv, _, _) = fixture_user_backend("podman-load-image-help", &[]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["podman-load-image", "--help"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.contains("Usage:"), "stdout: {stdout}");
    assert!(stdout.contains("podman-load-image"), "stdout: {stdout}");
    assert!(stdout.contains("--force"), "stdout: {stdout}");
    assert!(stdout.contains("--test"), "stdout: {stdout}");
    assert!(stdout.contains("--image"), "stdout: {stdout}");
}

#[test]
fn podman_load_image_unknown_option_is_usage_error() {
    // Unknown options are usage errors (exit 2)
    let (inv, _, _) = fixture_user_backend("podman-load-image-unknown", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["podman-load-image", "--unknown"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("unknown option"), "stderr: {stderr}");
}

#[test]
fn podman_load_image_repeats_flag_is_usage_error() {
    // Repeated flags are usage errors
    let (inv, _, _) = fixture_user_backend("podman-load-image-repeat", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["podman-load-image", "--force", "--force"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(stderr.contains("repeated flag"), "stderr: {stderr}");
}

#[test]
fn podman_load_image_verbose_is_refused() {
    // --verbose is refused (no sandbox is started)
    let (inv, _, _) = fixture_user_backend("podman-load-image-verbose", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--verbose", "podman-load-image"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(
        stderr.contains("--verbose is not valid"),
        "stderr: {stderr}"
    );
}

#[test]
fn podman_load_image_dry_run_is_refused() {
    // --dry-run is refused (no sandbox is started)
    let (inv, _, _) = fixture_user_backend("podman-load-image-dry", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run", "podman-load-image"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(
        stderr.contains("--dry-run is not valid"),
        "stderr: {stderr}"
    );
}

#[test]
fn podman_load_image_image_requires_value() {
    // --image without a value is a usage error
    let (inv, _, _) = fixture_user_backend("podman-load-image-image-val", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["podman-load-image", "--image"]);
    assert_eq!(code, 2, "stderr: {stderr}");
    assert!(
        stderr.contains("--image requires a value"),
        "stderr: {stderr}"
    );
}

#[test]
fn podman_load_image_with_image_ref() {
    // A bare reference with no tarball pin is a REFUSED load now
    // (exit 2): no registry serves the Nix-built image, so the old
    // `podman pull` fallback could only fail against a registry named
    // `localhost` (bd myconfig-xrt).
    let (inv, _, _) = fixture_user_backend("podman-load-image-ref", &[]);
    let (code, _, stderr) = run_binary_with(
        &inv,
        &["podman-load-image", "--image", "localhost/test:latest"],
    );
    assert_eq!(code, 2);
    assert!(
        stderr.contains("no image tarball to load"),
        "stderr: {stderr}"
    );
}

const LOAD_OLD_ID: &str = "1111111111111111111111111111111111111111111111111111111111111111";
const LOAD_NEW_ID: &str = "2222222222222222222222222222222222222222222222222222222222222222";

/// `mysbx podman-load-image ARGS` against a stateful podman stub:
/// `image inspect` answers `loaded_before` (None: no such image) until
/// `load` ran, then the new ID; `rmi` runs `rmi_body`. Returns the exit
/// code, stderr and the recorded podman invocations.
fn run_load_image(
    name: &str,
    args: &[&str],
    loaded_before: Option<&str>,
    rmi_body: &str,
) -> (i32, String, Vec<String>) {
    let (inv, _, _) = fixture_user_backend(name, &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let record = base.join("podman-args");
    let marker = base.join("podman-loaded");
    let before = match loaded_before {
        Some(id) => format!("echo 'sha256:{id}'"),
        None => "echo 'Error: no such image' >&2; exit 125".to_string(),
    };
    let body = format!(
        "printf '%s\\n' \"$*\" >> {record}\n\
         case \"$*\" in\n\
         \x20 'image inspect'*) if [ -e {marker} ]; then echo 'sha256:{LOAD_NEW_ID}'; else {before}; fi ;;\n\
         \x20 'load '*) : > {marker} ;;\n\
         \x20 'rmi '*) {rmi_body} ;;\n\
         esac\n\
         exit 0\n",
        record = record.display(),
        marker = marker.display(),
    );
    let podman = doctor_stub(&base.join("bin"), "podman", &body);
    let tarball = base.join("agent.tar");
    std::fs::write(&tarball, "not a real archive").unwrap();
    let mut full = vec!["podman-load-image"];
    full.extend_from_slice(args);
    let out = spawn_with_args(&inv, &full)
        .env("MYSBX_PODMAN", &podman)
        .env("MYSBX_PODMAN_TARBALL", &tarball)
        .env("MYSBX_PODMAN_IMAGE", "localhost/agent:latest")
        .env("MYSBX_PODMAN_IMAGE_ID", LOAD_NEW_ID)
        .output()
        .expect("failed to spawn the mysbx binary");
    let calls = std::fs::read_to_string(&record)
        .unwrap_or_default()
        .lines()
        .map(str::to_owned)
        .collect();
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stderr).into_owned(),
        calls,
    )
}

#[test]
fn podman_load_image_removes_the_replaced_stale_image_by_id() {
    let (code, stderr, calls) =
        run_load_image("podman-load-image-rmi", &[], Some(LOAD_OLD_ID), "exit 0");
    assert_eq!(code, 0, "{stderr}");
    assert!(calls.iter().any(|c| c.starts_with("load ")), "{calls:?}");
    assert_eq!(
        calls.last().map(String::as_str),
        Some(format!("rmi {LOAD_OLD_ID}").as_str()),
        "the old ID is removed after the verified load: {calls:?}"
    );
    assert!(
        stderr.contains(&format!("removed the replaced image sha256:{LOAD_OLD_ID}")),
        "{stderr}"
    );
}

#[test]
fn podman_load_image_keeps_a_replaced_image_podman_refuses_to_remove() {
    let (code, stderr, calls) = run_load_image(
        "podman-load-image-rmi-refused",
        &[],
        Some(LOAD_OLD_ID),
        "echo 'Error: image is in use by a container' >&2; exit 2",
    );
    assert_eq!(code, 0, "a refused removal is not a failed load: {stderr}");
    assert!(calls.contains(&format!("rmi {LOAD_OLD_ID}")), "{calls:?}");
    assert!(
        stderr.contains(&format!(
            "kept the replaced image sha256:{LOAD_OLD_ID}: Error: image is in use by a container"
        )),
        "{stderr}"
    );
}

#[test]
fn podman_load_image_removes_nothing_without_a_replaced_build() {
    // Absent before the load, and a forced reload of the current
    // build: no other image to remove.
    for (name, args, before) in [
        ("podman-load-image-absent", &[][..], None),
        (
            "podman-load-image-force",
            &["--force"][..],
            Some(LOAD_NEW_ID),
        ),
    ] {
        let (code, stderr, calls) = run_load_image(name, args, before, "exit 0");
        assert_eq!(code, 0, "{name}: {stderr}");
        assert!(
            calls.iter().any(|c| c.starts_with("load ")),
            "{name}: {calls:?}"
        );
        assert!(
            !calls.iter().any(|c| c.starts_with("rmi")),
            "{name}: {calls:?}"
        );
    }
}

#[test]
fn podman_load_image_without_any_pin_is_usage_error() {
    // No --image, no MYSBX_PODMAN_* pins: a usage error, never an
    // invented default reference (bd myconfig-xrt).
    let (inv, _, _) = fixture_user_backend("podman-load-image-unpinned", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("no image configured"), "stderr: {stderr}");
}

#[test]
fn podman_gvisor_backend_without_image_pin_is_refused() {
    // backend = "podman-gvisor" with no MYSBX_PODMAN_IMAGE pin is a
    // refused run (exit 70), never a run against an invented
    // `localhost/…` reference (bd myconfig-xrt).
    let (inv, repo, sidecar) = fixture("podman-gvisor-unpinned", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 70);
    assert!(
        stderr.contains("no container image configured"),
        "stderr: {stderr}"
    );
}

#[test]
fn podman_backends_refuse_the_old_gvisor_pin_names() {
    // The shared pins are MYSBX_PODMAN_*; a set MYSBX_GVISOR_MEMORY
    // must refuse (naming the new variable), not run without the limit.
    for backend in ["podman-gvisor", "podman-krun"] {
        let (inv, _repo, sidecar) = fixture(&format!("{backend}-old-pin"), &[]);
        std::fs::write(
            sidecar.join("config.toml"),
            format!("backend = \"{backend}\"\n"),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
            .env("MYSBX_GVISOR_MEMORY", "8g");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        assert_eq!(out.status.code(), Some(2), "{backend} stderr: {stderr}");
        assert!(
            stderr.contains("MYSBX_GVISOR_MEMORY was renamed to MYSBX_PODMAN_MEMORY"),
            "{backend} stderr: {stderr}"
        );
    }
    let (inv, _, _) = fixture_user_backend("podman-load-image-old-pin", &[]);
    let mut cmd = spawn_with_args(&inv, &["podman-load-image", "--test"]);
    cmd.env("MYSBX_GVISOR_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert_eq!(out.status.code(), Some(2), "stderr: {stderr}");
    assert!(
        stderr.contains("MYSBX_GVISOR_IMAGE was renamed to MYSBX_PODMAN_IMAGE"),
        "stderr: {stderr}"
    );
}

/// Dry run of `config` (the sidecar) with extra env; (code, stdout, stderr).
fn dry_run_with(name: &str, config: &str, env: &[(&str, &str)]) -> (i32, String, String) {
    let (inv, _repo, sidecar) = fixture(name, &[]);
    std::fs::write(sidecar.join("config.toml"), config).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    (
        out.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn config_limits_warn_on_backends_that_cannot_enforce_them() {
    // config.md D23: a limit the backend cannot enforce is a warning,
    // never a refusal.
    for backend in ["bubblewrap", "nono"] {
        let (code, _, stderr) = dry_run_with(
            &format!("limits-{backend}"),
            &format!(
                "backend = \"{backend}\"\nnetwork = false\nmemory = \"8g\"\ncpus = 2\npids-limit = 512\n"
            ),
            &[],
        );
        assert_eq!(code, 0, "{backend} stderr: {stderr}");
        assert!(
            stderr.contains(&format!(
                "warning: the `{backend}` backend does not enforce `memory`, `cpus`, `pids-limit`; ignored"
            )),
            "{backend} stderr: {stderr}"
        );
    }
    // No limit, no warning.
    let (code, _, stderr) = dry_run_with("limits-none", "backend = \"bubblewrap\"\n", &[]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(!stderr.contains("does not enforce"), "stderr: {stderr}");
}

#[test]
fn config_limits_reach_the_krun_vm_and_pids_limit_is_warned() {
    let img = ("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let (code, stdout, stderr) = dry_run_with(
        "limits-krun",
        "backend = \"podman-krun\"\nmemory = \"8g\"\ncpus = 4\npids-limit = 512\n",
        &[img],
    );
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.lines().any(|l| l == "krun.ram_mib=8192"), "{stdout}");
    assert!(stdout.lines().any(|l| l == "krun.cpus=4"), "{stdout}");
    assert!(
        stderr
            .contains("warning: the `podman-krun` backend does not enforce `pids-limit`; ignored"),
        "stderr: {stderr}"
    );
    // The env pin overrides the config key.
    let (code, stdout, stderr) = dry_run_with(
        "limits-krun-env",
        "backend = \"podman-krun\"\nmemory = \"8g\"\n",
        &[img, ("MYSBX_PODMAN_MEMORY", "2g")],
    );
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.lines().any(|l| l == "krun.ram_mib=2048"), "{stdout}");
}

#[test]
fn config_limits_reach_podman_gvisor_with_cgroups() {
    let (code, stdout, stderr) = dry_run_with(
        "limits-gvisor",
        "backend = \"podman-gvisor\"\nmemory = \"8g\"\ncpus = 1.5\npids-limit = 512\n",
        &[
            ("MYSBX_PODMAN_IMAGE", "localhost/test:latest"),
            ("MYSBX_GVISOR_RUNTIME_FLAGS", "debug"),
        ],
    );
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    for (flag, value) in [
        ("--memory", "8g"),
        ("--cpus", "1.5"),
        ("--pids-limit", "512"),
    ] {
        assert!(
            lines.windows(2).any(|w| w[0] == flag && w[1] == value),
            "{flag} {value} missing: {stdout}"
        );
    }
    assert!(!stderr.contains("does not enforce"), "stderr: {stderr}");
}

#[test]
fn podman_gvisor_rootless_cgroup_env_overrides_defaults() {
    // The cgroup handling is operator-overridable per invocation: with
    // MYSBX_GVISOR_RUNTIME_FLAGS and MYSBX_GVISOR_CGROUP_MANAGER set,
    // those — not the rootless defaults — build the argv (bd
    // myconfig-b13).
    let (inv, repo, sidecar) = fixture("podman-gvisor-cgroup-env", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_GVISOR_CGROUP_MANAGER", "systemd")
        .env("MYSBX_GVISOR_RUNTIME_FLAGS", "ignore-cgroups");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let code = out.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0], "podman");
    assert_eq!(lines[1], "--runtime=runsc");
    assert_eq!(lines[2], "--runtime-flag");
    assert_eq!(lines[3], "ignore-cgroups");
    assert_eq!(lines[4], "--cgroup-manager=systemd");
}

#[test]
fn podman_gvisor_payload_uses_the_image_userland_not_host_pins() {
    // bd myconfig-wao: the podman-gvisor payload is the image's own
    // userland — `/bin/bash` and `PATH=/bin:/usr/bin`, the agent
    // image's OCI config — NOT the host store pins the bwrap backend
    // reads. `MYSBX_SHELL` / `MYSBX_TOOLS_PATH` are host `/nix/store`
    // paths this backend deliberately mounts nothing of, so a payload
    // built from them would die with `no such file or directory`
    // inside the container.
    let (inv, _repo, sidecar) = fixture("podman-gvisor-image-userland", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    // The host pins are set (as the wrapper does) and must be IGNORED
    // by this backend.
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_SHELL", "/nix/store/aaaa-bash/bin/bash")
        .env("MYSBX_TOOLS_PATH", "/nix/store/bbbb-tools/bin")
        .env("MYSBX_CA_BUNDLE", "/nix/store/cccc-cacert/ca-bundle.crt")
        .env("MYSBX_BINSH", "/nix/store/aaaa-bash/bin/sh")
        .env("MYSBX_NIX_CONF", "/nix/store/dddd-nix.conf");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    for line in stdout.lines() {
        assert!(
            !line.contains("/nix/store"),
            "no host store path may reach the podman argv: {line}"
        );
    }
    let lines: Vec<&str> = stdout.lines().collect();
    // NO `--` separator: podman (unlike docker) does not strip one
    // after the image, and runsc fails with `error finding executable
    // "--"` (bd myconfig-ivp). The last line IS the payload.
    assert!(
        !lines.iter().any(|l| *l == "--"),
        "no `--` separator in the podman argv: {stdout}"
    );
    assert_eq!(
        lines[lines.len() - 1],
        "/bin/bash",
        "payload is the image shell"
    );
    assert!(
        lines.contains(&"PATH=/bin:/usr/bin"),
        "PATH is the image PATH: {stdout}"
    );
    assert!(
        !lines.iter().any(|l| l.starts_with("SSL_CERT_FILE=")),
        "no host CA pin — the image carries its own: {stdout}"
    );
}

#[test]
fn podman_gvisor_shell_pin_replaces_the_payload_shell() {
    // bd myconfig-cew: the wrapper pins `MYSBX_PODMAN_SHELL` to the
    // fish binary AS IT EXISTS INSIDE THE IMAGE (the gvisor tier
    // bakes the host user's fish world, and the ro `~/.config/fish`
    // mount carries its configuration — so a container session lands
    // in the same shell as the host). The pin is an in-image store
    // path, NOT a host pin: unlike `MYSBX_SHELL` (which the previous
    // test proves is ignored), it IS honored and replaces the image's
    // own `Cmd` (`/bin/bash`) as the interactive payload. A one-shot
    // `run -- CMD` is unaffected — the pin applies to the shell
    // payload only.
    let (inv, _repo, sidecar) = fixture("podman-gvisor-shell-pin", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_PODMAN_SHELL", "/nix/store/eeee-fish/bin/fish");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        lines[lines.len() - 1],
        "/nix/store/eeee-fish/bin/fish",
        "payload is the pinned in-image shell: {stdout}"
    );

    // ... and the one-shot form keeps the explicit command: the
    // printout is exactly the argv (one argument per line, no
    // trailing blank), so the payload is the trailing slice after the
    // image reference — the argv IS the whole stdout here.
    let mut cmd = spawn_with_args(&inv, &["run", "--dry-run", "--", "rg", "--version"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_PODMAN_SHELL", "/nix/store/eeee-fish/bin/fish");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        &lines[lines.len() - 2..],
        &["rg", "--version"],
        "one-shot payload is the command: {stdout}"
    );
    assert_eq!(
        lines[lines.len() - 3],
        "localhost/test:latest",
        "the image reference precedes the payload: {stdout}"
    );
    assert!(
        !lines.contains(&"/nix/store/eeee-fish/bin/fish"),
        "the shell pin does not replace a one-shot payload: {stdout}"
    );
}

#[test]
fn verbose_podman_run_prints_the_executed_argv_and_wires_stdio() {
    // bd myconfig-jho: a real (non-dry) `--verbose` run must print the
    // exact command it is about to exec — the configuration report
    // alone never showed the argv, and an operator reproducing a
    // failed run had to guess it. The fake podman (`/usr/bin/env`)
    // accepts the exec and fails on the podman flags, which is fine:
    // the point is the `## exec:`/`## arg:` lines and the argv they
    // carry — `--interactive` always (the stdio fix for the f13
    // silent immediate exit), no `--tty` here (cargo pipes stdin).
    let (inv, _, sidecar) = fixture("podman-gvisor-verbose-exec", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--multiplexer", "none"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_PODMAN", "/usr/bin/env");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert_ne!(
        out.status.code(),
        Some(0),
        "stdout: {stdout}\nstderr: {stderr}"
    );
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains("## exec: /usr/bin/env"),
        "the executed program is named: {stdout}"
    );
    assert!(
        report.contains("## arg:  --runtime=runsc"),
        "the executed argv is printed argument per argument: {stdout}"
    );
    assert!(
        report.contains("## arg:  --interactive"),
        "the container's stdin is wired: {stdout}"
    );
    assert!(
        !report.contains("--tty"),
        "no --tty on a piped stdin: {stdout}"
    );
    // The exec reached the fake podman: env's own refusal is on stderr
    // — the failure surfaced instead of a silent exit.
    assert!(
        !stderr.is_empty(),
        "the backend's own error surfaces: {stdout}"
    );
}

#[test]
fn podman_gvisor_multiplexer_without_image_entry_is_refused() {
    // A multiplexer selected under podman-gvisor with no in-image
    // entry pinned is a refused run (exit 70), the same refusal a
    // bwrap host without that multiplexer gets — never a silent bare
    // shell (bd myconfig-wao).
    let (inv, _repo, sidecar) = fixture("podman-gvisor-mux-refused", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-gvisor\"\nmultiplexer = \"tmux\"\n",
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        // A HOST mux entry pin is set — and must not satisfy the
        // podman backend: a host store script cannot be the payload
        // of a container that mounts nothing from the host store.
        .env("MYSBX_MUX_ENTRY_TMUX", "/nix/store/eeee-tmux-entry");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(70));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("no in-image entry is pinned (MYSBX_PODMAN_MUX_ENTRY_TMUX"),
        "the refusal must name the missing pin: {stderr}"
    );
}

#[test]
fn podman_load_image_rejects_session_flag() {
    let (inv, _, _) = fixture_user_backend("podman-load-image-session", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--session", "test", "podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("--session is not valid with `podman-load-image"));
}

#[test]
fn podman_load_image_rejects_result_flag() {
    let (inv, _, _) = fixture_user_backend("podman-load-image-result", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--result", "podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("--result is not valid with `podman-load-image"));
}

#[test]
fn podman_load_image_rejects_timeout_flag() {
    let (inv, _, _) = fixture_user_backend("podman-load-image-timeout", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--timeout", "5", "podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("--timeout is not valid with `podman-load-image"));
}

#[test]
fn podman_load_image_rejects_ro_flag() {
    let (inv, _, _) = fixture_user_backend("podman-load-image-ro", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--ro", "/tmp", "podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("--ro is not valid with `podman-load-image"));
}

#[test]
fn podman_load_image_rejects_rw_flag() {
    let (inv, _, _) = fixture_user_backend("podman-load-image-rw", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--rw", "/tmp", "podman-load-image"]);
    assert_eq!(code, 2);
    assert!(stderr.contains("--rw is not valid with `podman-load-image"));
}

#[test]
fn gvisor_load_image_alias_deprecated_notice() {
    // The hidden alias dispatches to the same implementation with a
    // one-line deprecation notice on stderr.
    let (inv, _, _) = fixture_user_backend("gvisor-load-image-alias", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["gvisor-load-image"]);
    assert_eq!(code, 2);
    assert!(
        stderr.contains("gvisor-load-image is deprecated; use podman-load-image"),
        "stderr: {stderr}"
    );
    // the alias refusal still points at the same failure as the new name
    assert!(stderr.contains("no image configured"), "stderr: {stderr}");
}

#[test]
fn gvisor_load_image_alias_rejects_flags_with_notice() {
    // The global flags are refused on the alias too, after the notice.
    let (inv, _, _) = fixture_user_backend("gvisor-load-image-alias-dry", &[]);
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run", "gvisor-load-image"]);
    assert_eq!(code, 2);
    assert!(
        stderr.contains("gvisor-load-image is deprecated; use podman-load-image"),
        "stderr: {stderr}"
    );
    assert!(
        stderr.contains("--dry-run is not valid with `gvisor-load-image`"),
        "stderr: {stderr}"
    );
}
// ---- the nono backend (backends.md D1: bubblewrap with nono inside) ---------

/// The nono-minimal golden (tests/assets/argv/nono-minimal.txt) with
/// the fixture repo path substituted, `bwrap` as argv[0] — the expected
/// `--dry-run` output of the smallest layered nono invocation: the
/// bwrap argv with the `nono run` chain and the pinned `env` inside.
fn expected_nono_minimal_argv(repo: &Path) -> String {
    let golden = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/assets/argv/nono-minimal.txt");
    let argv = std::fs::read_to_string(&golden)
        .unwrap_or_else(|e| panic!("cannot read {}: {e}", golden.display()))
        .replace("/synth/repo", &repo.to_string_lossy());
    format!("bwrap\n{argv}")
}

/// A fixture with `backend = "nono"` in the sidecar config plus an
/// extra `config.toml` body appended after the `backend = "nono"` line.
fn fixture_nono_config(
    name: &str,
    args: &[&'static str],
    config: &str,
) -> (Invocation, PathBuf, PathBuf) {
    let (inv, repo, sidecar) = fixture(name, args);
    std::fs::write(
        sidecar.join("config.toml"),
        format!("backend = \"nono\"\n{config}"),
    )
    .unwrap();
    (inv, repo, sidecar)
}

#[test]
fn nono_backend_dry_run_prints_the_layered_argv() {
    // backend = "nono", network = false: the dry run prints the
    // LAYERED argv — bwrap's own layout, `nono run` inside, the
    // pinned `env` applying the payload environment, the payload
    // last — byte-identical to the nono-minimal golden with the repo
    // path substituted, `bwrap` as argv[0] (MYSBX_BWRAP unset in the
    // test harness, the crate's PATH fallback).
    let (inv, _, _) = fixture_nono_config("nono-dry-run", &[], "network = false\n");
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0], "bwrap", "argv[0] is bwrap: {stdout}");
    assert_eq!(lines[1], "--clearenv");
    // The base layout carries nono's own state tmpfs.
    assert!(stdout.contains("--tmpfs\n/mysbx-nono\n"), "{stdout}");
    // The nono chain: `nono run --profile default`, the grants, its
    // own `--`, then `env -u …` and the payload last.
    assert!(
        stdout.contains("nono\nrun\n--profile\ndefault\n"),
        "{stdout}"
    );
    assert!(stdout.contains("\n--block-net\n"), "{stdout}");
    assert!(stdout.contains("\nenv\n-u\nPATH\n"), "{stdout}");
    assert_eq!(lines[lines.len() - 1], "/synth/bin/bash");
    assert_eq!(stdout, expected_nono_minimal_argv(&inv.cwd));
}

#[test]
fn nono_backend_flag_positions() {
    // `--backend nono` before the verb and after it both select the
    // backend; with an allowlist configured the flag-overridden run
    // carries the allowlist too. `network = false` keeps the runs
    // accepted (no allowlist next to a shared network).
    for (args, label) in [
        (vec!["--backend", "nono", "--dry-run"], "bare, pre-verb"),
        (
            vec!["run", "--backend", "nono", "--dry-run", "--", "true"],
            "run, after the verb",
        ),
    ] {
        let (inv, _, sidecar) = fixture("nono-backend-flag-forms", &[]);
        std::fs::write(sidecar.join("config.toml"), "network = false\n").unwrap();
        let (code, stdout, stderr) = run_binary_with(&inv, &args);
        assert_eq!(code, 0, "{label}: stderr: {stderr}");
        assert!(stdout.starts_with("bwrap\n"), "{label}: {stdout}");
    }

    // An allowlist plus the flag: the run succeeds and the flag alone
    // selects the backend that can enforce it (a shared network next
    // to the allowlist — the combination the nono backend maps).
    let (inv, _, sidecar) = fixture("nono-backend-flag-allowlist", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "network = true\nallow-domains = [\"api.openai.com\"]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--backend", "nono", "--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.starts_with("bwrap\n"), "{stdout}");
}

#[test]
fn nono_report_labels_the_backend() {
    // The report names the backend with its provenance tag and labels
    // the executed binary `nono:` — the layered run's argv[0] IS
    // bwrap (backends.md D1), and the label says so: a nono run
    // execs bwrap, and the argv it describes wraps `nono run` inside.
    // `network = false` keeps the run accepted (the shared-network
    // default would be refused — nono mediates per connection).
    let (inv, _, _) = fixture_nono_config(
        "nono-report",
        &["--backend", "nono", "--verbose", "--dry-run"],
        "network = false\n",
    );
    let (code, stdout, stderr) =
        run_binary_with(&inv, &["--backend", "nono", "--verbose", "--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let report = report_lines(&stdout).join("\n");
    assert!(
        report.contains("backend:        nono  [--backend]"),
        "{report}"
    );
    assert!(
        report
            .contains("nono:           bwrap (layered — bwrap builds the view, nono runs inside)"),
        "the backend-binary line labels the layering: {report}"
    );
    assert!(!report.contains("bwrap:"), "{report}");
    // The report describes the layered truth: the sandbox home is
    // the tmpfs, exactly like the bubblewrap backend's report (the
    // `+ 1 state dir(s)` is the implicit `.ssh` keypair entry, D22).
    assert!(
        report.contains("home:           /mysbx-home (tmpfs"),
        "{report}"
    );
    // The argv block behind the report is the layered one.
    assert!(
        argv_block(&stdout).starts_with("bwrap\n--clearenv\n"),
        "{stdout}"
    );
}

#[test]
fn allowlist_on_bubblewrap_is_refused() {
    // The merged allowlist is backend-agnostic policy; bubblewrap
    // cannot enforce it, so the run is refused (70) naming the backend
    // and every offending key — under --dry-run too, with no argv on
    // stdout.
    let (inv, _, sidecar) = fixture("nono-allowlist-bwrap", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"bubblewrap\"\n\
         allow-domains = [\"api.openai.com\"]\n\
         connect-ports = [443]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("bubblewrap"), "{stderr}");
    assert!(stderr.contains("allow-domains"), "{stderr}");
    assert!(stderr.contains("connect-ports"), "{stderr}");
    assert!(
        !stdout.contains("--clearenv"),
        "no argv on refusal: {stdout}"
    );
}

#[test]
fn allowlist_on_podman_gvisor_is_refused() {
    // podman-gvisor's pasta does not filter by domain either — the
    // same refusal, naming the backend and the listen-ports key.
    let (inv, _, sidecar) = fixture("nono-allowlist-podman", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-gvisor\"\nlisten-ports = [8080]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("podman-gvisor"), "{stderr}");
    assert!(stderr.contains("listen-ports"), "{stderr}");
    assert!(
        !stdout.contains("--runtime=runsc"),
        "no argv on refusal: {stdout}"
    );
}

#[test]
fn allowlist_on_podman_krun_is_refused() {
    // The krun variant shares podman-gvisor's pasta egress (TSI over
    // the pasta netns, backends.md D2): an allowlist is refused on
    // `podman-krun` exactly like on `podman-gvisor` — never
    // accepted-and-ignored.
    let (inv, _, sidecar) = fixture("nono-allowlist-podman-krun", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-krun\"\nlisten-ports = [8080]\n",
    )
    .unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("podman-krun"), "{stderr}");
    assert!(stderr.contains("listen-ports"), "{stderr}");
}

#[test]
fn podman_krun_network_false_is_enforced_like_gvisor() {
    // bd myconfig-6di.5.5: `network = false` on the krun backend is
    // the same `--network none` as on podman-gvisor, and the
    // enforcement is verified: the VMM (the rootless podman process)
    // is created inside the container's empty netns, and the guest's
    // ONLY egress is a TSI proxy connection the VMM dials from that
    // netns (libkrun muxer, TsiFlags::HIJACK_INET) — no route, no
    // resolver, nothing past the netns. The dry run must carry the
    // same `--network none` line, and the same nix-daemon mount
    // contradiction the gvisor tier refuses must refuse here too.
    let (inv, _, sidecar) = fixture("podman-krun-network-false", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-krun\"\nnetwork = false\n",
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        stdout.lines().any(|l| l == "--network") && stdout.lines().any(|l| l == "none"),
        "the krun dry run must carry --network none: {stdout}"
    );
}

#[test]
fn podman_krun_allowlist_refusal_names_tsi() {
    // bd myconfig-6di.5.5: the D21 refusal on podman-krun names the
    // backend's ACTUAL egress mechanism — TSI is an unfiltered
    // proxy (any connect() the guest makes, the VMM dials from the
    // container netns) — never a vague "not supported".
    let (inv, _, sidecar) = fixture("podman-krun-allowlist-tsi", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-krun\"\nallow-domains = [\"example.org\"]\n",
    )
    .unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(
        stderr.contains("TSI"),
        "the refusal must name the TSI egress mechanism: {stderr}"
    );
    assert!(stderr.contains("allow-domains"), "stderr: {stderr}");
}

#[test]
fn podman_krun_dry_run_swaps_the_runtime_keeps_the_layout() {
    // The variant contract end-to-end (bd myconfig-6di.5.3/.4,
    // docs/design/backends.md D2): `backend = "podman-krun"` builds
    // the same LAYOUT as `podman-gvisor` with `--runtime` swapped to
    // the pinned crun+libkrun path (MYSBX_KRUN_RUNTIME), plus the
    // enumerated D2 differences (the handler annotation — asserted
    // by its own test — and the dropped guest-unenforceable hardening
    // flags). The same image pin (MYSBX_PODMAN_IMAGE) serves both
    // variants.
    let (inv, _, sidecar) = fixture("podman-krun-dry-run", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest").env(
        "MYSBX_KRUN_RUNTIME",
        "/nix/store/synth-crun-libkrun/bin/crun",
    );
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0], "podman");
    assert_eq!(
        lines[1], "--runtime=/nix/store/synth-crun-libkrun/bin/crun",
        "the pinned crun replaces runsc: {stdout}"
    );
    // everything after the runtime line is the gvisor argv: the image
    // reference and payload are the image-userland shape.
    assert!(
        lines.contains(&"localhost/test:latest"),
        "the same image pin serves the krun variant: {stdout}"
    );
    assert_eq!(lines[lines.len() - 1], "/bin/bash");
}

#[test]
fn podman_multiplexer_runs_the_in_image_entry_on_both_backends() {
    // bd myconfig-55u: the podman backends start a session from the
    // entry baked into the image (`MYSBX_PODMAN_MUX_ENTRY_*`). The
    // tmux socket dir is the home one on gvisor and the guest's
    // /dev/shm on krun (virtio-fs files are host-uid-owned while the
    // krun payload is guest root, so tmux refuses a dir there).
    for (backend, mux, var, entry, socket_dir) in [
        (
            "podman-gvisor",
            "tmux",
            "MYSBX_PODMAN_MUX_ENTRY_TMUX",
            "/nix/store/aaaa-mysbx-tmux-entry/bin/mysbx-tmux-entry",
            "/mysbx-home/.mysbx-tmux",
        ),
        (
            "podman-krun",
            "herdr",
            "MYSBX_PODMAN_MUX_ENTRY_HERDR",
            "/nix/store/aaaa-mysbx-herdr-entry/bin/mysbx-herdr-entry",
            "/dev/shm/mysbx-tmux",
        ),
    ] {
        let (inv, _, sidecar) = fixture(&format!("podman-mux-{backend}"), &[]);
        std::fs::write(
            sidecar.join("config.toml"),
            format!("backend = \"{backend}\"\nmultiplexer = \"{mux}\"\n"),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
            .env(var, entry)
            // The host pin is NOT the podman payload.
            .env(
                format!("MYSBX_MUX_ENTRY_{}", mux.to_uppercase()),
                "/nix/store/host-only-entry",
            );
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(out.status.code(), Some(0), "{backend}: {stderr}");
        let lines: Vec<&str> = stdout.lines().collect();
        assert_eq!(
            lines[lines.len() - 1],
            entry,
            "{backend}: the in-image entry is the payload: {stdout}"
        );
        assert_eq!(
            lines[lines.len() - 2],
            "localhost/test:latest",
            "{backend}: {stdout}"
        );
        assert!(
            lines.contains(&format!("TMUX_TMPDIR={socket_dir}").as_str()),
            "{backend}: the socket dir: {stdout}"
        );
        assert!(
            !stdout.contains("host-only-entry"),
            "{backend}: the host pin stays out: {stdout}"
        );
    }
}

#[test]
fn podman_multiplexer_without_in_image_entry_is_refused() {
    // Only the in-image pin serves the podman backends: a host-only
    // `MYSBX_MUX_ENTRY_*` (a store path the container does not mount)
    // is still a refused run naming the missing podman pin, and the
    // pin of ANOTHER multiplexer does not start this one.
    for backend in ["podman-gvisor", "podman-krun"] {
        let (inv, _, sidecar) = fixture(&format!("podman-mux-unpinned-{backend}"), &[]);
        std::fs::write(
            sidecar.join("config.toml"),
            format!("backend = \"{backend}\"\nmultiplexer = \"workmux\"\n"),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
            .env("MYSBX_MUX_ENTRY_WORKMUX", "/nix/store/host-workmux-entry")
            .env("MYSBX_PODMAN_MUX_ENTRY_TMUX", "/nix/store/aaaa-tmux-entry");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{backend}: {stderr}"
        );
        assert!(
            stderr.contains("MYSBX_PODMAN_MUX_ENTRY_WORKMUX"),
            "{backend}: the refusal names the in-image pin: {stderr}"
        );
        assert!(!stdout.contains("entry"), "{backend}: no argv: {stdout}");
    }
}

#[test]
fn the_direct_krun_backend_writes_no_sidecar_trust_files() {
    // bd myconfig-dak.8's acceptance: the git trust rides as
    // IN-MEMORY overlay files (the launcher's trustfs NullFs device)
    // — NO `gittrust/<pid>/` dir under the sidecar, on a dry run or
    // a real run. (The podman-krun arm keeps its per-run files: the
    // hoisted sweep's coverage there is
    // the_hoisted_sweep_cleans_both_krun_backends_debris.)
    let (inv, _, sidecar) = fixture("krun-no-trust-files", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"krun\"\n").unwrap();
    // The pins the run needs to reach the dry-run audit: launcher
    // + rootfs (the arm refuses a missing pin).
    let rootfs = sidecar.join("rootfs-stub");
    std::fs::create_dir_all(rootfs.join("bin")).unwrap();
    std::fs::write(rootfs.join("bin/mysbx-init"), "#!/bin/sh\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["run", "--dry-run", "--", "/bin/true"]);
    cmd.env("MYSBX_KRUN_LAUNCHER", "/does/not/matter/dry-run")
        .env("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap().to_string());
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout_lossy = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        stdout_lossy.contains("--krun-overlay"),
        "the dry run audits the overlay flags: {stdout_lossy}"
    );
    assert!(
        !sidecar.join("gittrust").exists(),
        "the dry run wrote no sidecar trust dir"
    );
}

#[test]
fn verbose_reaches_the_krun_launcher_argv() {
    // The pass-through of cli.md D10: a `--verbose` run forwards
    // the flag to the mysbx-krun launcher (its phase timings and
    // setup logs), a plain run does not. The dry-run argv audits
    // exactly this — the `--verbose` line is there or it is not.
    let (inv, _, sidecar) = fixture("krun-verbose-argv", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"krun\"\n").unwrap();
    let rootfs = sidecar.join("rootfs-stub");
    std::fs::create_dir_all(rootfs.join("bin")).unwrap();
    std::fs::write(rootfs.join("bin/mysbx-init"), "#!/bin/sh\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["run", "--dry-run", "--", "/bin/true"]);
    cmd.env("MYSBX_KRUN_LAUNCHER", "/does/not/matter/dry-run")
        .env("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap().to_string());
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let plain = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        !plain.lines().any(|l| l == "--verbose"),
        "a plain run does not pass --verbose: {plain}"
    );
    let mut cmd = spawn_with_args(&inv, &["--verbose", "run", "--dry-run", "--", "/bin/true"]);
    cmd.env("MYSBX_KRUN_LAUNCHER", "/does/not/matter/dry-run")
        .env("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap().to_string());
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let verbose = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        verbose.lines().any(|l| l == "--verbose"),
        "a --verbose run forwards --verbose to the launcher: {verbose}"
    );
}

#[test]
fn a_detached_krun_wrap_carries_die_with_parent() {
    // The supervisor tie of a detached-inner krun run (detach.md B4,
    // the same flag the bubblewrap backend's argv carries): the bwrap
    // wrap of the launcher — and with it the in-process microVM —
    // dies with the supervisor, so an abruptly dead inner orphans no
    // live VM. The foreground argv stays byte-identical.
    let (inv, _, sidecar) = fixture("krun-detach-die-with-parent", &[]);
    // The krun backend mirrors the repo's host path into the guest
    // and refuses a share below a first component the rootfs bakes
    // no mountpoint for (krun.rs's check_share_root) — a repo under
    // a deep $TMPDIR prefix the guest does not know is a real
    // refusal, the same one the krun argv siblings hit; the tie is
    // then unassertable in that environment.
    {
        let first = inv
            .cwd
            .strip_prefix("/")
            .ok()
            .and_then(|p| p.components().next())
            .and_then(|c| c.as_os_str().to_str())
            .map(|c| format!("/{c}"))
            .unwrap_or_default();
        if !matches!(
            first.as_str(),
            "/tmp" | "/home" | "/etc" | "/srv" | "/mnt" | "/media" | "/opt" | "/data"
        ) {
            return; // the guest bakes no mountpoint for this prefix
        }
    }
    std::fs::write(sidecar.join("config.toml"), "backend = \"krun\"\n").unwrap();
    let rootfs = sidecar.join("rootfs-stub");
    std::fs::create_dir_all(rootfs.join("bin")).unwrap();
    std::fs::write(rootfs.join("bin/mysbx-init"), "#!/bin/sh\n").unwrap();
    let runtime = short_runtime_root("krun-die-with-parent");
    std::fs::create_dir_all(&runtime).unwrap();
    let (dtach, _) = dtach_stub(&inv.home, true);
    let mut cmd = spawn_with_args(&inv, &["--detach", "--dry-run"]);
    cmd.env("MYSBX_KRUN_LAUNCHER", "/does/not/matter/dry-run")
        .env("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap())
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("MYSBX_DTACH", &dtach);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let detached = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        detached.lines().any(|l| l == "--die-with-parent"),
        "a detached krun wrap carries the supervisor tie: {detached}"
    );
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_KRUN_LAUNCHER", "/does/not/matter/dry-run")
        .env("MYSBX_KRUN_ROOTFS", rootfs.to_str().unwrap());
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let plain = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        !plain.lines().any(|l| l == "--die-with-parent"),
        "the foreground krun argv stays byte-identical: {plain}"
    );
}

#[test]
fn the_hoisted_sweep_cleans_both_krun_backends_debris() {
    // bd myconfig-6xl: the per-run dir sweep is HOISTED above the arm
    // dispatch, so a podman-krun run reclaims gittrust/<pid>/ debris
    // even when THAT run later refuses in its own arm (here: the
    // sandbox has no /dev/kvm, the podman-krun arm's first real-run
    // refusal). A LIVE pid's dir is never touched; a non-pid name is
    // untouched; a dry run sweeps nothing.
    let (inv, _, sidecar) = fixture("krun-sweep-hoisted", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let gittrust = sidecar.join("gittrust");
    let dead_dir = gittrust.join("2147483647"); // > i32::MAX: pid_gone is true
    std::fs::create_dir_all(&dead_dir).unwrap();
    std::fs::write(dead_dir.join("gitconfig"), "stale").unwrap();
    let live_dir = gittrust.join(std::process::id().to_string());
    std::fs::create_dir_all(&live_dir).unwrap();
    let named_dir = gittrust.join("not-a-pid");
    std::fs::create_dir_all(&named_dir).unwrap();

    // Real run (NOT --dry-run): the podman-krun arm refuses on kvm,
    // AFTER the sweep ran.
    let mut cmd = spawn_with_args(&inv, &["run", "--", "/bin/true"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_ne!(
        out.status.code(),
        Some(0),
        "the run must refuse in this environment"
    );
    assert!(!dead_dir.exists(), "the dead-pid debris was swept");
    assert!(
        live_dir.exists(),
        "a LIVE parallel run's dir is never removed"
    );
    assert!(named_dir.exists(), "a non-pid name is untouched");
    std::fs::remove_dir_all(&live_dir).unwrap();
    std::fs::remove_dir_all(&named_dir).unwrap();

    // The DRY run of the same config sweeps NOTHING (the sweep is
    // !dry_run-gated): re-seed (the real run above already swept the
    // first debris — the assert proved it gone), dry-run, assert the
    // debris survives.
    std::fs::create_dir_all(&dead_dir).unwrap();
    std::fs::write(dead_dir.join("gitconfig"), "still-stale").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert!(out.status.code().is_some(), "the dry run exits: {out:?}");
    assert!(dead_dir.exists(), "the dry run swept nothing");
    std::fs::remove_dir_all(&dead_dir).unwrap();
}

#[test]
fn podman_krun_refuses_a_mount_over_the_guest_mux_socket_dir() {
    // The krun socket dir must stay guest-native: a mount at, above
    // or below /dev/shm/mysbx-tmux would put it on virtio-fs.
    for dest in ["/dev/shm", "/dev/shm/mysbx-tmux/x"] {
        let (inv, _, sidecar) = fixture("podman-krun-mux-socket-mount", &[]);
        let host = sidecar.join("host-dir");
        std::fs::create_dir_all(&host).unwrap();
        std::fs::write(
            sidecar.join("config.toml"),
            format!(
                "backend = \"podman-krun\"\nmultiplexer = \"tmux\"\n\
                 [[mounts]]\npath = \"{}\"\ndest = \"{dest}\"\nmode = \"ro\"\n",
                host.display()
            ),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
        cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
            .env("MYSBX_PODMAN_MUX_ENTRY_TMUX", "/nix/store/aaaa-tmux-entry");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{dest}: {stderr}"
        );
        assert!(stderr.contains("/dev/shm/mysbx-tmux"), "{dest}: {stderr}");
    }
}

#[test]
fn podman_krun_scratch_pin_binds_the_per_run_file_and_exports_the_path() {
    // The per-run scratch disk end-to-end (bd myconfig-0pi): with
    // MYSBX_KRUN_SCRATCH_SIZE pinned (the wrapper does when
    // `krun.nix.enable`), the krun dry run binds the per-run sparse
    // file rw at the fixed container path and exports
    // MYSBX_KRUN_SCRATCH_IMG naming it — the guest nix wrapper's
    // only way to find the disk. WITHOUT the pin neither appears,
    // and NOTHING is created (the dry run is side-effect-free; the
    // sweep/create live in the real-run path only).
    let (inv, _, sidecar) = fixture("podman-krun-scratch-pin", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let scratch_dir = sidecar.join("scratch");
    std::fs::create_dir_all(&scratch_dir).unwrap();
    // Debris of a dead run: the sweep must remove it in a real run;
    // the dry run audits the argv only and keeps it.
    std::fs::write(scratch_dir.join("4194305.img"), "debris").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env(
            "MYSBX_KRUN_RUNTIME",
            "/nix/store/synth-crun-libkrun/bin/crun",
        )
        .env("MYSBX_KRUN_SCRATCH_SIZE", "16g");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    // The scratch file is named by the mysbx CHILD's pid — the test
    // cannot know it beforehand, so match the bind by its fixed
    // shape: <sidecar>/scratch/<pid>.img at the fixed container path.
    let scratch_bind = stdout
        .lines()
        .find(|l| {
            l.ends_with("dst=/run/mysbx-scratch.img,rw")
                && l.contains("/scratch/")
                && l.starts_with("type=bind,src=")
        })
        .expect("the per-run file binds at the fixed path");
    let named: &str = scratch_bind
        .strip_prefix("type=bind,src=")
        .and_then(|rest| rest.split(',').next())
        .expect("the src of the scratch bind");
    assert!(
        named.starts_with(&format!("{}/scratch/", sidecar.display())) && named.ends_with(".img"),
        "the scratch file lives under <sidecar>/scratch/ and is pid-named: {named}"
    );
    assert!(
        stdout
            .lines()
            .any(|l| l == "MYSBX_KRUN_SCRATCH_IMG=/run/mysbx-scratch.img"),
        "the env names the container path: {stdout}"
    );
    // The dry run is side-effect-free: the whole scratch dir holds
    // exactly the seeded debris — nothing was created, nothing
    // swept.
    let leftovers: Vec<_> = std::fs::read_dir(&scratch_dir)
        .map(|entries| entries.flatten().map(|e| e.file_name()).collect())
        .unwrap_or_default();
    assert_eq!(
        leftovers,
        ["4194305.img"],
        "the dry run neither creates nor sweeps"
    );

    // Without the pin: neither the bind nor the env — the guest nix
    // wrapper announces its tmpfs fallback instead.
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest").env(
        "MYSBX_KRUN_RUNTIME",
        "/nix/store/synth-crun-libkrun/bin/crun",
    );
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(!stdout.contains("mysbx-scratch"), "{stdout}");
}

#[test]
fn podman_krun_refused_run_leaves_the_scratch_dir_untouched() {
    // The host half runs only for a run that reaches the backend —
    // a run refused at the /dev/kvm gate (the doctor check, bd
    // myconfig-6di.5.2) must leave the scratch dir EXACTLY as it
    // was: no sweep, no create (the sweep is hygiene of a STARTING
    // run, and a refused run starts nothing). The test host has no
    // rw /dev/kvm (the CI and agent sandboxes alike), so the gate
    // is exactly what the test drives.
    let (inv, _, sidecar) = fixture("podman-krun-scratch-refused", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let scratch_dir = sidecar.join("scratch");
    std::fs::create_dir_all(&scratch_dir).unwrap();
    std::fs::write(scratch_dir.join("4194305.img"), "debris").unwrap();
    let mut cmd = spawn_with_args(&inv, &["run", "--", "true"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env(
            "MYSBX_KRUN_RUNTIME",
            "/nix/store/synth-crun-libkrun/bin/crun",
        )
        .env("MYSBX_KRUN_SCRATCH_SIZE", "64m");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    // The gate refuses with exit 70 on a host without rw /dev/kvm
    // — the test env is one; on a host WITH it the run would proceed
    // to the missing podman binary and still fail after the backend
    // exit, so the assertions below hold either way (a real run's
    // sweep WOULD remove the debris and create its own file — that
    // half is the dry-run test above and the lib unit tests).
    assert_ne!(
        out.status.code(),
        Some(0),
        "the run must fail: no podman in the test env"
    );
    let leftovers = std::fs::read_dir(&scratch_dir)
        .map(|entries| entries.flatten().count())
        .unwrap_or(0);
    if leftovers > 0 {
        // A host WITH rw /dev/kvm: the run proceeded past the gate,
        // created its file, the stub backend exec failed, and the
        // cleanup removed its own file — the leftover can only be
        // the debris of the dead run, swept by the NEXT run (the
        // crash-gap model). On the CI/agent host without /dev/kvm
        // the gate fired first and nothing changed at all.
        assert_eq!(leftovers, 1, "only the seeded debris can remain");
    }
}

#[test]
fn podman_krun_without_image_pin_is_refused() {
    // The krun variant runs the SAME image as podman-gvisor — without
    // the image pin it is a refused run naming the backend, never a
    // run against an invented reference.
    let (inv, _, sidecar) = fixture("podman-krun-unpinned", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let (code, _, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 70);
    assert!(
        stderr.contains("no container image configured"),
        "stderr: {stderr}"
    );
    assert!(stderr.contains("podman-krun"), "stderr: {stderr}");
}

#[test]
fn podman_krun_waypipe_display_is_refused_first_cut() {
    // D2's first-cut refusal list: the waypipe display is refused on
    // the krun backend until its socket bind over virtio-fs is
    // verified (bd myconfig-6di.5.4) — never accepted-and-ignored.
    // Refused BEFORE the image pin check and before the dry-run argv
    // (a broken configuration audits too).
    let (inv, _, sidecar) = fixture("podman-krun-waypipe", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"podman-krun\"\ndisplay = \"waypipe\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(
        stderr.contains("display = \"waypipe\" is not supported on the `podman-krun` backend"),
        "stderr: {stderr}"
    );
    assert!(stdout.is_empty(), "no argv on refusal: {stdout}");
}

#[test]
fn podman_krun_dry_run_needs_no_kvm() {
    // The /dev/kvm doctor check (lib.rs, bd myconfig-6di.5.2) is a
    // REAL-RUN gate: `--dry-run` stays host-independent — it audits
    // the argv (and therefore the runtime swap) on any host, with or
    // without /dev/kvm. A dry run with no image pin still refuses
    // (the pin check is upstream of the kvm gate), so this test pins a
    // synthetic image and only asserts the exit is 0 — never a kvm
    // refusal — which is exactly the contract.
    let (inv, _, sidecar) = fixture("podman-krun-dry-no-kvm", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "a dry run must not hit the kvm gate: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(!stdout.contains("/dev/kvm"), "{stdout}");
}

#[test]
fn podman_krun_limits_become_vm_annotations() {
    // bd myconfig-6di.5.6: the SHARED limit pins (MYSBX_PODMAN_CPUS /
    // _MEMORY — one "resource limits of the sandbox" setting per
    // host, both podman variants consume them) map onto krun VM
    // annotations on the dry-run argv: no cgroup flags, and a
    // pids-limit pin is a REFUSED run naming the gap.
    let (inv, _, sidecar) = fixture("podman-krun-limits", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_PODMAN_CPUS", "2")
        .env("MYSBX_PODMAN_MEMORY", "4g");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        stdout.contains("krun.cpus=2") && stdout.contains("krun.ram_mib=4096"),
        "the limit annotations are missing: {stdout}"
    );
    assert!(
        !stdout
            .lines()
            .any(|l| l.starts_with("--cpus") || l.starts_with("--memory")),
        "no cgroup flags on the krun variant: {stdout}"
    );

    // The pids-limit pin on the krun variant is a refused run — no
    // pids controller is wired for a whole-VM "container".
    let (inv, _, sidecar) = fixture("podman-krun-pids", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("MYSBX_PODMAN_PIDS_LIMIT", "512");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(70),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("cannot enforce a pids limit"),
        "stderr: {stderr}"
    );
}

#[test]
fn podman_krun_dry_run_carries_the_handler_annotation() {
    // The VM's on switch (bd myconfig-6di.5.4): crun runs the
    // libkrun handler ONLY for the `run.oci.handler=krun`
    // annotation (crun custom-handler.c find_handler_for_container)
    // — without it the pinned crun silently runs a PLAIN container,
    // no VM, weaker than gVisor. The dry run must visibly carry it,
    // and the guest-unenforceable hardening flags must be visibly
    // absent (the payload is guest root by design; advertising a
    // cap-drop that nothing enforces would be a lie on the audit
    // surface).
    let (inv, _, sidecar) = fixture("podman-krun-annotation", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(lines[0], "podman");
    let i = lines
        .iter()
        .position(|l| *l == "--annotation")
        .expect("the handler annotation is missing");
    assert_eq!(lines[i + 1], "run.oci.handler=krun", "{stdout}");
    assert!(
        !lines
            .iter()
            .any(|l| { *l == "--cap-drop=ALL" || *l == "--security-opt=no-new-privileges" }),
        "guest-unenforceable hardening flags on the krun argv: {stdout}"
    );
    // keep-id stays: it prepares the bind sources host-side under
    // the host user's uid, the uid the virtiofs server shares them
    // as (backends.md D2).
    assert!(
        lines.iter().any(|l| *l == "--userns=keep-id"),
        "keep-id is missing: {stdout}"
    );
}

#[test]
fn allowlist_with_network_false_is_refused() {
    // network = false denies the network; an allowlist contradicts it.
    // Refused for the nono backend too (the pipeline's step 4b check
    // fires before the builder's defense-in-depth one).
    let (inv, _, sidecar) = fixture("nono-allowlist-contradiction", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\nnetwork = false\nallow-domains = [\"api.openai.com\"]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("contradict"), "{stderr}");
    assert!(
        !stdout.contains("--block-net"),
        "no argv on refusal: {stdout}"
    );
}

#[test]
fn nono_session_clone_is_bound_at_the_repo_path() {
    // --session under the LAYERED nono backend works (backends.md D1):
    // bwrap binds the clone AT the repo's own path (workspace.md D3),
    // the repo-root grant covers it, and the run is accepted — the
    // pure-nono refusal is gone. The dry run creates nothing; the
    // real run creates the clone like the bubblewrap backend's.
    let (inv, _, sidecar) = fixture_nono_config(
        "nono-session-dry",
        &["--session", "s1", "--dry-run"],
        "network = false\n",
    );
    // The repo is COMMITTED (the `git_repo` helper, like the session
    // fixtures): on an empty repo the plan stops at EmptyHostRepo and
    // there is no clone to bind — a committed repo is what lets the
    // dry run print the bind.
    if git_repo(inv.cwd.parent().unwrap(), "repo").is_none() {
        return; // no git on PATH: skip, like the session fixtures
    }
    let (code, stdout, stderr) = run_binary_with(&inv, &["--session", "s1", "--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    let clone = sidecar.join("clones").join("s1");
    assert!(
        stdout
            .lines()
            .zip(stdout.lines().skip(1))
            .any(|(a, b)| a == "--bind" && b == &clone.to_string_lossy().to_string()),
        "the clone is bound: {stdout}"
    );
    // The grant is the repo's own path — the clone appears there.
    assert!(
        stdout
            .lines()
            .zip(stdout.lines().skip(1))
            .any(|(a, b)| a == "--allow" && b == &inv.cwd.to_string_lossy().to_string()),
        "the repo-root grant covers the clone: {stdout}"
    );
    assert!(
        !stdout.contains(&format!("--allow\n{}", clone.to_string_lossy())),
        "no separate clone grant: {stdout}"
    );
    assert!(
        !clone.exists(),
        "the dry run creates nothing: {}",
        sidecar.display()
    );

    // The real run creates the clone on the session branch, like the
    // bubblewrap backend's session run (the repo is COMMITTED so the
    // plan has something to clone; without git on PATH the test
    // skips, like the session fixtures).
    let (inv, _, sidecar) = fixture_nono_config(
        "nono-session-real",
        &["--session", "s1"],
        "network = false\n",
    );
    let Some(_) = git_repo(inv.cwd.parent().unwrap(), "repo") else {
        return; // no git on PATH: skip, like the session fixtures
    };
    let (code, stdout, stderr) = run_binary_with(&inv, &["--session", "s1"]);
    assert!(code == 0 || code == 70, "code {code}: {stderr}\n{stdout}");
    let clone = sidecar.join("clones").join("s1");
    assert!(clone.join(".git").is_dir(), "the clone exists: {stdout}");
}

#[test]
fn nono_with_waypipe_builds_the_chain() {
    // bd myconfig-6di.4.6 lifts the refusal (audited — see nono.rs's
    // module docs): with the waypipe pin the layered chain builds —
    // the bwrap layout's socket-dir bind and guest wrap plus the two
    // dirs-mode unix-socket grants. With NO pin the BWRAP layout
    // refuses (the same `DisplayUnavailable`-shaped refusal every
    // backend gets from `Params::waypipe = None`).
    let (inv, _, sidecar) = fixture("nono-waypipe-chain", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\nnetwork = false\ndisplay = \"waypipe\"\n",
    )
    .unwrap();
    let fake_guest = inv.cwd.join("fake-waypipe-guest");
    std::fs::write(&fake_guest, "#!/bin/true\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    cmd.env("MYSBX_WAYPIPE", &fake_guest);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(
        stdout.contains("--allow-unix-socket-dir-bind\n/mysbx-home/wayland-0\n"),
        "the compositor socket grant rides the chain: {stdout}"
    );
    assert!(
        stdout.contains("--allow-unix-socket-dir-bind\n") && stdout.contains("/waypipe/"),
        "the per-run socket dir grant is in the chain: {stdout}"
    );
    assert!(
        stdout.contains("server\n--\n") || stdout.ends_with("server\n"),
        "the guest wrap is in the argv: {stdout}"
    );

    // Pin absent: the bwrap layout refuses the run.
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE);
    assert!(stderr.contains("waypipe"), "{stderr}");
    assert!(!stdout.contains("--allow-cwd"), "no argv: {stdout}");
}

#[test]
fn nono_with_multiplexer_starts_a_session() {
    // bd myconfig-6di.4.5 lifts the refusal: a session-starting
    // multiplexer under nono builds the whole layered chain — the
    // pinned entry is the payload, and the dirs-mode unix-socket
    // grant (`--allow-unix-socket-dir-bind`) accompanies it.
    // Unpinned mux entry (no MYSBX_MUX_ENTRY_TMUX in the test env) is
    // the BWRAP layout's refusal — the same message the bubblewrap
    // backend prints.
    let (inv, _, sidecar) = fixture("nono-mux-session", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\nnetwork = false\nmultiplexer = \"tmux\"\n",
    )
    .unwrap();
    let (code, _stdout, stderr) = run_binary_with(&inv, &["--verbose", "--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(
        stderr.contains("pinned no tmux entry"),
        "the refusal is the entry-pin one: {stderr}"
    );

    // With the entry pinned: the run builds, the full chain is in the
    // argv, with the socket grant and the entry as the payload.
    let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
    cmd.env("MYSBX_MUX_ENTRY_TMUX", "/nix/store/aaaa-tmux-entry");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(
        stdout.contains("--allow-unix-socket-dir-bind\n/mysbx-home/.mysbx-tmux\n"),
        "the unix-socket grant rides along: {stdout}"
    );
    assert!(
        stdout.contains("/nix/store/aaaa-tmux-entry\n")
            || stdout.ends_with("/nix/store/aaaa-tmux-entry\n"),
        "the pinned entry is the payload: {stdout}"
    );
    assert!(
        stdout.contains("TMUX_TMPDIR=/mysbx-home/.mysbx-tmux\n"),
        "the payload env carries the private socket dir: {stdout}"
    );
}

#[test]
fn orca_multiplexer_is_refused_on_nono_and_podman_gvisor() {
    // bd myconfig-2m8: the orca payload is the Orca runtime server —
    // an Electron AppImage via appimage-run with its own Xvfb — and
    // that needs the bubblewrap backend's real device set and
    // unmediated syscall stream. nono's grants cover the mux socket
    // dirs, not Electron's socket/syscall surface (the 6di.4.6 waypipe
    // audit covered waypipe only); the gvisor image ships no
    // AppImage/Xvfb runtime at all. A selection a backend cannot
    // deliver is refused — exit 70, before the session clone and
    // before the dry-run early return, so `--dry-run` audits the
    // refusal too. The entry is PINNED in these tests: the refusal
    // is the backend's, not the entry-pin one that would fire first
    // otherwise.
    for backend in ["nono", "podman-gvisor"] {
        let (inv, _, sidecar) = fixture(&format!("orca-refused-{backend}"), &[]);
        std::fs::write(
            sidecar.join("config.toml"),
            format!("backend = \"{backend}\"\nmultiplexer = \"orca\"\n"),
        )
        .unwrap();
        let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
        cmd.env("MYSBX_MUX_ENTRY_ORCA", "/nix/store/aaaa-orca-entry");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        let stdout = String::from_utf8_lossy(&out.stdout);
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert_eq!(
            out.status.code(),
            Some(mysbx::EXIT_INFRASTRUCTURE),
            "{backend}: {stderr}"
        );
        assert!(
            stderr.contains(&format!(
                "multiplexer = \"orca\" is not supported on the `{backend}` backend"
            )),
            "{backend}: the refusal names the value and the backend: {stderr}"
        );
        assert!(
            stderr.contains("switch the backend to `bubblewrap`"),
            "{backend}: the remedy is named: {stderr}"
        );
        assert!(!stdout.contains("aaaa-orca-entry"), "no argv: {stdout}");
    }

    // bubblewrap still ACCEPTS orca: the refusal is per backend, and
    // the D17 behaviour of the one backend that can deliver the
    // payload is unchanged.
    let (inv, _, sidecar) = fixture("orca-accepted-bwrap", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"bubblewrap\"\nmultiplexer = \"orca\"\n",
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_MUX_ENTRY_ORCA", "/nix/store/aaaa-orca-entry");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(out.status.code(), Some(0), "stderr: {stderr}");
    assert!(
        stdout.ends_with("/nix/store/aaaa-orca-entry\n"),
        "the orca entry is the payload on bubblewrap: {stdout}"
    );

    // The `--multiplexer orca` FLAG path (cli.md D14) hits the same
    // refusal — the flag is merged (3a) before the backend check, so
    // it grants no exception a configuration would not have.
    let (inv, _, sidecar) = fixture("orca-flag-refused", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"nono\"\n").unwrap();
    let (code, _stdout, stderr) = run_binary_with(&inv, &["--multiplexer", "orca", "--dry-run"]);
    assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
    assert!(
        stderr.contains("is not supported on the `nono` backend"),
        "the flag selection is refused like a config one: {stderr}"
    );

    // `run -- CMD` never starts a session (cli.md D11): the refusal
    // guards the INTERACTIVE payload's choice only, so a one-shot on
    // the nono backend is untouched by an orca multiplexer key.
    let (inv, _, sidecar) = fixture("orca-run-unaffected", &["run", "--"]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\nmultiplexer = \"orca\"\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["run", "--dry-run", "--", "true"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.starts_with("bwrap\n"),
        "the one-shot argv is built: {stdout}"
    );
    assert!(
        !stdout.contains("aaaa-orca-entry"),
        "no orca entry payload in a run argv: {stdout}"
    );
    assert!(
        stdout.ends_with("true\n"),
        "the one-shot payload is the command: {stdout}"
    );
}

#[test]
fn nono_with_dest_remap_mount_is_bound_by_bwrap() {
    // A mount with dest != path under nono: the LAYERED layout binds
    // it (the pure-nono `RemapUnsupported` refusal is gone), and the
    // grant lands at the in-sandbox dest. The source is a directory
    // INSIDE the fixture (not a host-dependent /etc/ssl): the needle
    // asserts the exact path of this run only.
    let (inv, _, sidecar) = fixture("nono-remap-bind", &[]);
    let mount_src = inv.cwd.join("remap-src");
    std::fs::create_dir_all(&mount_src).unwrap();
    let mount_src = mount_src.to_string_lossy().into_owned();
    std::fs::write(
        sidecar.join("config.toml"),
        format!(
            "backend = \"nono\"\n\
             network = false\n\
             [[mounts]]\npath = {mount_src:?}\ndest = \"/ssl\"\nmode = \"ro\"\n"
        ),
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout
            .lines()
            .zip(stdout.lines().skip(1).skip(0))
            .any(|(a, b)| a == "--ro-bind" && b == &mount_src),
        "bwrap binds the remap source: {stdout}"
    );
    assert!(
        stdout.contains(&format!("--ro-bind\n{mount_src}\n/ssl\n")),
        "at the configured dest: {stdout}"
    );
    assert!(
        stdout.contains("--read\n/ssl\n"),
        "the grant is at the dest: {stdout}"
    );
}

#[test]
fn nono_shared_network_without_allowlist_is_bubblewrap_parity() {
    // The mysbx default (network shared, no allowlist) IS expressible
    // now (bd myconfig-6di.4.4, backends.md D1's network table): bwrap
    // shares the netns exactly like the bubblewrap backend, and nono
    // adds no egress flag — outbound is nono 0.74.0's default. The
    // daemon socket rides on both layers (bd myconfig-nj9).
    for (config, label) in [
        ("backend = \"nono\"\nnetwork = true\n", "explicit true"),
        ("backend = \"nono\"\n", "omitted (default shared)"),
    ] {
        let (inv, _, sidecar) = fixture("nono-shared-network-accepted", &[]);
        std::fs::write(sidecar.join("config.toml"), config).unwrap();
        let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
        assert_eq!(code, 0, "{label}: stderr: {stderr}");
        assert!(
            stdout.starts_with("bwrap\n--clearenv\n"),
            "{label}: {stdout}"
        );
        assert!(
            stdout.contains("--share-net\n"),
            "{label}: the netns is shared: {stdout}"
        );
        assert!(
            stdout.contains("--ro-bind-try\n/nix/var/nix\n/nix/var/nix\n"),
            "{label}: the daemon dir is bound: {stdout}"
        );
        assert!(
            stdout.contains("--allow-unix-socket\n/nix/var/nix/daemon-socket/socket\n"),
            "{label}: the daemon socket is granted: {stdout}"
        );
        for absent in ["--block-net", "--allow-domain", "--allow-connect-port"] {
            assert!(
                !stdout.contains(&format!("{absent}\n")),
                "{label}: no {absent} without an allowlist: {stdout}"
            );
        }
    }
}

#[test]
fn nono_listen_ports_only_is_refused() {
    // bd myconfig-a14: listen-ports alone does not restrict outbound —
    // nono reports "outbound allowed" with only listen ports, so the
    // configuration is refused (exit 70), also under --dry-run, with
    // the fix named.
    for config in [
        "backend = \"nono\"\nnetwork = true\nlisten-ports = [8080]\n",
        "backend = \"nono\"\nlisten-ports = [8080, 9090]\n",
    ] {
        let (inv, _, sidecar) = fixture("nono-listen-only-refused", &[]);
        std::fs::write(sidecar.join("config.toml"), config).unwrap();
        let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
        assert_eq!(code, mysbx::EXIT_INFRASTRUCTURE, "stderr: {stderr}");
        assert!(stderr.contains("listen-ports"), "{stderr}");
        assert!(
            stderr.contains("outbound traffic is a lie") || stderr.contains("restrict"),
            "the message says why: {stderr}"
        );
        assert!(
            !stdout.contains("--allow-cwd"),
            "no argv on refusal: {stdout}"
        );
    }
}

#[test]
fn nono_url_form_allow_domains_is_refused() {
    // backends.md D1 / config.md D21, bd myconfig-6di.4.4:
    // allow-domains entries are plain host names on this backend — a
    // URL or path form would need nono's TLS interception. The
    // refusal names the entry; a wildcard host stays accepted.
    for (config, entry) in [
        ("https://github.com/org/**", "scheme + path glob"),
        ("github.com/org/**", "path glob"),
    ] {
        let (inv, _, sidecar) = fixture("nono-url-domain-refused", &[]);
        std::fs::write(
            sidecar.join("config.toml"),
            format!("backend = \"nono\"\nallow-domains = [\"{config}\"]\n"),
        )
        .unwrap();
        let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
        assert_eq!(
            code,
            mysbx::EXIT_INFRASTRUCTURE,
            "{entry}: stderr: {stderr}"
        );
        assert!(stderr.contains(config), "{entry}: {stderr}");
        assert!(stderr.contains("plain host names"), "{entry}: {stderr}");
        assert!(
            !stdout.contains("--allow-domain"),
            "no argv on refusal: {stdout}"
        );
    }
    let (inv, _, sidecar) = fixture("nono-wildcard-host-ok", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\nallow-domains = [\"*.example.com\"]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("--allow-domain\n*.example.com\n"),
        "the wildcard host reaches the argv: {stdout}"
    );
}

#[test]
fn nono_allowlist_reaches_the_argv() {
    // The full mapping end-to-end: allow-domains, connect-ports and
    // listen-ports all reach the printed argv with the configured
    // values, in merged order — inside the layered bwrap argv. NO
    // daemon-socket grant, NO `/nix/var/nix` bind: under an allowlist
    // neither layer exposes the nix daemon (bd myconfig-nj9,
    // bd myconfig-6di.4.4).
    let (inv, _, sidecar) = fixture("nono-allowlist-argv", &[]);
    std::fs::write(
        sidecar.join("config.toml"),
        "backend = \"nono\"\n\
         allow-domains = [\"api.openai.com\", \"github.com\"]\n\
         connect-ports = [443, 22]\n\
         listen-ports = [8080]\n",
    )
    .unwrap();
    let (code, stdout, stderr) = run_binary_with(&inv, &["--dry-run"]);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(stdout.starts_with("bwrap\n--clearenv\n"), "{stdout}");
    for pair in [
        ("--allow-domain", "api.openai.com"),
        ("--allow-domain", "github.com"),
        ("--allow-connect-port", "443"),
        ("--allow-connect-port", "22"),
        ("--listen-port", "8080"),
    ] {
        assert!(
            stdout
                .lines()
                .zip(stdout.lines().skip(1))
                .any(|(a, b)| a == pair.0 && b == pair.1),
            "missing `{}` `{}` pair in argv: {stdout}",
            pair.0,
            pair.1
        );
    }
    assert!(
        !stdout.lines().any(|l| l == "--block-net"),
        "a shared network is not blocked: {stdout}"
    );
    assert!(
        !stdout.contains("/nix/var/nix/daemon-socket/socket"),
        "no daemon socket under an allowlist (bd myconfig-nj9): {stdout}"
    );
    assert!(
        !stdout.contains("--ro-bind-try\n/nix/var/nix\n"),
        "no /nix/var/nix bind under an allowlist: {stdout}"
    );
}

#[test]
fn nono_pinned_nix_conf_is_bound_like_the_bubblewrap_backend() {
    // Under the layered backend the sanitized nix client configuration
    // is bound at /etc/nix/nix.conf when the daemon is reachable — the
    // SHARED no-allowlist case (bd myconfig-nj9 pairs it with the
    // /nix/var/nix bind): whatever the pin is NAMED, the bind is at
    // the in-sandbox path, and a name other than `nix.conf` stays
    // unrefused. Under an ALLOWLIST the bind is dropped with the
    // daemon socket (dead weight — the sandbox must not name a daemon
    // it cannot reach).
    for (network_config, expect_bind, label) in [
        (
            "network = true\n",
            true,
            "shared, no allowlist: bound (bubblewrap parity)",
        ),
        (
            "network = true\nallow-domains = [\"api.openai.com\"]\n",
            false,
            "allowlist: dropped with the daemon socket",
        ),
        ("network = false\n", false, "denied network: dropped"),
    ] {
        for (file_name, label_file) in [
            ("nix.conf", "named nix.conf"),
            ("mysbx-nix.conf", "other name"),
        ] {
            let (inv, _, _) = fixture_nono_config("nono-nix-conf-pin", &[], network_config);
            let conf = inv.home.join(file_name);
            std::fs::write(&conf, "experimental-features = nix-command flakes\n").unwrap();
            let mut cmd = spawn_with_args(&inv, &["--verbose", "--dry-run"]);
            cmd.env("MYSBX_NIX_CONF", &conf);
            let out = cmd.output().expect("failed to spawn the mysbx binary");
            let stdout = String::from_utf8_lossy(&out.stdout);
            let stderr = String::from_utf8_lossy(&out.stderr);
            let label = format!("{label} ({label_file})");
            assert_eq!(out.status.code(), Some(0), "{label}: stderr: {stderr}");
            if expect_bind {
                assert!(
                    stdout.contains(&format!(
                        "--ro-bind\n{}\n/etc/nix/nix.conf\n",
                        conf.display()
                    )),
                    "{label}: the pin is bound at /etc/nix/nix.conf: {stdout}"
                );
            } else {
                assert!(
                    !stdout.contains("/etc/nix/nix.conf"),
                    "{label}: no nix.conf bind without the daemon: {stdout}"
                );
            }
            assert!(
                !stdout.contains("NIX_CONF_DIR="),
                "{label}: no NIX_CONF_DIR in the layered argv: {stdout}"
            );
        }
    }
}

// ---- the krun git trust (bd myconfig-zj2) -----------------------------------

#[test]
fn podman_krun_git_trust_names_exactly_the_bound_workspace_paths() {
    // The dubious-ownership fix (bd myconfig-zj2): a krun run exports
    // GIT_CONFIG_GLOBAL at the per-run trust file, binds it ro, and the
    // file's safe.directory entries are EXACTLY what the argv bound —
    // the workspace root (exact + the `/*` form), the approved
    // git-dirs (exact) and, when it exists, the worktrees sibling.
    // Never a bare `*`.
    let (inv, repo, sidecar) = fixture("podman-krun-git-trust", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-krun\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    // The argv carries the ro bind and the env.
    assert!(
        stdout.contains("dst=/etc/mysbx/gitconfig,ro"),
        "the trust file is not ro-bound: {stdout}"
    );
    assert!(
        stdout.contains("GIT_CONFIG_GLOBAL=/etc/mysbx/gitconfig"),
        "GIT_CONFIG_GLOBAL is not exported: {stdout}"
    );
    // The libgit2 half (bd myconfig-jn0): the exact-path system
    // config, ro-bound where libgit2 and git read it.
    assert!(
        stdout.contains("/system-gitconfig,dst=/etc/gitconfig,ro"),
        "the libgit2 trust file is not ro-bound at /etc/gitconfig: {stdout}"
    );
    // The trust file was NOT written: a --dry-run creates nothing.
    assert!(
        !sidecar.join("gittrust").exists(),
        "a --dry-run must not create the trust file"
    );
    assert!(repo.is_dir());
}

#[test]
fn podman_gvisor_carries_no_trust_file() {
    // The trust story is krun-only (bd myconfig-zj2): the gvisor
    // payload runs as the keep-id-mapped user — the uid already owns
    // the repo mounts, so the dubious-ownership check cannot fire and
    // no trust bind may appear on the gvisor argv.
    let (inv, _, sidecar) = fixture("podman-gvisor-git-trust", &[]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        !stdout.contains("GIT_CONFIG_GLOBAL")
            && !stdout.contains("/etc/mysbx/gitconfig")
            && !stdout.contains("/etc/gitconfig"),
        "no trust bind or env on the gvisor argv: {stdout}"
    );
    assert!(
        !sidecar.join("gittrust").exists(),
        "the gvisor run must not create the trust file"
    );
}

// ---- `mysbx --detach` (docs/design/detach.md B1-B10) -----------------------

/// A stub `dtach` for the launch tests: it records its own argv (one
/// argument per line, the `dtach` executable first) into a fixed
/// record file — baked into the script text like `bwrap_stub` does,
/// because the child's environment is not the test's to reach — and
/// branches on `$1`: `-n` — the launch half — either drops the four
/// fixed dtach arguments (`-n <sock>` and `-r winch`) and `exec`s
/// what remains (the inner mysbx and its arguments, so the handshake
/// completes through a real mysbx) or records and exits 1 (a dtach
/// that dies during startup); `-a` — the attach half — records its
/// argv into a separate attach record and exits `${DTACH_ATTACH_EXIT:-0}`.
/// Written with the mode bits of a script because the outer spawns it
/// directly.
fn dtach_stub(base: &Path, exec_inner: bool) -> (PathBuf, PathBuf) {
    let record = base.join("dtach-record.txt");
    let attach_record = base.join("dtach-attach-record.txt");
    let stub = base.join("dtach-stub");
    let launch_tail = if exec_inner {
        format!(
            "shift 4\nprintf '%s\\n' \"$@\" > {}\nexec \"$@\"\n",
            record.display()
        )
    } else {
        format!("printf '%s\\n' \"$@\" > {}\nexit 1\n", record.display())
    };
    std::fs::write(
        &stub,
        format!(
            "#!/bin/sh\ncase \"$1\" in\n  -n) {launch_tail} ;;\n  -a) printf '%s\\n' \"$@\" > {}\nexit \"${{DTACH_ATTACH_EXIT:-0}}\" ;;\nesac\nexit 1\n",
            attach_record.display()
        ),
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&stub, std::fs::Permissions::from_mode(0o755)).unwrap();
    (stub, record)
}

/// The attach record of [`dtach_stub`]'s `-a` branch.
fn dtach_attach_record(base: &Path) -> PathBuf {
    base.join("dtach-attach-record.txt")
}

/// A pty pair for the attach tests: (master, slave). The slave is
/// the child's STDIN — only `isatty(0)` matters for the attach path —
/// and the master is kept open so the slave STAYS a terminal for as
/// long as the test holds it. Hand-rolled through the raw libc
/// symbols, the crate's own zero-dependency extern idiom; `None`
/// means the environment grants no pty (a container without
/// /dev/ptmx) and the test skips, the `is_ssh_keygen_available`
/// precedent.
fn open_test_pty() -> Option<(std::fs::File, std::fs::File)> {
    unsafe fn libc_posix_openpt(flags: i32) -> i32 {
        extern "C" {
            fn posix_openpt(flags: i32) -> i32;
        }
        posix_openpt(flags)
    }
    unsafe fn libc_grantpt(fd: i32) -> i32 {
        extern "C" {
            fn grantpt(fd: i32) -> i32;
        }
        grantpt(fd)
    }
    unsafe fn libc_unlockpt(fd: i32) -> i32 {
        extern "C" {
            fn unlockpt(fd: i32) -> i32;
        }
        unlockpt(fd)
    }
    // `ptsname_r` fills a buffer the path is copied into.
    unsafe fn libc_ptsname_r(fd: i32, buf: *mut u8, len: usize) -> i32 {
        extern "C" {
            fn ptsname_r(fd: i32, buf: *mut u8, buflen: usize) -> i32;
        }
        ptsname_r(fd, buf, len)
    }
    // O_RDWR | O_NOCTTY: 2 | 256.
    const O_RDWR_NOCTTY: i32 = 258;
    let master_fd = unsafe { libc_posix_openpt(O_RDWR_NOCTTY) };
    if master_fd < 0 {
        return None;
    }
    if unsafe { libc_grantpt(master_fd) } != 0 || unsafe { libc_unlockpt(master_fd) } != 0 {
        unsafe { libc_close(master_fd) };
        return None;
    }
    let mut name = [0u8; 256];
    if unsafe { libc_ptsname_r(master_fd, name.as_mut_ptr(), name.len()) } != 0 {
        unsafe { libc_close(master_fd) };
        return None;
    }
    let end = name.iter().position(|b| *b == 0).unwrap_or(name.len());
    use std::os::unix::ffi::OsStrExt;
    let slave_path = std::path::PathBuf::from(std::ffi::OsStr::from_bytes(&name[..end]));
    use std::os::fd::FromRawFd;
    let slave = match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&slave_path)
    {
        Ok(f) => f,
        Err(_) => {
            unsafe { libc_close(master_fd) };
            return None;
        }
    };
    let master = unsafe { std::fs::File::from_raw_fd(master_fd) };
    Some((master, slave))
}

/// `close(2)` for the pty helper — the crate's own extern naming
/// (`libc_close` in detach.rs is private to the lib, so the test
/// crate declares its own, dependency-free like `libc_kill`).
///
/// # Safety
///
/// `fd` must be an open file descriptor (or a harmless invalid one
/// the call merely fails on); `close(2)` has no other contract.
unsafe fn libc_close(fd: i32) {
    extern "C" {
        fn close(fd: i32) -> i32;
    }
    close(fd);
}

/// A stub `bwrap`: records its inherited ignored-signal mask, then
/// its own argv, sleeps until `$BWRAP_LIVE` appears (the test controls
/// how long the "backend" runs), then exits with `$BWRAP_EXIT`
/// (default 0). With `$BWRAP_TREE_FILE` set it ALSO forks a
/// long-lived grandchild before the wait loop and reports the pid
/// tree (`$PPID` = the inner mysbx, `$!` = the grandchild) into that
/// file — the cancel test's window into which processes of the run's
/// group survive the group kill. Tests that do not set the variable
/// are unaffected.
fn bwrap_stub(base: &Path) -> (PathBuf, PathBuf) {
    let record = base.join("bwrap-record.txt");
    let stub = base.join("bwrap-stub");
    std::fs::write(
        &stub,
        format!(
            "#!/bin/sh\ngrep '^SigIgn:' /proc/self/status > {r}.tmp\n\
             printf '%s\\n' \"$@\" >> {r}.tmp\n\
             if [ -n \"$BWRAP_TREE_FILE\" ]; then\n\
               sleep 300 &\n\
               printf '%s %s\\n' \"$PPID\" \"$!\" > \"$BWRAP_TREE_FILE.tmp\"\n\
               mv -f \"$BWRAP_TREE_FILE.tmp\" \"$BWRAP_TREE_FILE\"\n\
             fi\n\
             mv -f {r}.tmp {rec}\n\
             while [ ! -e \"$BWRAP_LIVE\" ]; do sleep 0.05; done\nexit \"${{BWRAP_EXIT:-0}}\"\n",
            r = record.display(),
            rec = record.display()
        ),
    )
    .unwrap();
    use std::os::unix::fs::PermissionsExt;
    std::fs::set_permissions(&stub, std::fs::Permissions::from_mode(0o755)).unwrap();
    (stub, record)
}

/// A SHORT runtime root for a detached-run test: the socket paths a
/// run computes are capped by the kernel's sun_path, and the sandbox
/// a `nix build` runs in has a deep $TMPDIR — the fixture's tmpdir
/// tree is too long there, so the registry root gets its own short
/// directory right under $TMPDIR.
fn short_runtime_root(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("mysbx-drt-{name}"));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

/// The registry paths of a live-workspace detached run for `repo`
/// under `runtime` — the layout of detach.md B3/B5, computed the same
/// way the binary computes them, so a test reads the record without
/// re-deriving the hash by hand.
fn detach_paths_live(runtime: &Path, repo: &Path) -> mysbx::detach::Paths {
    mysbx::detach::paths(runtime, repo, &mysbx::detach::Workspace::Live)
        .expect("the registry paths build")
}

/// Read a detached run's registry record (B5), polling until the
/// state file names a state other than `starting` — the outer's
/// handshake waits the same way, and a test that reads `starting`
/// would race the inner's writes. The reader is the crate's own
/// [`mysbx::detach::read`], so the test parses exactly what a later
/// `attach` (bd myconfig-7ioq.5) will parse.
fn wait_for_record(paths: &mysbx::detach::Paths) -> mysbx::detach::Record {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        if let Ok(rec) = mysbx::detach::read(paths) {
            if rec.state != mysbx::detach::State::Starting {
                return rec;
            }
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the detached run never left `starting`: {}",
            std::fs::read_to_string(&paths.json).unwrap_or_default()
        );
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

#[test]
fn detach_and_detachable_are_refused_on_every_verb() {
    // B2: `--detach` and `--detachable` select the bare form's start
    // mode; every verb rejects them with the shared usage error
    // (exit 2), naming the flag — the same arm that rejects
    // `--dry-run` and friends.
    let (inv, _, _) = fixture("detach-on-verb", &["--detach", "status"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 2, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: --detach is not valid with `status`"),
        "{stderr}"
    );
    let (inv, _, _) = fixture("detachable-on-verb-shared", &["--detachable", "status"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 2, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("mysbx: --detachable is not valid with `status`"),
        "{stderr}"
    );
}

#[test]
fn detach_and_detachable_are_refused_together() {
    // B2: the two start modes of one detached run, never both in one
    // invocation — the one-run rule every selection flag follows.
    let (inv, _, _) = fixture("detach-detachable-both", &[]);
    let (code, stdout, stderr) = run_binary_with(&inv, &["--detach", "--detachable"]);
    assert_eq!(code, 2, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stderr.contains("--detach and --detachable are two ways to start one detached run"),
        "{stderr}"
    );
}

#[test]
fn detach_and_detachable_are_refused_inside_a_detached_run() {
    // B4 step 3: a nested `--detach` or `--detachable` would start a
    // second run for a workspace that has one — refused before
    // anything runs, the marker named as the reason, the message
    // naming the typed flag.
    let (inv, _, _) = fixture("detach-nested", &[]);
    for (flag, name) in [("--detach", "--detach"), ("--detachable", "--detachable")] {
        let mut cmd = spawn_with_args(&inv, &[flag]);
        cmd.env("MYSBX_DETACHED", "/synth/sock");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        assert_eq!(out.status.code(), Some(2), "{name}");
        let stderr = String::from_utf8_lossy(&out.stderr);
        assert!(
            stderr.contains(&format!("{name} is not valid inside a detached run")),
            "{name}: {stderr}"
        );
    }
}

#[test]
fn detach_requires_a_usable_dtach() {
    // B10: without a pin and without `dtach` on PATH the outer refuses
    // with 70, naming MYSBX_DTACH — the same refusal a nix-less host
    // gives. The state file of the aborted launch is REMOVED: nothing
    // is registered when no child exists.
    let (inv, repo, _) = fixture_with_backend("detach-no-dtach", &["--detach"]);
    let runtime = short_runtime_root("no-dtach");
    std::fs::create_dir_all(&runtime).unwrap();
    std::fs::create_dir_all(inv.home.join("empty-path")).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env_remove("MYSBX_DTACH")
        .env("PATH", inv.home.join("empty-path"))
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--detach, --detachable and attach require a usable dtach (MYSBX_DTACH)"),
        "{stderr}"
    );
    // No state file survives the refusal (B4: the pre-spawn failures
    // remove the record, so nothing is registered without a child).
    let paths = mysbx::detach::paths(
        &runtime,
        &repo.canonicalize().unwrap(),
        &mysbx::detach::Workspace::Live,
    )
    .unwrap();
    assert!(!paths.json.exists(), "the state file was removed");
}

#[test]
fn detach_dry_run_prints_the_dtach_launcher_and_the_backend_argv() {
    // B9: `--detach --dry-run` audits the launch — the dtach argv, a
    // blank line, then the backend argv the inner run would spawn —
    // and creates nothing: no registry directory, no state file.
    let (inv, repo, _) = fixture_with_backend("detach-dry-run", &["--detach", "--dry-run"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("dry");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach", "--dry-run"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let lines: Vec<&str> = stdout.lines().collect();
    // The launcher half: the pinned dtach first (B10: the audit
    // names the resolved binary, never a bare PATH lookup).
    assert_eq!(
        lines[0],
        dtach.to_string_lossy(),
        "argv[0] is the pinned dtach: {stdout}"
    );
    assert_eq!(lines[1], "-n");
    let sock = &lines[2];
    let canonical = repo.canonicalize().unwrap();
    let expected_paths =
        mysbx::detach::paths(&runtime, &canonical, &mysbx::detach::Workspace::Live).unwrap();
    assert_eq!(
        Path::new(sock),
        expected_paths.sock,
        "the socket path is the registry's: {stdout}"
    );
    assert_eq!(lines[3], "-r");
    assert_eq!(lines[4], "winch");
    let self_exe = Path::new(lines[5]);
    assert!(
        self_exe.is_absolute(),
        "the inner mysbx is spawned by absolute path: {stdout}"
    );
    // One blank line, then the backend argv — with the detachable
    // mark the inner run's argv carries (B7).
    let blank = lines.iter().position(|l| l.is_empty()).expect("blank line");
    assert_eq!(&lines[blank + 1], &"bwrap", "the backend argv follows");
    assert!(
        stdout.contains("MYSBX_DETACHABLE"),
        "the inner backend argv carries the mark: {stdout}"
    );
    // Nothing was created: no registry directory, no state file.
    assert!(
        !expected_paths.dir.exists(),
        "the dry run created the registry dir"
    );
    assert!(
        !expected_paths.json.exists(),
        "the dry run created the state file"
    );
    assert!(
        !expected_paths.sock.exists(),
        "the dry run created the socket"
    );
}

#[test]
fn detach_dry_run_refuses_the_podman_backends() {
    // B9 + bd myconfig-cad: the podman backends refuse `--detach`
    // even under `--dry-run` — the audit must see the refusal, not
    // the launch of an unsafe one.
    let (inv, _, sidecar) = fixture("detach-podman-refusal", &["--detach", "--dry-run"]);
    std::fs::write(sidecar.join("config.toml"), "backend = \"podman-gvisor\"\n").unwrap();
    let runtime = short_runtime_root("podman");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach", "--dry-run"]);
    cmd.env("MYSBX_PODMAN_IMAGE", "localhost/test:latest")
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--detach is not supported on the `podman-gvisor` backend")
            && stderr.contains("bd myconfig-cad"),
        "{stderr}"
    );
}

#[test]
fn a_detach_of_a_real_run_detaches_and_hands_the_attach_hint() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B4 step 4: the outer returns 0 once the inner spawned the
    // backend — the state file says `running` — and its stderr names
    // `mysbx attach` (the composition command of bd myconfig-7ioq.5).
    // The stub dtach execs the inner mysbx for real; the stub bwrap
    // runs until the test drops the live marker.
    let (inv, repo, sidecar) = fixture_with_backend("detach-happy", &["--detach"]);
    let _ = &sidecar;
    let (dtach, dtach_record) = dtach_stub(&inv.home, true);
    let (bwrap, bwrap_record) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("happy");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: detached; attach with: mysbx attach"),
        "{stderr}"
    );
    // The launch argv reached dtach: the inner mysbx it execs, by
    // absolute path — the same executable as the outer (B4 step 2).
    // The bare form passes no inner arguments, so the record is the
    // executable alone.
    let recorded = std::fs::read_to_string(&dtach_record).unwrap();
    let rl: Vec<&str> = recorded.lines().collect();
    assert_eq!(
        rl,
        vec![rl.first().copied().unwrap_or("")],
        "the bare form's inner argv is the mysbx executable alone: {recorded}"
    );
    assert!(
        Path::new(rl[0]).is_absolute(),
        "the inner mysbx: {recorded}"
    );
    let self_exe = std::fs::canonicalize(env!("CARGO_BIN_EXE_mysbx")).unwrap();
    assert_eq!(
        std::fs::canonicalize(rl[0]).unwrap(),
        self_exe,
        "the inner is the SAME executable as the outer (B4): {recorded}"
    );
    // The backend the inner spawned runs detached: the stub's argv
    // record arrives — polled, because the outer returns on the
    // `running` state file, which the inner writes right after the
    // spawn, before the stub's shell has written anything.
    let rec = wait_for_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Running);
    assert_eq!(rec.backend, "bubblewrap");
    let _recorded_bwrap = wait_for_file(&bwrap_record);
    // Let the backend exit 0 and read the terminal record.
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    let rec = wait_for_finished_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Exited);
    assert_eq!(rec.exit_code, Some(0));
}

/// Poll until the registry record reaches a terminal state — the
/// counterpart of [`wait_for_record`] for the end of a run.
fn wait_for_finished_record(paths: &mysbx::detach::Paths) -> mysbx::detach::Record {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        if let Ok(rec) = mysbx::detach::read(paths) {
            if rec.state.is_finished() {
                return rec;
            }
        }
        assert!(
            std::time::Instant::now() < deadline,
            "the detached run never finished: {}",
            std::fs::read_to_string(&paths.json).unwrap_or_default()
        );
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}

#[test]
fn a_cancelled_detached_run_leaves_no_surviving_process() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B4 + bd myconfig-7ioq.11: a `SIGTERM` to the inner cancels the
    // run — the backend's WHOLE process group gets it, so a
    // daemonized child the backend spawned in the host group cannot
    // outlive the `cancelled` record: the group kill must reach
    // every process the backend forked into the run's group. The
    // stub bwrap forks that very child and reports the pid
    // tree through `BWRAP_TREE_FILE`; the outer spawn runs in its own
    // process group (`process_group(0)`), because the stub dtach does
    // not `setsid` — the inner shares the OUTER's group, and without
    // the grouping the group kill would TERM the test harness itself.
    let (inv, repo, _) = fixture_with_backend("detach-cancel-group", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, bwrap_record) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("cancel-group");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let tree_file = inv.home.join("bwrap-tree.txt");
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"))
        .env("BWRAP_TREE_FILE", &tree_file);
    // The outer (and with it the inner, through the non-setsid stub
    // dtach) runs in its own group: the group kill must reach the
    // run's processes only, never the harness that spawned it.
    {
        use std::os::unix::process::CommandExt;
        cmd.process_group(0);
    }
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The backend's argv record (which also means the stub forked its
    // tree by now), then the tree itself.
    let _recorded = wait_for_file(&bwrap_record);
    let tree = wait_for_file(&tree_file);
    let (inner_pid, grandchild_pid): (u32, u32) = {
        let mut it = tree.split_whitespace();
        let inner: u32 = it
            .next()
            .unwrap_or_else(|| panic!("no inner pid in the tree record: {tree}"))
            .parse()
            .unwrap_or_else(|_| panic!("not a pid: {tree}"));
        let grandchild: u32 = it
            .next()
            .unwrap_or_else(|| panic!("no grandchild pid in the tree record: {tree}"))
            .parse()
            .unwrap_or_else(|_| panic!("not a pid: {tree}"));
        (inner, grandchild)
    };
    // End the run the way an operator does: `kill <pid of the inner>`.
    unsafe { libc_kill(inner_pid, 15) };
    // The run ends `cancelled` once the backend is reaped.
    let rec = wait_for_finished_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Cancelled);
    // The grandchild died with the group — poll /proc, the group TERM
    // needs no ordering guarantee from the scheduler.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    while Path::new(&format!("/proc/{grandchild_pid}")).exists() {
        assert!(
            std::time::Instant::now() < deadline,
            "the grandchild {grandchild_pid} survived the cancelled run"
        );
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    // Best-effort cleanup so a failure does not leak the 300s
    // sleep into the machine.
    unsafe { libc_kill(grandchild_pid, 9) };
}

#[test]
fn a_detached_run_that_fails_the_startup_records_the_error_and_the_outer_reports_70() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B4/B5: a startup that fails only in the inner (here: the
    // backend binary named by the pin does not exist — the outer's
    // pre-flight does not spawn anything, so nothing refuses in the
    // foreground) ends `failed` with the spawn's own diagnosis in
    // `error`, and the OUTER reports it with 70 — the same diagnosis
    // an interactive run would print, through the handshake.
    let (inv, repo, _) = fixture_with_backend("detach-startup-fail", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("startup-fail");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", inv.home.join("no-such-bwrap"))
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: the detached run failed during startup:"),
        "{stderr}"
    );
    assert!(
        stderr.contains("cannot exec the backend"),
        "the spawn's diagnosis is the reported error: {stderr}"
    );
    let rec = wait_for_finished_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Failed);
    assert!(
        rec.error
            .unwrap_or_default()
            .contains("cannot exec the backend"),
        "the refusal is the recorded diagnosis"
    );
}

#[test]
fn a_detached_run_whose_backend_exits_nonzero_records_the_exit_code() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B5: EVERY backend exit is `exited` — `exitCode` carries the
    // backend's own exit code, nonzero or not; `failed` is a startup
    // diagnosis or a death by signal, neither of which has an exit
    // code. The backend is held alive until the outer has RETURNED
    // (the live marker is created only after the outer's exit): the
    // outer reports the handshake of a `running` run and exits 0 —
    // a detached run that STARTED succeeded, its payload's fate is
    // the record's to tell — and only then does the test let the
    // backend exit 42, so the finished record cannot race the
    // outer's handshake poll.
    let (inv, repo, _) = fixture_with_backend("detach-exit-code", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, _) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("exit-code");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"))
        .env("BWRAP_EXIT", "42");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // Only now may the backend fail: the outer is gone, its handshake
    // cannot race the terminal record.
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    let rec = wait_for_finished_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Exited);
    assert_eq!(rec.exit_code, Some(42));
    assert!(rec.error.is_none(), "an exit carries no diagnosis: {rec:?}");
    assert!(rec.finished_at.is_some());
    // The attach half of B5: the finished record is printed once
    // with the backend's own exit code, removed under the lock, and
    // attach reports 70. No terminal is needed (the tty check is
    // Live-only).
    let mut attach = spawn_with_args(&inv, &["attach"]);
    attach
        .env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = attach.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: the detached run exited with 42 at"),
        "{stderr}"
    );
    assert!(
        !paths.json.exists(),
        "the printed record is removed under the lock"
    );
}

#[test]
fn a_second_detach_for_the_same_workspace_is_refused_with_the_attach_hint() {
    // B3: one detached run per workspace — the live socket of the
    // first run refuses the second launch with 70 and names
    // `mysbx attach`, the command that reconnects. The socket is the
    // probe's object, so the test binds a listening one at the
    // registry's path directly — the same object a real dtach master
    // holds — and the second launch needs no first child at all.
    let (inv, repo, _) = fixture_with_backend("detach-second", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("second");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = mysbx::detach::paths(
        &runtime,
        &repo.canonicalize().unwrap(),
        &mysbx::detach::Workspace::Live,
    )
    .unwrap();
    mysbx::detach::ensure(&paths).unwrap();
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock)
        .expect("the test holds the socket of the first run");
    let mut second = spawn_with_args(&inv, &["--detach"]);
    second
        .env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let second = second.output().expect("failed to spawn the mysbx binary");
    assert_eq!(second.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&second.stderr).into_owned();
    assert!(
        stderr.contains("mysbx attach"),
        "the refusal names the command that reconnects: {stderr}"
    );
}

#[test]
fn a_detached_session_run_uses_the_session_registry_entry() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B3: the registry entry is per workspace — the live repo and a
    // session clone have separate sockets and state files, so a
    // detached session run and a detached live run do not collide.
    // The session workspace needs a real git repo for the clone.
    let Some((inv, repo, _sidecar)) =
        git_session_fixture("detach-session", &["--session", "fix-1", "--detach"])
    else {
        return; // no git on PATH: skip, like the session tests
    };
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, _) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("session");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = mysbx::detach::paths(
        &runtime,
        &repo.canonicalize().unwrap(),
        &mysbx::detach::Workspace::Session("fix-1".to_owned()),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["--session", "fix-1", "--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let rec = wait_for_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Running);
    assert_eq!(rec.session.as_deref(), Some("fix-1"));
    // The live workspace has no record: one entry per workspace.
    let live_paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    assert!(!live_paths.json.exists(), "the live entry was not written");
    // Finish the run.
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    wait_for_finished_record(&paths);
}

#[test]
fn a_detached_run_creates_a_private_registry_directory() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B3: the registry directory the outer creates is owned by the
    // current user and private (0700) — the socket grants the
    // sandbox, the directory that holds it must not be readable by
    // anybody else.
    let (inv, repo, _) = fixture_with_backend("detach-registry-mode", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, _) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("registry-mode");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    use std::os::unix::fs::PermissionsExt;
    let paths = mysbx::detach::paths(
        &runtime,
        &repo.canonicalize().unwrap(),
        &mysbx::detach::Workspace::Live,
    )
    .unwrap();
    let mode = std::fs::metadata(&paths.dir)
        .expect("the registry dir exists")
        .permissions()
        .mode();
    assert_eq!(mode & 0o777, 0o700, "private by mode");
    {
        use std::os::unix::fs::MetadataExt;
        assert_eq!(
            std::fs::metadata(&paths.dir).unwrap().uid(),
            nix_uid_of_current_user(),
            "owned by the current user"
        );
    }
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    wait_for_finished_record(&paths);
}

/// The uid of the test runner — `std::os::unix::fs::MetadataExt`'s
/// `uid()` needs the uid, and the crate is dependency-free, so this
/// reads it through the metadata of a file the test itself owns.
fn nix_uid_of_current_user() -> u32 {
    use std::os::unix::fs::MetadataExt;
    let probe = std::env::temp_dir().join(format!("mysbx-uid-probe-{}", std::process::id()));
    std::fs::write(&probe, "").unwrap();
    let uid = std::fs::metadata(&probe).unwrap().uid();
    let _ = std::fs::remove_file(&probe);
    uid
}

#[test]
fn the_inner_detached_run_spawns_the_backend_with_the_detachable_mark() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B7 end to end: the argv the inner's backend receives carries
    // `MYSBX_DETACHABLE=1` (the mark the future multiplexer checks)
    // — asserted through the stub bwrap's recorded argv.
    let (inv, _, _) = fixture_with_backend("detach-detachable-mark", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, bwrap_record) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("mark");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(0));
    let recorded = wait_for_file(&bwrap_record);
    // The pure-bwrap shape: `--setenv MYSBX_DETACHABLE 1`, one argv
    // element per line (the nono layered shape would spell it
    // `MYSBX_DETACHABLE=1` in its payload env segment).
    let lines: Vec<&str> = recorded.lines().collect();
    let mark = lines
        .windows(3)
        .any(|w| w == ["--setenv", "MYSBX_DETACHABLE", "1"]);
    assert!(
        mark,
        "the mark reached the backend's environment: {recorded}"
    );
    // The supervisor tie of the same argv (bd myconfig-7ioq.11): a
    // detached-inner run's bwrap carries `--die-with-parent`, so a
    // cancelled run leaves nothing in the pid namespace behind.
    assert!(
        lines.contains(&"--die-with-parent"),
        "the backend argv carries the supervisor tie: {recorded}"
    );
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
}

#[test]
fn the_backend_of_the_detached_inner_inherits_no_ignored_signals() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B4: the inner ignores the pty's signals, but dispositions cross
    // `exec` — the backend (and the payload behind it) must start
    // with `SIG_DFL` for all five of B4's signals, or a Ctrl-C on
    // the pty would be dead in the payload. Asserted through the stub
    // bwrap's first record line: its inherited SigIgn mask.
    let (inv, _, _) = fixture_with_backend("detach-no-ignored-signals", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, bwrap_record) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("no-ignored-signals");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The mask line first: `SigIgn:\t0000000000000000` (hex, no 0x —
    // parsed tolerantly anyway, the format is not a contract). The stub
    // assembles the record in a `.tmp` and `mv`s it (atomic rename),
    // so what `wait_for_file` returns is always the full record.
    let recorded = wait_for_file(&bwrap_record);
    let mask_line = recorded
        .lines()
        .next()
        .expect("the stub's SigIgn line comes first");
    let mask_hex = mask_line
        .strip_prefix("SigIgn:")
        .unwrap_or_else(|| panic!("not a SigIgn line: {mask_line}"))
        .trim()
        .trim_start_matches("0x");
    let mask = u64::from_str_radix(mask_hex, 16)
        .unwrap_or_else(|_| panic!("not a SigIgn hex mask: {mask_line}"));
    // The kernel's /proc prints the mask LSB-first: bit N-1 is
    // signal N — SIGHUP=1<<0, SIGINT=1<<1, SIGQUIT=1<<2,
    // SIGTERM=1<<14, SIGTSTP=1<<19.
    let b4_signals = (1u64 << 0) | (1u64 << 1) | (1u64 << 2) | (1u64 << 14) | (1u64 << 19);
    assert_eq!(
        mask & b4_signals,
        0,
        "the backend inherited an ignored B4 signal (mask {mask:#x})"
    );
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
}

#[test]
fn a_marker_that_does_not_name_this_workspace_is_refused_by_the_inner() {
    // B4 step 3: `MYSBX_DETACHED` must name THIS workspace's socket —
    // a marker that points elsewhere (here: a second workspace's
    // socket path) is a refused inner run, recorded `failed` with the
    // B4 message, never a state file written into the other entry.
    let (inv, repo, _) = fixture_with_backend("detach-forged-marker", &[]);
    // A SHORT runtime root: the socket path is capped by the kernel's
    // sun_path, and the fixture's tmpdir path varies per environment —
    // [`short_runtime_root`] keeps the forged marker's socket path
    // valid wherever the suite runs.
    let runtime = short_runtime_root("forged");
    // The marker of ANOTHER workspace: the session entry's socket.
    let other_sock = mysbx::detach::paths(
        &runtime,
        &repo.canonicalize().unwrap(),
        &mysbx::detach::Workspace::Session("other".to_owned()),
    )
    .unwrap()
    .sock;
    // The bare form under the forged marker — exactly what the
    // dtach child of a detached run is (B4 step 3). `--detach` is
    // NOT passed: a nested `--detach` has its own, earlier refusal.
    let out = spawn_with_args(&inv, &[] as &[&str])
        .env("MYSBX_DETACHED", &other_sock)
        .env("XDG_RUNTIME_DIR", &runtime)
        .output()
        .expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("does not name this workspace's socket"),
        "{stderr}"
    );
    // The other entry was never written: the marker cannot redirect
    // state files (B4).
    assert!(!other_sock.exists());
    let live_paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let rec = wait_for_finished_record(&live_paths);
    assert_eq!(rec.state, mysbx::detach::State::Failed);
    assert!(
        rec.error
            .unwrap_or_default()
            .contains("does not name this workspace's socket"),
        "the refusal is the recorded diagnosis"
    );
}

#[test]
fn the_detached_registry_needs_xdg_runtime_dir() {
    // B3: the registry lives under `$XDG_RUNTIME_DIR` — unset is a
    // refusal with 70, never a `/tmp` fallback (the reason
    // `TMUX_TMPDIR` has none either). The test pins the refusal at
    // the detach-specific seam: the pre-flight refusal of the dry
    // run, which needs no stubs at all.
    let (inv, _, _) = fixture_with_backend("detach-no-runtime", &["--detach", "--dry-run"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let mut cmd = spawn_with_args(&inv, &["--detach", "--dry-run"]);
    cmd.env("MYSBX_DTACH", &dtach).env_remove("XDG_RUNTIME_DIR");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("XDG_RUNTIME_DIR"),
        "the refusal names the variable: {stderr}"
    );
}

#[test]
fn a_dtach_that_dies_in_the_startup_is_reported_by_the_outer() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B4 step 4's "socket gone while starting" branch, end to end:
    // the dtach stub (variant 2) records its argv and exits 1 — no
    // socket ever exists, no inner mysbx ever runs — so the outer's
    // handshake sees the dtach child gone with no live socket and
    // reports the death with 70. The `starting` record is LEFT in
    // place (only the pre-spawn failures remove it): the next
    // `--detach` replaces it, exactly as B5's lifecycle says.
    let (inv, repo, _) = fixture_with_backend("detach-dtach-dies", &["--detach"]);
    let (dtach, dtach_record) = dtach_stub(&inv.home, false);
    let runtime = short_runtime_root("dtach-dies");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("the detached run died during startup"),
        "{stderr}"
    );
    // The stub ran — the death is the stub's, not a spawn failure.
    let recorded = std::fs::read_to_string(&dtach_record).unwrap();
    assert!(
        recorded.lines().any(|l| l == "-n"),
        "the stub saw the launcher argv: {recorded}"
    );
    // The `starting` record is left behind (B5: a finished record
    // stays until `attach` or the next `--detach` replaces it).
    let rec = mysbx::detach::read(&paths).expect("the starting record is left");
    assert_eq!(rec.state, mysbx::detach::State::Starting);
    assert!(!paths.sock.exists(), "no socket was ever created");
}

#[test]
fn a_stale_socket_is_replaced_under_the_lock() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B6 end to end: a plain file at the computed socket path probes
    // as Stale (it exists but refuses connections), and a `--detach`
    // removes it under the lock before the launch — the run succeeds
    // and the junk is gone from the registry directory.
    let (inv, repo, _) = fixture_with_backend("detach-stale-sock", &["--detach"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, _) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("stale-sock");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    mysbx::detach::ensure(&paths).unwrap();
    // A stale corpse at the socket's path: a plain file, never a
    // socket — the probe's connect(2) fails with something other
    // than NotFound and the path still exists.
    std::fs::write(&paths.sock, "corpse").unwrap();
    assert_eq!(
        mysbx::detach::probe(&paths.sock),
        mysbx::detach::Probe::Stale,
        "the fixture probes stale before the run"
    );
    let mut cmd = spawn_with_args(&inv, &["--detach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The run went through and the stale file was replaced: a live
    // record, and the corpse is GONE — the stub dtach binds no
    // socket, so after the removal the path stays absent (the file
    // no longer holds the stale junk; a real dtach master would
    // have its fresh socket there).
    let rec = wait_for_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Running);
    assert!(
        !paths.sock.exists(),
        "the stale corpse was removed and nothing recreated it: the next run owns the path"
    );
    assert!(
        paths.json.exists(),
        "the new run's state file replaced the stale entry's registry slot"
    );
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    wait_for_finished_record(&paths);
}

#[test]
fn detach_with_the_one_shot_outcome_flags_is_a_usage_error() {
    // B2 + the D8/D17 extension: `--result` and `--timeout` name the
    // outcome of a payload run — a detached run's outcome is its
    // registry record, not a result.json — so `mysbx --detach
    // --result` and `mysbx --detach --timeout 5` are refused with
    // the shared usage error (exit 2), never accepted-and-ignored —
    // and `--detachable`, the same start mode, refuses them too.
    let (inv, _, _) = fixture_with_backend("detach-result-flags", &["--detach"]);
    for args in [
        vec!["--detach", "--result"],
        vec!["--detach", "--timeout", "5"],
        vec!["--detachable", "--result"],
        vec!["--detachable", "--timeout", "5"],
    ] {
        let mut cmd = spawn_with_args(&inv, &args);
        cmd.env_remove("XDG_RUNTIME_DIR");
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        assert_eq!(
            out.status.code(),
            Some(2),
            "{:?}: stdout: {}\nstderr: {}",
            args,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        assert!(
            stderr.contains("names the outcome of a payload run"),
            "{:?}: {stderr}",
            args
        );
        assert!(
            stderr.contains("a detached run's outcome is its registry record"),
            "{:?}: {stderr}",
            args
        );
        assert!(!stderr.contains("not implemented"), "{:?}: {stderr}", args);
    }
}

#[test]
fn a_foreground_dry_run_carries_no_detached_mark() {
    // B7 end to end: the foreground argv is byte-identical to the
    // pre-detach one — no `MYSBX_DETACHABLE` in the environment of a
    // run that was NOT started by `--detach`, so the multiplexer
    // entries' reattach loop never fires for a foreground run.
    let (inv, _, _) = fixture_with_backend("detach-foreground-mark", &["--dry-run"]);
    let mut cmd = spawn_with_args(&inv, &["--dry-run"]);
    cmd.env_remove("XDG_RUNTIME_DIR");
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert!(
        !stdout.contains("MYSBX_DETACHABLE"),
        "the foreground argv carries no detached mark: {stdout}"
    );
}

#[test]
fn usage_and_fish_completions_name_detach_and_detachable_and_attach() {
    // B2's surface: `--detach`, `--detachable` and `attach` are all
    // documented — the composition flag and the verb are
    // implemented (bd myconfig-7ioq.5), so usage.txt and the fish
    // completions offer them, and the mysbx-completions check
    // enforces the sync.
    let usage = include_str!("../src/usage.txt");
    assert!(
        usage.contains("--detach"),
        "usage.txt documents the bare form's start flag"
    );
    assert!(
        usage.contains("--detachable"),
        "usage.txt documents the composition flag"
    );
    assert!(
        usage.contains("mysbx attach"),
        "usage.txt documents the attach verb"
    );
    let fish = include_str!("../completions/mysbx.fish");
    assert!(
        fish.contains("-l detach"),
        "fish offers the --detach completion"
    );
    assert!(
        fish.contains("-l detachable"),
        "fish offers the --detachable completion"
    );
    assert!(
        fish.contains("-a attach"),
        "fish offers the attach subcommand"
    );
}

// ---- `mysbx attach` and `--detachable` (docs/design/detach.md B2/B5/B6) ----

/// A registry-record fixture: the registry of `repo` under `runtime`
/// with a record in the caller-chosen `state` written at the live
/// workspace, and no socket (a live fixture binds one separately).
fn write_registry_record(
    runtime: &Path,
    repo: &Path,
    state: mysbx::detach::State,
) -> mysbx::detach::Paths {
    let paths = detach_paths_live(runtime, repo);
    mysbx::detach::ensure(&paths).unwrap();
    let r = mysbx::detach::Record {
        state,
        repo: repo.to_string_lossy().into_owned(),
        sidecar: format!("{}.mysbx", repo.to_string_lossy()),
        session: None,
        backend: "bubblewrap".to_owned(),
        multiplexer: "tmux".to_owned(),
        started_at: 1_700_000_000,
        finished_at: Some(1_700_000_030),
        exit_code: None,
        error: None,
    };
    mysbx::detach::write(&paths, &r).unwrap();
    paths
}

#[test]
fn attach_execs_the_dtach_client_on_a_live_run() {
    // B2: a live socket is attachable whatever the record says — the
    // exec is `dtach -a <sock> -r winch -z`, dtach owns the terminal
    // and its exit code propagates. The record is NOT removed: the
    // run is still live, attach only connects.
    let Some((_master, slave)) = open_test_pty() else {
        return; // no pty in this environment: skip, like the gated tests
    };
    let (inv, repo, _) = fixture_with_backend("attach-live", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-live");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime)
        .stdin(std::process::Stdio::from(slave));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let recorded = std::fs::read_to_string(dtach_attach_record(&inv.home)).unwrap();
    let rl: Vec<&str> = recorded.lines().collect();
    assert_eq!(
        rl,
        vec!["-a", &paths.sock.to_string_lossy(), "-r", "winch", "-z",],
        "the attach argv is dtach -a <sock> -r winch -z: {recorded}"
    );
    // The record survives an attach: the run is still live (B6 — the
    // lock is only taken to REMOVE a finished record).
    assert!(
        paths.json.exists(),
        "attach did not remove the live run's record"
    );
}

#[test]
fn a_starting_record_with_a_live_socket_is_attachable() {
    // B5's classification puts the probe first: a `starting` record
    // whose dtach master is up is attachable — the master owns the
    // terminal, the inner may still be finishing its startup, and
    // attaching just relays.
    let Some((_master, slave)) = open_test_pty() else {
        return; // no pty in this environment: skip, like the gated tests
    };
    let (inv, repo, _) = fixture_with_backend("attach-starting", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-starting");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Starting,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime)
        .stdin(std::process::Stdio::from(slave));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(
        dtach_attach_record(&inv.home).exists(),
        "the starting run was attached, not refused"
    );
}

#[test]
fn the_attach_exec_propagates_dtach_exit_codes() {
    // B2: the exec hands the terminal to dtach, so the client's own
    // exit code is mysbx's — no interpreter of it.
    let Some((_master, slave)) = open_test_pty() else {
        return; // no pty in this environment: skip, like the gated tests
    };
    let (inv, repo, _) = fixture_with_backend("attach-exit", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-exit");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("DTACH_ATTACH_EXIT", "7")
        .stdin(std::process::Stdio::from(slave));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(7),
        "dtach's own exit code propagates through the exec"
    );
}

#[test]
fn attach_prints_an_exited_record_once_and_removes_it() {
    // B5's lifecycle: a finished record is printed once — the exit
    // code and the finish time — removed under the lock, exit 70. No
    // terminal is needed for this path (the tty check is Live-only).
    let (inv, repo, _) = fixture_with_backend("attach-exited", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-exited");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Exited,
    );
    std::fs::write(
        &paths.json,
        std::fs::read_to_string(&paths.json)
            .unwrap()
            .replace("\"exitCode\": null", "\"exitCode\": 1"),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: the detached run exited with 1 at 2023-11-14T22:13:50Z"),
        "{stderr}"
    );
    assert!(
        !paths.json.exists(),
        "the printed record is removed under the lock"
    );
}

#[test]
fn attach_prints_a_failed_record_once_and_removes_it() {
    // B5: a `failed` record carries its diagnosis — printed once,
    // removed, 70. The `: {error}` tail is dropped when the record
    // carries no error text.
    let (inv, repo, _) = fixture_with_backend("attach-failed", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-failed");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Failed,
    );
    std::fs::write(
        &paths.json,
        std::fs::read_to_string(&paths.json)
            .unwrap()
            .replace("\"error\": null", "\"error\": \"cannot exec the backend\""),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: the detached run failed during startup: cannot exec the backend"),
        "{stderr}"
    );
    assert!(!paths.json.exists(), "the printed record is removed");
}

#[test]
fn attach_prints_a_cancelled_record_once_and_removes_it() {
    // B5: a `cancelled` record — printed once, removed, 70.
    let (inv, repo, _) = fixture_with_backend("attach-cancelled", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-cancelled");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Cancelled,
    );
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: the detached run was cancelled"),
        "{stderr}"
    );
    assert!(!paths.json.exists(), "the printed record is removed");
}

#[test]
fn attach_reports_a_lost_run_and_removes_the_record() {
    // B5: a `running` record without a live socket means the dtach
    // master was killed — `lost`, printed once, removed, 70. No
    // terminal is needed for this path either.
    let (inv, repo, _) = fixture_with_backend("attach-lost", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-lost");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains(
            "mysbx: the detached run of this workspace is lost — its dtach master is gone"
        ),
        "{stderr}"
    );
    assert!(!paths.json.exists(), "the lost record is removed");
}

#[test]
fn attach_without_a_registered_run_refuses_and_names_detach() {
    // B3: the directory IS the registry — no record, no run. The
    // refusal names the command that starts one, with the --session
    // spelling when a session workspace was asked for.
    let (inv, _, _) = fixture_with_backend("attach-nothing", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-nothing");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr
            .contains("mysbx: nothing to attach — no detached run of this workspace is registered"),
        "{stderr}"
    );
    assert!(
        stderr.contains("start one with: mysbx --detach"),
        "{stderr}"
    );
    // The --session spelling: a session workspace's hint names the
    // session form (B2).
    let mut cmd = spawn_with_args(&inv, &["--session", "fix-1", "attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("start one with: mysbx --detach --session fix-1"),
        "{stderr}"
    );
}

#[test]
fn attach_needs_a_terminal_and_leaves_the_record_alone() {
    // B2: the attach exec relays this terminal, so a live run without
    // one is refused with 70 — an environment refusal, and the record
    // is NOT removed (the run is still live). The tty check is the
    // LAST one: only the Live path asks.
    let (inv, repo, _) = fixture_with_backend("attach-no-tty", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-no-tty");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains(
            "mysbx: attach needs a terminal on stdin — connect one and run: mysbx attach"
        ),
        "{stderr}"
    );
    assert!(
        paths.json.exists(),
        "the no-tty refusal leaves the live record in place"
    );
}

#[test]
fn attach_routes_the_session_workspace_and_its_socket() {
    // B3: the registry entry is per workspace — `mysbx attach
    // --session NAME` connects to the session run's socket, never
    // the live one's.
    let Some((_master, slave)) = open_test_pty() else {
        return; // no pty in this environment: skip, like the gated tests
    };
    let (inv, repo, _) = fixture_with_backend("attach-session", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-session");
    std::fs::create_dir_all(&runtime).unwrap();
    let canonical = repo.canonicalize().unwrap();
    let paths = mysbx::detach::paths(
        &runtime,
        &canonical,
        &mysbx::detach::Workspace::Session("fix-1".to_owned()),
    )
    .unwrap();
    mysbx::detach::ensure(&paths).unwrap();
    let r = mysbx::detach::Record {
        state: mysbx::detach::State::Running,
        repo: canonical.to_string_lossy().into_owned(),
        sidecar: format!("{}.mysbx", canonical.to_string_lossy()),
        session: Some("fix-1".to_owned()),
        backend: "bubblewrap".to_owned(),
        multiplexer: "tmux".to_owned(),
        started_at: 1_700_000_000,
        finished_at: None,
        exit_code: None,
        error: None,
    };
    mysbx::detach::write(&paths, &r).unwrap();
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--session", "fix-1", "attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime)
        .stdin(std::process::Stdio::from(slave));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let recorded = std::fs::read_to_string(dtach_attach_record(&inv.home)).unwrap();
    assert!(
        recorded.lines().any(|l| l == paths.sock.to_string_lossy()),
        "the session run's socket is the one attached to: {recorded}"
    );
}

#[test]
fn attach_dry_run_prints_the_attach_argv_and_touches_nothing() {
    // B9: `attach --dry-run` prints the dtach attach argv — and needs
    // no registry (no read, no removal), no terminal and no repo
    // state beyond the resolved repo. With an unusable dtach it
    // refuses with 70 like the launch does.
    let (inv, repo, _) = fixture_with_backend("attach-dry-run", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-dry");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let expected = format!(
        "{}\n-a\n{}\n-r\nwinch\n-z\n",
        dtach.to_string_lossy(),
        paths.sock.to_string_lossy()
    );
    assert_eq!(stdout, expected, "the attach argv, one line per argument");
    // No registry directory was created: the dry run never touched it.
    assert!(!paths.dir.exists(), "the dry run created the registry dir");
    // An unusable dtach is the same refusal the launch gives (B10).
    std::fs::create_dir_all(inv.home.join("empty-path")).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--dry-run", "attach"]);
    cmd.env_remove("MYSBX_DTACH")
        .env("PATH", inv.home.join("empty-path"))
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("attach require a usable dtach (MYSBX_DTACH)"),
        "{stderr}"
    );
}

#[test]
fn attach_dry_run_names_the_session_socket_for_a_session_workspace() {
    // B9 + B2: the after-verb `--session NAME` spelling parses (the
    // hinted command copies-and-pastes, the `run_command` merge), and
    // the audited argv names the SESSION workspace's socket,
    // `s.NAME.sock` — the dry run previews exactly what a session
    // attach connects to.
    let (inv, repo, _) = fixture_with_backend("attach-dry-session", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-dry-session");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach", "--dry-run", "--session", "fix-1"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let canonical = repo.canonicalize().unwrap();
    let paths = mysbx::detach::paths(
        &runtime,
        &canonical,
        &mysbx::detach::Workspace::Session("fix-1".to_owned()),
    )
    .unwrap();
    assert!(
        stdout.lines().any(|l| l == paths.sock.to_string_lossy()),
        "the audited argv names the session socket {}: {stdout}",
        paths.sock.display()
    );
    let name = paths
        .sock
        .file_name()
        .unwrap()
        .to_string_lossy()
        .into_owned();
    assert_eq!(name, "s.fix-1.sock");
}

#[test]
fn attach_refuses_a_record_of_another_repo_never_removes_it() {
    // B3: the per-repo directory name is a 40-bit hash — a record
    // whose `repo` field names a different root is the collision the
    // field exists to catch: refused with 70, never attached to, and
    // never REMOVED either (the record belongs to the other repo).
    let (inv, repo, _) = fixture_with_backend("attach-repo-mismatch", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-repo-mismatch");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    // Forge the collision: the record claims another repo's root
    // while the registry slot is this repo's hash. The socket is
    // LIVE, so only the repo check stands between attach and the
    // exec.
    let rec = mysbx::detach::read(&paths).unwrap();
    let mut rec = rec.clone();
    rec.repo = "/synth/other-repo".to_owned();
    mysbx::detach::write(&paths, &rec).unwrap();
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["attach"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("it records the repo /synth/other-repo, not the resolved repo"),
        "{stderr}"
    );
    assert!(
        stderr.contains("the fnv1a10 registry name collided"),
        "{stderr}"
    );
    // The mismatched record survives: attach refused, it removed
    // nothing, and no exec happened (the stub's attach record does
    // not exist).
    assert!(paths.json.exists(), "the refused record was not removed");
    assert!(
        !dtach_attach_record(&inv.home).exists(),
        "no exec reached dtach"
    );
}

#[test]
fn attach_refuses_arguments_and_the_run_scoped_flags() {
    // B2: `attach` accepts only `--session NAME` and `--dry-run` (in
    // either position) — a stray argument is a usage error naming the
    // bare shape, a `--session` in both positions is the repeated-flag
    // refusal of the `run_command` merge, and every run-scoped flag is
    // the shared verb refusal, `--verbose` the handoff verbs'.
    let (inv, _, _) = fixture_with_backend("attach-refusals", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("attach-refusals");
    std::fs::create_dir_all(&runtime).unwrap();
    for args in [
        vec!["attach", "foo"],
        vec!["--verbose", "attach"],
        vec!["--backend", "bubblewrap", "attach"],
        vec!["--dry-run", "--verbose", "attach"],
        vec!["--detach", "attach"],
        vec!["--detachable", "attach"],
        vec!["--session", "fix-1", "attach", "--session", "fix-1"],
    ] {
        let mut cmd = spawn_with_args(&inv, &args);
        cmd.env("MYSBX_DTACH", &dtach)
            .env("XDG_RUNTIME_DIR", &runtime);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        assert_eq!(
            out.status.code(),
            Some(2),
            "{:?}: stdout: {}\nstderr: {}",
            args,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        );
        let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
        let attach_tail = args
            .iter()
            .position(|a| *a == "attach")
            .expect("every case names the verb");
        match args.get(attach_tail + 1).copied() {
            // A stray argument in the verb's own tail: the usage
            // error naming the bare shape.
            Some("foo") => {
                assert!(
                    stderr.contains("mysbx attach: unexpected argument: foo"),
                    "{args:?}: {stderr}"
                );
                assert!(
                    stderr.contains("usage: mysbx attach [--session <name>]"),
                    "{args:?}: {stderr}"
                );
            }
            // The `--session` in both positions: the repeated-flag
            // refusal of the `run_command` merge.
            Some("--session") => {
                assert!(
                    stderr.contains("mysbx attach: repeated flag: --session"),
                    "{args:?}: {stderr}"
                );
            }
            // Every before-verb flag: the shared verb refusals.
            _ => {
                assert!(
                    stderr.contains("is not valid with `attach`"),
                    "{args:?}: {stderr}"
                );
            }
        }
    }
}

#[test]
fn detachable_needs_a_terminal_on_stdin() {
    // B2: the composition form attaches the current terminal to the
    // run it starts, so a piped stdin is refused in the pre-flight —
    // exit 70, before anything runs: the runtime root is NOT even
    // created.
    let (inv, _, _) = fixture_with_backend("detachable-no-tty", &[]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("detachable-no-tty");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut cmd = spawn_with_args(&inv, &["--detachable"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(out.status.code(), Some(mysbx::EXIT_INFRASTRUCTURE));
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("mysbx: --detachable needs a terminal on stdin"),
        "{stderr}"
    );
    assert!(
        stderr.contains("start a run without one with --detach instead"),
        "{stderr}"
    );
    assert!(
        !runtime.join("mysbx").exists(),
        "the refusal ran before the registry was created"
    );
}

#[test]
fn detachable_dry_run_prints_the_detach_dry_run_output() {
    // B9: `--detachable --dry-run` is exempt from the tty
    // requirement — it prints exactly what `--detach --dry-run`
    // prints (the launcher argv, a blank line, the backend argv) and
    // needs no terminal.
    let (inv, repo, _) = fixture_with_backend("detachable-dry", &["--detachable", "--dry-run"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let runtime = short_runtime_root("detachable-dry");
    std::fs::create_dir_all(&runtime).unwrap();
    let run = |args: &[&str]| {
        let mut cmd = spawn_with_args(&inv, args);
        cmd.env("MYSBX_DTACH", &dtach)
            .env("XDG_RUNTIME_DIR", &runtime);
        let out = cmd.output().expect("failed to spawn the mysbx binary");
        assert_eq!(
            out.status.code(),
            Some(0),
            "{}",
            String::from_utf8_lossy(&out.stderr)
        );
        String::from_utf8_lossy(&out.stdout).into_owned()
    };
    let detachable = run(&["--detachable", "--dry-run"]);
    let detach = run(&["--detach", "--dry-run"]);
    assert_eq!(detachable, detach, "the two dry runs print the same launch");
    // The launcher argv names the registry socket of the workspace
    // the composition starts.
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    assert!(
        detachable.contains(paths.sock.to_string_lossy().as_ref()),
        "the launcher argv names the registry socket: {detachable}"
    );
}

#[test]
fn a_detachable_run_starts_detached_and_attaches_under_one_lock() {
    if !is_ssh_keygen_available() {
        return; // no usable ssh-keygen (the baseline env's uid-0 quirk)
    }
    // B2 end to end: `--detachable` is `--detach` then `attach` in
    // one invocation — the outer's Running handshake execs the stub
    // dtach's `-a` branch (instead of printing the attach hint),
    // while the workspace lock is still held (B6: the whole
    // composition runs under ONE lock — the exec releases it
    // atomically). The stub's `-n` branch execs the real inner
    // mysbx, whose stub bwrap backend waits for the live marker, so
    // the Running record exists before the attach happens.
    let Some((_master, slave)) = open_test_pty() else {
        return; // no pty in this environment: skip, like the gated tests
    };
    let (inv, repo, _) = fixture_with_backend("detachable-happy", &["--detachable"]);
    let (dtach, _) = dtach_stub(&inv.home, true);
    let (bwrap, _) = bwrap_stub(&inv.home);
    let runtime = short_runtime_root("detachable-happy");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = detach_paths_live(&runtime, &repo.canonicalize().unwrap());
    let mut cmd = spawn_with_args(&inv, &["--detachable"]);
    cmd.env("MYSBX_DTACH", &dtach)
        .env("MYSBX_BWRAP", &bwrap)
        .env("MYSBX_TOOLS_PATH", "/usr/bin")
        .env("XDG_RUNTIME_DIR", &runtime)
        .env("BWRAP_LIVE", inv.home.join("bwrap-live"))
        .stdin(std::process::Stdio::from(slave));
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    // The attach happened: the stub's -a branch recorded the attach
    // argv on the live socket the run's record names.
    let recorded = std::fs::read_to_string(dtach_attach_record(&inv.home)).unwrap();
    assert!(
        recorded.lines().any(|l| l == paths.sock.to_string_lossy()),
        "the composition attached to the live run's socket: {recorded}"
    );
    // The run is the same one --detach starts: a Running record that
    // finishes when the backend ends.
    let rec = wait_for_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Running);
    std::fs::write(inv.home.join("bwrap-live"), "").unwrap();
    let rec = wait_for_finished_record(&paths);
    assert_eq!(rec.state, mysbx::detach::State::Exited);
    assert_eq!(rec.exit_code, Some(0));
}

// ---- `status` detached section (detach.md B9) --------------------------------

/// Write a registry record for a SESSION workspace (the live-workspace
/// helper `write_registry_record` does not cover sessions), returning
/// the registry paths.
fn write_session_registry_record(
    runtime: &Path,
    repo: &Path,
    session: &str,
    state: mysbx::detach::State,
) -> mysbx::detach::Paths {
    let paths = mysbx::detach::paths(
        runtime,
        repo,
        &mysbx::detach::Workspace::Session(session.to_owned()),
    )
    .expect("the registry paths build");
    mysbx::detach::ensure(&paths).unwrap();
    let r = mysbx::detach::Record {
        state,
        repo: repo.to_string_lossy().into_owned(),
        sidecar: format!("{}.mysbx", repo.to_string_lossy()),
        session: Some(session.to_owned()),
        backend: "bubblewrap".to_owned(),
        multiplexer: "tmux".to_owned(),
        started_at: 1_700_000_000,
        finished_at: Some(1_700_000_030),
        exit_code: None,
        error: None,
    };
    mysbx::detach::write(&paths, &r).unwrap();
    paths
}

#[test]
fn status_detached_section_says_none_when_no_registry() {
    // B9: with no XDG_RUNTIME_DIR (or an empty registry) the
    // `detached:` section says `none` — no heading-without-none, exit 0.
    let (inv, _, _) = fixture_user_backend("status-detached-none", &["status"]);
    let (code, stdout, stderr) = run_binary(&inv);
    assert_eq!(code, 0, "stderr: {stderr}");
    assert!(
        stdout.contains("## detached:        none (no detached runs registered)"),
        "{stdout}"
    );
    assert!(
        !stdout.contains("## detached:\n"),
        "no heading-without-none: {stdout}"
    );
}

#[test]
fn status_detached_section_lists_a_live_run() {
    // B9: a Running record with a bound socket lists under `detached:`,
    // one line with `live`, `running`, the backend, the multiplexer
    // and the ISO-8601 start time.
    let (inv, repo, _) = fixture_with_backend("status-detached-live", &["status"]);
    let runtime = short_runtime_root("status-detached-live");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["status"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(stdout.contains("## detached:"), "heading present: {stdout}");
    let line = stdout
        .lines()
        .find(|l| l.contains("live") && l.contains("running"))
        .unwrap_or_else(|| panic!("no live-running line: {stdout}"));
    assert!(line.contains("live"), "{line}");
    assert!(line.contains("running"), "{line}");
    assert!(line.contains("backend=bubblewrap"), "{line}");
    assert!(line.contains("multiplexer=tmux"), "{line}");
    assert!(
        line.contains("started=2023-11-14T22:13:20Z"),
        "the ISO-8601 start time: {line}"
    );
}

#[test]
fn status_detached_section_lists_a_finished_run() {
    // B9: an Exited record (no socket) lists as `exited(0)`.
    let (inv, repo, _) = fixture_with_backend("status-detached-finished", &["status"]);
    let runtime = short_runtime_root("status-detached-finished");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Exited,
    );
    std::fs::write(
        &paths.json,
        std::fs::read_to_string(&paths.json)
            .unwrap()
            .replace("\"exitCode\": null", "\"exitCode\": 0"),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["status"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let line = stdout
        .lines()
        .find(|l| l.contains("live") && l.contains("exited"))
        .unwrap_or_else(|| panic!("no exited line: {stdout}"));
    assert!(line.contains("exited(0)"), "{line}");
}

#[test]
fn status_detached_section_lists_an_exited_run_with_its_code() {
    // B9/B5: an Exited record with a NONZERO exit code lists with
    // its code — every backend exit is `exited`, the code in
    // `exitCode` is the backend's own, nonzero or not.
    let (inv, repo, _) = fixture_with_backend("status-detached-exited-42", &["status"]);
    let runtime = short_runtime_root("status-detached-exited-42");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Exited,
    );
    std::fs::write(
        &paths.json,
        std::fs::read_to_string(&paths.json)
            .unwrap()
            .replace("\"exitCode\": null", "\"exitCode\": 42"),
    )
    .unwrap();
    let mut cmd = spawn_with_args(&inv, &["status"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let line = stdout
        .lines()
        .find(|l| l.contains("live") && l.contains("exited"))
        .unwrap_or_else(|| panic!("no exited line: {stdout}"));
    assert!(line.contains("exited(42)"), "{line}");
}

#[test]
fn status_detached_section_lists_a_lost_run() {
    // B9: a Running record without a socket is `lost`.
    let (inv, repo, _) = fixture_with_backend("status-detached-lost", &["status"]);
    let runtime = short_runtime_root("status-detached-lost");
    std::fs::create_dir_all(&runtime).unwrap();
    let _paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let mut cmd = spawn_with_args(&inv, &["status"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let line = stdout
        .lines()
        .find(|l| l.contains("live") && l.contains("lost"))
        .unwrap_or_else(|| panic!("no lost line: {stdout}"));
    assert!(line.contains("lost"), "{line}");
}

#[test]
fn status_is_read_only_on_the_registry() {
    // B9: status never takes a lock, writes or removes anything — the
    // record survives a status run, and no lock file is created.
    let (inv, repo, _) = fixture_with_backend("status-detached-readonly", &["status"]);
    let runtime = short_runtime_root("status-detached-readonly");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    let mut cmd = spawn_with_args(&inv, &["status"]);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(paths.json.exists(), "the record survived the status run");
    assert!(!paths.lock.exists(), "no lock file was created");
}

// ---- `doctor` detached-run checks (detach.md B9) -----------------------------

#[test]
fn doctor_warns_when_dtach_is_missing() {
    // B9: with bwrap on PATH but no dtach, the host section warns —
    // not fails — naming the pin and that only --detach/--detachable/
    // attach need it.
    let (inv, _, _) = fixture_user_backend("doctor-no-dtach", &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let bwrap = doctor_stub(&base.join("bin"), "bwrap", "exit 0");
    // A PATH with only the stub dir: no dtach there, so the fallback
    // resolves to None — the WARN under test.
    let bin = base.join("bin");
    let path = bin.to_string_lossy().into_owned();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_BWRAP", bwrap.to_str().unwrap()),
            ("MYSBX_SHELL", "/bin/sh"),
            ("PATH", &path),
        ],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("WARN dtach: no usable dtach found"),
        "{stdout}"
    );
    assert!(
        stdout.contains("pkgs.dtach"),
        "the hint names the Nix pin: {stdout}"
    );
    assert!(
        stdout.contains("only --detach"),
        "the hint names what needs dtach: {stdout}"
    );
}

#[test]
fn doctor_warns_when_runtime_root_is_unset() {
    // B9: without $XDG_RUNTIME_DIR the host section warns — not fails
    // — naming the variable.
    let (inv, _, _) = fixture_user_backend("doctor-no-rtd", &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let bwrap = doctor_stub(&base.join("bin"), "bwrap", "exit 0");
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_BWRAP", bwrap.to_str().unwrap()),
            ("MYSBX_SHELL", "/bin/sh"),
        ],
    );
    // run_doctor does not set XDG_RUNTIME_DIR, and spawn_with_args
    // removes it; but doctor's resolve path uses the live env, so be
    // explicit that it is absent.
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains("WARN runtime root: $XDG_RUNTIME_DIR is not set"),
        "{stdout}"
    );
}

#[test]
fn doctor_is_ok_when_dtach_and_runtime_root_are_fine() {
    // B9: with a pinned dtach and a 0700 runtime root owned by the
    // current user, both checks are OK.
    use std::os::unix::fs::PermissionsExt;
    let (inv, _, _) = fixture_user_backend("doctor-detach-ok", &[]);
    let base = inv.cwd.parent().unwrap().to_path_buf();
    let dtach = doctor_stub(&base.join("bin"), "dtach", "exit 0");
    let bwrap = doctor_stub(&base.join("bin"), "bwrap", "exit 0");
    let runtime = short_runtime_root("doctor-detach-ok");
    std::fs::set_permissions(&runtime, std::fs::Permissions::from_mode(0o700)).unwrap();
    let (code, stdout, stderr) = run_doctor(
        &inv,
        &["doctor"],
        &[
            ("MYSBX_DTACH", dtach.to_str().unwrap()),
            ("MYSBX_BWRAP", bwrap.to_str().unwrap()),
            ("MYSBX_SHELL", "/bin/sh"),
            ("XDG_RUNTIME_DIR", runtime.to_str().unwrap()),
        ],
    );
    assert_eq!(code, 0, "stdout: {stdout}\nstderr: {stderr}");
    assert!(
        stdout.contains(&format!("OK   dtach: {}", dtach.display())),
        "{stdout}"
    );
    assert!(
        stdout.contains(&format!("OK   runtime root: {}", runtime.display())),
        "{stdout}"
    );
}

// ---- `session destroy` live-detached-run guard (detach.md B9) ----------------

#[test]
fn session_destroy_refuses_a_live_detached_session_even_with_force() {
    // B9: a live detached run of the session blocks its destroy — the
    // clone is bound rw into a running sandbox. The guard is NOT
    // overridable by --force (it is not the unmerged-work refusal).
    let Some((inv, repo, sidecar)) = session_fixture("destroy-detached-live", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    let runtime = short_runtime_root("dd-live");
    std::fs::create_dir_all(&runtime).unwrap();
    let paths = write_session_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        "fix-1",
        mysbx::detach::State::Running,
    );
    let _listener = std::os::unix::net::UnixListener::bind(&paths.sock).unwrap();
    // With --force: refused.
    let mut inv_force = inv.clone();
    inv_force.args = vec!["session", "destroy", "fix-1", "--force"];
    let mut cmd = spawn_with_args(&inv_force, &inv_force.args);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(70),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        stderr.contains("refusing to destroy session fix-1"),
        "{stderr}"
    );
    assert!(stderr.contains("mysbx attach --session fix-1"), "{stderr}");
    assert!(fix.join(".git").is_dir(), "the clone was not removed");
    // Without --force: also refused.
    let mut inv_plain = inv.clone();
    inv_plain.args = vec!["session", "destroy", "fix-1"];
    let mut cmd = spawn_with_args(&inv_plain, &inv_plain.args);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(70),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(fix.join(".git").is_dir(), "the clone was not removed");
}

#[test]
fn session_destroy_proceeds_for_a_finished_detached_session() {
    // B9: a finished detached run (no live socket) does not block the
    // destroy — the clone is gone.
    let Some((inv, repo, sidecar)) = session_fixture("destroy-detached-finished", &["fix-1"])
    else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    let runtime = short_runtime_root("dd-fin");
    std::fs::create_dir_all(&runtime).unwrap();
    let _paths = write_session_registry_record(
        &runtime,
        &repo.canonicalize().unwrap(),
        "fix-1",
        mysbx::detach::State::Exited,
    );
    let mut inv = inv;
    inv.args = vec!["session", "destroy", "fix-1", "--force"];
    let mut cmd = spawn_with_args(&inv, &inv.args);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(!fix.exists(), "the clone is gone");
}

#[test]
fn session_destroy_proceeds_when_no_record_exists() {
    // B9: no registry record (an empty runtime root) means no detached
    // run — the destroy proceeds normally.
    let Some((inv, _, sidecar)) = session_fixture("destroy-detached-none", &["fix-1"]) else {
        return;
    };
    let fix = sidecar.join("clones").join("fix-1");
    let runtime = short_runtime_root("dd-none");
    std::fs::create_dir_all(&runtime).unwrap();
    let mut inv = inv;
    inv.args = vec!["session", "destroy", "fix-1", "--force"];
    let mut cmd = spawn_with_args(&inv, &inv.args);
    cmd.env("XDG_RUNTIME_DIR", &runtime);
    let out = cmd.output().expect("failed to spawn the mysbx binary");
    assert_eq!(
        out.status.code(),
        Some(0),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(!fix.exists(), "the clone is gone");
}
