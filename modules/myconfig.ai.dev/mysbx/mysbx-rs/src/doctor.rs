// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx doctor` — the host health check of the sandbox backends.
//!
//! Host-side and read-only like `status`, but it probes instead of
//! listing: for the configured backend (or the backends named on the
//! command line) it checks the runtime binaries the wrapper pins, the
//! container image, `/dev/kvm` for krun, the Landlock ABI for nono, a
//! throwaway sandbox start and the model endpoint through the
//! backend's network.
//!
//! Output: `== section ==` headers, one `OK`/`WARN`/`FAIL` line per
//! check with a remediation hint, and a final problem count. A `FAIL`
//! is something that stops the backend from starting; a `WARN` is not
//! (a stale image, an unreachable model endpoint). Components the host
//! does not have on purpose (no endpoint configured, the network
//! denied, a backend that is not configured) are reported as not
//! applicable, never as a failure. A probe that depends on a failed
//! check is a `SKIP` line naming what it depends on.
//!
//! Every probe command is bounded by [`PROBE_TIMEOUT`], the endpoint's
//! name resolution by [`RESOLVE_TIMEOUT`]; a timeout is reported like
//! a failed probe.
//!
//! Exit codes: `0` no `FAIL`, `1` at least one `FAIL`, `2` usage
//! error, `70` when the configuration cannot be loaded.
//!
//! The probe argv builders are pure so the tests assert the exact
//! arguments; the podman runtime settings mirror the ones `sandbox`
//! in lib.rs computes for a run.

use crate::config::Config;
use crate::merge;
use crate::repo;
use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::mpsc;
use std::time::{Duration, Instant};

const USAGE_DOCTOR: &str = "usage: mysbx doctor [BACKEND...]";

/// The backends the doctor knows, the same set the run pipeline accepts.
pub const BACKENDS: &[&str] = &["bubblewrap", "podman-gvisor", "nono", "podman-krun"];

/// The variables that name a model endpoint, in lookup order.
pub const ENDPOINT_VARS: &[&str] = &["OPENAI_BASE_URL", "ANTHROPIC_BASE_URL"];

/// The in-container endpoint probe: any HTTP answer counts as
/// reachable; the URL travels as `$0`, so it needs no quoting.
pub const CURL_CMD: &str = "curl -sS -o /dev/null --max-time 5 -w 'HTTP %{http_code}' \"$0\"";

/// The upper bound of one probe command; a krun startup boots a VM.
pub const PROBE_TIMEOUT: Duration = Duration::from_secs(30);

/// The upper bound of the endpoint's host name resolution.
pub const RESOLVE_TIMEOUT: Duration = Duration::from_secs(5);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Ok,
    Warn,
    Fail,
    /// Not run because a check it depends on failed; not counted.
    Skip,
}

/// One check result: one output line.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Check {
    pub level: Level,
    pub name: String,
    pub detail: String,
    pub hint: Option<String>,
}

impl Check {
    pub fn ok(name: &str, detail: impl Into<String>) -> Check {
        Check {
            level: Level::Ok,
            name: name.to_owned(),
            detail: detail.into(),
            hint: None,
        }
    }

    pub fn warn(name: &str, detail: impl Into<String>, hint: impl Into<String>) -> Check {
        Check {
            level: Level::Warn,
            name: name.to_owned(),
            detail: detail.into(),
            hint: Some(hint.into()),
        }
    }

    /// A probe that was not run because `blockers` failed.
    pub fn skip(name: &str, blockers: &[&str]) -> Check {
        Check {
            level: Level::Skip,
            name: name.to_owned(),
            detail: format!("not run (depends on: {})", blockers.join(", ")),
            hint: None,
        }
    }

    pub fn fail(name: &str, detail: impl Into<String>, hint: impl Into<String>) -> Check {
        Check {
            level: Level::Fail,
            name: name.to_owned(),
            detail: detail.into(),
            hint: Some(hint.into()),
        }
    }

    /// The output line: `OK   name: detail`, `FAIL name: detail — hint`.
    pub fn line(&self) -> String {
        let tag = match self.level {
            Level::Ok => "OK  ",
            Level::Warn => "WARN",
            Level::Fail => "FAIL",
            Level::Skip => "SKIP",
        };
        match &self.hint {
            Some(hint) => format!("{tag} {}: {} — {hint}", self.name, self.detail),
            None => format!("{tag} {}: {}", self.name, self.detail),
        }
    }
}

/// A `== title ==` section and its checks.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Section {
    pub title: String,
    pub checks: Vec<Check>,
}

/// The output lines of `sections` plus the summary line, and the
/// number of `FAIL` and `WARN` checks.
pub fn render(sections: &[Section]) -> (Vec<String>, usize, usize) {
    let mut lines = Vec::new();
    let mut problems = 0;
    let mut warnings = 0;
    for section in sections {
        lines.push(format!("== {} ==", section.title));
        for check in &section.checks {
            match check.level {
                Level::Fail => problems += 1,
                Level::Warn => warnings += 1,
                Level::Ok | Level::Skip => {}
            }
            lines.push(check.line());
        }
    }
    lines.push(format!("mysbx doctor: {problems} problem(s), {warnings} warning(s)"));
    (lines, problems, warnings)
}

/// Resolve `name` the way `Command::new` would: a name with a `/` is
/// taken as a path, a bare name is looked up in `path_var`. `None`
/// when no executable regular file is found.
pub fn resolve_executable(name: &str, path_var: Option<&OsStr>) -> Option<PathBuf> {
    if name.is_empty() {
        return None;
    }
    if name.contains('/') {
        let path = PathBuf::from(name);
        return if is_executable(&path) {
            Some(path)
        } else {
            None
        };
    }
    let path_var = path_var?;
    std::env::split_paths(path_var)
        .map(|dir| dir.join(name))
        .find(|candidate| is_executable(candidate))
}

fn is_executable(path: &Path) -> bool {
    use std::os::unix::fs::PermissionsExt;
    std::fs::metadata(path)
        .map(|m| m.is_file() && m.permissions().mode() & 0o111 != 0)
        .unwrap_or(false)
}

/// The bwrap startup probe (args only, no program name): a throwaway
/// sandbox with the namespaces a run unshares — `--share-net` when the
/// run shares the network — running `shell -c 'exit 0'`.
pub fn bwrap_probe_argv(shell: &str, share_net: bool) -> Vec<String> {
    let mut argv = vec!["--unshare-all".to_owned()];
    if share_net {
        argv.push("--share-net".to_owned());
    }
    for arg in [
        "--die-with-parent",
        "--ro-bind",
        "/",
        "/",
        "--proc",
        "/proc",
        "--dev",
        "/dev",
        "--",
        shell,
        "-c",
        "exit 0",
    ] {
        argv.push(arg.to_owned());
    }
    argv
}

/// The OCI runtime settings of a podman backend run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PodmanRuntime {
    pub runtime: String,
    pub runtime_flags: Vec<String>,
    pub cgroup_manager: Option<String>,
}

/// The runtime settings `sandbox` in lib.rs uses: runsc for gvisor,
/// the `MYSBX_KRUN_RUNTIME` pin (default `crun`) for krun; a rootless
/// gvisor run defaults to the `ignore-cgroups` runtime flag, a
/// rootless run of either variant to the `cgroupfs` manager. The
/// `Option` arguments are the pins, `None` when unset or empty.
pub fn podman_runtime(
    krun: bool,
    rootless: bool,
    krun_runtime: Option<&str>,
    cgroup_manager: Option<&str>,
    runtime_flags: Option<&str>,
) -> PodmanRuntime {
    let runtime = if krun {
        krun_runtime.unwrap_or("crun").to_owned()
    } else {
        "runsc".to_owned()
    };
    let default_flags = if !krun && rootless {
        "ignore-cgroups"
    } else {
        ""
    };
    let runtime_flags = runtime_flags
        .unwrap_or(default_flags)
        .split_whitespace()
        .map(str::to_owned)
        .collect();
    let cgroup_manager = match cgroup_manager {
        Some(manager) => Some(manager.to_owned()),
        None if rootless => Some("cgroupfs".to_owned()),
        None => None,
    };
    PodmanRuntime {
        runtime,
        runtime_flags,
        cgroup_manager,
    }
}

/// podman's global args for `rt`, in the order the run argv uses.
pub fn podman_global_args(rt: &PodmanRuntime) -> Vec<String> {
    let mut argv = vec![format!("--runtime={}", rt.runtime)];
    for flag in &rt.runtime_flags {
        argv.push("--runtime-flag".to_owned());
        argv.push(flag.clone());
    }
    if let Some(manager) = &rt.cgroup_manager {
        argv.push(format!("--cgroup-manager={manager}"));
    }
    argv
}

/// `podman --runtime=<name> info`: fails when podman does not know a
/// runtime of that name.
pub fn runtime_info_argv(runtime: &str) -> Vec<String> {
    vec![format!("--runtime={runtime}"), "info".to_owned()]
}

/// `podman image inspect --format {{.Id}} <image>`.
pub fn image_id_argv(image: &str) -> Vec<String> {
    ["image", "inspect", "--format", "{{.Id}}", image]
        .iter()
        .map(|s| s.to_string())
        .collect()
}

/// What a podman probe container runs with.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PodmanProbe<'a> {
    pub runtime: &'a PodmanRuntime,
    pub krun: bool,
    pub image: &'a str,
    pub shell: &'a str,
}

fn podman_probe_base(probe: &PodmanProbe<'_>, network: Option<&str>) -> Vec<String> {
    let mut argv = podman_global_args(probe.runtime);
    for arg in ["run", "--rm", "--pull=never", "--userns=keep-id"] {
        argv.push(arg.to_owned());
    }
    if probe.krun {
        argv.push("--annotation".to_owned());
        argv.push("run.oci.handler=krun".to_owned());
        argv.push("--group-add=keep-groups".to_owned());
    }
    argv.push("--read-only".to_owned());
    argv.push("--read-only-tmpfs=true".to_owned());
    if !probe.krun {
        argv.push("--cap-drop=ALL".to_owned());
        argv.push("--security-opt=no-new-privileges".to_owned());
    }
    if let Some(network) = network {
        argv.push("--network".to_owned());
        argv.push(network.to_owned());
    }
    argv.push(probe.image.to_owned());
    argv.push(probe.shell.to_owned());
    argv.push("-c".to_owned());
    argv
}

/// The podman startup probe (args only): a throwaway `--rm` container
/// of the run's image and runtime, without network, running `exit 0`.
pub fn startup_probe_argv(probe: &PodmanProbe<'_>) -> Vec<String> {
    let mut argv = podman_probe_base(probe, Some("none"));
    argv.push("exit 0".to_owned());
    argv
}

/// The podman endpoint probe (args only): like the startup probe, on
/// the run's network (`None` = podman's default), curling `endpoint`.
pub fn endpoint_probe_argv(
    probe: &PodmanProbe<'_>,
    network: Option<&str>,
    endpoint: &str,
) -> Vec<String> {
    let mut argv = podman_probe_base(probe, network);
    argv.push(CURL_CMD.to_owned());
    argv.push(endpoint.to_owned());
    argv
}

/// The model endpoint a run would see: the first of [`ENDPOINT_VARS`]
/// set in the backend pins (`KEY=VALUE` entries), the `[env]` of the
/// configuration, or the forwarded host environment — in that order
/// per variable. Returns `(variable, url)`.
pub fn model_endpoint(
    pinned: &[String],
    configured: &BTreeMap<String, String>,
    forwarded: &[(String, String)],
) -> Option<(String, String)> {
    for var in ENDPOINT_VARS {
        let prefix = format!("{var}=");
        let value = pinned
            .iter()
            .find_map(|entry| entry.strip_prefix(prefix.as_str()))
            .or_else(|| configured.get(*var).map(String::as_str))
            .or_else(|| {
                forwarded
                    .iter()
                    .find(|(name, _)| name.as_str() == *var)
                    .map(|(_, value)| value.as_str())
            });
        if let Some(value) = value.filter(|v| !v.is_empty()) {
            return Some((var.to_string(), value.to_owned()));
        }
    }
    None
}

/// Host and port of an `http://` or `https://` URL; `None` for any
/// other scheme or an unparsable authority.
pub fn host_port(url: &str) -> Option<(String, u16)> {
    let (rest, default_port) = if let Some(rest) = url.strip_prefix("http://") {
        (rest, 80)
    } else if let Some(rest) = url.strip_prefix("https://") {
        (rest, 443)
    } else {
        return None;
    };
    let authority = rest
        .split(|c: char| c == '/' || c == '?' || c == '#')
        .next()
        .unwrap_or("");
    let authority = authority.rsplit('@').next().unwrap_or(authority);
    if authority.is_empty() {
        return None;
    }
    if let Some(bracketed) = authority.strip_prefix('[') {
        let (host, after) = bracketed.split_once(']')?;
        if host.is_empty() {
            return None;
        }
        let port = if after.is_empty() {
            default_port
        } else {
            after.strip_prefix(':')?.parse().ok()?
        };
        return Some((host.to_owned(), port));
    }
    match authority.rsplit_once(':') {
        Some((host, port)) if !host.is_empty() => Some((host.to_owned(), port.parse().ok()?)),
        Some(_) => None,
        None => Some((authority.to_owned(), default_port)),
    }
}

/// The `/dev/kvm` check of the krun variant.
pub fn kvm_check(exists: bool, rw: bool) -> Check {
    if !exists {
        Check::fail(
            "/dev/kvm",
            "missing",
            "load the kvm module (kvm-intel / kvm-amd) and enable virtualization in the \
             firmware, or use the podman-gvisor backend",
        )
    } else if !rw {
        Check::fail(
            "/dev/kvm",
            "not readable+writable for this user",
            "add the user to the `kvm` group (or enable the seat udev ACL) and log in again",
        )
    } else {
        Check::ok("/dev/kvm", "readable+writable")
    }
}

/// The Landlock check of the nono backend; `abi` is the kernel's
/// Landlock ABI version, `None` when Landlock is unavailable.
pub fn landlock_check(abi: Option<i64>) -> Check {
    match abi {
        Some(v) => Check::ok("Landlock", format!("ABI v{v}")),
        None => Check::fail(
            "Landlock",
            "not available in this kernel",
            "nono needs Landlock: boot a kernel with CONFIG_SECURITY_LANDLOCK and `landlock` \
             in the `lsm=` list, or use the bubblewrap backend",
        ),
    }
}

/// The user-namespace hint of the bwrap-based backends, from the
/// content of `/proc/sys/user/max_user_namespaces`; `None` when the
/// file is unreadable. Only a warning: the startup probe is the
/// verdict.
pub fn userns_check(max_user_namespaces: Option<&str>) -> Option<Check> {
    let value = max_user_namespaces?.trim();
    Some(if value == "0" {
        Check::warn(
            "user namespaces",
            "user.max_user_namespaces is 0",
            "bwrap needs unprivileged user namespaces (NixOS: security.allowUserNamespaces)",
        )
    } else {
        Check::ok(
            "user namespaces",
            format!("user.max_user_namespaces = {value}"),
        )
    })
}

fn short_id(id: &str) -> &str {
    id.get(..12).unwrap_or(id)
}

/// Whether a `podman image inspect` error says the image is absent.
fn image_unknown(error: &str) -> bool {
    let error = error.to_ascii_lowercase();
    error.contains("image not known") || error.contains("no such image")
}

/// The image check of the podman backends: `inspect` is the image ID
/// `podman image inspect` printed, or its error line when it failed;
/// `expected` is the `MYSBX_PODMAN_IMAGE_ID` pin.
pub fn image_check(image: &str, inspect: Result<&str, &str>, expected: Option<&str>) -> Check {
    let loaded = match inspect {
        Ok(loaded) => loaded,
        Err(error) if image_unknown(error) => {
            return Check::fail(
                "image",
                format!("{image} is not in the podman store ({error})"),
                "run: mysbx podman-load-image",
            );
        }
        Err(error) => {
            return Check::fail(
                "image",
                format!("`podman image inspect {image}` failed: {error}"),
                "check that podman works for this user (podman info)",
            );
        }
    };
    let loaded = loaded.trim();
    let loaded = loaded.strip_prefix("sha256:").unwrap_or(loaded);
    match expected {
        None => Check::ok(
            "image",
            format!(
                "{image} present ({}; no MYSBX_PODMAN_IMAGE_ID pin, freshness not checked)",
                short_id(loaded)
            ),
        ),
        Some(expected) if expected == loaded => Check::ok(
            "image",
            format!("{image} current ({})", short_id(loaded)),
        ),
        Some(expected) => Check::warn(
            "image",
            format!(
                "{image} is stale: loaded {}, this build expects {}",
                short_id(loaded),
                short_id(expected)
            ),
            "run: mysbx podman-load-image",
        ),
    }
}

/// The kernel's Landlock ABI version (`landlock_create_ruleset` with
/// `LANDLOCK_CREATE_RULESET_VERSION`), `None` when unsupported.
fn landlock_abi() -> Option<i64> {
    use std::os::raw::c_long;
    extern "C" {
        fn syscall(num: c_long, ...) -> c_long;
    }
    const SYS_LANDLOCK_CREATE_RULESET: c_long = 444;
    const LANDLOCK_CREATE_RULESET_VERSION: u32 = 1;
    let abi = unsafe {
        syscall(
            SYS_LANDLOCK_CREATE_RULESET,
            std::ptr::null::<u8>(),
            0usize,
            LANDLOCK_CREATE_RULESET_VERSION,
        )
    };
    if abi > 0 { Some(abi as i64) } else { None }
}

/// Resolve `host` in a helper thread, bounded by `timeout`: a hanging
/// resolver leaves the thread behind instead of blocking the doctor.
fn resolve_bounded(
    host: &str,
    port: u16,
    timeout: Duration,
) -> Result<Vec<std::net::SocketAddr>, String> {
    use std::net::ToSocketAddrs;
    let (tx, rx) = mpsc::channel();
    let owned = host.to_owned();
    std::thread::spawn(move || {
        let resolved = (owned.as_str(), port)
            .to_socket_addrs()
            .map(|addrs| addrs.collect::<Vec<_>>());
        let _ = tx.send(resolved);
    });
    match rx.recv_timeout(timeout) {
        Ok(Ok(addrs)) => Ok(addrs),
        Ok(Err(e)) => Err(format!("cannot resolve {host}: {e}")),
        Err(_) => Err(format!("resolving {host} timed out after {} s", timeout.as_secs())),
    }
}

fn tcp_reachable(host: &str, port: u16) -> Result<(), String> {
    use std::net::TcpStream;
    let addrs = resolve_bounded(host, port, RESOLVE_TIMEOUT)?;
    let mut last = format!("{host} resolves to no address");
    for addr in addrs {
        match TcpStream::connect_timeout(&addr, Duration::from_secs(3)) {
            Ok(_) => return Ok(()),
            Err(e) => last = format!("{addr}: {e}"),
        }
    }
    Err(last)
}

/// The outcome of one probe command.
struct Probe {
    ok: bool,
    timed_out: bool,
    stdout: String,
    stderr: String,
}

impl Probe {
    /// The detail of a failed probe: the timeout, or the last line of
    /// its stderr.
    fn error(&self) -> String {
        if self.timed_out {
            format!("timed out after {} s", PROBE_TIMEOUT.as_secs())
        } else {
            error_line(&self.stderr)
        }
    }
}

/// Read `pipe` to its end in a helper thread; the receiver gets the
/// bytes once the pipe closes.
fn read_pipe<R: Read + Send + 'static>(pipe: Option<R>) -> mpsc::Receiver<Vec<u8>> {
    let (tx, rx) = mpsc::channel();
    if let Some(mut pipe) = pipe {
        std::thread::spawn(move || {
            let mut buf = Vec::new();
            let _ = pipe.read_to_end(&mut buf);
            let _ = tx.send(buf);
        });
    }
    rx
}

/// Run one probe command bounded by [`PROBE_TIMEOUT`]; a command that
/// outlives it is killed. Output a detached grandchild keeps open is
/// given a short grace period, then dropped.
fn run_probe(bin: &Path, argv: &[String]) -> Probe {
    let spawned = Command::new(bin)
        .args(argv)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn();
    let mut child = match spawned {
        Ok(child) => child,
        Err(e) => {
            return Probe {
                ok: false,
                timed_out: false,
                stdout: String::new(),
                stderr: format!("cannot run {}: {e}", bin.display()),
            };
        }
    };
    let stdout = read_pipe(child.stdout.take());
    let stderr = read_pipe(child.stderr.take());
    let deadline = Instant::now() + PROBE_TIMEOUT;
    let (ok, timed_out) = loop {
        match child.try_wait() {
            Ok(Some(status)) => break (status.success(), false),
            Ok(None) if Instant::now() < deadline => {
                std::thread::sleep(Duration::from_millis(20));
            }
            Ok(None) => {
                let _ = child.kill();
                let _ = child.wait();
                break (false, true);
            }
            Err(_) => {
                let _ = child.kill();
                let _ = child.wait();
                break (false, false);
            }
        }
    };
    let grace = Duration::from_secs(2);
    let text = |rx: mpsc::Receiver<Vec<u8>>| {
        let bytes = rx.recv_timeout(grace).unwrap_or_default();
        String::from_utf8_lossy(&bytes).trim().to_owned()
    };
    Probe {
        ok,
        timed_out,
        stdout: text(stdout),
        stderr: text(stderr),
    }
}

/// The last non-empty line of a probe's stderr, shortened — the line
/// podman and bwrap put their error on.
fn error_line(stderr: &str) -> String {
    let line = stderr
        .lines()
        .rev()
        .map(str::trim)
        .find(|l| !l.is_empty())
        .unwrap_or("no error output");
    let mut out: String = line.chars().take(200).collect();
    if line.chars().count() > 200 {
        out.push('…');
    }
    out
}

/// The binary check shared by every backend: resolve the pin (or its
/// PATH fallback) and report it. Returns the resolved path.
fn binary_check(
    checks: &mut Vec<Check>,
    name: &str,
    pin: &str,
    fallback: &str,
    hint: &str,
) -> Option<PathBuf> {
    let value = crate::env_or(pin, fallback);
    let path_var = std::env::var_os("PATH");
    match resolve_executable(&value, path_var.as_deref()) {
        Some(path) => {
            checks.push(Check::ok(name, path.display().to_string()));
            Some(path)
        }
        None => {
            checks.push(Check::fail(
                name,
                format!("`{value}` is not an executable file ({pin} or PATH)"),
                hint,
            ));
            None
        }
    }
}

/// bwrap binary, sandbox shell, user namespaces and the startup
/// probe — shared by the bubblewrap and nono backends. `share_net` is
/// the run's network sense.
fn bwrap_checks(checks: &mut Vec<Check>, share_net: bool) {
    let bwrap = binary_check(
        checks,
        "bwrap binary",
        "MYSBX_BWRAP",
        "bwrap",
        "install bubblewrap or set MYSBX_BWRAP (the Nix wrapper pins it)",
    );
    let shell = binary_check(
        checks,
        "sandbox shell",
        "MYSBX_SHELL",
        "/bin/sh",
        "set MYSBX_SHELL to an executable shell (the Nix wrapper pins it)",
    );
    let userns = std::fs::read_to_string("/proc/sys/user/max_user_namespaces").ok();
    if let Some(check) = userns_check(userns.as_deref()) {
        checks.push(check);
    }
    let (bwrap, shell) = match (bwrap, shell) {
        (Some(bwrap), Some(shell)) => (bwrap, shell),
        (bwrap, _) => {
            let blocker = if bwrap.is_none() {
                "bwrap binary"
            } else {
                "sandbox shell"
            };
            checks.push(Check::skip("startup probe", &[blocker]));
            return;
        }
    };
    let argv = bwrap_probe_argv(&shell.to_string_lossy(), share_net);
    let probe = run_probe(&bwrap, &argv);
    checks.push(if probe.ok {
        Check::ok(
            "startup probe",
            "a throwaway bwrap sandbox started and exited 0",
        )
    } else {
        Check::fail(
            "startup probe",
            probe.error(),
            "bwrap cannot create its namespaces: check unprivileged user namespaces \
             and that no outer sandbox forbids nesting",
        )
    });
}

/// nono binary, profile and Landlock.
fn nono_checks(checks: &mut Vec<Check>) {
    binary_check(
        checks,
        "nono binary",
        "MYSBX_NONO",
        "nono",
        "install nono or set MYSBX_NONO (the Nix wrapper pins it)",
    );
    let profile = crate::env_or("MYSBX_NONO_PROFILE", "default");
    if profile.contains('/') {
        checks.push(if Path::new(&profile).is_file() {
            Check::ok("nono profile", profile)
        } else {
            Check::fail(
                "nono profile",
                format!("{profile} does not exist"),
                "point MYSBX_NONO_PROFILE at an existing profile file or a profile name",
            )
        });
    } else {
        checks.push(Check::ok(
            "nono profile",
            format!("`{profile}` (a profile name, resolved by nono)"),
        ));
    }
    checks.push(landlock_check(landlock_abi()));
}

/// The model endpoint of the bwrap-based backends: they share the
/// host network, so a host-side TCP connect is the same path.
fn host_endpoint_check(
    checks: &mut Vec<Check>,
    merged: &merge::Merged,
    forwarded: &[(String, String)],
) {
    if !merged.network {
        checks.push(Check::ok(
            "model endpoint",
            "not applicable: the network is denied (network = false)",
        ));
        return;
    }
    let Some((var, url)) = model_endpoint(&[], &merged.env, forwarded) else {
        checks.push(Check::ok(
            "model endpoint",
            format!(
                "not applicable: none of {} is configured",
                ENDPOINT_VARS.join(", ")
            ),
        ));
        return;
    };
    let Some((host, port)) = host_port(&url) else {
        checks.push(Check::warn(
            "model endpoint",
            format!("{var}={url} is not an http(s) URL"),
            "set it to http://HOST:PORT/…",
        ));
        return;
    };
    checks.push(match tcp_reachable(&host, port) {
        Ok(()) => Check::ok(
            "model endpoint",
            format!("{var}={url} accepts connections (the sandbox shares the host network)"),
        ),
        Err(e) => Check::warn(
            "model endpoint",
            format!("{var}={url}: {e}"),
            "start the model proxy (e.g. litellm.service) or fix the URL",
        ),
    });
}

/// podman binary, OCI runtime, `/dev/kvm` (krun), image, startup probe
/// and the model endpoint through the run's network.
fn podman_checks(
    checks: &mut Vec<Check>,
    backend: &str,
    merged: &merge::Merged,
    forwarded: &[(String, String)],
) {
    let krun = backend == "podman-krun";
    // The names of the failed checks the startup probe depends on.
    let mut blockers: Vec<&str> = Vec::new();
    if crate::refuse_renamed_podman_pins("mysbx doctor") {
        blockers.push("pins");
        checks.push(Check::fail(
            "pins",
            "an old MYSBX_GVISOR_* pin is set (named on stderr)",
            "rename it to MYSBX_PODMAN_*; a run refuses the old name",
        ));
    }
    let podman = binary_check(
        checks,
        "podman binary",
        "MYSBX_PODMAN",
        "podman",
        "install podman (NixOS: virtualisation.podman.enable) or set MYSBX_PODMAN",
    );
    if podman.is_none() {
        blockers.push("podman binary");
    }

    if krun {
        let check = kvm_check(Path::new("/dev/kvm").exists(), crate::kvm_available());
        if check.level != Level::Ok {
            blockers.push("/dev/kvm");
        }
        checks.push(check);
    }

    let rootless = unsafe { crate::libc_geteuid() != 0 };
    let (cgroup_env, flags_env) = if krun {
        ("MYSBX_KRUN_CGROUP_MANAGER", "MYSBX_KRUN_RUNTIME_FLAGS")
    } else {
        ("MYSBX_GVISOR_CGROUP_MANAGER", "MYSBX_GVISOR_RUNTIME_FLAGS")
    };
    let krun_runtime = crate::env_opt("MYSBX_KRUN_RUNTIME");
    let cgroup_manager = crate::env_opt(cgroup_env);
    let runtime_flags = crate::env_opt(flags_env);
    let rt = podman_runtime(
        krun,
        rootless,
        krun_runtime.as_deref(),
        cgroup_manager.as_deref(),
        runtime_flags.as_deref(),
    );

    let runtime_check = if krun {
        let path_var = std::env::var_os("PATH");
        match resolve_executable(&rt.runtime, path_var.as_deref()) {
            None => Check::fail(
                "OCI runtime",
                format!(
                    "`{}` is not an executable file (MYSBX_KRUN_RUNTIME or PATH)",
                    rt.runtime
                ),
                "point MYSBX_KRUN_RUNTIME at crun built with libkrun \
                 (myconfig.ai.dev.mysbx.krun.runtime)",
            ),
            Some(path) => {
                let version = run_probe(&path, &["--version".to_owned()]);
                if version.ok && version.stdout.contains("+LIBKRUN") {
                    Check::ok(
                        "OCI runtime",
                        format!("{} (crun with +LIBKRUN)", path.display()),
                    )
                } else if version.ok {
                    Check::fail(
                        "OCI runtime",
                        format!("{} --version does not list +LIBKRUN", path.display()),
                        "pin a crun built with libkrun (myconfig.ai.dev.mysbx.krun.runtime)",
                    )
                } else {
                    Check::fail(
                        "OCI runtime",
                        format!("{} --version failed: {}", path.display(), version.error()),
                        "pin a crun built with libkrun (myconfig.ai.dev.mysbx.krun.runtime)",
                    )
                }
            }
        }
    } else if let Some(podman) = &podman {
        let info = run_probe(podman, &runtime_info_argv(&rt.runtime));
        if info.ok {
            Check::ok(
                "OCI runtime",
                format!("`{}` is registered with podman", rt.runtime),
            )
        } else {
            Check::fail(
                "OCI runtime",
                format!("`{}` is not usable: {}", rt.runtime, info.error()),
                "register it in containers.conf (NixOS: \
                 virtualisation.containers.containersConf.settings.engine.runtimes)",
            )
        }
    } else {
        Check::skip("OCI runtime", &["podman binary"])
    };
    if runtime_check.level == Level::Fail {
        blockers.push("OCI runtime");
    }
    checks.push(runtime_check);

    let image = crate::env_opt("MYSBX_PODMAN_IMAGE");
    match (&image, &podman) {
        (None, _) => {
            blockers.push("image");
            checks.push(Check::fail(
                "image",
                "no image pinned (MYSBX_PODMAN_IMAGE)",
                "the Nix wrapper pins it when the host builds the agent image \
                 (myconfig.ai.dev.mysbx.podman.image), or set MYSBX_PODMAN_IMAGE",
            ));
        }
        (Some(image), Some(podman)) => {
            let inspect = run_probe(podman, &image_id_argv(image));
            let error = inspect.error();
            let result = if inspect.ok {
                Ok(inspect.stdout.as_str())
            } else {
                Err(error.as_str())
            };
            let expected = crate::env_opt("MYSBX_PODMAN_IMAGE_ID");
            let check = image_check(image, result, expected.as_deref());
            if check.level == Level::Fail {
                blockers.push("image");
            }
            checks.push(check);
        }
        (Some(_), None) => checks.push(Check::skip("image", &["podman binary"])),
    }

    let started = match (&podman, &image, blockers.is_empty()) {
        (Some(podman), Some(image), true) => {
            let shell = crate::env_or("MYSBX_PODMAN_SHELL", "/bin/bash");
            let probe = PodmanProbe {
                runtime: &rt,
                krun,
                image: image.as_str(),
                shell: shell.as_str(),
            };
            let started = run_probe(podman, &startup_probe_argv(&probe));
            if started.ok {
                checks.push(Check::ok(
                    "startup probe",
                    "a throwaway container started and exited 0",
                ));
                Some(shell)
            } else {
                let hint = if krun {
                    "try the run by hand; common causes: /dev/kvm reachable only through a \
                     group, missing /etc/subuid and /etc/subgid ranges"
                } else {
                    "common causes: the systemd cgroup manager (`Access denied`: set \
                     MYSBX_GVISOR_CGROUP_MANAGER=cgroupfs), runsc without the ignore-cgroups \
                     runtime flag, missing /etc/subuid and /etc/subgid ranges"
                };
                checks.push(Check::fail("startup probe", started.error(), hint));
                None
            }
        }
        _ => {
            checks.push(Check::skip("startup probe", &blockers));
            None
        }
    };

    if !merged.network {
        checks.push(Check::ok(
            "model endpoint",
            "not applicable: the network is denied (network = false)",
        ));
        return;
    }
    let pinned: Vec<String> = crate::env_or("MYSBX_PODMAN_ENV", "")
        .split_whitespace()
        .map(str::to_owned)
        .collect();
    let Some((var, url)) = model_endpoint(&pinned, &merged.env, forwarded) else {
        checks.push(Check::ok(
            "model endpoint",
            format!(
                "not applicable: none of {} is configured",
                ENDPOINT_VARS.join(", ")
            ),
        ));
        return;
    };
    let (Some(shell), Some(podman), Some(image)) = (started, &podman, &image) else {
        checks.push(Check::skip("model endpoint", &["startup probe"]));
        return;
    };
    let probe = PodmanProbe {
        runtime: &rt,
        krun,
        image: image.as_str(),
        shell: shell.as_str(),
    };
    let network = crate::env_opt("MYSBX_PODMAN_PASTA_SPEC");
    let argv = endpoint_probe_argv(&probe, network.as_deref(), &url);
    let answer = run_probe(podman, &argv);
    checks.push(if answer.ok {
        Check::ok(
            "model endpoint",
            format!(
                "{var}={url} answered inside the container ({})",
                answer.stdout
            ),
        )
    } else {
        Check::warn(
            "model endpoint",
            format!("{var}={url}: {}", answer.error()),
            "the container reaches the host proxy through pasta --map-guest-addr and the \
             LiteLLM forwarder: check MYSBX_PODMAN_PASTA_SPEC and that the forwarder \
             socket and the proxy are up",
        )
    });
}

/// The effective configuration and the check describing where it came
/// from: the repo the cwd resolves to, or the user layer alone when
/// there is none. A directory a run refuses (inside a session clone, a
/// forbidden git dir, a repo containing the home) is a `WARN`; the
/// home directory and `/` are "no repo here".
fn effective_config() -> Result<(merge::Merged, Check), String> {
    let home_os = std::env::var_os("HOME").unwrap_or_default();
    let home = Path::new(&home_os);
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    match repo::resolve_cwd() {
        Ok(repo) => {
            let layers = merge::load_layers(home, xdg.as_deref(), &repo.sidecar)
                .map_err(|e| e.to_string())?;
            let merged = merge::merge(
                layers.user.0,
                layers.sidecar.0,
                &layers.user.1,
                &layers.sidecar.1,
                home,
            )
            .map_err(|e| e.to_string())?;
            let state = if repo.sidecar.join("config.toml").is_file() {
                "sidecar inited"
            } else {
                "sidecar not inited, user layer only"
            };
            let scope = format!("repo {} ({state})", repo.root.display());
            Ok((merged, Check::ok("configuration", scope)))
        }
        Err(err) => {
            let user_path = merge::user_config_path(home, xdg.as_deref());
            let user = if user_path.exists() {
                Config::load(&user_path).map_err(|e| e.to_string())?
            } else {
                Config::default()
            };
            let merged = merge::merge(user, Config::default(), &user_path, &user_path, home)
                .map_err(|e| e.to_string())?;
            let check = match err {
                repo::Error::HomeDir(_) | repo::Error::RootDir => Check::ok(
                    "configuration",
                    format!("user layer only (no repo here: {err})"),
                ),
                _ => Check::warn(
                    "configuration",
                    format!("user layer only: {err}"),
                    "a run from this directory is refused; run doctor from the repository",
                ),
            };
            Ok((merged, check))
        }
    }
}

/// `mysbx doctor [BACKEND...]`: check the named backends, or the
/// configured one; see the module doc for output and exit codes.
pub fn run(args: &[String]) -> i32 {
    let mut named: Vec<String> = Vec::new();
    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => {
                println!("{USAGE_DOCTOR}");
                println!(
                    "  checks the configured backend, or each BACKEND named ({})",
                    BACKENDS.join(", ")
                );
                return 0;
            }
            option if option.starts_with('-') => {
                eprintln!("mysbx doctor: unknown option: {option}");
                eprintln!("{USAGE_DOCTOR}");
                return 2;
            }
            name if BACKENDS.contains(&name) => {
                if !named.iter().any(|n| n == name) {
                    named.push(name.to_owned());
                }
            }
            other => {
                eprintln!("mysbx doctor: unknown backend: {other}");
                eprintln!("  backends: {}", BACKENDS.join(", "));
                eprintln!("{USAGE_DOCTOR}");
                return 2;
            }
        }
    }

    let (merged, scope) = match effective_config() {
        Ok(x) => x,
        Err(e) => {
            eprintln!("mysbx doctor: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };

    let mut header = vec![scope];
    let from_command_line = !named.is_empty();
    let targets: Vec<String> = if from_command_line {
        header.push(Check::ok("backend", format!("{} [command line]", named.join(", "))));
        named
    } else {
        match merged.backend.as_deref() {
            Some(b) if BACKENDS.contains(&b) => {
                header.push(Check::ok("backend", format!("{b} [configuration]")));
                vec![b.to_owned()]
            }
            Some(b) => {
                header.push(Check::fail(
                    "backend",
                    format!("unknown backend `{b}` in the configuration"),
                    format!("use one of: {}", BACKENDS.join(", ")),
                ));
                Vec::new()
            }
            None => {
                header.push(Check::fail(
                    "backend",
                    "no backend configured",
                    "set `backend = \"<name>\"` in ~/.config/mysbx/config.toml or the sidecar \
                     (mysbx edit), or name one: mysbx doctor <backend>",
                ));
                Vec::new()
            }
        }
    };

    let forwarded: Vec<(String, String)> = crate::forwarded_env_vars(&merged)
        .into_iter()
        .filter_map(|name| {
            std::env::var(&name)
                .ok()
                .filter(|v| !v.is_empty())
                .map(|v| (name, v))
        })
        .collect();

    let mut sections = vec![Section {
        title: "mysbx doctor".to_owned(),
        checks: header,
    }];
    for backend in &targets {
        let mut checks = Vec::new();
        match backend.as_str() {
            "bubblewrap" => {
                bwrap_checks(&mut checks, merged.network);
                host_endpoint_check(&mut checks, &merged, &forwarded);
            }
            "nono" => {
                bwrap_checks(&mut checks, merged.network);
                nono_checks(&mut checks);
                host_endpoint_check(&mut checks, &merged, &forwarded);
            }
            _ => podman_checks(&mut checks, backend, &merged, &forwarded),
        }
        sections.push(Section {
            title: backend.clone(),
            checks,
        });
    }
    let others: Vec<&str> = BACKENDS
        .iter()
        .copied()
        .filter(|b| !targets.iter().any(|t| t.as_str() == *b))
        .collect();
    if !targets.is_empty() && !others.is_empty() {
        sections.push(Section {
            title: "other backends".to_owned(),
            checks: vec![Check::ok(
                "not checked",
                if from_command_line {
                    format!(
                        "{} (check one with: mysbx doctor <backend>)",
                        others.join(", ")
                    )
                } else {
                    format!(
                        "{} (not configured; check one with: mysbx doctor <backend>)",
                        others.join(", ")
                    )
                },
            )],
        });
    }

    let (lines, problems, _) = render(&sections);
    for line in &lines {
        println!("{line}");
    }
    if problems > 0 { 1 } else { 0 }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(v: &[&str]) -> Vec<String> {
        v.iter().map(|x| x.to_string()).collect()
    }

    #[test]
    fn check_lines_carry_the_tag_and_the_hint() {
        assert_eq!(Check::ok("a", "fine").line(), "OK   a: fine");
        assert_eq!(Check::warn("b", "meh", "do x").line(), "WARN b: meh — do x");
        assert_eq!(
            Check::fail("c", "broken", "do y").line(),
            "FAIL c: broken — do y"
        );
        assert_eq!(
            Check::skip("d", &["x", "y"]).line(),
            "SKIP d: not run (depends on: x, y)"
        );
    }

    #[test]
    fn render_counts_problems_and_warnings() {
        let sections = vec![
            Section {
                title: "one".into(),
                checks: vec![Check::ok("a", "x"), Check::fail("b", "y", "z")],
            },
            Section {
                title: "two".into(),
                checks: vec![Check::warn("c", "y", "z")],
            },
        ];
        let (lines, problems, warnings) = render(&sections);
        assert_eq!((problems, warnings), (1, 1));
        assert_eq!(lines[0], "== one ==");
        assert_eq!(lines[3], "== two ==");
        assert_eq!(
            lines.last().unwrap(),
            "mysbx doctor: 1 problem(s), 1 warning(s)"
        );
        let (lines, problems, _) = render(&sections[1..]);
        assert_eq!(problems, 0);
        assert_eq!(
            lines.last().unwrap(),
            "mysbx doctor: 0 problem(s), 1 warning(s)"
        );
    }

    #[test]
    fn bwrap_probe_argv_is_exact() {
        let tail = [
            "--die-with-parent",
            "--ro-bind",
            "/",
            "/",
            "--proc",
            "/proc",
            "--dev",
            "/dev",
            "--",
            "/bin/sh",
            "-c",
            "exit 0",
        ];
        // network = false: every namespace unshared
        let mut want = s(&["--unshare-all"]);
        want.extend(s(&tail));
        assert_eq!(bwrap_probe_argv("/bin/sh", false), want);
        // network = true: the run's --share-net right after --unshare-all
        let mut want = s(&["--unshare-all", "--share-net"]);
        want.extend(s(&tail));
        assert_eq!(bwrap_probe_argv("/bin/sh", true), want);
    }

    #[test]
    fn podman_runtime_mirrors_the_run_defaults() {
        // rootless gvisor: ignore-cgroups + cgroupfs
        let rt = podman_runtime(false, true, None, None, None);
        assert_eq!(rt.runtime, "runsc");
        assert_eq!(rt.runtime_flags, s(&["ignore-cgroups"]));
        assert_eq!(rt.cgroup_manager.as_deref(), Some("cgroupfs"));
        // root gvisor: no flags, podman's own cgroup manager
        let rt = podman_runtime(false, false, None, None, None);
        assert!(rt.runtime_flags.is_empty());
        assert_eq!(rt.cgroup_manager, None);
        // rootless krun: the pin, no flags, cgroupfs
        let rt = podman_runtime(true, true, Some("/nix/store/x-crun/bin/crun"), None, None);
        assert_eq!(rt.runtime, "/nix/store/x-crun/bin/crun");
        assert!(rt.runtime_flags.is_empty());
        assert_eq!(rt.cgroup_manager.as_deref(), Some("cgroupfs"));
        // unpinned krun falls back to crun on PATH; pins override
        let rt = podman_runtime(true, false, None, Some("systemd"), Some("a  b"));
        assert_eq!(rt.runtime, "crun");
        assert_eq!(rt.runtime_flags, s(&["a", "b"]));
        assert_eq!(rt.cgroup_manager.as_deref(), Some("systemd"));
    }

    #[test]
    fn podman_probe_argvs_are_exact() {
        let rt = podman_runtime(false, true, None, None, None);
        let probe = PodmanProbe {
            runtime: &rt,
            krun: false,
            image: "localhost/agent:latest",
            shell: "/bin/bash",
        };
        assert_eq!(
            startup_probe_argv(&probe),
            s(&[
                "--runtime=runsc",
                "--runtime-flag",
                "ignore-cgroups",
                "--cgroup-manager=cgroupfs",
                "run",
                "--rm",
                "--pull=never",
                "--userns=keep-id",
                "--read-only",
                "--read-only-tmpfs=true",
                "--cap-drop=ALL",
                "--security-opt=no-new-privileges",
                "--network",
                "none",
                "localhost/agent:latest",
                "/bin/bash",
                "-c",
                "exit 0",
            ])
        );
        let rt = podman_runtime(true, false, Some("/k/crun"), None, None);
        let probe = PodmanProbe {
            runtime: &rt,
            krun: true,
            image: "img",
            shell: "/bin/bash",
        };
        assert_eq!(
            endpoint_probe_argv(
                &probe,
                Some("pasta:--map-guest-addr,10.0.2.2"),
                "http://e/v1"
            ),
            s(&[
                "--runtime=/k/crun",
                "run",
                "--rm",
                "--pull=never",
                "--userns=keep-id",
                "--annotation",
                "run.oci.handler=krun",
                "--group-add=keep-groups",
                "--read-only",
                "--read-only-tmpfs=true",
                "--network",
                "pasta:--map-guest-addr,10.0.2.2",
                "img",
                "/bin/bash",
                "-c",
                CURL_CMD,
                "http://e/v1",
            ])
        );
        // no pasta spec: podman's default network, no --network
        let argv = endpoint_probe_argv(&probe, None, "http://e/v1");
        assert!(!argv.iter().any(|a| a == "--network"), "{argv:?}");
    }

    #[test]
    fn helper_argvs_are_exact() {
        assert_eq!(runtime_info_argv("runsc"), s(&["--runtime=runsc", "info"]));
        assert_eq!(
            image_id_argv("img"),
            s(&["image", "inspect", "--format", "{{.Id}}", "img"])
        );
    }

    #[test]
    fn model_endpoint_prefers_pins_then_config_then_host() {
        let mut configured = BTreeMap::new();
        let forwarded = vec![(
            "OPENAI_BASE_URL".to_string(),
            "http://host:1/v1".to_string(),
        )];
        assert_eq!(
            model_endpoint(&[], &configured, &forwarded),
            Some((
                "OPENAI_BASE_URL".to_string(),
                "http://host:1/v1".to_string()
            ))
        );
        configured.insert("OPENAI_BASE_URL".to_string(), "http://cfg:2/v1".to_string());
        assert_eq!(
            model_endpoint(&[], &configured, &forwarded).unwrap().1,
            "http://cfg:2/v1"
        );
        let pinned = s(&["FOO=bar", "OPENAI_BASE_URL=http://pin:3/v1"]);
        assert_eq!(
            model_endpoint(&pinned, &configured, &forwarded).unwrap().1,
            "http://pin:3/v1"
        );
        // negative: nothing set, or only an empty value
        assert_eq!(model_endpoint(&[], &BTreeMap::new(), &[]), None);
        assert_eq!(
            model_endpoint(&s(&["OPENAI_BASE_URL="]), &BTreeMap::new(), &[]),
            None
        );
        // the second variable when the first is unset
        let pinned = s(&["ANTHROPIC_BASE_URL=http://a:4"]);
        assert_eq!(
            model_endpoint(&pinned, &BTreeMap::new(), &[]),
            Some(("ANTHROPIC_BASE_URL".to_string(), "http://a:4".to_string()))
        );
    }

    #[test]
    fn host_port_parses_http_urls() {
        for (url, want) in [
            ("http://127.0.0.1:4000/v1", Some(("127.0.0.1", 4000u16))),
            ("http://example.org/v1", Some(("example.org", 80))),
            ("https://example.org", Some(("example.org", 443))),
            ("http://user:pw@h:81?x", Some(("h", 81))),
            ("http://[::1]:4000/v1", Some(("::1", 4000))),
            ("http://[::1]/v1", Some(("::1", 80))),
            ("ftp://h", None),
            ("http://", None),
            ("http://h:notaport/", None),
            ("http://:80/", None),
        ] {
            assert_eq!(
                host_port(url),
                want.map(|(h, p)| (h.to_string(), p)),
                "{url}"
            );
        }
    }

    #[test]
    fn kvm_check_distinguishes_missing_and_denied() {
        assert_eq!(kvm_check(true, true).level, Level::Ok);
        let denied = kvm_check(true, false);
        assert_eq!(denied.level, Level::Fail);
        assert!(denied.hint.unwrap().contains("`kvm` group"));
        let missing = kvm_check(false, false);
        assert_eq!(missing.level, Level::Fail);
        assert!(missing.detail.contains("missing"));
    }

    #[test]
    fn landlock_and_userns_checks() {
        assert_eq!(landlock_check(Some(4)).line(), "OK   Landlock: ABI v4");
        assert_eq!(landlock_check(None).level, Level::Fail);
        assert_eq!(userns_check(None), None);
        assert_eq!(userns_check(Some("63432\n")).unwrap().level, Level::Ok);
        assert_eq!(userns_check(Some("0\n")).unwrap().level, Level::Warn);
    }

    #[test]
    fn image_check_reports_absent_stale_and_current() {
        let id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
        let absent = image_check("img", Err("Error: img: image not known"), Some(id));
        assert_eq!(absent.level, Level::Fail);
        assert!(absent.detail.contains("image not known"));
        assert!(absent.hint.unwrap().contains("mysbx podman-load-image"));
        // any other inspect error names the error, not the load verb
        let broken = image_check("img", Err("Error: database is locked"), Some(id));
        assert_eq!(broken.level, Level::Fail);
        assert!(broken.detail.contains("database is locked"));
        assert!(!broken.hint.unwrap().contains("podman-load-image"));
        let timed_out = image_check("img", Err("timed out after 30 s"), None);
        assert!(!timed_out.hint.unwrap().contains("podman-load-image"));
        let loaded = format!("sha256:{id}\n");
        assert_eq!(
            image_check("img", Ok(loaded.as_str()), Some(id)).line(),
            "OK   image: img current (0123456789ab)"
        );
        let stale = image_check("img", Ok(loaded.as_str()), Some("ffffffffffffffff"));
        assert_eq!(stale.level, Level::Warn);
        assert!(stale.detail.contains("stale"), "{}", stale.detail);
        let unpinned = image_check("img", Ok(loaded.as_str()), None);
        assert_eq!(unpinned.level, Level::Ok);
    }

    #[test]
    fn resolve_executable_finds_only_executable_files() {
        use std::os::unix::fs::PermissionsExt;
        let dir = std::env::temp_dir().join(format!("mysbx-doctor-exe-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let exe = dir.join("tool");
        std::fs::write(&exe, "#!/bin/sh\n").unwrap();
        std::fs::set_permissions(&exe, std::fs::Permissions::from_mode(0o755)).unwrap();
        let plain = dir.join("plain");
        std::fs::write(&plain, "").unwrap();
        std::fs::set_permissions(&plain, std::fs::Permissions::from_mode(0o644)).unwrap();

        let path_var = std::env::join_paths([Path::new("/nonexistent"), dir.as_path()]).unwrap();
        assert_eq!(
            resolve_executable("tool", Some(path_var.as_os_str())),
            Some(exe.clone())
        );
        assert_eq!(
            resolve_executable(&exe.to_string_lossy(), None),
            Some(exe.clone())
        );
        assert_eq!(
            resolve_executable("plain", Some(path_var.as_os_str())),
            None
        );
        assert_eq!(resolve_executable(&plain.to_string_lossy(), None), None);
        assert_eq!(resolve_executable("tool", None), None);
        assert_eq!(resolve_executable("", Some(path_var.as_os_str())), None);
        assert_eq!(
            resolve_executable(&dir.to_string_lossy(), None),
            None,
            "a directory is not an executable"
        );
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn error_line_takes_the_last_non_empty_line() {
        assert_eq!(error_line("a\nError: boom\n\n"), "Error: boom");
        assert_eq!(error_line(""), "no error output");
        assert_eq!(error_line(&"x".repeat(300)).chars().count(), 201);
    }
}
