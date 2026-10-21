// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The runtime-root registry of detached runs (docs/design/detach.md
//! B3, B5, B6, B8).
//!
//! A detached run is registered in the runtime root, not in the
//! sidecar: `$XDG_RUNTIME_DIR/mysbx/<fnv1a10(repo root)>/`, holding
//! the socket, the state file and the lock of the two workspaces —
//! `live.*` for a plain run, `s.NAME.*` for a session run:
//!
//! ```text
//! $XDG_RUNTIME_DIR/mysbx/<fnv1a10(repo root)>/
//!   live.sock   live.json   live.lock
//!   s.NAME.sock s.NAME.json s.NAME.lock
//! ```
//!
//! The directory IS the registry (detach.md B3), the `clones/`
//! precedent of workspace.md D2: a file exists ⇒ a run exists, there
//! is no index file. The runtime root is a tmpfs that disappears at
//! logout, which matches the lifetime of a detached run; a worktree
//! sidecar cannot be used instead because Unix socket paths are
//! limited to 107 bytes and a sidecar plus a 64-character session
//! name exceeds that.
//!
//! Liveness is a `connect(2)` to the socket, NEVER the state file's
//! `state` field (detach.md B5): a state that says `running` without
//! a live socket means the dtach master was killed, reported as
//! `lost`. A stale socket (connection refused) is removed under the
//! per-workspace lock (detach.md B6) before a new run starts.
//!
//! This module also OWNS the `--detach` orchestration on top of the
//! registry (detach.md B2, B4): [`launch_outer`] is the outer
//! mysbx's half — lock, `starting` record, `dtach -n` spawn,
//! handshake wait — and [`drive_inner`]/[`inner_spawn_wait`] are
//! the inner mysbx's: run the pipeline for real, spawn the backend,
//! record the outcome, clean up. The launcher argv
//! ([`launcher_argv`]) and the inner-argv filter ([`inner_args_of`])
//! are pure so the tests assert them exactly (B4 step 2).
//!
//! This module is pure data and filesystem plumbing plus the
//! orchestration around it: no CLI parsing. Every function takes
//! the runtime root as a parameter; the functions that read
//! process state or spawn processes are [`runtime_root_from_env`]
//! and [`runtime_dir_from_env`] (the `XDG_RUNTIME_DIR`/
//! `MYSBX_RUNTIME_DIR` environment), [`resolve_dtach`] and
//! [`detach_inner_args`] (`MYSBX_DTACH`, `PATH`, `argv`),
//! [`launch_outer`] (spawns dtach), and [`drive_inner`]/
//! [`inner_spawn_wait`] (spawn the backend; the stderr tee touches
//! fd 2 and starts a reader thread), so the tests never mutate the
//! environment. Zero dependencies, like the rest of the crate.

use std::fmt;
use std::path::{Path, PathBuf};

/// The maximum length of a Unix-domain socket path, the size of
/// `struct sockaddr_un::sun_path`: the kernel refuses to `bind(2)`
/// anything longer. A computed path over the limit is refused,
/// never silently shortened (detach.md B3).
pub const SUN_PATH_LIMIT: usize = 107;

/// The schema version of the state file, the `"version"` field: a
/// future format change bumps this and a reader that does not know
/// the number refuses the file instead of misreading it.
pub const SCHEMA_VERSION: i64 = 1;

/// How far a detached run has come (detach.md B5): the outer mysbx
/// writes `starting` before it spawns dtach, the inner writes
/// `running` once the backend was spawned, and one of the terminal
/// states after it ends. `status` and `attach` interpret a
/// non-terminal state WITHOUT a live socket as a killed master —
/// `lost`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum State {
    /// The outer mysbx is starting the dtach child.
    Starting,
    /// The inner mysbx has spawned the backend.
    Running,
    /// The backend exited — `exitCode` carries its code, any
    /// value (B5: every backend exit is `exited`, nonzero or not).
    Exited,
    /// A startup failure (diagnosis in `error`, no `exitCode`) or a
    /// death by signal (B5) — never a plain exit, which is
    /// [`State::Exited`] with its code however nonzero.
    Failed,
    /// `SIGHUP`/`SIGTERM` ended the run.
    Cancelled,
}

impl State {
    pub fn as_str(&self) -> &'static str {
        match self {
            State::Starting => "starting",
            State::Running => "running",
            State::Exited => "exited",
            State::Failed => "failed",
            State::Cancelled => "cancelled",
        }
    }

    /// Whether the run has ended: `exited`, `failed` and `cancelled`
    /// leave a record behind that `attach` may print and remove.
    pub fn is_finished(&self) -> bool {
        matches!(self, State::Exited | State::Failed | State::Cancelled)
    }

    /// The state spelled in the state file, or `None` for an
    /// unknown spelling.
    fn parse(raw: &str) -> Option<State> {
        match raw {
            "starting" => Some(State::Starting),
            "running" => Some(State::Running),
            "exited" => Some(State::Exited),
            "failed" => Some(State::Failed),
            "cancelled" => Some(State::Cancelled),
            _ => None,
        }
    }
}

/// Which of the two workspaces of one repo a detached run occupies
/// (detach.md B3): the live checkout, or the clone of session `NAME`.
/// One detached run per workspace — a second one for the same
/// workspace is refused.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Workspace {
    /// The live checkout.
    Live,
    /// The clone of session `NAME` (the same grammar as
    /// `crate::session::valid_name`).
    Session(String),
}

impl Workspace {
    /// The file stem of this workspace: `live` or `s.NAME`
    /// (detach.md B3).
    pub fn stem(&self) -> String {
        match self {
            Workspace::Live => "live".to_owned(),
            Workspace::Session(name) => format!("s.{name}"),
        }
    }

    /// The inverse of [`stem`]: `live` → [`Live`], `s.NAME` →
    /// [`Session(NAME)`] when NAME is a valid session name, `None`
    /// for a foreign stem (detach.md B9). Read-only listing uses
    /// this to recover the workspace from a registry file's stem.
    pub fn parse_stem(stem: &str) -> Option<Workspace> {
        if stem == "live" {
            return Some(Workspace::Live);
        }
        let name = stem.strip_prefix("s.")?;
        if crate::session::valid_name(name) {
            Some(Workspace::Session(name.to_owned()))
        } else {
            None
        }
    }
}

/// One registered detached run, as plain data (detach.md B5) — pure,
/// so the state file round-trips are unit-testable. Written
/// atomically by [`write`], read back by [`read`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    /// How far the run has come.
    pub state: State,
    /// The resolved repo root — the field that catches a hash
    /// collision (detach.md B3): an entry whose recorded root differs
    /// from the resolved repo is refused, never attached to.
    pub repo: String,
    /// The sidecar of that repo.
    pub sidecar: String,
    /// The session `NAME`, or `None` for the live workspace.
    pub session: Option<String>,
    /// The merged backend the run uses.
    pub backend: String,
    /// The merged multiplexer the run uses.
    pub multiplexer: String,
    /// Seconds since the Unix epoch (see `crate::result::epoch_secs`).
    pub started_at: u64,
    /// Seconds since the Unix epoch, present once the run ended.
    pub finished_at: Option<u64>,
    /// The backend's exit code, for `exited`.
    pub exit_code: Option<i32>,
    /// The `mysbx: ` diagnosis of a `failed` startup.
    pub error: Option<String>,
}

/// Why the registry could not be used. Every variant is a refusal
/// (cli.md D8 exit code `70` at the CLI layer); the registry itself
/// never falls back to a less suitable place (detach.md B3).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// `$XDG_RUNTIME_DIR` is not set. Refused, no `/tmp` fallback —
    /// the reason `TMUX_TMPDIR` has no fallback either.
    RuntimeDirUnset,
    /// `$XDG_RUNTIME_DIR` is a relative path. The runtime root must
    /// name the per-user tmpfs the session manager mounts; a relative
    /// path depends on the caller's directory.
    RuntimeDirRelative(PathBuf),
    /// A session NAME that fails `crate::session::valid_name` — a
    /// usage error at parse time (cli.md D8), like every schema edge.
    InvalidSession { name: String },
    /// The computed socket path exceeds [`SUN_PATH_LIMIT`]: refused,
    /// never silently shortened — a shortened path would name another
    /// run's socket (detach.md B3).
    SocketPathTooLong { path: PathBuf, len: usize },
    /// A registry directory is not owned by the current uid
    /// (detach.md B3): a directory somebody else controls is not a
    /// place to put a socket a payload connects to.
    NotOwnedByUs { path: PathBuf, owner: u32 },
    /// A registry directory is group- or world-accessible
    /// (detach.md B3): the socket grants the sandbox; the registry
    /// that holds it must be private.
    AccessibleByOthers { path: PathBuf, mode: u32 },
    /// The state file records a different repo root than the one
    /// resolved — the 40-bit `fnv1a10` collided (detach.md B3). The
    /// entry is refused, never attached to.
    RepoMismatch { recorded: String, resolved: String },
    /// The filesystem could not be read or written. `kind` is the
    /// `std::io::ErrorKind` of the underlying error, kept so a
    /// caller can tell a missing file from any other failure
    /// without string-matching the message (the same idiom the
    /// probe at the socket uses).
    Io {
        path: PathBuf,
        kind: std::io::ErrorKind,
        why: String,
    },
    /// The state file could not be parsed: not the format [`render`]
    /// writes.
    Unparsable { why: String },
    /// The state file carries a `"version"` this reader does not
    /// know; refusing it is the point of the field.
    UnsupportedVersion { found: i64 },
    /// The workspace lock is held by another run (detach.md B6):
    /// contention, reported separately from an unusable registry so
    /// the caller can tell the two apart.
    LockContended { path: PathBuf },
    /// A mount source is — or lies below or above — the runtime root
    /// (detach.md B8): a sandbox that could reach the registry could
    /// connect to a dtach socket and type into every detached run
    /// of the user.
    RuntimeRootExposed {
        /// The mount source, canonicalized.
        source: PathBuf,
        /// The runtime root it exposes.
        runtime_root: PathBuf,
        /// How the source relates to the runtime root.
        relation: RuntimeRootRelation,
    },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::RuntimeDirUnset => write!(
                f,
                "refusing to detach without $XDG_RUNTIME_DIR: the detached-run \
                 registry lives in the per-user runtime root, and /tmp is not \
                 used as a fallback (detach.md B3)"
            ),
            Error::RuntimeDirRelative(dir) => write!(
                f,
                "refusing to detach with a relative $XDG_RUNTIME_DIR: {}: the \
                 runtime root must name the per-user tmpfs, not a path that \
                 depends on the caller's directory (detach.md B3)",
                dir.display()
            ),
            Error::InvalidSession { name } => write!(
                f,
                "refusing to detach for the session `{name}`: not a valid \
                 session NAME (detach.md B3)"
            ),
            Error::SocketPathTooLong { path, len } => write!(
                f,
                "refusing to detach: the socket path {} is {len} bytes, over \
                 the 107-byte sun_path limit — it is never silently shortened \
                 (detach.md B3)",
                path.display()
            ),
            Error::NotOwnedByUs { path, owner } => write!(
                f,
                "refusing to use the registry directory {}: owned by uid \
                 {owner}, not the current user (detach.md B3)",
                path.display()
            ),
            Error::AccessibleByOthers { path, mode } => write!(
                f,
                "refusing to use the registry directory {}: mode {:o} is \
                 group- or world-accessible (detach.md B3)",
                path.display(),
                mode
            ),
            Error::RepoMismatch { recorded, resolved } => write!(
                f,
                "refusing to use the registry entry: it records the repo \
                 {recorded}, not the resolved repo {resolved} — the \
                 fnv1a10 registry name collided (detach.md B3)"
            ),
            Error::Io { path, why, .. } => {
                write!(f, "cannot use {}: {why}", path.display())
            }
            Error::Unparsable { why } => {
                write!(f, "the detached-run state file is unparsable: {why}")
            }
            Error::UnsupportedVersion { found } => write!(
                f,
                "refusing to read the detached-run state file: \
                 version {found} is not {SCHEMA_VERSION} (detach.md B5)"
            ),
            Error::LockContended { path } => write!(
                f,
                "refusing to use the registry: the workspace lock {} is \
                 held by another detached run (detach.md B6)",
                path.display()
            ),
            Error::RuntimeRootExposed {
                source,
                runtime_root,
                relation,
            } => write!(
                f,
                "the mount source {} {} the runtime root {} \
                 ($XDG_RUNTIME_DIR/mysbx, the registry of detached runs) — \
                 a sandbox that could reach it could type into every \
                 detached run of the user (detach.md B8)",
                source.display(),
                match relation {
                    RuntimeRootRelation::Equal => "is",
                    RuntimeRootRelation::Below => "is below",
                    RuntimeRootRelation::Above => "is above",
                },
                runtime_root.display()
            ),
        }
    }
}

impl std::error::Error for Error {}

impl Error {
    /// Whether the error is a state file that is simply GONE — the
    /// re-read under the attach lock races the next `--detach`, for
    /// which a missing record is an outcome, not a failure.
    fn is_not_found(&self) -> bool {
        matches!(
            self,
            Error::Io {
                kind: std::io::ErrorKind::NotFound,
                ..
            }
        )
    }
}

/// Read `$XDG_RUNTIME_DIR` (detach.md B3): unset or relative is
/// refused, `/tmp` is never used as a fallback. Reads the
/// environment, like [`runtime_dir_from_env`]; everything else
/// takes the root as a parameter, so the tests never mutate it.
pub fn runtime_root_from_env() -> Result<PathBuf, Error> {
    let raw = std::env::var("XDG_RUNTIME_DIR").ok();
    runtime_root_from(raw.as_deref())
}

/// Read `$XDG_RUNTIME_DIR` for the B8 mount refusal: unset or
/// relative passes there, unlike [`runtime_root_from_env`], which
/// refuses both — the seam the pipeline reads instead of the
/// variable itself.
pub fn runtime_dir_from_env() -> Option<std::ffi::OsString> {
    std::env::var_os("XDG_RUNTIME_DIR")
}

/// How a mount source relates to the runtime root
/// `$XDG_RUNTIME_DIR/mysbx` (detach.md B8): a source that IS it,
/// lies below it or contains it exposes the registry of detached
/// runs to the sandbox.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeRootRelation {
    /// The source IS the runtime root.
    Equal,
    /// The source lies below the runtime root
    /// (`$XDG_RUNTIME_DIR/mysbx/<repo>`).
    Below,
    /// The source contains the runtime root (`$XDG_RUNTIME_DIR`,
    /// `/run/user`, `/run`, `/`).
    Above,
}

/// The registry root a raw `$XDG_RUNTIME_DIR` value names:
/// `<runtime_dir>/mysbx`, with a raw-value fallback on either
/// canonicalization — the `home_canon` precedent in `merge.rs`'s
/// `merge` and `resolve_cli_path` — so a symlinked runtime dir and
/// a pre-existing `mysbx` symlink anchor the refusal at the real
/// registry location, and the not-yet-existing `mysbx/` child
/// needs no filesystem probe.
fn registry_root_from(runtime_dir: &Path) -> PathBuf {
    let dir = std::fs::canonicalize(runtime_dir).unwrap_or_else(|_| runtime_dir.to_path_buf());
    let root = dir.join("mysbx");
    std::fs::canonicalize(&root).unwrap_or(root)
}

/// Refuse a mount whose source is the runtime root, below it or
/// above it (detach.md B8): a process that can connect to a dtach
/// socket can type into that terminal, so a sandbox that could reach
/// `$XDG_RUNTIME_DIR/mysbx/` could type into every detached run of
/// the user — including runs of other repos with other grants. This
/// is a refusal of every run, detached or not: a foreground run
/// could reach detached ones just the same.
///
/// An unset or relative `$XDG_RUNTIME_DIR` passes: without a
/// runtime root there is nothing at, below or above to refuse —
/// detached runs refuse the variable themselves (B3, no fallback),
/// but a foreground run must not break over an environment without
/// it. Pure, so the tests never mutate the environment; the mount
/// sources are the merge's canonicalized values (config.md D8),
/// compared as stored.
pub fn check_mount_exposure(
    runtime_dir: Option<&std::ffi::OsStr>,
    mounts: &[crate::config::Mount],
) -> Result<(), Error> {
    let Some(raw) = runtime_dir else {
        return Ok(());
    };
    let dir = Path::new(raw);
    if !dir.is_absolute() {
        return Ok(());
    }
    let root = registry_root_from(dir);
    for m in mounts {
        let source = Path::new(&m.path);
        let relation = if source == root {
            Some(RuntimeRootRelation::Equal)
        } else if source.starts_with(&root) {
            Some(RuntimeRootRelation::Below)
        } else if root.starts_with(source) {
            Some(RuntimeRootRelation::Above)
        } else {
            None
        };
        if let Some(relation) = relation {
            return Err(Error::RuntimeRootExposed {
                source: source.to_path_buf(),
                runtime_root: root,
                relation,
            });
        }
    }
    Ok(())
}

/// Interpret the raw `$XDG_RUNTIME_DIR` value (detach.md B3): unset
/// or relative is refused — the runtime root must name the per-user
/// tmpfs the session manager mounts, not a path that depends on the
/// caller's directory.
fn runtime_root_from(raw: Option<&str>) -> Result<PathBuf, Error> {
    let root = PathBuf::from(raw.ok_or(Error::RuntimeDirUnset)?);
    if !root.is_absolute() {
        return Err(Error::RuntimeDirRelative(root));
    }
    Ok(root)
}

/// The files of one workspace in the registry: the per-repo directory
/// and the socket, state-file and lock paths inside it (detach.md
/// B3, B5, B6). Plain data, built by [`paths`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Paths {
    /// `$XDG_RUNTIME_DIR/mysbx/<fnv1a10(repo root)>/`.
    pub dir: PathBuf,
    /// `<dir>/<stem>.sock` — the dtach socket.
    pub sock: PathBuf,
    /// `<dir>/<stem>.json` — the state file (B5).
    pub json: PathBuf,
    /// `<dir>/<stem>.lock` — the per-workspace lock (B6).
    pub lock: PathBuf,
}

/// Compute the registry paths of `workspace` under `runtime_root`
/// for `repo_root` (detach.md B3): the per-repo directory is named by
/// the `fnv1a10` hash of the repo root, the same digest the podman
/// container names use. Refuses a relative runtime root, an invalid
/// session NAME and a socket path over [`SUN_PATH_LIMIT`] — the last
/// one is never silently shortened.
pub fn paths(runtime_root: &Path, repo_root: &Path, workspace: &Workspace) -> Result<Paths, Error> {
    if !runtime_root.is_absolute() {
        return Err(Error::RuntimeDirRelative(runtime_root.to_owned()));
    }
    if let Workspace::Session(name) = workspace {
        if !crate::session::valid_name(name) {
            return Err(Error::InvalidSession { name: name.clone() });
        }
    }
    let stem = workspace.stem();
    let dir = runtime_root
        .join("mysbx")
        .join(crate::repo::fnv1a10(&repo_root.to_string_lossy()));
    let sock = dir.join(format!("{stem}.sock"));
    // sun_path is bytes, not characters — measure the raw bytes of
    // the path, like bind(2) does.
    let len = std::os::unix::ffi::OsStrExt::as_bytes(sock.as_os_str()).len();
    if len > SUN_PATH_LIMIT {
        return Err(Error::SocketPathTooLong { path: sock, len });
    }
    Ok(Paths {
        sock,
        json: dir.join(format!("{stem}.json")),
        lock: dir.join(format!("{stem}.lock")),
        dir,
    })
}

/// The real effective UID (libc `geteuid`) — the same zero-dependency
/// raw extern idiom as `libc_geteuid` in lib.rs.
unsafe fn libc_geteuid() -> u32 {
    extern "C" {
        fn geteuid() -> u32;
    }
    geteuid()
}

/// Whether a registry directory is ours (detach.md B3): owned by the
/// effective uid and private to it. Pure, so the tests can exercise
/// the ownership decision — they run unprivileged and cannot chown.
fn dir_is_ours(owner: u32, mode: u32, euid: u32) -> bool {
    owner == euid && (mode & 0o077) == 0
}

/// Create `mysbx/` and the per-repo directory `0700` and verify both
/// (detach.md B3): a directory not owned by the current uid or
/// group/world-accessible is refused — the socket grants the
/// sandbox, the registry that holds it must be private to the user.
pub fn ensure(paths: &Paths) -> Result<(), Error> {
    let mysbx_dir = paths
        .dir
        .parent()
        .ok_or_else(|| Error::Io {
            path: paths.dir.clone(),
            kind: std::io::ErrorKind::NotFound,
            why: "the registry directory has no parent".to_owned(),
        })?
        .to_owned();
    use std::fs::DirBuilder;
    use std::os::unix::fs::DirBuilderExt;
    let mut builder = DirBuilder::new();
    builder.recursive(true).mode(0o700);
    builder.create(&mysbx_dir).map_err(|e| Error::Io {
        path: mysbx_dir.clone(),
        kind: e.kind(),
        why: e.to_string(),
    })?;
    // `mysbx/` is verified before anything is created inside it: a
    // directory that fails verification must not gain contents.
    verify_dir(&mysbx_dir)?;
    builder.create(&paths.dir).map_err(|e| Error::Io {
        path: paths.dir.clone(),
        kind: e.kind(),
        why: e.to_string(),
    })?;
    verify_dir(&paths.dir)
}

/// Verify one registry directory: owned by the current uid, private
/// to it (detach.md B3).
fn verify_dir(dir: &Path) -> Result<(), Error> {
    use std::os::unix::fs::MetadataExt;
    let md = std::fs::symlink_metadata(dir).map_err(|e| Error::Io {
        path: dir.to_owned(),
        kind: e.kind(),
        why: e.to_string(),
    })?;
    // The raw st_mode, not Permissions: the decision is about the
    // bits on disk, in the same terms detach.md B3 states them.
    let mode = md.mode();
    let euid = unsafe { libc_geteuid() };
    if !dir_is_ours(md.uid(), mode, euid) {
        if md.uid() != euid {
            return Err(Error::NotOwnedByUs {
                path: dir.to_owned(),
                owner: md.uid(),
            });
        }
        return Err(Error::AccessibleByOthers {
            path: dir.to_owned(),
            mode,
        });
    }
    Ok(())
}

/// Render `r` as the state-file text (detach.md B5): pretty-printed
/// with two spaces, a fixed key order, one trailing newline — the
/// same layout `crate::result::render` gives `result.json`, so the
/// registry file reads like the sidecar file. The order is fixed so
/// byte comparisons (tests) are stable. Absent optionals render as
/// `null`, so the field set is closed and a reader never has to guess
/// which keys a state carries.
pub fn render(r: &Record) -> String {
    let mut fields: Vec<String> = Vec::new();
    let mut s = |v: String| fields.push(v);
    s(format!("  \"version\": {SCHEMA_VERSION}"));
    s(format!(
        "  \"state\": {}",
        crate::result::quote(r.state.as_str())
    ));
    s(format!("  \"repo\": {}", crate::result::quote(&r.repo)));
    s(format!(
        "  \"sidecar\": {}",
        crate::result::quote(&r.sidecar)
    ));
    s(format!(
        "  \"session\": {}",
        r.session
            .as_deref()
            .map(crate::result::quote)
            .unwrap_or_else(|| "null".to_owned())
    ));
    s(format!(
        "  \"backend\": {}",
        crate::result::quote(&r.backend)
    ));
    s(format!(
        "  \"multiplexer\": {}",
        crate::result::quote(&r.multiplexer)
    ));
    s(format!(
        "  \"startedAt\": {}",
        crate::result::quote(&crate::result::iso8601(r.started_at))
    ));
    s(format!(
        "  \"finishedAt\": {}",
        r.finished_at
            .map(|t| crate::result::quote(&crate::result::iso8601(t)))
            .unwrap_or_else(|| "null".to_owned())
    ));
    s(format!(
        "  \"exitCode\": {}",
        r.exit_code
            .map(|c| c.to_string())
            .unwrap_or_else(|| "null".to_owned())
    ));
    s(format!(
        "  \"error\": {}",
        r.error
            .as_deref()
            .map(crate::result::quote)
            .unwrap_or_else(|| "null".to_owned())
    ));
    format!("{{\n{}\n}}\n", fields.join(",\n"))
}

/// Find `"key": <value>` in the rendered state file and return the
/// value text: a quoted string with [`crate::result::quote`]'s escapes
/// undone. Hand-rolled like the TOML parser, the crate has no
/// serde; unparsable input is refused, never guessed at
/// (docs/design/config.md D8).
fn string_field(text: &str, key: &str) -> Result<String, Error> {
    let start = field_start(text, key)?;
    string_at(text, key, start)
}

/// Where a `"key": ` field's value begins in `text`.
fn field_start(text: &str, key: &str) -> Result<usize, Error> {
    let needle = format!("\"{key}\": ");
    text.find(&needle)
        .map(|at| at + needle.len())
        .ok_or_else(|| Error::Unparsable {
            why: format!("no `{key}` field"),
        })
}

/// Find `"key": <value>` and return it as `None` for `null`, or the
/// parsed value: a quoted string for the string fields, a number for
/// `exitCode`. The timestamps go through [`string_field`] and
/// `crate::result::iso8601` in reverse — epoch seconds parsed back
/// out of the ISO text, the same round trip `result.json` readers do
/// by hand.
fn optional_field(text: &str, key: &str, kind: FieldKind) -> Result<Option<String>, Error> {
    let start = field_start(text, key)?;
    let rest = &text[start..];
    if rest.starts_with("null") {
        return Ok(None);
    }
    match kind {
        FieldKind::String => Ok(Some(string_at(text, key, start)?)),
        FieldKind::Number => {
            let raw = rest.split(',').next().unwrap_or(rest).trim();
            raw.parse::<i64>()
                .map(|n| Some(n.to_string()))
                .map_err(|_| Error::Unparsable {
                    why: format!("`{key}` is not a number"),
                })
        }
    }
}

/// What kind of value an [`optional_field`] holds.
#[derive(Clone, Copy)]
enum FieldKind {
    String,
    Number,
}

/// The quoted-string value at `start` (already past `"key": `),
/// with [`crate::result::quote`]'s escapes undone — the one copy of
/// the decoding loop both field readers use.
fn string_at(text: &str, key: &str, start: usize) -> Result<String, Error> {
    let rest = &text[start..];
    if !rest.starts_with('"') {
        return Err(Error::Unparsable {
            why: format!("`{key}` is not a string"),
        });
    }
    let mut out = String::new();
    let mut chars = rest[1..].chars();
    while let Some(c) = chars.next() {
        match c {
            '"' => return Ok(out),
            '\\' => match chars.next() {
                Some('"') => out.push('"'),
                Some('\\') => out.push('\\'),
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some('t') => out.push('\t'),
                Some('b') => out.push('\u{08}'),
                Some('f') => out.push('\u{0c}'),
                Some('u') => {
                    let hex: String = chars.by_ref().take(4).collect();
                    let code = u32::from_str_radix(&hex, 16).map_err(|_| Error::Unparsable {
                        why: format!("`{key}` has an unparsable \\u escape"),
                    })?;
                    out.push(char::from_u32(code).ok_or_else(|| Error::Unparsable {
                        why: format!("`{key}` has an invalid \\u escape"),
                    })?);
                }
                _ => {
                    return Err(Error::Unparsable {
                        why: format!("`{key}` has an unknown escape"),
                    })
                }
            },
            c => out.push(c),
        }
    }
    Err(Error::Unparsable {
        why: format!("`{key}` is not terminated"),
    })
}

/// Parse a state file written by [`render`] (detach.md B5). The
/// reader refuses a `"version"` it does not know and an unknown
/// `state` value instead of misreading the file — the same contract
/// `crate::result::SCHEMA_VERSION` gives the `result.json` reader.
/// The timestamps are ISO-8601 text, parsed back to epoch seconds so
/// the record stays plain data.
pub fn read(paths: &Paths) -> Result<Record, Error> {
    let text = std::fs::read_to_string(&paths.json).map_err(|e| Error::Io {
        path: paths.json.clone(),
        kind: e.kind(),
        why: e.to_string(),
    })?;
    parse(&text)
}

/// Parse state-file text. Split from [`read`] so the parser is
/// testable without touching the filesystem.
fn parse(text: &str) -> Result<Record, Error> {
    let version = required_number(text, "version")?;
    if version != SCHEMA_VERSION {
        return Err(Error::UnsupportedVersion { found: version });
    }
    let state_raw = string_field(text, "state")?;
    let state = State::parse(&state_raw).ok_or_else(|| Error::Unparsable {
        why: format!("unknown state `{state_raw}`"),
    })?;
    let started_text = string_field(text, "startedAt")?;
    let started_at = parse_iso8601(&started_text)?;
    let finished_at = match optional_field(text, "finishedAt", FieldKind::String)? {
        Some(t) => Some(parse_iso8601(&t)?),
        None => None,
    };
    Ok(Record {
        state,
        repo: string_field(text, "repo")?,
        sidecar: string_field(text, "sidecar")?,
        session: optional_field(text, "session", FieldKind::String)?,
        backend: string_field(text, "backend")?,
        multiplexer: string_field(text, "multiplexer")?,
        started_at,
        finished_at,
        exit_code: optional_field(text, "exitCode", FieldKind::Number)?
            .map(|n| n.parse::<i32>())
            .transpose()
            .map_err(|_| Error::Unparsable {
                why: "exitCode is not an i32".to_owned(),
            })?,
        error: optional_field(text, "error", FieldKind::String)?,
    })
}

/// A `"key": <number>` field, mandatory: `version` is the one
/// numeric always-present field.
fn required_number(text: &str, key: &str) -> Result<i64, Error> {
    let needle = format!("\"{key}\": ");
    let start = text.find(&needle).ok_or_else(|| Error::Unparsable {
        why: format!("no `{key}` field"),
    })? + needle.len();
    let rest = &text[start..];
    let raw = rest.split([',', '\n']).next().unwrap_or(rest).trim();
    raw.parse::<i64>().map_err(|_| Error::Unparsable {
        why: format!("`{key}` is not a number"),
    })
}

/// ISO-8601 UTC (`1970-01-01T00:00:00Z`, the shape
/// `crate::result::iso8601` writes) back to epoch seconds. The
/// format is fixed by the writer; anything else is unparsable.
fn parse_iso8601(text: &str) -> Result<u64, Error> {
    let unparsable = |why: &str| Error::Unparsable {
        why: format!("timestamp `{text}` is not ISO-8601: {why}"),
    };
    let bytes = text.as_bytes();
    if bytes.len() != 20
        || bytes[4] != b'-'
        || bytes[7] != b'-'
        || bytes[10] != b'T'
        || bytes[13] != b':'
        || bytes[16] != b':'
        || bytes[19] != b'Z'
    {
        return Err(unparsable("expected YYYY-MM-DDThh:mm:ssZ"));
    }
    let digits: Vec<u32> = text[..19]
        .bytes()
        .filter(|b| *b != b'-' && *b != b'T' && *b != b':')
        .map(|b| b.is_ascii_digit().then(|| u32::from(b - b'0')))
        .collect::<Option<Vec<u32>>>()
        .ok_or_else(|| unparsable("expected digits"))?;
    if digits.len() != 14 {
        return Err(unparsable("expected digits"));
    }
    let year = digits[0] * 1000 + digits[1] * 100 + digits[2] * 10 + digits[3];
    let month = digits[4] * 10 + digits[5];
    let day = digits[6] * 10 + digits[7];
    let (hour, min, sec) = (
        digits[8] * 10 + digits[9],
        digits[10] * 10 + digits[11],
        digits[12] * 10 + digits[13],
    );
    if !(1..=12).contains(&month) || hour > 23 || min > 59 || sec > 59 {
        return Err(unparsable("component out of range"));
    }
    // The day is checked against the month's actual length, so a
    // corrupt date like Feb 30 is refused instead of silently
    // reading as the next month.
    let leap = year.is_multiple_of(4) && (!year.is_multiple_of(100) || year.is_multiple_of(400));
    let month_len = match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if leap => 29,
        _ => 28,
    };
    if day < 1 || day > month_len {
        return Err(unparsable("component out of range"));
    }
    // Days since the epoch from the civil date: Howard Hinnant's
    // `days_from_civil`, the inverse of result.rs's `civil_from_days`.
    let y = i64::from(year) - i64::from(month <= 2);
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = if month > 2 { month - 3 } else { month + 9 } as i64;
    let doy = (153 * mp + 2) / 5 + i64::from(day) - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    let days = era * 146_097 + doe - 719_468;
    if days < 0 {
        return Err(unparsable("before the epoch"));
    }
    Ok((days * 86_400 + i64::from(hour) * 3_600 + i64::from(min) * 60 + i64::from(sec)) as u64)
}

/// Write `r` to the state file atomically (detach.md B5), via the
/// crate's shared [`crate::write_atomically`].
pub fn write(paths: &Paths, r: &Record) -> Result<(), Error> {
    crate::write_atomically(&paths.json, &render(r)).map_err(|why| Error::Io {
        path: paths.json.clone(),
        kind: std::io::ErrorKind::Other,
        why,
    })
}

/// Refuse an entry that records a different repo root than the one
/// resolved (detach.md B3): the per-repo directory name is a 40-bit
/// hash, and a collision must never let one repo attach to another's
/// run.
pub fn ensure_same_repo(r: &Record, repo_root: &Path) -> Result<(), Error> {
    let resolved = repo_root.to_string_lossy().into_owned();
    if r.repo != resolved {
        return Err(Error::RepoMismatch {
            recorded: r.repo.clone(),
            resolved,
        });
    }
    Ok(())
}

/// What a `connect(2)` to the socket found. Liveness is the socket,
/// never the state file's field (detach.md B5).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Probe {
    /// A connection was accepted: the dtach master is alive.
    Live,
    /// The socket file exists but refuses connections: the master
    /// died and left the path behind — removed under the lock (B6).
    Stale,
    /// No socket at the path.
    Absent,
}

/// Probe the socket: `connect(2)` and nothing else. A live probe
/// drops the stream again immediately — connecting is the whole
/// question (B5).
pub fn probe(sock: &Path) -> Probe {
    match std::os::unix::net::UnixStream::connect(sock) {
        Ok(_) => Probe::Live,
        Err(e) => {
            // The connect can race with the socket's unlink: a
            // `ENOENT` from the connect means absent, and so does a
            // path that is gone once inspected.
            if e.kind() == std::io::ErrorKind::NotFound {
                return Probe::Absent;
            }
            if sock.exists() {
                Probe::Stale
            } else {
                Probe::Absent
            }
        }
    }
}

/// How `status` and `attach` report a record: liveness first, the
/// state only when there is nothing to contradict it (detach.md B5).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Status {
    /// The socket accepts connections.
    Live,
    /// A `running`/`starting` record whose socket is gone or dead:
    /// the dtach master was killed.
    Lost,
    /// A terminal state: the record stays until `attach` removes it.
    Finished,
}

/// Classify a record together with its probe: a live socket means a
/// live run whatever the state says; a terminal state means finished
/// whatever the socket says (the socket is dead by then); anything
/// else is lost (B5).
pub fn classify(r: &Record, p: Probe) -> Status {
    if p == Probe::Live {
        return Status::Live;
    }
    if r.state.is_finished() {
        return Status::Finished;
    }
    Status::Lost
}

/// One entry of the read-only listing [`list_records`] returns
/// (detach.md B9): the workspace, the record and the probed status
/// — the three facts a `detached:` line of `status` prints.
#[derive(Debug, Clone)]
pub struct Listed {
    /// The workspace the record belongs to.
    pub workspace: Workspace,
    /// The state file's contents.
    pub record: Record,
    /// Liveness-first classification of the record and its socket.
    pub status: Status,
}

/// List the detached runs registered for `repo_root` under
/// `runtime_root` (detach.md B9): read-only, the `status` verb's
/// `detached:` section. Reads the per-repo registry directory
/// `runtime_root/mysbx/<fnv1a10(repo_root)>/`, filters `*.json`,
/// recovers the workspace from each file's stem and classifies the
/// record against its socket's probe.
///
/// A missing directory is the empty registry (no runs). A corrupt
/// record is SKIPPED silently: status is read-only and must not
/// crash on a bad file — `attach` owns cleanup. The entries are
/// sorted by stem for stable output. Pure except for the
/// `read_dir`/`read`/`probe` IO; the function takes no lock, writes
/// nothing and removes nothing.
pub fn list_records(runtime_root: &Path, repo_root: &Path) -> Vec<Listed> {
    let dir = runtime_root
        .join("mysbx")
        .join(crate::repo::fnv1a10(&repo_root.to_string_lossy()));
    let rd = match std::fs::read_dir(&dir) {
        Ok(rd) => rd,
        Err(_) => return Vec::new(),
    };
    let mut stems: Vec<String> = rd
        .filter_map(|e| e.ok())
        .filter_map(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            name.strip_suffix(".json").map(str::to_owned)
        })
        .collect();
    stems.sort();
    let mut out = Vec::new();
    for stem in stems {
        let Some(workspace) = Workspace::parse_stem(&stem) else {
            continue;
        };
        let Ok(paths) = paths(runtime_root, repo_root, &workspace) else {
            continue;
        };
        let Ok(record) = read(&paths) else {
            continue;
        };
        let status = classify(&record, probe(&paths.sock));
        out.push(Listed {
            workspace,
            record,
            status,
        });
    }
    out
}

// The flock(2) operation constants from <sys/file.h>.
const LOCK_EX: i32 = 2;
const LOCK_NB: i32 = 4;

/// An exclusive lock on `<workspace>.lock` (detach.md B6), held for
/// the guard's lifetime: `--detach`/`--detachable` hold it from the
/// liveness probe until the handshake ends, `attach` while it removes
/// a finished record. Dropping the guard closes the file, which
/// releases the flock.
#[derive(Debug)]
pub struct WorkspaceLock {
    // The file is the lock's whole state — kept, not read: dropping
    // it closes the descriptor, which releases the flock.
    _fd: std::fs::File,
}

/// Take the workspace lock, blocking until it is free (detach.md
/// B6).
pub fn lock(paths: &Paths) -> Result<WorkspaceLock, Error> {
    let fd = open_lock_file(paths)?;
    flock_file(&fd, LOCK_EX).map_err(|e| Error::Io {
        path: paths.lock.clone(),
        kind: e.kind(),
        why: e.to_string(),
    })?;
    Ok(WorkspaceLock { _fd: fd })
}

/// Try to take the workspace lock without waiting (detach.md B6):
/// `EWOULDBLOCK` — the lock is held, by a concurrent `--detach` — is
/// reported as [`Error::LockContended`], never a silent pass; the
/// caller decides what contention means.
pub fn try_lock(paths: &Paths) -> Result<WorkspaceLock, Error> {
    let fd = open_lock_file(paths)?;
    match flock_file(&fd, LOCK_EX | LOCK_NB) {
        Ok(()) => Ok(WorkspaceLock { _fd: fd }),
        Err(e) if e.raw_os_error() == Some(LIBC_EWOULDBLOCK) => Err(Error::LockContended {
            path: paths.lock.clone(),
        }),
        Err(e) => Err(Error::Io {
            path: paths.lock.clone(),
            kind: e.kind(),
            why: e.to_string(),
        }),
    }
}

/// Open the lock file, creating it if absent. The file is only ever
/// a flock carrier — its contents never matter.
fn open_lock_file(paths: &Paths) -> Result<std::fs::File, Error> {
    std::fs::File::create(&paths.lock).map_err(|e| Error::Io {
        path: paths.lock.clone(),
        kind: e.kind(),
        why: e.to_string(),
    })
}

/// Call `flock(2)` — the same zero-dependency raw extern idiom as
/// `libc_geteuid`. On the blocking operation every error is real; on
/// the non-blocking one the caller decides what `EWOULDBLOCK` means.
fn flock_file(file: &std::fs::File, op: i32) -> std::io::Result<()> {
    use std::os::fd::AsRawFd;
    let rc = unsafe { libc_flock(file.as_raw_fd(), op) };
    if rc != 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

unsafe fn libc_flock(fd: i32, op: i32) -> i32 {
    extern "C" {
        fn flock(fd: i32, op: i32) -> i32;
    }
    flock(fd, op)
}

// `EWOULDBLOCK` from <errno.h>, compared against
// `std::io::Error::raw_os_error`.
const LIBC_EWOULDBLOCK: i32 = 11;

/// Remove a stale socket (detach.md B6): a socket whose file exists
/// but refuses connections is the corpse of a killed master. Never
/// touches a live socket; returns whether it removed one. Intended
/// to be called with the workspace lock held — without it, two
/// concurrent `--detach` runs could both find a stale socket and the
/// second could unlink the first one's fresh socket.
pub fn remove_stale(paths: &Paths) -> Result<bool, Error> {
    if probe(&paths.sock) == Probe::Stale {
        match std::fs::remove_file(&paths.sock) {
            Ok(()) => return Ok(true),
            // The socket vanished between the probe and the unlink:
            // absent, not stale — nothing was removed.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(false),
            Err(e) => {
                return Err(Error::Io {
                    path: paths.sock.clone(),
                    kind: e.kind(),
                    why: e.to_string(),
                })
            }
        }
    }
    Ok(false)
}

// ---- the --detach orchestration (detach.md B2, B4, B10) ---------------------

/// The outer mysbx's launch parameters (detach.md B4): the resolved
/// repo and the merged backend/multiplexer the run uses, carried as
/// plain data so `sandbox` hands them over without a borrow of its
/// whole local scope.
pub struct Launch<'a> {
    /// The resolved repo root — the identity the registry hashes.
    pub repo_root: &'a Path,
    /// The repo's sidecar.
    pub sidecar: &'a Path,
    /// The session NAME of a clone run, or `None` for the live
    /// workspace.
    pub session: Option<&'a str>,
    /// The merged backend of the run.
    pub backend: &'a str,
    /// The merged multiplexer of the run.
    pub multiplexer: &'a str,
}

/// The original argument tail of the invocation minus `--detach`,
/// `--detachable` and `--verbose` (detach.md B4 step 2): what the
/// outer passes to the inner mysbx, so the inner is a plain bare-form
/// run of the same workspace. Pure over the given arguments so the
/// tests assert the filter exactly.
pub fn inner_args_of(args: impl Iterator<Item = String>) -> Vec<String> {
    args.filter(|a| !matches!(a.as_str(), "--detach" | "--detachable" | "--verbose"))
        .collect()
}

/// [`inner_args_of`] over this process's own arguments (without
/// argv[0]) — the tail the outer relaunches.
pub fn detach_inner_args() -> Vec<String> {
    inner_args_of(std::env::args().skip(1))
}

/// The `dtach` launcher argv (detach.md B4 step 2):
/// `dtach -n <socket> -r winch <self> <inner args...>`, one entry per
/// vector element, the executable INCLUDED. `-n` does not attach a
/// client (the run starts detached), `-r winch` redraws the client's
/// terminal on attach. Pure, so the tests assert the exact shape.
pub fn launcher_argv(
    dtach: &str,
    sock: &Path,
    self_exe: &str,
    inner_args: &[String],
) -> Vec<String> {
    let mut argv = vec![
        dtach.to_owned(),
        "-n".to_owned(),
        sock.to_string_lossy().into_owned(),
        "-r".to_owned(),
        "winch".to_owned(),
        self_exe.to_owned(),
    ];
    argv.extend(inner_args.iter().cloned());
    argv
}

/// The workspace of a run's `--session` flag (detach.md B3): the
/// named session's clone, or the live checkout.
fn workspace_of(session: Option<&str>) -> Workspace {
    match session {
        Some(name) => Workspace::Session(name.to_owned()),
        None => Workspace::Live,
    }
}

/// Resolve the pinned `dtach` (detach.md B10): `MYSBX_DTACH`, the
/// `MYSBX_BWRAP` idiom, with the bare `dtach` PATH fallback of an
/// unwrapped build. An unpinned, missing or non-executable dtach is
/// `None` — the caller refuses with `70` naming the variable; other
/// runs are unaffected.
pub(crate) fn resolve_dtach() -> Option<PathBuf> {
    let name = match std::env::var("MYSBX_DTACH") {
        Ok(v) if !v.is_empty() => v,
        _ => "dtach".to_owned(),
    };
    crate::doctor::resolve_executable(&name, std::env::var_os("PATH").as_deref())
}

/// [`resolve_dtach`] with the operator-facing refusal (detach.md
/// B10) attached: a `None` resolution prints the two-line refusal —
/// a missing or non-executable dtach disables `--detach`,
/// `--detachable` and `attach` alone — and yields the `70` the
/// callers return; the text lives here once for every user.
pub(crate) fn resolve_dtach_or_refuse() -> Result<PathBuf, i32> {
    match resolve_dtach() {
        Some(p) => Ok(p),
        None => {
            eprintln!(
                "mysbx: --detach, --detachable and attach require a usable dtach (MYSBX_DTACH) — none was found on PATH"
            );
            eprintln!(
                "  a missing or non-executable dtach disables all three --detach, --detachable and attach — other runs are unaffected (docs/design/detach.md B10)"
            );
            Err(crate::EXIT_INFRASTRUCTURE)
        }
    }
}

/// The verb's own usage line, printed by its usage errors (the same
/// pairing rule usage.txt follows, cli.md D5).
pub const USAGE_ATTACH: &str = "usage: mysbx attach [--session <name>]";

/// The `dtach` attach argv (detach.md B2):
/// `dtach -a <socket> -r winch -z` — `-a` attaches a client, `-r
/// winch` redraws this terminal on attach, `-z` sends `^Z` into the
/// run instead of suspending the client. The detach key is dtach's
/// default `^\`. Pure, so the tests assert the exact shape.
pub fn attach_argv(dtach: &str, sock: &Path) -> Vec<String> {
    vec![
        dtach.to_owned(),
        "-a".to_owned(),
        sock.to_string_lossy().into_owned(),
        "-r".to_owned(),
        "winch".to_owned(),
        "-z".to_owned(),
    ]
}

/// `mysbx attach [--session NAME]` (detach.md B2, B5, B6): connect
/// the current terminal to the detached run of the live repo (or of
/// `--session NAME`) — a host-side verb like `status`, no sandbox is
/// started. `rest_tail` is what follows the verb; `session` and
/// `dry_run` are the BEFORE-verb global flags. The after-verb
/// spellings `--session NAME` and `--dry-run` parse HERE, the same
/// one-position-rule merge `run_command` gives `run` (D10: both
/// spellings are the same attach, and a `--session` given in both
/// positions is the repeated-flag refusal `run` gives too) — the
/// hinted command of the one-per-workspace refusals (B2's
/// `mysbx attach --session NAME`) must copy-and-paste. Everything
/// else in the tail is a usage error naming the bare shape (B2).
///
/// No lock is taken for the probe or the exec — attaching reads the
/// registry and connects to the socket, it replaces nothing (B6).
/// The lock is taken ONLY to remove a finished or lost record, and
/// under it the record is re-read and the socket re-probed: a
/// concurrent `--detach` may have replaced it while this waited.
///
/// Liveness is the socket, never the state field (B5): a live
/// socket is attachable whatever the record says — the probe is
/// the first classification, so a `starting` record whose master
/// is up attaches too (the master owns the terminal; the inner may
/// still be finishing its startup, attaching just relays).
///
/// The tty check is LAST, only on the Live path: a finished or lost
/// record is printed and removed without a terminal, and `--dry-run`
/// never needs one. A missing tty refuses with `70` and leaves the
/// record untouched.
pub fn attach(rest_tail: &[String], session: Option<&str>, dry_run: bool) -> i32 {
    // A closed surface like the handoff verbs (B2): only `--session`
    // and `--dry-run` parse after the verb — the same merge
    // `run_command` gives `run` (a `--session` in both positions is
    // the repeated-flag refusal, `--dry-run` is idempotent) — any
    // other tail argument is a usage error naming the bare shape.
    let mut session = session.map(str::to_owned);
    let mut dry_run = dry_run;
    let mut idx = 0;
    while let Some(arg) = rest_tail.get(idx) {
        match arg.as_str() {
            "--dry-run" => {
                dry_run = true;
                idx += 1;
            }
            "--session" => {
                if session.is_some() {
                    eprintln!("mysbx attach: repeated flag: --session");
                    eprintln!("{USAGE_ATTACH}");
                    return 2;
                }
                let value = match rest_tail.get(idx + 1) {
                    Some(v) => v.clone(),
                    None => {
                        eprintln!("mysbx attach: --session requires a name");
                        eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                        eprintln!("{USAGE_ATTACH}");
                        return 2;
                    }
                };
                if !crate::session::valid_name(&value) {
                    eprintln!("mysbx attach: --session: invalid session name `{value}`");
                    eprintln!("  the grammar is [A-Za-z0-9][A-Za-z0-9._-]{{0,63}} — no slashes, no leading dot");
                    eprintln!("{USAGE_ATTACH}");
                    return 2;
                }
                session = Some(value);
                idx += 2;
            }
            other => {
                eprintln!("mysbx attach: unexpected argument: {other}");
                eprintln!("{USAGE_ATTACH}");
                return 2;
            }
        }
    }
    // The repo comes from the cwd, never a flag (cli.md D1).
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let root = match runtime_root_from_env() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let paths = match paths(&root, &repo.root, &workspace_of(session.as_deref())) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    // `attach --dry-run` (B9): the dtach attach argv, one argument
    // per line — audited like every dry run, so it refuses an
    // unresolvable repo, an unusable runtime root and an unusable
    // dtach (all `70`, D8), but touches the registry NOT AT ALL: no
    // read, no removal, and no tty needed. An invalid session NAME
    // is refused by the parser with `2` before this runs, so it is
    // not an audit case here.
    if dry_run {
        let dtach = match resolve_dtach_or_refuse() {
            Ok(p) => p,
            Err(code) => return code,
        };
        for arg in attach_argv(&dtach.to_string_lossy(), &paths.sock) {
            println!("{arg}");
        }
        return 0;
    }
    // Nothing registered (B3: the directory IS the registry, a file
    // exists ⇒ a run exists): no record, no run — the refusal names
    // the command that starts one.
    if !paths.json.exists() {
        eprintln!("mysbx: nothing to attach — no detached run of this workspace is registered");
        eprintln!("  start one with: {}", start_hint(session.as_deref()));
        return crate::EXIT_INFRASTRUCTURE;
    }
    let record = match read(&paths) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    // B3: a hash collision must never let one repo attach to
    // another's run — the recorded root must be the resolved one.
    if let Err(e) = ensure_same_repo(&record, &repo.root) {
        eprintln!("mysbx: {e}");
        return crate::EXIT_INFRASTRUCTURE;
    }
    attach_classified(&paths, &record, probe(&paths.sock), &repo.root)
}

/// Act on one classification of the record: the Live path execs
/// dtach (no lock — attaching replaces nothing), the Finished and
/// Lost paths print the outcome, remove the record under the lock
/// (re-reading and re-probing under it, B6) and exit `70` (B5).
/// `repo_root` is the resolved repo every record — the first read
/// AND the under-lock re-read — is refused against unless it names
/// the same root (B3: a hash collision must never let one repo
/// attach to another's run, not even a record that replaced the
/// original one while this waited for the lock).
fn attach_classified(paths: &Paths, record: &Record, p: Probe, repo_root: &Path) -> i32 {
    match classify(record, p) {
        Status::Live => {
            // The tty check is last (B2): the attach exec relays this
            // terminal into the run, so without one there is nothing
            // to attach — an environment refusal, `70`, and the
            // record is NOT removed (the run is still live).
            if !crate::stdin_is_tty() {
                eprintln!(
                    "mysbx: attach needs a terminal on stdin — connect one and run: {}",
                    attach_hint(record.session.as_deref())
                );
                return crate::EXIT_INFRASTRUCTURE;
            }
            let dtach = match resolve_dtach_or_refuse() {
                Ok(p) => p,
                Err(code) => return code,
            };
            attach_exec(&dtach, &paths.sock)
        }
        Status::Finished | Status::Lost => {
            // The removal runs under the workspace lock (B6), and
            // under it the record is RE-READ and the socket
            // RE-PROBED: a concurrent `--detach` may have replaced
            // the finished record with a fresh starting one while
            // this waited for the lock — a live re-classification
            // takes the Live path instead, a gone record is the
            // nothing path, and only a still-finished record is
            // removed.
            let guard = match lock(paths) {
                Ok(g) => g,
                Err(e) => {
                    eprintln!("mysbx: {e}");
                    return crate::EXIT_INFRASTRUCTURE;
                }
            };
            match read(paths) {
                Ok(r) => {
                    // B3: the re-read record is refused against the
                    // resolved repo like the first one — a concurrent
                    // `--detach` of a colliding repo must not turn
                    // this into an attach to (or a removal of)
                    // another repo's record.
                    if let Err(e) = ensure_same_repo(&r, repo_root) {
                        eprintln!("mysbx: {e}");
                        return crate::EXIT_INFRASTRUCTURE;
                    }
                    match classify(&r, probe(&paths.sock)) {
                        Status::Live => {
                            drop(guard);
                            attach_classified(paths, &r, probe(&paths.sock), repo_root)
                        }
                        Status::Finished => {
                            print_finished(&r);
                            remove_record(paths);
                            crate::EXIT_INFRASTRUCTURE
                        }
                        Status::Lost => {
                            eprintln!(
                                "mysbx: the detached run of this workspace is lost — its dtach master is gone"
                            );
                            remove_record(paths);
                            crate::EXIT_INFRASTRUCTURE
                        }
                    }
                }
                // The record vanished while this waited for the lock:
                // the nothing path.
                Err(e) if e.is_not_found() => {
                    eprintln!(
                        "mysbx: nothing to attach — no detached run of this workspace is registered"
                    );
                    eprintln!(
                        "  start one with: {}",
                        start_hint(record.session.as_deref())
                    );
                    crate::EXIT_INFRASTRUCTURE
                }
                Err(e) => {
                    eprintln!("mysbx: {e}");
                    crate::EXIT_INFRASTRUCTURE
                }
            }
        }
    }
}

/// The one-line handshake of an already-exited run: the backend's
/// exit code (B5: every exit is `exited`, the code in `exitCode`,
/// nonzero or not) plus where the outcome is read again — the record
/// is left in place (B5's lifecycle: a finished record stays until
/// `attach` removes it), so a silent `0` would leave the operator
/// without a follow-up.
fn exited_handshake_line(r: &Record) -> String {
    format!(
        "mysbx: the detached run already exited (exit code {}) — \
         its outcome is in the registry record, `mysbx attach` prints it",
        r.exit_code.unwrap_or(0)
    )
}

/// The one-line outcome of a finished record (detach.md B5's
/// lifecycle: `attach` prints it once and removes the record).
fn print_finished(r: &Record) {
    match r.state {
        State::Exited => eprintln!(
            "mysbx: the detached run exited with {} at {}",
            r.exit_code.unwrap_or(0),
            crate::result::iso8601(r.finished_at.unwrap_or(r.started_at))
        ),
        State::Failed => match r.error.as_deref() {
            Some(error) if !error.is_empty() => {
                eprintln!("mysbx: the detached run failed during startup: {error}")
            }
            _ => eprintln!("mysbx: the detached run failed during startup"),
        },
        State::Cancelled => eprintln!("mysbx: the detached run was cancelled"),
        // The caller classified the record first; a non-terminal
        // state without a live socket is `lost`, never finished.
        State::Starting | State::Running => unreachable!("classify puts the probe first"),
    }
}

/// Remove the finished record, tolerating a removal raced by the
/// next `--detach` (B6: it replaces the file under the same lock,
/// but a record gone between the re-read and the unlink is the
/// nothing path's outcome, not an error).
fn remove_record(paths: &Paths) {
    if let Err(e) = std::fs::remove_file(&paths.json) {
        if e.kind() != std::io::ErrorKind::NotFound {
            eprintln!("mysbx: {e}");
        }
    }
}

/// `exec dtach -a <sock> -r winch -z` (detach.md B2): the tool
/// replaces this process, so it owns the terminal and its exit code
/// propagates unchanged (cli.md D8, the `hunk` precedent of
/// worktree.md W4). The argv is [`attach_argv`] — one source of
/// truth for the dry-run audit and the exec. A dtach that cannot be
/// exec'd is the plain runtime failure of the exec.
fn attach_exec(dtach: &Path, sock: &Path) -> i32 {
    use std::os::unix::process::CommandExt;
    let dtach = dtach.to_string_lossy().into_owned();
    let mut cmd = std::process::Command::new(&dtach);
    // The executable is argv[0] of [`attach_argv`], everything after
    // it is the argument tail.
    let e = cmd
        .args(attach_argv(&dtach, sock).into_iter().skip(1))
        .exec();
    eprintln!("mysbx: cannot exec {}: {e}", dtach);
    crate::EXIT_INFRASTRUCTURE
}

/// The attach hint of the one-per-workspace refusals and the success
/// report (detach.md B2): `mysbx attach`, or `mysbx attach --session
/// NAME` for a session workspace.
fn attach_hint(session: Option<&str>) -> String {
    match session {
        Some(name) => format!("mysbx attach --session {name}"),
        None => "mysbx attach".to_owned(),
    }
}

/// The `--detach` spelling of the nothing-registered hint (B2): the
/// flag that starts a detached run, with the `--session` name when a
/// workspace was asked for.
fn start_hint(session: Option<&str>) -> String {
    match session {
        Some(name) => format!("mysbx --detach --session {name}"),
        None => "mysbx --detach".to_owned(),
    }
}

/// The outer mysbx's half of `--detach` (detach.md B4): take the
/// workspace lock, remove a stale socket under it, write the
/// `starting` record, spawn `dtach -n <sock> -r winch <self> <args>`
/// with `MYSBX_DETACHED=<sock>` and wait until the state file leaves
/// `starting` or the socket (or the dtach child) is gone — no
/// timeout, because a first `--session` run may clone a large repo
/// (B4). The lock guard stays alive until this function returns, so
/// the registry is serialized from the liveness probe until the
/// handshake ends (B6).
///
/// `attach_after` is the `--detachable` composition (B2): the Running
/// handshake execs `dtach -a` instead of printing the attach hint,
/// so the whole `--detach && attach` runs under ONE lock (B6) — a
/// concurrent `--detach` cannot take the workspace in between. The
/// exec releases the guard's flock atomically (Rust `File`s are
/// `O_CLOEXEC`). Every non-Running outcome is identical to
/// `--detach`, exit codes included.
pub fn launch_outer(l: &Launch, attach_after: bool) -> i32 {
    let workspace = workspace_of(l.session);
    // The registry location itself is a refusal, not a fallback
    // question (B3: no `/tmp`).
    let root = match runtime_root_from_env() {
        Ok(r) => r,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let paths = match paths(&root, l.repo_root, &workspace) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    if let Err(e) = ensure(&paths) {
        eprintln!("mysbx: {e}");
        return crate::EXIT_INFRASTRUCTURE;
    }
    // One detached run per workspace (B3/B6): the lock serializes the
    // probe-and-replace below against a concurrent `--detach`.
    let _lock_guard = match try_lock(&paths) {
        Ok(guard) => guard,
        Err(Error::LockContended { .. }) => {
            eprintln!(
                "mysbx: another detached run of this workspace is being started — attach with: `{}`",
                attach_hint(l.session)
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    // Liveness is the socket, never the state file (B5): a live run
    // is refused, a stale corpse is removed under the lock (B6).
    match probe(&paths.sock) {
        Probe::Live => {
            eprintln!(
                "mysbx: a detached run of this workspace is already live — attach with: `{}`",
                attach_hint(l.session)
            );
            return crate::EXIT_INFRASTRUCTURE;
        }
        Probe::Stale => {
            if let Err(e) = remove_stale(&paths) {
                eprintln!("mysbx: {e}");
                return crate::EXIT_INFRASTRUCTURE;
            }
        }
        Probe::Absent => {}
    }
    let started_at = crate::result::epoch_secs(std::time::SystemTime::now());
    let record = Record {
        state: State::Starting,
        repo: l.repo_root.to_string_lossy().into_owned(),
        sidecar: l.sidecar.to_string_lossy().into_owned(),
        session: l.session.map(str::to_owned),
        backend: l.backend.to_owned(),
        multiplexer: l.multiplexer.to_owned(),
        started_at,
        finished_at: None,
        exit_code: None,
        error: None,
    };
    if let Err(e) = write(&paths, &record) {
        eprintln!("mysbx: {e}");
        return crate::EXIT_INFRASTRUCTURE;
    }
    // dtach is pinned like every other tool (B10): a missing or
    // non-executable pin disables `--detach` alone. This failure is
    // BEFORE the spawn, so the just-written `starting` record is
    // removed again — nothing is registered when no child exists to
    // write a state after it.
    let dtach = match resolve_dtach_or_refuse() {
        Ok(p) => p,
        Err(code) => {
            let _ = std::fs::remove_file(&paths.json);
            return code;
        }
    };
    let self_exe = match std::env::current_exe() {
        Ok(p) => p.to_string_lossy().into_owned(),
        Err(e) => {
            let _ = std::fs::remove_file(&paths.json);
            eprintln!("mysbx: cannot locate my own executable: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    // The launch (B4 step 2): the outer's own cwd and environment,
    // `MYSBX_DETACHED` marks the child, stdio pointed at
    // `/dev/null` — the outer must not read stdin (B2), its
    // diagnostics already went to its own stderr.
    let mut cmd = std::process::Command::new(&dtach);
    cmd.arg("-n")
        .arg(&paths.sock)
        .arg("-r")
        .arg("winch")
        .arg(&self_exe)
        .args(detach_inner_args())
        .env("MYSBX_DETACHED", &paths.sock)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null());
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            let _ = std::fs::remove_file(&paths.json);
            eprintln!("mysbx: cannot start {}: {e}", dtach.display());
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    // The handshake wait (B4 step 4): poll the state file until it
    // leaves `starting`, or until the run died during startup — the
    // socket was live once and went away, or the dtach child exited
    // and no live socket remains. No timeout: a first `--session`
    // run may clone a large repo, and a dead inner removes the
    // socket.
    let mut socket_seen = false;
    loop {
        std::thread::sleep(std::time::Duration::from_millis(50));
        match read(&paths) {
            Ok(r) if r.state != State::Starting => {
                break report_handshake(&r, l.session, attach_after, &paths, &dtach);
            }
            _ => {}
        }
        let probe_now = probe(&paths.sock);
        socket_seen |= probe_now == Probe::Live;
        let child_gone = matches!(child.try_wait(), Ok(Some(_)));
        // The run died during startup — the socket was live once and
        // went away, or the dtach child exited with no live socket —
        // UNLESS the record it managed to write names the outcome: a
        // refusal the inner diagnosed, a backend that exited before
        // the first poll. Re-read once more before declaring the
        // death, so the diagnosis is reported, never swallowed by a
        // poll landing in the gap between the record write and the
        // child's exit.
        if (socket_seen || child_gone) && probe_now != Probe::Live {
            match read(&paths) {
                Ok(r) if r.state != State::Starting => {
                    break report_handshake(&r, l.session, attach_after, &paths, &dtach);
                }
                _ => {
                    eprintln!("mysbx: the detached run died during startup");
                    return crate::EXIT_INFRASTRUCTURE;
                }
            }
        }
    }
}

/// Report the handshake outcome (detach.md B4 step 4) from the
/// terminal-state record the inner wrote. In the `--detachable`
/// composition (B2) the Running case execs `dtach -a` instead of
/// printing the attach hint — the lock guard `launch_outer` still
/// holds stays alive through the exec (Rust `File`s are
/// `O_CLOEXEC`, the exec closes it), so the whole `--detach &&
/// attach` ran under ONE lock (B6) and no concurrent `--detach`
/// could take the workspace in between.
fn report_handshake(
    r: &Record,
    session: Option<&str>,
    attach_after: bool,
    paths: &Paths,
    dtach: &Path,
) -> i32 {
    match r.state {
        State::Running => {
            if attach_after {
                return attach_exec(dtach, &paths.sock);
            }
            eprintln!("mysbx: detached; attach with: {}", attach_hint(session));
            0
        }
        State::Exited => {
            eprintln!("{}", exited_handshake_line(r));
            0
        }
        State::Failed => {
            match r.error.as_deref() {
                Some(error) if !error.is_empty() => {
                    eprintln!("mysbx: the detached run failed during startup: {error}")
                }
                _ => eprintln!("mysbx: the detached run failed during startup"),
            }
            crate::EXIT_INFRASTRUCTURE
        }
        State::Cancelled => {
            eprintln!("mysbx: the detached run was cancelled during startup");
            crate::EXIT_INFRASTRUCTURE
        }
        State::Starting => unreachable!("the caller breaks out of starting first"),
    }
}

// ---- the inner mysbx (detach.md B4 step 3) ----------------------------------

// The signal numbers (the usual Linux ones — the crate targets
// NixOS/Linux only; each is a raw <signal.h> constant like the
// `gui` detach's own set in lib.rs).
const SIGINT: i32 = 2;
const SIGQUIT: i32 = 3;
const SIGTSTP: i32 = 20;
const SIGHUP: i32 = 1;
const SIGTERM: i32 = 15;
const SIG_DFL: usize = 0;
const SIG_IGN: usize = 1;

/// The number of the terminating signal (`SIGHUP`/`SIGTERM`) that
/// reached the inner mysbx, `0` for none — the same flag-and-atomic
/// shape `CANCELLED` in lib.rs gives the `--result` wait loop.
static TERMINATED: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

/// Whether [`inner_spawn_wait`] already wrote the terminal state of
/// the run — the distinction [`drive_inner`] needs: a pipeline
/// refusal happens with no state written by the backend waiter, and
/// `drive_inner` records it from the stderr capture instead (B5's
/// `error` field is the `mysbx: ` message of a failed startup).
static INNER_FINALIZED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// The one handler of the terminating signals: record the number,
/// return. Async-signal-safe like lib.rs's `on_signal`.
unsafe extern "C" fn on_terminate(signum: i32) {
    TERMINATED.store(signum, std::sync::atomic::Ordering::SeqCst);
}

/// `signal(2)` via the raw libc symbol — the zero-dependency idiom of
/// lib.rs's `libc_signal_catch`/`libc_signal_ignore`, restated here
/// because the handlers live in this module.
unsafe fn libc_signal(signum: i32, handler: usize) {
    #[allow(non_snake_case)]
    extern "C" {
        fn signal(signum: i32, handler: usize) -> usize;
    }
    signal(signum, handler);
}

/// Ignore the pty's signals (detach.md B4): `SIGINT`/`SIGQUIT`/`SIGTSTP`
/// come from the dtach pty and belong to the PAYLOAD, which is in the
/// same foreground process group — the inner must not die over a
/// Ctrl-C meant for the shell it supervises.
unsafe fn ignore_pty_signals() {
    libc_signal(SIGINT, SIG_IGN);
    libc_signal(SIGQUIT, SIG_IGN);
    libc_signal(SIGTSTP, SIG_IGN);
}

/// Catch `SIGHUP`/`SIGTERM` with [`on_terminate`]: the dtach master
/// going away (`SIGHUP`) or an operator's end-the-run (`SIGTERM`)
/// ends the run as `cancelled` (B4).
unsafe fn catch_terminate_signals() {
    libc_signal(SIGHUP, on_terminate as unsafe extern "C" fn(i32) as usize);
    libc_signal(SIGTERM, on_terminate as unsafe extern "C" fn(i32) as usize);
}

/// Restore the default action for every signal this module installs —
/// a leftover handler or ignore would eat the NEXT command's Ctrl-C,
/// the same reason lib.rs's `libc_signal_restore` exists.
unsafe fn restore_signals() {
    libc_signal(SIGINT, SIG_DFL);
    libc_signal(SIGQUIT, SIG_DFL);
    libc_signal(SIGTSTP, SIG_DFL);
    libc_signal(SIGHUP, SIG_DFL);
    libc_signal(SIGTERM, SIG_DFL);
}

/// Reset B4's five signals to `SIG_DFL` in the forked child before
/// exec: dispositions cross the exec boundary, so the inner's
/// `SIG_IGN`/handler setup must not reach the backend (and the
/// payload behind it), or Ctrl-C on the pty would be eaten.
///
/// # Safety
/// Runs in the child between fork and exec (pre_exec): only
/// async-signal-safe calls are made (the raw `signal(2)`).
unsafe fn reset_signals_for_child() -> std::io::Result<()> {
    restore_signals();
    Ok(())
}

/// `kill(2)`: one raw call — a positive pid signals one process, a
/// negative one the whole process group (the wait loop below uses
/// the group form).
unsafe fn libc_kill_pid(pid: i32, signum: i32) {
    extern "C" {
        fn kill(pid: i32, signum: i32) -> i32;
    }
    kill(pid, signum);
}

/// `getpgrp(2)` via the raw libc symbol — the zero-dependency idiom
/// lib.rs's helpers use, restated here because the wait loop lives in
/// this module.
unsafe fn libc_getpgrp() -> i32 {
    extern "C" {
        fn getpgrp() -> i32;
    }
    getpgrp()
}

// ---- the stderr capture tee (detach.md B5) ----------------------------------

/// How much of the inner's stderr the capture keeps: the `error` field
/// is a diagnosis, not a log (the non-goal: no output log of a
/// detached run).
const TEE_CAP: usize = 8 * 1024;

/// The installed tee: the saved real stderr fd (still fd 2's original
/// target, held open for the reader thread's echo) and the channel
/// the reader reports the captured text through on EOF.
struct Tee {
    saved_fd: i32,
    captured: std::sync::mpsc::Receiver<String>,
}

/// The installed tee of the inner mysbx, if any. `Mutex` so a
/// `tee_restore` racing the pipeline's only thread cannot double-
/// restore (it is single-threaded in practice; the lock makes that a
/// guarantee instead of an assumption).
static TEE: std::sync::Mutex<Option<Tee>> = std::sync::Mutex::new(None);

// `pipe(2)` — the tee tests' sink pipes (the tee itself uses
// `pipe2` with `O_CLOEXEC` below).
#[cfg(test)]
unsafe fn libc_pipe(fds: &mut [i32; 2]) -> i32 {
    extern "C" {
        fn pipe(fds: *mut i32) -> i32;
    }
    pipe(fds.as_mut_ptr())
}

// `pipe2(2)` — the tee's pipe is born close-on-exec (detach.md B4:
// the backend's exec must not inherit the capture descriptors).
unsafe fn libc_pipe2(fds: &mut [i32; 2], flags: i32) -> i32 {
    extern "C" {
        fn pipe2(fds: *mut i32, flags: i32) -> i32;
    }
    pipe2(fds.as_mut_ptr(), flags)
}

unsafe fn libc_dup(fd: i32) -> i32 {
    extern "C" {
        fn dup(fd: i32) -> i32;
    }
    dup(fd)
}

unsafe fn libc_dup2(from: i32, to: i32) -> i32 {
    extern "C" {
        fn dup2(oldfd: i32, newfd: i32) -> i32;
    }
    dup2(from, to)
}

unsafe fn libc_close(fd: i32) -> i32 {
    extern "C" {
        fn close(fd: i32) -> i32;
    }
    close(fd)
}

// `fcntl(2)`'s FD_CLOEXEC probe: nonzero when the fd is
// close-on-exec (the tee tests assert the flag).
#[cfg(test)]
unsafe fn libc_fcntl_cloexec_probe(fd: i32) -> i32 {
    extern "C" {
        fn fcntl(fd: i32, cmd: i32, ...) -> i32;
    }
    const F_GETFD: i32 = 1;
    fcntl(fd, F_GETFD) & 1
}

// `fcntl(2)`'s `F_SETFD` with `FD_CLOEXEC`: set close-on-exec.
unsafe fn libc_fcntl_setfd_cloexec(fd: i32) {
    extern "C" {
        fn fcntl(fd: i32, cmd: i32, ...) -> i32;
    }
    const F_SETFD: i32 = 2;
    let _ = fcntl(fd, F_SETFD, 1);
}

// `fcntl(2)`'s int-valued query, test-only (the sink's capacity).
#[cfg(test)]
const F_GETPIPE_SZ: i32 = 1032;

#[cfg(test)]
unsafe fn libc_fcntl_int(fd: i32, cmd: i32) -> i32 {
    extern "C" {
        fn fcntl(fd: i32, cmd: i32, ...) -> i32;
    }
    fcntl(fd, cmd)
}

// `read(2)` for the test's drain; the same extern shape as
// `libc_write` below.
#[cfg(test)]
unsafe fn libc_read(fd: i32, buf: &mut [u8]) -> std::io::Result<usize> {
    extern "C" {
        fn read(fd: i32, buf: *mut std::ffi::c_void, count: usize) -> isize;
    }
    let n = read(fd, buf.as_mut_ptr().cast(), buf.len());
    if n < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(n as usize)
    }
}

/// Capture the inner mysbx's own stderr (detach.md B5): B5's `error`
/// field records the `mysbx: ` message of a failed startup, and the
/// capture makes EVERY pipeline refusal land there without touching
/// each refusal site — the refusals keep printing to stderr, the tee
/// just keeps a copy. Everything after [`tee_restore`] (the backend
/// spawn, the payload) writes to the real stderr again, so no
/// payload output is kept.
fn tee_install() {
    let mut fds = [0i32; 2];
    // O_CLOEXEC: the pipe is the capture's plumbing — the backend's
    // exec must not carry it (the `libc_dup2` below always clears
    // the flag on fd 2's number again, that is what fd 2 is for).
    const O_CLOEXEC: i32 = 0o2000000; // Linux's octal flag
    if unsafe { libc_pipe2(&mut fds, O_CLOEXEC) } != 0 {
        return; // no capture, not no run — the tee is best-effort
    }
    let saved_fd = unsafe { libc_dup(2) };
    if saved_fd < 0 {
        unsafe {
            libc_close(fds[0]);
            libc_close(fds[1]);
        }
        return;
    }
    // The saved fd is close-on-exec too: it only serves the reader
    // thread's echo and this process's `dup2` restores — a spawned
    // backend (which may outlive the tee's use of fd 2) must not
    // inherit it. `tee_restore`'s `dup2` onto 2 clears the flag on
    // the destination number, which is the inheritable copy.
    unsafe { libc_fcntl_setfd_cloexec(saved_fd) };
    unsafe { libc_dup2(fds[1], 2) };
    unsafe { libc_close(fds[1]) };
    let (tx, rx) = std::sync::mpsc::channel::<String>();
    std::thread::spawn(move || {
        use std::io::Read;
        use std::os::fd::FromRawFd;
        let mut file = unsafe { std::fs::File::from_raw_fd(fds[0]) };
        let mut captured = String::new();
        let mut buf = [0u8; 512];
        loop {
            match file.read(&mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => {
                    // Echo to the saved fd first: the pty's own copy
                    // must survive, the capture is the ADDITIONAL
                    // copy for the record.
                    let mut written = 0;
                    while written < n {
                        match unsafe { libc_write(saved_fd, &buf[written..n]) } {
                            Ok(w) if w > 0 => written += w,
                            _ => break,
                        }
                    }
                    if captured.len() < TEE_CAP {
                        let rest = &buf[..n.min(TEE_CAP - captured.len())];
                        captured.push_str(&String::from_utf8_lossy(rest));
                    }
                }
            }
        }
        let _ = unsafe { libc_close(saved_fd) };
        let _ = tx.send(captured);
    });
    let mut slot = TEE.lock().unwrap();
    *slot = Some(Tee {
        saved_fd,
        captured: rx,
    });
}

/// End the capture and put the real stderr back on fd 2 (idempotent).
/// Called by `sandbox` before the first child spawn of the inner, so
/// the backend and the waypipe client inherit the real stderr, and
/// their output is not kept (the non-goal stands: no output log).
/// The saved fd is NOT closed here: the reader thread still echoes
/// to it until it sees the EOF — it is the sole closer (the reader
/// above), so the number is never closed twice and never reused
/// under a live writer.
pub(crate) fn tee_restore() {
    let mut slot = TEE.lock().unwrap();
    if let Some(tee) = slot.take() {
        unsafe {
            libc_dup2(tee.saved_fd, 2);
        }
    }
}

/// Restore the stderr (if still captured), wait for the reader to see
/// EOF and return the captured text — the fallback `error` of a
/// startup that failed before [`inner_spawn_wait`] could record one.
/// The saved fd is the reader's to close (see [`tee_restore`]): after
/// the `recv` below it is gone — EOF was seen, the echo is done.
fn tee_finish() -> String {
    let slot = {
        let mut slot = TEE.lock().unwrap();
        slot.take()
    };
    match slot {
        Some(tee) => {
            unsafe {
                libc_dup2(tee.saved_fd, 2);
            }
            tee.captured.recv().unwrap_or_default()
        }
        None => String::new(),
    }
}

// `write(2)` with the C signature's `const void *` spelled as the
// std library's own externs do, so the linker and the checker agree.
unsafe fn libc_write(fd: i32, buf: &[u8]) -> std::io::Result<usize> {
    extern "C" {
        fn write(fd: i32, buf: *const std::ffi::c_void, count: usize) -> isize;
    }
    let n = write(fd, buf.as_ptr().cast(), buf.len());
    if n < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(n as usize)
    }
}

// ---- the inner run (detach.md B4 step 3, B5) ---------------------------------

/// The registry paths of the inner's workspace, recomputed exactly
/// like [`launch_outer`] computed them — the marker check (B4 step 3)
/// is that the two processes agree on the socket.
fn inner_paths(repo_root: &Path, session: Option<&str>) -> Result<Paths, Error> {
    let root = runtime_root_from_env()?;
    paths(&root, repo_root, &workspace_of(session))
}

/// The inner mysbx of a detached run (detach.md B4 step 3): the bare
/// form under `MYSBX_DETACHED`. Verifies that the marker names THIS
/// workspace's socket (so it cannot redirect state files), installs
/// the stderr capture, runs the pipeline for real — the refusals end
/// up in the capture — and records the outcome of anything
/// [`inner_spawn_wait`] did not already record.
pub(crate) fn drive_inner(flags: crate::Flags) -> i32 {
    let repo = match crate::repo::resolve_cwd() {
        Ok(r) => r,
        Err(e) => {
            // No repo: nothing to record into — the registry entry is
            // keyed by the repo, and the outer's starting record
            // reports the death through the handshake.
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let paths = match inner_paths(&repo.root, flags.session.as_deref()) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    if let Err(e) = ensure(&paths) {
        eprintln!("mysbx: {e}");
        return crate::EXIT_INFRASTRUCTURE;
    }
    // The workspace of the run, captured before `sandbox` takes the
    // flags by value — the fallback records below still need it.
    let session = flags.session.clone();
    // The backend/multiplexer of a record the MERGE never produced
    // (both failure paths below are before the pipeline ran): the
    // flags' own values when set, `unknown` otherwise — B5's fields
    // say "the merged values the run uses", and nothing was merged.
    let unmerged_backend = flags
        .backend
        .clone()
        .unwrap_or_else(|| "unknown".to_owned());
    let unmerged_multiplexer = flags
        .multiplexer
        .map(crate::config::Multiplexer::name)
        .unwrap_or("unknown")
        .to_owned();
    // The marker check (B4 step 3): `MYSBX_DETACHED` must name the
    // socket THIS workspace computes, so the marker cannot redirect
    // state files onto another run's registry entry.
    let found = std::env::var("MYSBX_DETACHED").unwrap_or_default();
    let expected = paths.sock.to_string_lossy().into_owned();
    if found != expected {
        let error = format!(
            "MYSBX_DETACHED ({found}) does not name this workspace's socket ({expected}) \
             — refusing to run (detach.md B4)"
        );
        let record = Record {
            state: State::Failed,
            repo: repo.root.to_string_lossy().into_owned(),
            sidecar: repo.sidecar.to_string_lossy().into_owned(),
            session,
            backend: unmerged_backend.clone(),
            multiplexer: unmerged_multiplexer.clone(),
            started_at: crate::result::epoch_secs(std::time::SystemTime::now()),
            finished_at: Some(crate::result::epoch_secs(std::time::SystemTime::now())),
            exit_code: None,
            error: Some(error.clone()),
        };
        // Best-effort: the eprintln below is the diagnosis, the
        // record is what `attach` reads later.
        let _ = write(&paths, &record);
        eprintln!("mysbx: {error}");
        return crate::EXIT_INFRASTRUCTURE;
    }
    tee_install();
    INNER_FINALIZED.store(false, std::sync::atomic::Ordering::SeqCst);
    let code = crate::sandbox(
        flags,
        crate::bwrap::Payload::Shell,
        crate::RunMode::DetachedInner,
    );
    let captured = tee_finish();
    if !INNER_FINALIZED.load(std::sync::atomic::Ordering::SeqCst) {
        // A startup the backend waiter never reached: a pipeline
        // refusal. Its `mysbx: ` message is the capture — B5's `error`
        // field is exactly that message.
        let error = if captured.trim().is_empty() {
            "the detached run failed during startup".to_owned()
        } else {
            captured.trim().to_owned()
        };
        let record = Record {
            state: State::Failed,
            repo: repo.root.to_string_lossy().into_owned(),
            sidecar: repo.sidecar.to_string_lossy().into_owned(),
            session,
            backend: unmerged_backend,
            multiplexer: unmerged_multiplexer,
            started_at: crate::result::epoch_secs(std::time::SystemTime::now()),
            finished_at: Some(crate::result::epoch_secs(std::time::SystemTime::now())),
            exit_code: None,
            error: Some(error),
        };
        let _ = write(&paths, &record);
    }
    code
}

/// Spawn the backend of the inner run and wait for it, recording the
/// outcome in the registry state file (detach.md B4/B5): `running`
/// after the spawn, `exited`/`failed` with the backend's exit code,
/// `cancelled` when `SIGHUP`/`SIGTERM` ended the run. The backend's
/// argv is the interactive one the pipeline built — the payload
/// always has the dtach pty (B4). Also owns the signal policy of the
/// inner: the pty's signals are ignored, the terminating ones are
/// caught and forwarded.
pub(crate) fn inner_spawn_wait(
    mut cmd: std::process::Command,
    repo: &crate::repo::Repo,
    session: Option<&str>,
    backend: &str,
    multiplexer: &str,
) -> i32 {
    let paths = match inner_paths(&repo.root, session) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let started_at = crate::result::epoch_secs(std::time::SystemTime::now());
    let base = || Record {
        state: State::Running,
        repo: repo.root.to_string_lossy().into_owned(),
        sidecar: repo.sidecar.to_string_lossy().into_owned(),
        session: session.map(str::to_owned),
        backend: backend.to_owned(),
        multiplexer: multiplexer.to_owned(),
        started_at,
        finished_at: None,
        exit_code: None,
        error: None,
    };
    // The record of a terminal state: `finished_at` always, the rest
    // by the outcome.
    let finalize = |mut r: Record| {
        r.finished_at = Some(crate::result::epoch_secs(std::time::SystemTime::now()));
        let res = write(&paths, &r);
        INNER_FINALIZED.store(true, std::sync::atomic::Ordering::SeqCst);
        if let Err(e) = res {
            // Best-effort like result.json: the run happened, its
            // outcome is what it is — the operator learns the state
            // was not recorded.
            eprintln!("mysbx: {e}");
        }
    };
    // The child gets its own default dispositions (see
    // `reset_signals_for_child`): the ignores below stay in THIS
    // process only (B4).
    use std::os::unix::process::CommandExt;
    unsafe { cmd.pre_exec(|| reset_signals_for_child()) };
    unsafe {
        ignore_pty_signals();
        catch_terminate_signals();
    }
    let mut child = match cmd.spawn() {
        Ok(c) => c,
        Err(e) => {
            let mut r = base();
            r.state = State::Failed;
            r.error = Some(format!("cannot exec the backend: {e}"));
            finalize(r);
            eprintln!("mysbx: cannot exec the backend: {e}");
            unsafe { restore_signals() };
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let r = base();
    if let Err(e) = write(&paths, &r) {
        eprintln!("mysbx: {e}");
    }
    // The wait loop: the same 50ms poll `run_with_result` uses. A
    // terminating signal forwards to the backend's whole process
    // group (the inner itself survives — it catches the two, and
    // `--die-with-parent` takes the pid-namespace payload when the
    // bwrap supervisor dies) and waits for its end — the run is
    // `cancelled` (B4), the exit code is the shell's `128 + signum`.
    let mut signum: i32;
    let waited: Option<std::process::ExitStatus>;
    loop {
        signum = TERMINATED.load(std::sync::atomic::Ordering::SeqCst);
        if signum != 0 {
            unsafe { libc_kill_pid(-libc_getpgrp(), signum) };
            // Keep waiting: the record is written once the backend
            // has actually ended, never before.
            match child.try_wait() {
                Ok(Some(status)) => {
                    waited = Some(status);
                    break;
                }
                _ => std::thread::sleep(std::time::Duration::from_millis(50)),
            }
        } else {
            match child.try_wait() {
                Ok(Some(status)) => {
                    waited = Some(status);
                    break;
                }
                Ok(None) => std::thread::sleep(std::time::Duration::from_millis(50)),
                Err(e) => {
                    eprintln!("mysbx: cannot wait for the backend: {e}");
                    let mut r = base();
                    r.state = State::Failed;
                    r.error = Some(format!("cannot wait for the backend: {e}"));
                    finalize(r);
                    unsafe { restore_signals() };
                    return crate::EXIT_INFRASTRUCTURE;
                }
            }
        }
    }
    let status = waited.expect("the loop only exits with a status");
    let exit = if signum != 0 {
        let mut r = base();
        r.state = State::Cancelled;
        finalize(r);
        128 + signum
    } else {
        use std::os::unix::process::ExitStatusExt;
        if let Some(code) = status.code() {
            let mut r = base();
            // B5: `exitCode` is the backend's exit code — every exit
            // is `exited`, nonzero or not; `failed` is a startup
            // diagnosis or a death by signal, neither of which has an
            // exit code.
            r.state = State::Exited;
            // B5's `exitCode` is "the backend's exit code" — a
            // code-0 exit is recorded as `Some(0)`, not left null.
            r.exit_code = Some(code);
            finalize(r);
            code
        } else {
            // A backend killed by its own crash: no exit code exists
            // to record (B4 records the EXIT code, a signal has
            // none), so the state is `failed` with the signal named
            // and the exit code is mysbx's infrastructure `70`.
            let mut r = base();
            r.state = State::Failed;
            r.error = Some(format!(
                "the backend died by signal {}",
                crate::result::signal_name(status.signal().unwrap_or(0))
            ));
            finalize(r);
            crate::EXIT_INFRASTRUCTURE
        }
    };
    unsafe { restore_signals() };
    exit
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    /// The tee tests touch the process's fd 2 and the global TEE:
    /// one at a time.
    static TEE_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

    /// A fresh temporary directory per test; hand-rolled, the crate
    /// has no dependencies. Unique per test name and pid, cleaned up
    /// like the repo.rs helper it mirrors.
    fn tmpdir(name: &str) -> PathBuf {
        // A SHORT prefix: the socket paths these fixtures build are
        // capped by the kernel's sun_path, and the sandbox a `nix
        // build` runs in has a deep $TMPDIR — every character of the
        // prefix is one fewer for the fixture's name.
        let dir = std::env::temp_dir().join(format!("dt-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn sample_record() -> Record {
        Record {
            state: State::Running,
            repo: "/src/repo".to_owned(),
            sidecar: "/src/repo.mysbx".to_owned(),
            session: None,
            backend: "podman".to_owned(),
            multiplexer: "tmux".to_owned(),
            started_at: 1_700_000_000,
            finished_at: None,
            exit_code: None,
            error: None,
        }
    }

    fn mode_of(path: &Path) -> u32 {
        use std::os::unix::fs::MetadataExt;
        std::fs::symlink_metadata(path).unwrap().mode() & 0o777
    }

    // ---- path computation (detach.md B3) -----------------------------

    #[test]
    fn live_workspace_paths_use_the_live_prefix() {
        let root = tmpdir("paths-live");
        let p = paths(&root, Path::new("/src/repo"), &Workspace::Live).unwrap();
        assert_eq!(
            p.sock,
            root.join("mysbx")
                .join(crate::repo::fnv1a10("/src/repo"))
                .join("live.sock")
        );
        assert!(p.json.to_string_lossy().ends_with("live.json"));
        assert!(p.lock.to_string_lossy().ends_with("live.lock"));
    }

    #[test]
    fn a_64_char_session_name_is_accepted() {
        // A synthetic one-byte runtime root, like a real
        // `/run/user/<uid>`: the point is the NAME, not the root's
        // length (the over-limit case has its own test).
        let name = "a".repeat(64);
        let p = paths(
            Path::new("/r"),
            Path::new("/src/repo"),
            &Workspace::Session(name),
        )
        .expect("64 chars is the documented maximum");
        assert!(
            p.sock
                .to_string_lossy()
                .ends_with(&format!("s.{}.sock", "a".repeat(64))),
            "{}",
            p.sock.display()
        );
    }

    #[test]
    fn a_65_char_session_name_is_refused() {
        let root = tmpdir("paths-session-65");
        let name = "a".repeat(65);
        match paths(&root, Path::new("/src/repo"), &Workspace::Session(name)) {
            Err(Error::InvalidSession { .. }) => {}
            other => panic!("expected InvalidSession, got {other:?}"),
        }
    }

    #[test]
    fn an_over_limit_socket_path_is_refused_never_shortened() {
        let mut root = tmpdir("paths-toolong");
        for _ in 0..40 {
            root = root.join("0123456789");
        }
        let name = "a".repeat(64);
        match paths(&root, Path::new("/src/repo"), &Workspace::Session(name)) {
            Err(Error::SocketPathTooLong { path, len }) => {
                assert!(len > SUN_PATH_LIMIT);
                let msg = Error::SocketPathTooLong { path, len }.to_string();
                assert!(msg.contains("never silently shortened"), "{msg}");
            }
            other => panic!("expected SocketPathTooLong, got {other:?}"),
        }
    }

    #[test]
    fn a_relative_runtime_root_is_refused() {
        let root = PathBuf::from("relative/run");
        match paths(&root, Path::new("/src/repo"), &Workspace::Live) {
            Err(Error::RuntimeDirRelative(d)) => assert_eq!(d, root),
            other => panic!("expected RuntimeDirRelative, got {other:?}"),
        }
    }

    #[test]
    fn an_unset_runtime_dir_is_refused() {
        match runtime_root_from(None) {
            Err(Error::RuntimeDirUnset) => {}
            other => panic!("expected RuntimeDirUnset, got {other:?}"),
        }
    }

    #[test]
    fn an_empty_runtime_dir_is_a_relative_path_and_refused() {
        match runtime_root_from(Some("")) {
            Err(Error::RuntimeDirRelative(d)) => assert_eq!(d, PathBuf::from("")),
            other => panic!("expected RuntimeDirRelative, got {other:?}"),
        }
    }

    #[test]
    fn a_relative_runtime_dir_value_is_refused() {
        match runtime_root_from(Some("relative/run")) {
            Err(Error::RuntimeDirRelative(d)) => assert_eq!(d, PathBuf::from("relative/run")),
            other => panic!("expected RuntimeDirRelative, got {other:?}"),
        }
    }

    #[test]
    fn an_absolute_runtime_dir_value_is_taken_as_the_root() {
        assert_eq!(
            runtime_root_from(Some("/run/user/1000")).unwrap(),
            PathBuf::from("/run/user/1000")
        );
    }

    // ---- permission checks (detach.md B3) ----------------------------

    #[test]
    fn ensure_creates_the_registry_private_to_the_user() {
        let root = tmpdir("ensure-private");
        let p = paths(&root, Path::new("/src/repo"), &Workspace::Live).unwrap();
        ensure(&p).unwrap();
        assert_eq!(mode_of(&p.dir), 0o700, "the per-repo dir is 0700");
        let mysbx = p.dir.parent().unwrap();
        assert_eq!(mode_of(mysbx), 0o700, "mysbx/ is 0700");
    }

    #[test]
    fn a_group_accessible_registry_dir_is_refused() {
        let root = tmpdir("ensure-group-accessible");
        let p = paths(&root, Path::new("/src/repo"), &Workspace::Live).unwrap();
        ensure(&p).unwrap();
        std::fs::set_permissions(&p.dir, std::fs::Permissions::from_mode(0o755)).unwrap();
        match ensure(&p) {
            Err(Error::AccessibleByOthers { path, mode }) => {
                assert_eq!(path, p.dir);
                assert_eq!(mode & 0o077, 0o055);
            }
            other => panic!("expected AccessibleByOthers, got {other:?}"),
        }
    }

    #[test]
    fn the_pure_ownership_decision_rejects_foreign_owners_and_open_bits() {
        let euid = 1000;
        assert!(dir_is_ours(1000, 0o700, euid));
        assert!(!dir_is_ours(1001, 0o700, euid), "a foreign owner");
        assert!(!dir_is_ours(1000, 0o750, euid), "group bits");
        assert!(!dir_is_ours(1000, 0o705, euid), "world bits");
        assert!(!dir_is_ours(1000, 0o1777, euid), "group and world bits");
    }

    fn registry(root: &Path) -> Paths {
        let p = paths(root, Path::new("/src/repo"), &Workspace::Live).unwrap();
        ensure(&p).unwrap();
        p
    }

    fn mount_at(path: &str) -> crate::config::Mount {
        crate::config::Mount {
            path: path.to_owned(),
            dest: None,
            mode: crate::config::Mode::Ro,
            file: false,
        }
    }

    // ---- the mount refusal (detach.md B8) ---------------------------

    #[test]
    fn a_mount_at_the_runtime_root_is_refused() {
        let root = tmpdir("expose-equal");
        let source = root.join("mysbx");
        std::fs::create_dir_all(&source).unwrap();
        match check_mount_exposure(
            Some(root.as_os_str()),
            &[mount_at(source.to_str().unwrap())],
        ) {
            Err(Error::RuntimeRootExposed {
                source,
                runtime_root,
                relation: RuntimeRootRelation::Equal,
            }) => {
                assert_eq!(source, root.join("mysbx"));
                assert_eq!(runtime_root, root.join("mysbx"));
            }
            other => panic!("expected RuntimeRootExposed, got {other:?}"),
        }
        // The message names the runtime root and the decision.
        let msg = Error::RuntimeRootExposed {
            source: root.join("mysbx"),
            runtime_root: root.join("mysbx"),
            relation: RuntimeRootRelation::Equal,
        }
        .to_string();
        assert!(msg.contains("runtime root"), "{msg}");
        assert!(msg.contains("detach.md B8"), "{msg}");
    }

    #[test]
    fn a_mount_below_the_runtime_root_is_refused() {
        let root = tmpdir("expose-below");
        let source = root.join("mysbx").join("repo");
        match check_mount_exposure(
            Some(root.as_os_str()),
            &[mount_at(source.to_str().unwrap())],
        ) {
            Err(Error::RuntimeRootExposed { relation, .. }) => {
                assert_eq!(relation, RuntimeRootRelation::Below);
            }
            other => panic!("expected RuntimeRootExposed, got {other:?}"),
        }
    }

    #[test]
    fn a_mount_above_the_runtime_root_is_refused() {
        for above in ["/run/user/1000", "/run/user", "/run", "/"] {
            match check_mount_exposure(
                Some(std::ffi::OsStr::new("/run/user/1000")),
                &[mount_at(above)],
            ) {
                Err(Error::RuntimeRootExposed { relation, .. }) => {
                    assert_eq!(relation, RuntimeRootRelation::Above, "{above}");
                }
                other => panic!("{above}: expected RuntimeRootExposed, got {other:?}"),
            }
        }
    }

    #[test]
    fn a_mount_of_a_sibling_of_the_runtime_root_is_allowed() {
        let root = tmpdir("expose-sibling");
        let sibling = root.join("wayland-0");
        std::fs::write(&sibling, "").unwrap();
        check_mount_exposure(
            Some(root.as_os_str()),
            &[mount_at(sibling.to_str().unwrap())],
        )
        .expect("a sibling of the runtime root is not the runtime root");
    }

    #[test]
    fn a_mount_without_a_runtime_dir_is_allowed() {
        check_mount_exposure(None, &[mount_at("/run")])
            .expect("no runtime root, nothing to refuse");
    }

    #[test]
    fn a_mount_with_a_relative_runtime_dir_is_allowed() {
        check_mount_exposure(
            Some(std::ffi::OsStr::new("relative/run")),
            &[mount_at("/run")],
        )
        .expect("a relative runtime dir names no runtime root");
    }

    #[test]
    fn a_symlinked_runtime_dir_refuses_the_real_target() {
        let real = tmpdir("expose-symlink-real");
        let link = tmpdir("expose-symlink-link");
        let _ = std::fs::remove_dir_all(&link);
        std::os::unix::fs::symlink(&real, &link).unwrap();
        let source = real.join("mysbx").join("repo");
        match check_mount_exposure(
            Some(link.as_os_str()),
            &[mount_at(source.to_str().unwrap())],
        ) {
            Err(Error::RuntimeRootExposed { relation, .. }) => {
                assert_eq!(relation, RuntimeRootRelation::Below);
            }
            other => panic!("expected RuntimeRootExposed, got {other:?}"),
        }
    }

    #[test]
    fn a_symlinked_mysbx_child_refuses_the_real_target() {
        let real = tmpdir("expose-symlink-mysbx-real");
        let runtime = tmpdir("expose-symlink-mysbx-runtime");
        std::fs::create_dir_all(real.join("repo")).unwrap();
        std::os::unix::fs::symlink(&real, runtime.join("mysbx")).unwrap();
        let source = real.to_str().unwrap();
        match check_mount_exposure(Some(runtime.as_os_str()), &[mount_at(source)]) {
            Err(Error::RuntimeRootExposed { relation, .. }) => {
                assert_eq!(relation, RuntimeRootRelation::Equal);
            }
            other => panic!("expected RuntimeRootExposed, got {other:?}"),
        }
    }

    // ---- state round-trip (detach.md B5) -----------------------------

    #[test]
    fn a_record_round_trips_through_the_state_file() {
        let root = tmpdir("roundtrip");
        let p = registry(&root);
        let r = Record {
            state: State::Failed,
            repo: "/src/repo".to_owned(),
            sidecar: "/src/repo.mysbx".to_owned(),
            session: Some("feature".to_owned()),
            backend: "podman".to_owned(),
            multiplexer: "tmux".to_owned(),
            started_at: 1_700_000_000,
            finished_at: Some(1_700_000_030),
            exit_code: Some(3),
            error: None,
        };
        write(&p, &r).unwrap();
        assert_eq!(read(&p).unwrap(), r);
    }

    #[test]
    fn quotes_and_backslashes_survive_the_round_trip() {
        let root = tmpdir("roundtrip-escapes");
        let p = registry(&root);
        let r = Record {
            state: State::Failed,
            repo: "/src/re\"po\\x".to_owned(),
            sidecar: "/src/repo.mysbx".to_owned(),
            session: None,
            backend: "podman".to_owned(),
            multiplexer: "tmux".to_owned(),
            started_at: 1_700_000_000,
            finished_at: None,
            exit_code: None,
            error: Some("a \"quoted\" \\ diagnosis\nwith a newline".to_owned()),
        };
        write(&p, &r).unwrap();
        assert_eq!(read(&p).unwrap(), r);
    }

    #[test]
    fn the_live_workspace_serializes_session_as_null() {
        let text = render(&sample_record());
        assert!(
            text.contains("\"session\": null"),
            "session is null for the live workspace: {text}"
        );
    }

    #[test]
    fn a_state_file_with_a_wrong_version_is_refused() {
        let root = tmpdir("wrong-version");
        let p = registry(&root);
        let text = render(&sample_record()).replace("\"version\": 1", "\"version\": 2");
        std::fs::write(&p.json, text).unwrap();
        match read(&p) {
            Err(Error::UnsupportedVersion { found }) => assert_eq!(found, 2),
            other => panic!("expected UnsupportedVersion, got {other:?}"),
        }
    }

    #[test]
    fn an_unknown_state_value_is_refused() {
        let root = tmpdir("unknown-state");
        let p = registry(&root);
        let text = render(&sample_record()).replace("\"running\"", "\"zombie\"");
        std::fs::write(&p.json, text).unwrap();
        match read(&p) {
            Err(Error::Unparsable { why }) => assert!(why.contains("zombie")),
            other => panic!("expected Unparsable, got {other:?}"),
        }
    }

    #[test]
    fn a_record_of_another_repo_is_refused() {
        let r = sample_record();
        match ensure_same_repo(&r, Path::new("/other/repo")) {
            Err(Error::RepoMismatch { recorded, resolved }) => {
                assert_eq!(recorded, "/src/repo");
                assert_eq!(resolved, "/other/repo");
            }
            other => panic!("expected RepoMismatch, got {other:?}"),
        }
        ensure_same_repo(&r, Path::new("/src/repo")).unwrap();
    }

    #[test]
    fn iso8601_round_trips_through_the_parser() {
        for secs in [0u64, 1, 86_399, 1_700_000_000, 4_102_444_799] {
            let text = crate::result::iso8601(secs);
            assert_eq!(parse_iso8601(&text).unwrap(), secs, "{text}");
        }
    }

    #[test]
    fn a_malformed_timestamp_is_refused_not_panicked_over() {
        // Each broken timestamp must be refused, never decoded by
        // guesswork — the control bytes land inside the seconds
        // field, so the length and separator checks pass and the
        // digit conversion itself must refuse them.
        let refused = [
            "1970-01-01T00:00:0".to_owned(),       // wrong length
            "1970-01-01 00:00:00Z".to_owned(),     // wrong separator
            "1970-00-01T00:00:00Z".to_owned(),     // month out of range
            "1970-13-01T00:00:00Z".to_owned(),     // month out of range
            "1970-01-32T00:00:00Z".to_owned(),     // day out of range
            "1970-02-30T00:00:00Z".to_owned(),     // Feb 30
            "2023-02-29T00:00:00Z".to_owned(),     // no Feb 29 in 2023
            "1970-01-01T00:00: 0Z".to_owned(),     // a space in the seconds
            "1970-01-01T00:00:\u{0}0Z".to_owned(), // a NUL in the seconds
            "1970-01-01T00:00:#0Z".to_owned(),     // `#` in the seconds
        ];
        for text in refused {
            assert!(parse_iso8601(&text).is_err(), "{text:?} must be refused");
        }
        // A leap day that exists parses: the day check knows the
        // month's real length, both ways.
        assert_eq!(
            parse_iso8601("2024-02-29T00:00:00Z").unwrap(),
            1_709_164_800
        );
    }

    #[test]
    fn a_nul_escaped_into_a_timestamp_is_refused_through_the_state_file() {
        // `crate::result::quote` writes a NUL as a `\u0000` escape, so
        // a corrupt state file can carry one inside `startedAt`;
        // decoded, the digit conversion must refuse it, not
        // underflow on it.
        let root = tmpdir("timestamp-nul");
        let p = registry(&root);
        let text = render(&sample_record()).replace(
            "\"startedAt\": \"2023-11-14T22:13:20Z\"",
            "\"startedAt\": \"2023-11-14T22:13:\\u00000Z\"",
        );
        std::fs::write(&p.json, text).unwrap();
        match read(&p) {
            Err(Error::Unparsable { why }) => {
                assert!(why.contains("ISO-8601"), "{why}")
            }
            other => panic!("expected Unparsable, got {other:?}"),
        }
    }

    #[test]
    fn starting_with_an_absent_socket_classifies_as_lost() {
        // `starting` without a live socket is a killed master (B5):
        // the current classification reports `lost`.
        let mut r = sample_record();
        r.state = State::Starting;
        assert_eq!(classify(&r, Probe::Absent), Status::Lost);
    }

    // ---- liveness classification (detach.md B5) ----------------------

    #[test]
    fn a_listening_socket_is_live() {
        let root = tmpdir("probe-live");
        let sock = root.join("live.sock");
        let _listener = std::os::unix::net::UnixListener::bind(&sock).unwrap();
        assert_eq!(probe(&sock), Probe::Live);
    }

    #[test]
    fn a_bound_then_abandoned_socket_is_stale() {
        let root = tmpdir("probe-stale");
        let sock = root.join("live.sock");
        {
            let _listener = std::os::unix::net::UnixListener::bind(&sock).unwrap();
        }
        assert_eq!(probe(&sock), Probe::Stale);
    }

    #[test]
    fn a_missing_socket_is_absent() {
        let root = tmpdir("probe-absent");
        assert_eq!(probe(&root.join("live.sock")), Probe::Absent);
    }

    #[test]
    fn a_running_record_with_a_dead_socket_is_lost() {
        let r = sample_record();
        assert_eq!(classify(&r, Probe::Stale), Status::Lost);
        assert_eq!(classify(&r, Probe::Absent), Status::Lost);
    }

    #[test]
    fn a_live_record_is_live_whatever_the_state_says() {
        let r = sample_record();
        assert_eq!(classify(&r, Probe::Live), Status::Live);
    }

    #[test]
    fn an_exited_record_is_finished_whatever_the_socket() {
        let mut r = sample_record();
        r.state = State::Exited;
        assert_eq!(classify(&r, Probe::Stale), Status::Finished);
        assert_eq!(classify(&r, Probe::Absent), Status::Finished);
        assert_eq!(classify(&r, Probe::Live), Status::Live, "liveness wins");
    }

    // ---- lock and stale cleanup (detach.md B6) ------------------------

    #[test]
    fn a_second_non_blocking_lock_attempt_fails_without_waiting() {
        let root = tmpdir("lock-contended");
        let p = registry(&root);
        let held = lock(&p).unwrap();
        // A second OPEN file description gets a separate flock, so
        // this is contended within one process too.
        match try_lock(&p) {
            Err(Error::LockContended { path }) => assert_eq!(path, p.lock),
            Ok(_) => panic!("the held workspace lock must refuse try_lock"),
            other => panic!("expected LockContended, got {other:?}"),
        }
        drop(held);
        try_lock(&p).expect("the released lock is free again");
    }

    #[test]
    fn remove_stale_unlinks_a_stale_socket_under_the_lock() {
        let root = tmpdir("remove-stale");
        let p = registry(&root);
        {
            let _listener = std::os::unix::net::UnixListener::bind(&p.sock).unwrap();
        }
        let held = lock(&p).unwrap();
        assert!(remove_stale(&p).unwrap());
        assert!(!p.sock.exists(), "the stale socket is gone");
        assert_eq!(probe(&p.sock), Probe::Absent);
        drop(held);
    }

    #[test]
    fn remove_stale_never_touches_a_live_socket() {
        let root = tmpdir("remove-live");
        let p = registry(&root);
        let listener = std::os::unix::net::UnixListener::bind(&p.sock).unwrap();
        let held = lock(&p).unwrap();
        assert!(!remove_stale(&p).unwrap(), "a live socket is not stale");
        assert_eq!(probe(&p.sock), Probe::Live, "the listener still accepts");
        drop(held);
        drop(listener);
    }

    #[test]
    fn remove_stale_reports_absence_as_no_removal() {
        let root = tmpdir("remove-absent");
        let p = registry(&root);
        let held = lock(&p).unwrap();
        assert!(!remove_stale(&p).unwrap());
        drop(held);
    }

    // ---- the launch argv (detach.md B4 step 2) -----------------------

    #[test]
    fn inner_args_drop_the_three_launch_flags_and_keep_the_rest() {
        let args = [
            "--session".to_owned(),
            "fix-1".to_owned(),
            "--verbose".to_owned(),
            "--detach".to_owned(),
            "--detachable".to_owned(),
            "--dry-run".to_owned(),
            "--ro".to_owned(),
        ];
        assert_eq!(
            inner_args_of(args.into_iter()),
            ["--session", "fix-1", "--dry-run", "--ro"]
        );
        // The filter is positional and drops every occurrence, so a
        // pathological `--session --detach` (a session NAMED
        // `--detach`) leaves the inner a value-less `--session` — a
        // parse refusal there, not a silent misparse; value-awareness
        // is not worth the coupling to `split_global_flags`.
        assert_eq!(
            inner_args_of(
                [
                    "--detach".to_owned(),
                    "--session".to_owned(),
                    "--detach".to_owned()
                ]
                .into_iter()
            ),
            ["--session"]
        );
    }

    #[test]
    fn the_launcher_argv_is_dtach_n_sock_winch_self_then_the_inner_args() {
        let argv = launcher_argv(
            "/nix/store/aaaa-dtach/bin/dtach",
            Path::new("/run/user/1000/mysbx/abc/live.sock"),
            "/nix/store/bbbb-mysbx/bin/mysbx",
            &["--session".to_owned(), "fix-1".to_owned()],
        );
        assert_eq!(
            argv,
            vec![
                "/nix/store/aaaa-dtach/bin/dtach".to_owned(),
                "-n".to_owned(),
                "/run/user/1000/mysbx/abc/live.sock".to_owned(),
                "-r".to_owned(),
                "winch".to_owned(),
                "/nix/store/bbbb-mysbx/bin/mysbx".to_owned(),
                "--session".to_owned(),
                "fix-1".to_owned(),
            ]
        );
    }

    #[test]
    fn the_saved_stderr_fd_is_not_reused_before_the_reader_drains() {
        let _tee_guard = TEE_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        // Invariant: the reader thread is the SOLE closer of the
        // saved fd — while it is blocked mid-echo, the number stays
        // pinned, and files opened after `tee_restore` never land
        // on it. Pinning the reader: fd 2 is a FULL pipe when the
        // tee is installed, so the reader's echo blocks on its
        // first write; `tee_restore` runs while it is blocked, the
        // probes take the low free numbers, and the drain lets the
        // reader finish — a second closer would land on a probe.
        //
        // `libc_write` on fd 2, not `eprintln!`: the test harness
        // captures the macro's output before fd 2, so only a raw
        // write reaches the tee.
        let real_stderr = unsafe { libc_dup(2) };
        assert!(real_stderr >= 0);
        unsafe { libc_fcntl_setfd_cloexec(real_stderr) };
        let mut sink = [0i32; 2];
        assert_eq!(unsafe { libc_pipe(&mut sink) }, 0);
        unsafe {
            libc_fcntl_setfd_cloexec(sink[0]);
            libc_fcntl_setfd_cloexec(sink[1]);
        }
        // Fill the sink to its exact capacity: the next write blocks.
        let cap = unsafe { libc_fcntl_int(sink[1], F_GETPIPE_SZ) };
        assert!(cap > 0);
        let mut filled = 0usize;
        while filled < cap as usize {
            let want = (cap as usize - filled).min(4096);
            let n = unsafe { libc_write(sink[1], &[b'j'; 4096][..want]) }
                .expect("the sink fills without blocking");
            assert!(n > 0);
            filled += n;
        }
        unsafe { libc_dup2(sink[1], 2) }; // fd 2 = the full sink
        tee_install();
        // Vacuity guard: fd 2 is now the tee's pipe, not the sink.
        let held_2 = std::fs::read_link("/proc/self/fd/2").expect("fd 2 resolves");
        let held_sink =
            std::fs::read_link(format!("/proc/self/fd/{}", sink[1])).expect("sink resolves");
        assert_ne!(held_2, held_sink, "the tee holds fd 2 (vacuity guard)");
        let _ = unsafe { libc_write(2, b"mysbx: tee reuse probe\n") };
        // The reader has read the probe text and is blocked inside
        // its echo write to the full sink.
        std::thread::sleep(std::time::Duration::from_millis(100));
        tee_restore(); // the dup2 back; the saved fd stays OPEN
                       // Enough files to cover every low free number, the saved
                       // fd's included — one of them lands exactly on it. Raw fds,
                       // not `File`s: a probe closed behind the test's back must be
                       // an assertion failure, not an I/O-safety abort.
        let dir = tmpdir("tee-reuse-probe");
        let probe_fds: Vec<i32> = (0..64)
            .map(|i| {
                use std::os::fd::IntoRawFd;
                std::fs::File::create(dir.join(format!("probe-{i}")))
                    .expect("a probe opens")
                    .into_raw_fd()
            })
            .collect();
        // The drain: the reader's echo completes, it sees EOF and
        // closes — a second closer would land on a probe fd here.
        let mut drain = [0u8; 4096];
        let _ = unsafe { libc_read(sink[0], &mut drain) };
        let mut victim = None;
        for _ in 0..100 {
            std::thread::sleep(std::time::Duration::from_millis(10));
            victim = probe_fds
                .iter()
                .copied()
                .find(|fd| std::fs::read_link(format!("/proc/self/fd/{fd}")).is_err());
            if victim.is_some() {
                break;
            }
        }
        // The real stderr comes back BEFORE any output of a failure.
        unsafe { libc_dup2(real_stderr, 2) };
        assert!(
            victim.is_none(),
            "a reused descriptor was closed (fd {victim:?}): the \
             saved fd has more than one closer"
        );
        for i in 0..64 {
            let content =
                std::fs::read_to_string(dir.join(format!("probe-{i}"))).expect("probe reads");
            assert!(content.is_empty(), "probe-{i} was written to: {content}");
        }
        for fd in probe_fds {
            unsafe { libc_close(fd) };
        }
        unsafe {
            libc_close(sink[0]);
            libc_close(sink[1]);
            libc_close(real_stderr);
        }
    }

    #[test]
    fn tee_finish_still_returns_the_captured_text() {
        let _tee_guard = TEE_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        tee_install();
        // `libc_write` on fd 2, not `eprintln!` (the harness captures
        // the macro before fd 2 — see the reuse test).
        let _ = unsafe { libc_write(2, b"mysbx: tee finish probe\n") };
        let captured = tee_finish();
        assert_eq!(
            captured.trim(),
            "mysbx: tee finish probe",
            "the capture survives the ownership fix (B5's error field)"
        );
        // fd 2 is the real stderr again and inheritable: the restore's
        // dup2 clears close-on-exec on the destination.
        assert_eq!(unsafe { libc_fcntl_cloexec_probe(2) }, 0);
    }

    #[test]
    fn the_attach_argv_is_dtach_a_sock_winch_z() {
        let argv = attach_argv(
            "/nix/store/aaaa-dtach/bin/dtach",
            Path::new("/run/user/1000/mysbx/abc/live.sock"),
        );
        assert_eq!(
            argv,
            vec![
                "/nix/store/aaaa-dtach/bin/dtach".to_owned(),
                "-a".to_owned(),
                "/run/user/1000/mysbx/abc/live.sock".to_owned(),
                "-r".to_owned(),
                "winch".to_owned(),
                "-z".to_owned(),
            ]
        );
    }

    #[test]
    fn the_tee_pipe_and_the_saved_fd_are_close_on_exec() {
        let _tee_guard = TEE_TEST_LOCK.lock().unwrap_or_else(|e| e.into_inner());
        // The pipe and the dup'd saved fd crossed the exec into the
        // backend: captured startup stderr could ride along in the
        // payload's fd table. Close-on-exec on every descriptor the
        // tee creates keeps the exec boundary clean (detach.md B4's
        // exec, B5's capture) even though the reader thread may still
        // hold the saved fd when the backend spawns.
        let before = open_fds();
        tee_install();
        let after = open_fds();
        let created: Vec<(i32, i32)> = after
            .iter()
            .filter(|fd| !before.contains(fd))
            .map(|fd| (*fd, unsafe { libc_fcntl_cloexec_probe(*fd) }))
            .collect();
        tee_restore();
        // Cleanup before any assertion — a panic here must not
        // poison fd 2 for the sibling tests. The slot is gone (the
        // restore took it), so this returns at once; the reader
        // drains in the background.
        let _ = tee_finish();
        assert!(
            !created.is_empty(),
            "the tee installed without creating descriptors (vacuity)"
        );
        let leaked: Vec<i32> = created
            .iter()
            .filter(|(_, flag)| *flag == 0)
            .map(|(fd, _)| *fd)
            .collect();
        assert!(
            leaked.is_empty(),
            "the tee created non-close-on-exec fds: {leaked:?} \
             (flags: {created:?})"
        );
        assert_eq!(
            unsafe { libc_fcntl_cloexec_probe(2) },
            0,
            "fd 2 is inheritable again after the restore (dup2 clears \
             the flag on its destination)"
        );
    }

    /// The fd numbers the process holds right now.
    fn open_fds() -> Vec<i32> {
        std::fs::read_dir("/proc/self/fd")
            .expect("fd list")
            .map(|e| {
                e.expect("fd entry")
                    .file_name()
                    .to_string_lossy()
                    .parse()
                    .expect("fd numbers")
            })
            .collect()
    }

    #[test]
    fn report_handshake_names_the_exit_code_of_an_exited_run() {
        // B5: the outer's handshake of an already-finished run names
        // the backend's exit code, nonzero or not. The wording is
        // pinned through the pure [`exited_handshake_line`] the
        // handshake prints — the harness's stderr capture stands
        // between an assert and the `eprintln!` on fd 2.
        let mut r = sample_record();
        r.state = State::Exited;
        r.exit_code = Some(42);
        r.finished_at = Some(r.started_at + 30);
        let line = exited_handshake_line(&r);
        assert_eq!(
            line,
            "mysbx: the detached run already exited (exit code 42) — \
             its outcome is in the registry record, `mysbx attach` prints it"
        );
        assert!(
            !line.contains("failed"),
            "a plain exit is not a startup failure: {line}"
        );
        // The handshake's Exited arm reports 0 — the run's own code
        // went out with the record, the launch itself succeeded.
        let root = tmpdir("report-exited");
        let paths = registry(&root);
        assert_eq!(
            report_handshake(
                &r,
                None,
                false,
                &paths,
                Path::new("/nix/store/aaaa-dtach/bin/dtach"),
            ),
            0
        );
    }
}
