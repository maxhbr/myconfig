// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The runtime-root registry of detached runs (docs/design/detach.md
//! B3, B5, B6).
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
//! This module is pure data and filesystem plumbing: no CLI, no
//! process spawning. Every function takes the runtime root as a
//! parameter; only [`runtime_root_from_env`] reads the environment,
//! so the tests never mutate it. Zero dependencies, like the rest of
//! the crate.

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
    /// The backend exited `0`.
    Exited,
    /// The backend exited non-zero, or the startup failed.
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
    /// The filesystem could not be read or written.
    Io { path: PathBuf, why: String },
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
            Error::Io { path, why } => {
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
        }
    }
}

impl std::error::Error for Error {}

/// Read `$XDG_RUNTIME_DIR` (detach.md B3): unset or relative is
/// refused, `/tmp` is never used as a fallback. The only function in
/// this module that touches the environment — everything else takes
/// the root as a parameter, so the tests never mutate it.
pub fn runtime_root_from_env() -> Result<PathBuf, Error> {
    let raw = std::env::var("XDG_RUNTIME_DIR").ok();
    runtime_root_from(raw.as_deref())
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
            why: "the registry directory has no parent".to_owned(),
        })?
        .to_owned();
    use std::fs::DirBuilder;
    use std::os::unix::fs::DirBuilderExt;
    let mut builder = DirBuilder::new();
    builder.recursive(true).mode(0o700);
    builder.create(&mysbx_dir).map_err(|e| Error::Io {
        path: mysbx_dir.clone(),
        why: e.to_string(),
    })?;
    // `mysbx/` is verified before anything is created inside it: a
    // directory that fails verification must not gain contents.
    verify_dir(&mysbx_dir)?;
    builder.create(&paths.dir).map_err(|e| Error::Io {
        path: paths.dir.clone(),
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
            why: e.to_string(),
        }),
    }
}

/// Open the lock file, creating it if absent. The file is only ever
/// a flock carrier — its contents never matter.
fn open_lock_file(paths: &Paths) -> Result<std::fs::File, Error> {
    std::fs::File::create(&paths.lock).map_err(|e| Error::Io {
        path: paths.lock.clone(),
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
                    why: e.to_string(),
                })
            }
        }
    }
    Ok(false)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;

    /// A fresh temporary directory per test; hand-rolled, the crate
    /// has no dependencies. Unique per test name and pid, cleaned up
    /// like the repo.rs helper it mirrors.
    fn tmpdir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-detach-test-{}-{name}", std::process::id()));
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
}
