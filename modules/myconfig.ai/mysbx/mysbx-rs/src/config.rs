// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The `config.toml` schema (docs/design/config.md).
//!
//! Parsing is strict: unknown keys, wrong types and unknown enum values are
//! errors, never warnings. A sandbox built from a half-understood
//! configuration would be a sandbox with unknown confinement, so the tool
//! fails fast instead (docs/design/config.md D8, D9).

use crate::toml::{self, Table, Value};
use std::collections::BTreeMap;
use std::fmt;
use std::path::Path;

/// Access mode of a mount.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Ro,
    Rw,
}

impl Mode {
    fn parse(s: &str, at: &str) -> Result<Mode, Error> {
        match s {
            "ro" => Ok(Mode::Ro),
            "rw" => Ok(Mode::Rw),
            other => Err(Error::Schema(format!(
                "{at}: invalid mode `{other}`, expected `ro` or `rw`"
            ))),
        }
    }
}

impl fmt::Display for Mode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Mode::Ro => "ro",
            Mode::Rw => "rw",
        })
    }
}

/// One additional host path exposed inside the sandbox.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mount {
    /// The host path **as written** in the file: absolute, `~/…` or
    /// relative to the directory of the config file that declared it.
    /// Parsing is string-level only; expansion and canonicalization
    /// happen in `crate::merge` (docs/design/config.md D8), which is the
    /// only place that knows `$HOME` and the file each mount came from.
    pub path: String,
    /// Destination inside the sandbox; `None` means "same path".
    /// Always absolute: it names a path in the sandbox's own filesystem
    /// view, so neither `~/` (no host home in there) nor "relative to the
    /// config file" (a host location) would mean anything
    /// (docs/design/config.md D8).
    pub dest: Option<String>,
    /// Defaults to `ro` (D9: nothing from the host filesystem unless
    /// declared).
    pub mode: Mode,
}

// The repo itself is deliberately not part of the schema: it is the sidecar's
// repo, always mounted rw at its real host path, not expressible in
// configuration (docs/design/config.md D13).

/// A parsed `config.toml`, from either configuration layer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    /// Sandbox technology; `None` means "not decided by this layer"
    /// (docs/design/cli.md D7: never auto-detected).
    pub backend: Option<String>,
    /// Whether the network is shared. `None` means "not decided by
    /// this layer" — like `backend`: a layer that does not mention
    /// `network` must not count as an explicit `true` (which would
    /// make an omitted sidecar value re-enable what the user config
    /// denied, docs/design/config.md D7). The shared-by-default `true`
    /// of docs/plan.md is applied AFTER the merge (see
    /// `crate::merge::merge`), never inside a layer.
    pub network: Option<bool>,
    pub mounts: Vec<Mount>,
    /// Environment forwarded into the sandbox.
    pub env: BTreeMap<String, String>,
    /// Host directories whose git metadata a repo's `.git` FILE may
    /// point at — the approval list for the external git-dir binds
    /// (review-2 item 1). Written as host paths in the same three
    /// forms as `[[mounts]]` paths (D8): absolute, `~/…` (expanded
    /// against the invoking user's home at run time) or relative to
    /// the config file's directory. A repo-writable `.git` file is
    /// untrusted content (D3): the bind happens only when the
    /// resolved target is at or below an entry of this list in the
    /// user config or the sidecar — `mysbx init` snapshots the
    /// discovered directories into a fresh sidecar so the common
    /// worktree/submodule case works out of the box.
    pub git_dirs: Vec<String>,
    /// State directories (docs/design/config.md D15): paths relative
    /// to the sandbox home whose content should persist across runs.
    /// mysbx backs each entry with `<sidecar>/state/<entry>` on the
    /// host, creates it before the backend starts and binds it `rw`
    /// at `/mysbx-home/<entry>` — a sandboxed agent keeps its sessions
    /// and caches per repository, without any host-home path entering
    /// the sandbox. Both layers declare; the lists concatenate like
    /// mounts. Unlike `[[mounts]]` paths these are NEVER resolved
    /// against the host (D8 does not apply): the host path is
    /// synthesized from the sidecar, and the only thing a layer may
    /// say is the shape of the path below the sandbox home.
    pub state_dirs: Vec<String>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            backend: None,
            network: None,
            mounts: Vec::new(),
            env: BTreeMap::new(),
            git_dirs: Vec::new(),
            state_dirs: Vec::new(),
        }
    }
}

/// Why a configuration could not be loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// The file is not valid TOML (of the supported subset).
    Syntax(toml::Error),
    /// The file is valid TOML but not a valid configuration.
    Schema(String),
    /// The file could not be read.
    Io(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Syntax(e) => write!(f, "invalid TOML: {e}"),
            Error::Schema(m) => f.write_str(m),
            Error::Io(m) => f.write_str(m),
        }
    }
}

impl std::error::Error for Error {}

impl Config {
    /// Parse a configuration from TOML text.
    pub fn parse(input: &str) -> Result<Config, Error> {
        let root = toml::parse(input).map_err(Error::Syntax)?;
        Config::from_table(&root)
    }

    /// Read and parse a configuration file.
    pub fn load(path: &Path) -> Result<Config, Error> {
        let text = std::fs::read_to_string(path)
            .map_err(|e| Error::Io(format!("cannot read {}: {e}", path.display())))?;
        Config::parse(&text).map_err(|e| match e {
            Error::Schema(m) => Error::Schema(format!("{}: {m}", path.display())),
            Error::Syntax(inner) => {
                Error::Schema(format!("{}: invalid TOML: {inner}", path.display()))
            }
            other => other,
        })
    }

    fn from_table(root: &Table) -> Result<Config, Error> {
        let mut config = Config::default();
        for (key, value) in root {
            match key.as_str() {
                "backend" => config.backend = Some(string(value, "backend")?.to_owned()),
                "network" => config.network = Some(boolean(value, "network")?),
                "mounts" => config.mounts = mounts(value)?,
                "env" => config.env = env(table(value, "env")?)?,
                "git-dirs" => config.git_dirs = git_dirs(value)?,
                "state-dirs" => config.state_dirs = state_dirs(value)?,
                other => return Err(unknown("top level", other)),
            }
        }
        Ok(config)
    }
}

fn mounts(value: &Value) -> Result<Vec<Mount>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "mounts: expected an array of tables ([[mounts]]), found {}",
            value.type_name()
        ))
    })?;
    let mut out = Vec::with_capacity(items.len());
    for (n, item) in items.iter().enumerate() {
        let at = format!("[[mounts]] #{}", n + 1);
        let t = item.as_table().ok_or_else(|| {
            Error::Schema(format!(
                "{at}: expected a table, found {}",
                item.type_name()
            ))
        })?;
        let mut path = None;
        let mut dest = None;
        let mut mode = Mode::Ro;
        for (key, value) in t {
            match key.as_str() {
                "path" => {
                    path = Some(host_path(
                        string(value, &format!("{at}: path"))?,
                        &format!("{at}: path"),
                    )?)
                }
                "dest" => {
                    dest = Some(absolute(
                        string(value, &format!("{at}: dest"))?,
                        &format!("{at}: dest"),
                    )?)
                }
                "mode" => mode = Mode::parse(string(value, &format!("{at}: mode"))?, &at)?,
                other => return Err(unknown(&at, other)),
            }
        }
        let path =
            path.ok_or_else(|| Error::Schema(format!("{at}: missing required key `path`")))?;
        out.push(Mount { path, dest, mode });
    }
    Ok(out)
}

/// Parse the `git-dirs` approval list: an array of host-path strings,
/// each in one of the D8 forms (absolute, `~/…`, relative). The same
/// `host_path` shape check as `[[mounts]]` paths applies; canonicalization
/// happens in the merge, eagerly (D8), so a dangling approval is a hard
/// error rather than a silent no-op.
fn git_dirs(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "git-dirs: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("git-dirs #{}", i + 1);
            host_path(string(v, &at)?, &at)
        })
        .collect()
}

/// Parse the `state-dirs` list (docs/design/config.md D15): an array
/// of sandbox-home-relative paths. See [`state_dir_path`] for the
/// per-entry shape check.
fn state_dirs(value: &Value) -> Result<Vec<String>, Error> {
    let items = value.as_array().ok_or_else(|| {
        Error::Schema(format!(
            "state-dirs: expected an array of strings, found {}",
            value.type_name()
        ))
    })?;
    items
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let at = format!("state-dirs #{}", i + 1);
            state_dir_path(string(v, &at)?, &at)
        })
        .collect()
}

/// A `state-dirs` entry (docs/design/config.md D15): a path *relative
/// to the sandbox home* naming where the persistent directory is bound
/// (`/mysbx-home/<entry>`), with the host backing store synthesized by
/// mysbx at `<sidecar>/state/<entry>`. It is neither a host path (so
/// the D8 forms do not apply) nor a free-form in-sandbox `dest`: the
/// host side is decided by the sidecar (D2/D10), the sandbox side by
/// D14. The only thing a layer may say is the shape of the path
/// below the home — and it must be unambiguous, because the runtime
/// joins the entry into both trees: a leading `/`, a `~/` prefix and
/// `.`/`..`/empty components are rejected here, so no entry can climb
/// out of the sandbox home or of the sidecar's state tree, whatever
/// joins it.
fn state_dir_path(path: &str, at: &str) -> Result<String, Error> {
    let bad = |why: &str| {
        Error::Schema(format!(
            "{at}: must be a relative path below the sandbox home ({why}): `{path}`"
        ))
    };
    if path.is_empty() {
        return Err(bad("must not be empty"));
    }
    if path.starts_with('/') {
        return Err(bad("no leading `/` — the destination is always /mysbx-home/<entry>"));
    }
    if path.starts_with('~') {
        return Err(bad("no `~/` prefix — the sandbox home is not the host home"));
    }
    for component in path.split('/') {
        if component.is_empty() {
            return Err(bad("no empty `//` components"));
        }
        if component == "." || component == ".." {
            return Err(bad("no `.` or `..` components"));
        }
    }
    Ok(path.to_owned())
}

fn env(t: &Table) -> Result<BTreeMap<String, String>, Error> {
    let mut out = BTreeMap::new();
    for (key, value) in t {
        // Keys become `--setenv` arguments in the bwrap argv
        // (src/bwrap.rs); one that looks like a flag, or contains `=`
        // (ambiguous parsing), or is empty, would smuggle data past the
        // option parser. TOML bare keys allow all of these, so reject
        // them here, at the schema edge, where every other impossible
        // value is rejected too.
        if key.is_empty() || key.starts_with('-') || key.contains('=') {
            return Err(Error::Schema(format!(
                "env key {key:?} is not a usable variable name"
            )));
        }
        out.insert(
            key.clone(),
            string(value, &format!("env.{key}"))?.to_owned(),
        );
    }
    Ok(out)
}

// ---- small typed accessors -----------------------------------------------

fn string<'v>(value: &'v Value, at: &str) -> Result<&'v str, Error> {
    value.as_str().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a string, found {}",
            value.type_name()
        ))
    })
}

fn boolean(value: &Value, at: &str) -> Result<bool, Error> {
    value.as_bool().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a boolean, found {}",
            value.type_name()
        ))
    })
}

fn table<'v>(value: &'v Value, at: &str) -> Result<&'v Table, Error> {
    value.as_table().ok_or_else(|| {
        Error::Schema(format!(
            "{at}: expected a table, found {}",
            value.type_name()
        ))
    })
}

/// A host path of a `[[mounts]]` entry (docs/design/config.md D8). It may
/// be absolute, `~/…` (the invoking user's home) or relative to the
/// directory of the config file that declared it. Parsing stays
/// string-level: the parser knows neither `$HOME` nor which file it is
/// reading, so expansion and canonicalization happen in `crate::merge`,
/// eagerly, before the merge.
///
/// Rejected here are only the spellings that can never be resolved: the
/// empty string, and a `~` that is not the `~/` prefix — `~` alone and
/// `~user/…` are not supported, because "another user's home" is a
/// lookup this tool deliberately does not do.
fn host_path(path: &str, at: &str) -> Result<String, Error> {
    if path.is_empty() {
        return Err(Error::Schema(format!("{at}: must not be empty")));
    }
    if path.starts_with('~') && !path.starts_with("~/") {
        return Err(Error::Schema(format!(
            "{at}: only the `~/` prefix is supported, not `~` alone or `~user`: `{path}`"
        )));
    }
    Ok(path.to_owned())
}

/// A `dest` is absolute (docs/design/config.md D8). Unlike a mount's
/// host path it is an *in-sandbox* path: there is no host home to expand
/// `~/` against and no config file to be relative to inside the sandbox's
/// filesystem view, and it is never canonicalized against the host (see
/// `crate::merge::canonicalize_layer`). Anything but an absolute path is
/// therefore a configuration mistake and is rejected here.
fn absolute(path: &str, at: &str) -> Result<String, Error> {
    if Path::new(path).is_absolute() {
        Ok(path.to_owned())
    } else {
        Err(Error::Schema(format!(
            "{at}: must be absolute (it is a path inside the sandbox: no `~/`, no relative paths): `{path}`"
        )))
    }
}

fn unknown(at: &str, key: &str) -> Error {
    Error::Schema(format!("{at}: unknown key `{key}`"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_config_is_all_defaults() {
        let c = Config::parse("").unwrap();
        assert_eq!(c, Config::default());
        // `network` is tri-state in a layer: None means "not decided" —
        // the shared-by-default `true` is applied after the merge.
        assert_eq!(c.network, None);
        assert!(c.backend.is_none());
    }

    #[test]
    fn mount_mode_defaults_to_ro() {
        let c = Config::parse("[[mounts]]\npath = \"/etc/hosts\"\n").unwrap();
        assert_eq!(c.mounts[0].mode, Mode::Ro);
        assert_eq!(c.mounts[0].dest, None);
    }

    #[test]
    fn unknown_keys_are_rejected() {
        assert!(matches!(Config::parse("nope = 1\n"), Err(Error::Schema(_))));
    }

    #[test]
    fn repo_table_is_rejected() {
        // The repo is implicit (docs/design/config.md D13); a `[repo]` table is
        // an unknown top-level key and therefore a schema error (D11).
        assert!(matches!(Config::parse("[repo]\n"), Err(Error::Schema(_))));
        assert!(matches!(
            Config::parse("[repo]\npath = \"/home/user/src/project\"\n"),
            Err(Error::Schema(_))
        ));
    }

    #[test]
    fn relative_and_home_paths_are_accepted_verbatim() {
        // The parser stores the path as written; `crate::merge` expands
        // `~/` and resolves relative paths against the config file's
        // directory (docs/design/config.md D8).
        let c = Config::parse("[[mounts]]\npath = \"rel/sub\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "rel/sub");
        let c = Config::parse("[[mounts]]\npath = \"~/.config/git\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "~/.config/git");
        let c = Config::parse("[[mounts]]\npath = \"../outside\"\n").unwrap();
        assert_eq!(c.mounts[0].path, "../outside");
    }

    #[test]
    fn unsupported_tilde_spellings_are_rejected() {
        for p in ["~", "~other", "~other/data"] {
            let e = Config::parse(&format!("[[mounts]]\npath = \"{p}\"\n")).unwrap_err();
            assert!(e.to_string().contains("only the `~/` prefix"), "{p}: {e}");
        }
    }

    // ---- state-dirs (docs/design/config.md D15) -----------------------

    #[test]
    fn state_dirs_parse_as_home_relative_paths() {
        let c = Config::parse(
            "state-dirs = [\".local/share/opencode\", \".local/state/opencode\"]\n",
        )
        .unwrap();
        assert_eq!(c.state_dirs, vec![".local/share/opencode", ".local/state/opencode"]);

        // A single-element array and an empty array both parse; the
        // empty list is also the default.
        let c = Config::parse("state-dirs = [\".cache/build\"]\n").unwrap();
        assert_eq!(c.state_dirs, vec![".cache/build"]);
        let c = Config::parse("state-dirs = []\n").unwrap();
        assert!(c.state_dirs.is_empty());
        assert!(Config::default().state_dirs.is_empty());
    }

    #[test]
    fn state_dirs_reject_non_string_entries() {
        assert!(Config::parse("state-dirs = [1]\n").is_err());
        assert!(Config::parse("state-dirs = \".cache\"\n").is_err());
        // A wrong type names the key, like every other schema error.
        let e = Config::parse("state-dirs = true\n").unwrap_err();
        assert!(e.to_string().contains("state-dirs"), "{e}");
    }

    #[test]
    fn state_dirs_reject_climbing_and_ambiguous_spellings() {
        // The entry is joined into TWO trees (the sidecar's state dir on
        // the host, the sandbox home below the tmpfs), so every spelling
        // that could make either join escape is rejected at the schema
        // edge (D15): absolute (a host-path confusion), `~/`, empty,
        // `//` runs, `.` and `..`.
        for p in [
            "/abs",
            "/",
            "~/x",
            "",
            "a//b",
            "a/./b",
            "../x",
            "a/../b",
            "x/..",
        ] {
            let e = Config::parse(&format!("state-dirs = [\"{p}\"]\n")).unwrap_err();
            let msg = e.to_string();
            assert!(msg.contains("state-dirs"), "{p:?}: {msg}");
            assert!(msg.contains("sandbox home"), "{p:?}: {msg}");
        }
    }

    #[test]
    fn empty_path_is_rejected() {
        let e = Config::parse("[[mounts]]\npath = \"\"\n").unwrap_err();
        assert!(e.to_string().contains("must not be empty"), "{e}");
    }

    #[test]
    fn non_absolute_dest_is_rejected() {
        // `dest` is an in-sandbox path and stays absolute-only.
        for d in ["rel", "~/inside"] {
            let e =
                Config::parse(&format!("[[mounts]]\npath = \"/a\"\ndest = \"{d}\"\n")).unwrap_err();
            assert!(e.to_string().contains("must be absolute"), "{d}: {e}");
        }
    }
}
