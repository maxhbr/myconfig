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
    pub path: String,
    /// Destination inside the sandbox; `None` means "same path".
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
    /// Defaults to `true`: the network is shared; `network = false` is the
    /// explicit deny switch.
    pub network: bool,
    pub mounts: Vec<Mount>,
    /// Environment forwarded into the sandbox.
    pub env: BTreeMap<String, String>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            backend: None,
            network: true,
            mounts: Vec::new(),
            env: BTreeMap::new(),
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
                "network" => config.network = boolean(value, "network")?,
                "mounts" => config.mounts = mounts(value)?,
                "env" => config.env = env(table(value, "env")?)?,
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
                    path = Some(absolute(
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

/// Paths in the configuration are absolute (docs/design/config.md D8).
/// Canonicalization happens later, when the backend is prepared; a relative
/// path is a configuration mistake and is rejected here.
fn absolute(path: &str, at: &str) -> Result<String, Error> {
    if Path::new(path).is_absolute() {
        Ok(path.to_owned())
    } else {
        Err(Error::Schema(format!("{at}: must be absolute: `{path}`")))
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
        assert!(c.network);
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
    fn relative_paths_are_rejected() {
        let e = Config::parse("[[mounts]]\npath = \"rel\"\n").unwrap_err();
        assert!(e.to_string().contains("must be absolute"), "{e}");
    }
}
