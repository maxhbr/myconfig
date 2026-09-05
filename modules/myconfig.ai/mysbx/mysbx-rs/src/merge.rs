// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The two-layer configuration merge (docs/design/config.md D6, D7).
//!
//! Precedence, lowest first: built-in defaults → user config
//! (`$XDG_CONFIG_HOME/mysbx/config.toml`) → sidecar `config.toml`
//! (`<repo>.mysbx/config.toml`). Command-line flags are applied on top of
//! the `Merged` value by a later item. The built-in defaults are already
//! folded into every `Config` by the parser (`Config::default`), so this
//! module merges exactly the two files.
//!
//! The sidecar may **narrow**, never **widen** (docs/design/config.md D7):
//!
//! - **Mounts**: a sidecar mount is accepted only if its path is *at or
//!   below* a path the user config grants — prefix containment on
//!   canonicalized paths, at a path-component boundary (`/a/bc` is not
//!   below `/a/b`) — and its mode may equal the granted mode or downgrade
//!   `rw` → `ro`, never upgrade. When the user config does not exist the
//!   user layer is empty and grants nothing: any sidecar `[[mounts]]` is
//!   then a hard error telling the user to grant the path in the user
//!   config first. There is no implicit allow-all — mounting anything into
//!   the sandbox is a grant (docs/design/config.md D9: nothing from the
//!   host filesystem is available unless it is declared).
//! - **Env**: the sidecar *may* introduce variables the user config never
//!   mentions, but *may not override* a variable the user config sets. The
//!   asymmetry is deliberate: an invented variable is a value the repo
//!   already controls, while overriding a user-set one is how a repo would
//!   redirect a tool at something the user did not choose.
//! - **Backend**: the sidecar may name a backend
//!   (docs/design/cli.md D7: never auto-detected); presence or absence of
//!   a backend is not a widening concern. Whatever either layer set is
//!   carried through unchanged. The "backend cannot be silently absent"
//!   hard error (neither layer named one) is NOT enforced here — it is
//!   deferred to the CLI/item that consumes `Merged` and must start a
//!   real backend.
//! - **Network**: the sidecar may set `false`; it may NOT set `true` when
//!   the user config has already set `false`.
//!
//! A violation is a hard error naming the offending key, the sidecar path
//! and the granting (or missing) user-config entry — never a warning.
//!
//! Every path is canonicalized eagerly, before the merge, and fails on a
//! missing path (docs/design/config.md D8) — so the error message points
//! at the layer that wrote the path. The containment comparison uses the
//! canonicalized paths on *both* sides: symlink resolution can turn a
//! granted path into one outside the grant, and that is said loudly in
//! the error instead of silently passing or failing.
//!
//! Mounts are never sorted or deduplicated: mount order is argv order,
//! and a later `rw` bind nested inside an earlier `ro` bind is a real
//! pattern (`../../../fns/bubblewrap-app.nix (as referenced by the spec)` relies on it). Every stored
//! path (user layer and sidecar layer alike) is the canonicalized one:
//! what gets mounted is what the path resolves to
//! (docs/design/config.md D8).
//!
//! `merge` returns a distinct `Merged` type so no later code can
//! accidentally consume an unmerged layer: `Merged` is the only input
//! item 4 (the argv builder) accepts. `Config` values are layer *inputs*
//! and never leave this module's boundary as effective configuration.

use crate::config::{Config, Mode, Mount};
use std::collections::BTreeMap;
use std::fmt;
use std::path::{Path, PathBuf};

/// The effective configuration after both configuration layers merged.
/// The only input item 4 (the argv builder) accepts.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Merged {
    /// The backend from whichever layer decided it; `None` only when
    /// neither layer named one (docs/design/cli.md D7: never
    /// auto-detected).
    pub backend: Option<String>,
    pub network: bool,
    /// User-config mounts first (in declaration order), then the accepted
    /// sidecar mounts (in their declaration order within the sidecar
    /// file). Never sorted, never deduplicated.
    pub mounts: Vec<Mount>,
    /// Effective environment: user-config variables plus the accepted
    /// sidecar-only variables (overrides were rejected before this value
    /// existed).
    pub env: BTreeMap<String, String>,
}

/// Why the two configuration layers could not be merged or loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A path in one of the layers could not be canonicalized. Which file
    /// wrote the path is part of the message (D8: fail in the layer).
    Canonicalize {
        file: PathBuf,
        key: String,
        path: PathBuf,
        source: String,
    },
    /// A sidecar mount has no covering user-config grant. The message
    /// names the sidecar path and the missing user-config entry.
    MountNotGranted { message: String },
    /// A sidecar mount sits under a granted path but asks for more
    /// access (`ro` → `rw` upgrade).
    MountUpgrade { message: String },
    /// A sidecar `[env]` entry overrides a user-set variable.
    EnvOverride { key: String, message: String },
    /// The sidecar re-enables the network the user config set to `false`.
    NetworkUpgrade { message: String },
    /// Reading or parsing one of the layers failed.
    Load(String),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Canonicalize {
                file,
                key,
                path,
                source,
            } => write!(
                f,
                "{}: `{}`: cannot canonicalize {} — every path must exist and resolve (docs/design/config.md D8): {}",
                file.display(),
                key,
                path.display(),
                source,
            ),
            Error::MountNotGranted { message } => f.write_str(message),
            Error::MountUpgrade { message } => f.write_str(message),
            Error::EnvOverride { key, message } => write!(f, "{message} (key: `{key}`)"),
            Error::NetworkUpgrade { message } => f.write_str(message),
            Error::Load(m) => f.write_str(m),
        }
    }
}

impl std::error::Error for Error {}

/// Load the user config and the sidecar config for `repo`'s sidecar
/// directory. The user config lives at
/// `$XDG_CONFIG_HOME/mysbx/config.toml`, falling back to
/// `~/.config/mysbx/config.toml` when `XDG_CONFIG_HOME` is unset.
/// A file that does not exist is an *empty layer*; a file that exists but
/// cannot be read or parsed is a hard error.
pub fn load_layers(
    home: &Path,
    xdg_config_home: Option<&str>,
    sidecar_dir: &Path,
) -> Result<LoadedLayers, Error> {
    let user_path = user_config_path(home, xdg_config_home);
    let sidecar_path = sidecar_dir.join("config.toml");
    Ok(LoadedLayers {
        user: (load_optional(&user_path, "user")?, user_path),
        sidecar: (load_optional(&sidecar_path, "sidecar")?, sidecar_path),
    })
}

/// The two loaded layers with the exact paths they were loaded from, so
/// callers pass truthful file paths to [`merge`] (whose violation messages
/// name them) without re-implementing the `$XDG_CONFIG_HOME` fallback.
pub struct LoadedLayers {
    pub user: (Config, PathBuf),
    pub sidecar: (Config, PathBuf),
}

// `Debug` is required by tests that `unwrap_err()` a `load_layers` result.
impl std::fmt::Debug for LoadedLayers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoadedLayers")
            .field("user", &self.user.1)
            .field("sidecar", &self.sidecar.1)
            .finish()
    }
}

/// The user config path (docs/design/config.md D6): `$XDG_CONFIG_HOME` if
/// set (a non-absolute value is treated as unset, per the XDG base-dir
/// spec), otherwise `~/.config`.
fn user_config_path(home: &Path, xdg_config_home: Option<&str>) -> PathBuf {
    match xdg_config_home {
        Some(x) if !x.is_empty() && Path::new(x).is_absolute() => {
            Path::new(x).join("mysbx").join("config.toml")
        }
        _ => home.join(".config").join("mysbx").join("config.toml"),
    }
}

/// Load one layer; an absent file yields the all-defaults config. Any
/// other I/O error, and any schema or TOML error, is a hard error that
/// names the file.
fn load_optional(path: &Path, layer: &str) -> Result<Config, Error> {
    match std::fs::read_to_string(path) {
        Ok(text) => Config::parse(&text)
            .map_err(|e| Error::Load(format!("{} ({} config): {e}", path.display(), layer))),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(Config::default()),
        Err(e) => Err(Error::Load(format!(
            "{} ({} config): cannot read: {e}",
            path.display(),
            layer
        ))),
    }
}

/// Canonicalize every `[[mounts]]` path of one layer, eagerly, failing on
/// a missing path (docs/design/config.md D8); the error points at the
/// file that wrote the path, so it is produced before the merge starts.
///
/// `dest` is deliberately NOT canonicalized: it is an in-sandbox path,
/// so canonicalizing it against the host filesystem would be wrong. Do
/// not "fix" this.
fn canonicalize_layer(cfg: &Config, file: &Path) -> Result<Vec<(PathBuf, Mount)>, Error> {
    cfg.mounts
        .iter()
        .enumerate()
        .map(|(i, m)| {
            let raw = PathBuf::from(&m.path);
            let canon = std::fs::canonicalize(&raw).map_err(|e| Error::Canonicalize {
                file: file.to_owned(),
                key: format!("[[mounts]] #{}", i + 1),
                path: raw.clone(),
                source: e.to_string(),
            })?;
            Ok((canon, m.clone()))
        })
        .collect()
}

/// True when `sub` is at or below `grant` on a path-component boundary:
/// equal, or `sub` extends `grant` by whole components. `/a/bc` is NOT
/// below `/a/b`; `/a/b/c` is. `Path::starts_with` already compares whole
/// components, so this is exactly it (and `/` contains everything).
fn contains(grant: &Path, sub: &Path) -> bool {
    sub.starts_with(grant)
}

/// Merge the two loaded layers (docs/design/config.md D6, D7).
///
/// `user_file` and `sidecar_file` are the paths the configs were loaded
/// from; they appear in every violation message so the user can see which
/// file to fix and which entry granted (or failed to grant) what.
///
/// The user config may be absent (then it granted nothing) and the
/// sidecar is guaranteed to exist by item 2, but both are accepted as
/// plain `Config` values — absence was already turned into the empty
/// default layer by `load_layers`.
pub fn merge(
    user: Config,
    sidecar: Config,
    user_file: &Path,
    sidecar_file: &Path,
) -> Result<Merged, Error> {
    // D8: canonicalize every path eagerly, before the merge, so error
    // messages point at the layer that wrote the path.
    let granted = canonicalize_layer(&user, user_file)?;
    let sidecar_canon = canonicalize_layer(&sidecar, sidecar_file)?;

    // network: the sidecar may deny (false), not re-enable (D7).
    if !user.network && sidecar.network {
        return Err(Error::NetworkUpgrade {
            message: format!(
                "{}: network = true re-enables the network the user config {} denied with `network = false` — the sidecar may narrow, not widen (docs/design/config.md D7)",
                sidecar_file.display(),
                user_file.display(),
            ),
        });
    }
    let network = user.network && sidecar.network;

    // env: sidecar-only keys pass through, user-set keys must not be
    // overridden — the asymmetry is deliberate (see module docs).
    let mut env = BTreeMap::new();
    for (key, value) in &user.env {
        env.insert(key.clone(), value.clone());
    }
    for (key, value) in &sidecar.env {
        if let Some(user_value) = user.env.get(key) {
            return Err(Error::EnvOverride {
                key: key.clone(),
                message: format!(
                    "{}: [env] {} = {:?} overrides the user config {}, which sets it to {:?} — the sidecar may introduce new variables but must not override user-set ones (docs/design/config.md D7)",
                    sidecar_file.display(),
                    key,
                    value,
                    user_file.display(),
                    user_value,
                ),
            });
        }
        env.insert(key.clone(), value.clone());
    }

    // mounts: every sidecar mount must be covered by a user grant and
    // never upgrade the granted mode. Mount order is argv order; user
    // mounts keep their order, accepted sidecar mounts follow in the
    // sidecar's order — no sorting, no deduplication. All stored paths
    // are the canonicalized ones (D8): what gets mounted is what the
    // path resolves to.
    let mut mounts: Vec<Mount> = granted
        .iter()
        .map(|(canon, m)| Mount {
            path: canon.to_string_lossy().into_owned(),
            dest: m.dest.clone(),
            mode: m.mode,
        })
        .collect();
    for (canon, requested) in &sidecar_canon {
        // Find the most specific (deepest) covering grant: among all
        // grants containing this path, the one with the most components
        // decides the allowed mode, so a wide grant cannot silently bless
        // what a narrow grant beside it restricts. Grants are compared on
        // canonicalized paths on BOTH sides.
        let mut best: Option<&(PathBuf, Mount)> = None;
        for g in &granted {
            if contains(&g.0, canon) {
                best = match best {
                    Some(b) if b.0.components().count() >= g.0.components().count() => best,
                    _ => Some(g),
                };
            }
        }
        let grant = match best {
            Some(g) => g,
            None => {
                return Err(Error::MountNotGranted {
                    message: format!(
                        "{}: [[mounts]] path {} is not at or below any path granted by the user config {} — grant it there first (docs/design/config.md D7: the sidecar may narrow, not widen). Note: both paths are compared after symlink resolution; if a symlink moved the grant, fix the grant, not the sidecar",
                        sidecar_file.display(),
                        canon.display(),
                        user_file.display(),
                    ),
                });
            }
        };
        if requested.mode == Mode::Rw && grant.1.mode == Mode::Ro {
            return Err(Error::MountUpgrade {
                message: format!(
                    "{}: [[mounts]] path {} wants rw but the user config {} grants ro at {} — the sidecar may downgrade rw to ro but never upgrade ro to rw (docs/design/config.md D7)",
                    sidecar_file.display(),
                    canon.display(),
                    user_file.display(),
                    grant.0.display(),
                ),
            });
        }
        mounts.push(Mount {
            path: canon.to_string_lossy().into_owned(),
            dest: requested.dest.clone(),
            mode: requested.mode,
        });
    }

    Ok(Merged {
        backend: sidecar.backend.or(user.backend),
        network,
        mounts,
        env,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::Config;

    /// A fresh temporary directory per test; hand-rolled, the crate has
    /// no dependencies.
    fn tmpdir(name: &str) -> PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("mysbx-merge-test-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn cfg(toml: &str) -> Config {
        Config::parse(toml).unwrap()
    }

    /// Display-only file names for the error-message assertions; the
    /// tests call `merge` on parsed configs, not on real files.
    fn user_file() -> PathBuf {
        PathBuf::from("/tmp/fake-user-config.toml")
    }

    fn sidecar_file() -> PathBuf {
        PathBuf::from("/tmp/fake-repo.mysbx/config.toml")
    }

    /// Build real directories under `base` (so canonicalization works)
    /// and return their canonical paths.
    fn dir(base: &Path, parts: &[&str]) -> PathBuf {
        let mut p = base.to_owned();
        for part in parts {
            p = p.join(part);
            std::fs::create_dir_all(&p).unwrap();
        }
        std::fs::canonicalize(&p).unwrap()
    }

    fn mount_toml(p: &Path, mode: &str) -> String {
        format!("[[mounts]]\npath = \"{}\"\nmode = \"{mode}\"\n", p.display())
    }

    #[test]
    fn user_layer_path_failure_names_the_user_config() {
        // Eager canonicalization (D8) runs for the user layer itself: a
        // granted path that does not exist must fail with the USER config
        // named, before the merge even looks at the sidecar.
        let base = tmpdir("user-canon-fail");
        let ghost = base.join("does-not-exist");
        let e = merge(
            cfg(&mount_toml(&ghost, "ro")),
            Config::default(),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap_err();
        match e {
            Error::Canonicalize { ref file, ref path, .. } => {
                assert_eq!(file, &user_file());
                assert_eq!(path, &ghost);
            }
            other => panic!("wrong error: {other}"),
        }
        assert!(e.to_string().contains("fake-user-config.toml"), "{e}");
    }

    #[test]
    fn sidecar_path_failure_names_the_sidecar() {
        // The layer that wrote a broken path is the layer that gets
        // blamed (D8); the merge never starts.
        let base = tmpdir("sidecar-canon-fail");
        let good = dir(&base, &["granted"]);
        let ghost = base.join("missing");
        let u = cfg(&mount_toml(&good, "rw"));
        let s = cfg(&mount_toml(&ghost, "ro"));
        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        match e {
            Error::Canonicalize { ref file, ref path, .. } => {
                assert_eq!(file, &sidecar_file());
                assert_eq!(path, &ghost);
            }
            other => panic!("wrong error: {other}"),
        }
        assert!(e.to_string().contains("config.toml"), "{e}");
        assert!(!e.to_string().contains("not at or below"), "{e}");
    }

    #[test]
    fn identical_sidecar_mount_is_accepted() {
        // Equal is "at or below": the trivial narrowing case.
        let base = tmpdir("identical");
        let granted = dir(&base, &["some", "path"]);
        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&granted, "rw"));
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert!(
            merged
                .mounts
                .iter()
                .all(|x| x.path == granted.to_string_lossy() && x.mode == Mode::Rw)
        );
    }

    #[test]
    fn containment_accepted_sibling_rejected() {
        // One accepted subdirectory, one rejected sibling: `/a/bc` is NOT
        // below `/a/b` (component boundary).
        let base = tmpdir("containment");
        let granted = dir(&base, &["grant-root"]);
        let below = dir(&granted, &["ac", "b"]);
        let sibling = dir(&base, &["abc"]);

        let u = cfg(&mount_toml(&granted, "rw"));
        let stoml = mount_toml(&below, "ro") + &mount_toml(&sibling, "ro");
        let e = merge(u, cfg(&stoml), &user_file(), &sidecar_file()).unwrap_err();
        assert!(matches!(e, Error::MountNotGranted { .. }), "{e}");
        // The message names the offending path, the sidecar file and the
        // missing user-config entry.
        let msg = e.to_string();
        assert!(msg.contains("config.toml"), "{msg}");
        assert!(msg.contains(&format!("{}", sibling.display())), "{msg}");
        assert!(msg.contains("not at or below"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
    }

    #[test]
    fn deep_subpath_accepted() {
        let base = tmpdir("deep");
        let granted = dir(&base, &["home", "user"]);
        let deep = dir(&granted, &["data", "sub"]);
        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&deep, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[1].path, deep.to_string_lossy());
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
    }

    #[test]
    fn component_boundary_rejected() {
        // The spec case verbatim: the sidecar path string extends the
        // grant string but not at a component boundary.
        let base = tmpdir("boundary");
        let granted = dir(&base, &["a", "b"]);
        let sibling = dir(&base, &["a", "bc"]);

        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&sibling, "ro"));
        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        assert!(matches!(e, Error::MountNotGranted { .. }), "{e}");
    }

    #[test]
    fn ro_to_rw_upgrade_rejected() {
        let base = tmpdir("upgrade");
        let granted = dir(&base, &["grant"]);
        let below = dir(&granted, &["sub"]);

        let u = cfg(&mount_toml(&granted, "ro"));
        let s = cfg(&mount_toml(&below, "rw"));
        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        assert!(matches!(e, Error::MountUpgrade { .. }), "{e}");
        let msg = e.to_string();
        assert!(msg.contains("never upgrade"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
        assert!(msg.contains("config.toml"), "{msg}");
    }

    #[test]
    fn rw_to_ro_downgrade_accepted() {
        let base = tmpdir("downgrade");
        let granted = dir(&base, &["grant"]);
        let below = dir(&granted, &["sub"]);

        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&below, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[0].mode, Mode::Rw);
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
        assert_eq!(merged.mounts[1].path, below.to_string_lossy());
    }

    #[test]
    fn sidecar_only_env_accepted() {
        // The deliberate asymmetry: the sidecar may introduce new
        // variables the user config never mentions.
        let mut u = Config::default();
        u.env.insert("USER_ONLY".to_owned(), "u".to_owned());
        let s = cfg("[env]\nSIDECAR_NEW = \"invented\"\n");

        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.env.len(), 2);
        assert_eq!(merged.env.get("SIDECAR_NEW").unwrap(), "invented");
        assert_eq!(merged.env.get("USER_ONLY").unwrap(), "u");
    }

    #[test]
    fn env_override_rejected() {
        let mut u = Config::default();
        u.env.insert("EDITOR".to_owned(), "system-nvim".to_owned());
        let s = cfg("[env]\nEDITOR = \"repo-nvim\"\n");

        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        match e {
            Error::EnvOverride { ref key, .. } => assert_eq!(key, "EDITOR"),
            other => panic!("wrong error: {other}"),
        }
        let msg = e.to_string();
        assert!(msg.contains("config.toml"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
        assert!(msg.contains("must not override"), "{msg}");
    }

    #[test]
    fn network_upgrade_rejected() {
        let mut u = Config::default();
        u.network = false;
        let mut s = Config::default();
        s.network = true; // the sidecar re-enables: hard error

        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        assert!(matches!(e, Error::NetworkUpgrade { .. }), "{e}");
        let msg = e.to_string();
        assert!(msg.contains("network"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
        assert!(msg.contains("config.toml"), "{msg}");
    }

    #[test]
    fn network_false_narrowing_accepted() {
        let mut u = Config::default();
        u.network = true;
        let mut s = Config::default();
        s.network = false;

        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert!(!merged.network);
    }

    #[test]
    fn network_false_in_both_stays_false() {
        let mut u = Config::default();
        u.network = false;
        let mut s = Config::default();
        s.network = false;

        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert!(!merged.network);
    }

    #[test]
    fn missing_user_config_sidecar_mount_rejected() {
        // The honest D7 reading: an absent user config grants nothing, so
        // ANY sidecar [[mounts]] entry is a violation telling the user to
        // grant the path in the user config first. No implicit allow-all.
        let base = tmpdir("no-user");
        let wanted = dir(&base, &["some", "data"]);

        let e = merge(
            Config::default(),
            cfg(&mount_toml(&wanted, "ro")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap_err();
        assert!(matches!(e, Error::MountNotGranted { .. }), "{e}");
        let msg = e.to_string();
        assert!(msg.contains("grant it there first"), "{msg}");
        assert!(msg.contains("config.toml"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
    }

    #[test]
    fn missing_user_config_env_and_network_deny_allowed() {
        // With an absent user config, env introduction and network denial
        // are NOT grants and stay allowed; only mounts need a grant.
        let mut s = Config::default();
        s.network = false;
        let merged = merge(
            Config::default(),
            cfg("[env]\nFOO = \"1\"\n"),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap();
        assert_eq!(merged.env.get("FOO").unwrap(), "1");

        let merged = merge(Config::default(), s, &user_file(), &sidecar_file()).unwrap();
        assert!(!merged.network);
        assert!(merged.mounts.is_empty());
    }

    #[test]
    fn symlink_outside_grant_rejected() {
        // Canonicalization can turn a granted-looking path outside the
        // grant: the sidecar path is under the granted directory, but it
        // is a symlink resolving OUTSIDE it — compared on canonicalized
        // paths on both sides, this is a rejection, and the error says
        // the comparison happened after symlink resolution.
        let base = tmpdir("symlink-out");
        let real = dir(&base, &["real", "data"]);
        let granted = dir(&base, &["grants"]);
        let link = granted.join("data-link");
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&link, "ro"));
        let e = merge(u, s, &user_file(), &sidecar_file()).unwrap_err();
        assert!(
            matches!(e, Error::MountNotGranted { .. }),
            "a symlink resolving outside the grant must be rejected: {e}"
        );
        assert!(e.to_string().contains("symlink"), "{e}");
    }

    #[test]
    fn symlink_inside_grant_accepted() {
        // The flip side: a symlink inside the granted tree resolving to
        // another part of the granted tree stays covered.
        let base = tmpdir("symlink-in");
        let granted = dir(&base, &["grant"]);
        let real = dir(&granted, &["real"]);
        let link = granted.join("alias");
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&link, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[1].path, real.to_string_lossy());
    }

    #[test]
    fn mounts_keep_declaration_order() {
        // Mount order is argv order; a later rw bind nested inside an
        // earlier ro bind is a real pattern
        // (../../../fns/bubblewrap-app.nix (as referenced by the spec) relies on it). No sorting, no
        // deduplication.
        let base = tmpdir("order");
        let outer = dir(&base, &["outer"]);
        let inner = dir(&outer, &["nested"]);

        let u = cfg(&mount_toml(&outer, "rw"));
        let s = cfg(&(mount_toml(&outer, "ro") + &mount_toml(&inner, "rw")));
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 3);
        assert_eq!(merged.mounts[0].path, outer.to_string_lossy());
        assert_eq!(merged.mounts[0].mode, Mode::Rw);
        assert_eq!(merged.mounts[1].path, outer.to_string_lossy());
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
        assert_eq!(merged.mounts[2].path, inner.to_string_lossy());
        assert_eq!(merged.mounts[2].mode, Mode::Rw);
    }

    #[test]
    fn backend_passes_through_from_either_layer() {
        // Backend is not a widening concern (cli.md D7): whichever layer
        // named one decides; when both do, the sidecar layer wins by
        // layer precedence (D6: later layers override earlier ones).
        let mut u = Config::default();
        u.backend = Some("bubblewrap".to_owned());
        let merged = merge(u, Config::default(), &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.backend.as_deref(), Some("bubblewrap"));

        let mut s = Config::default();
        s.backend = Some("qemu".to_owned());
        let merged = merge(Config::default(), s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.backend.as_deref(), Some("qemu"));

        let mut u = Config::default();
        u.backend = Some("bubblewrap".to_owned());
        let mut s = Config::default();
        s.backend = Some("microvm".to_owned());
        let merged = merge(u, s, &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.backend.as_deref(), Some("microvm"));
    }

    #[test]
    fn load_layers_reads_both_files_and_merges() {
        // End-to-end through `load_layers`: real files in a fake XDG
        // home and sidecar, then merge.
        let base = tmpdir("load-layers");
        let home = dir(&base, &["home"]);
        let xdg = dir(&base, &["xdg"]);
        let granted = dir(&base, &["data"]);
        let sd = dir(&base, &["fake-repo.mysbx"]);

        std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
        std::fs::write(
            xdg.join("mysbx").join("config.toml"),
            format!("{}\n[env]\nEDITOR = \"nvim\"\n", mount_toml(&granted, "rw")),
        )
        .unwrap();
        let below = dir(&granted, &["sub"]);
        std::fs::write(
            sd.join("config.toml"),
            format!("{}\n[env]\nPROJECT = \"x\"\n", mount_toml(&below, "ro")),
        )
        .unwrap();

        let layers = load_layers(&home, Some(&xdg.to_string_lossy()), &sd).unwrap();
        assert_eq!(layers.user.1, xdg.join("mysbx").join("config.toml"));
        assert_eq!(layers.sidecar.1, sd.join("config.toml"));
        let merged = merge(
            layers.user.0,
            layers.sidecar.0,
            &layers.user.1,
            &layers.sidecar.1,
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.env.len(), 2);
        assert_eq!(merged.env.get("EDITOR").unwrap(), "nvim");
        assert_eq!(merged.env.get("PROJECT").unwrap(), "x");
        assert!(merged.network);
    }

    #[test]
    fn load_layers_missing_user_file_is_empty_layer() {
        let base = tmpdir("load-missing-user");
        let home = dir(&base, &["home"]);
        let sd = dir(&base, &["fake-repo.mysbx"]);
        std::fs::write(sd.join("config.toml"), "network = false\n").unwrap();

        // No XDG_CONFIG_HOME: fall back to ~/.config/mysbx/config.toml,
        // which does not exist here — the user layer is EMPTY, not an
        // error.
        let layers = load_layers(&home, None, &sd).unwrap();
        assert_eq!(layers.user.0, Config::default());
        assert!(!layers.sidecar.0.network);

        // An XDG_CONFIG_HOME pointing at a directory without a mysbx
        // config: also an empty user layer.
        let layers = load_layers(&home, Some(&home.join(".config").to_string_lossy()), &sd)
            .unwrap();
        assert_eq!(layers.user.0, Config::default());
        assert!(!layers.sidecar.0.network);
    }

    #[test]
    fn load_layers_missing_sidecar_file_is_empty_layer() {
        // Item 2 guarantees the sidecar exists, but the
        // absent-file-is-empty-layer rule is symmetric for both layers.
        let base = tmpdir("load-missing-sidecar");
        let home = dir(&base, &["home"]);
        let xdg = dir(&base, &["xdg"]);
        let sd = dir(&base, &["fake-repo.mysbx"]);

        let layers = load_layers(&home, Some(&xdg.to_string_lossy()), &sd).unwrap();
        assert_eq!(layers.user.0, Config::default());
        assert_eq!(layers.sidecar.0, Config::default());
    }

    #[test]
    fn load_layers_broken_user_file_is_a_hard_error() {
        // "File absent" is an empty layer; "file present but broken" is
        // not silent — it is a hard error naming the file.
        let base = tmpdir("load-broken-user");
        let home = dir(&base, &["home"]);
        let xdg = dir(&base, &["xdg"]);
        let sd = dir(&base, &["fake-repo.mysbx"]);
        std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
        std::fs::write(xdg.join("mysbx").join("config.toml"), "unknown_key = 1\n").unwrap();

        let e = load_layers(&home, Some(&xdg.to_string_lossy()), &sd).unwrap_err();
        assert!(matches!(e, Error::Load(_)), "{e}");
        assert!(e.to_string().contains("user config"), "{e}");
        assert!(e.to_string().contains("config.toml"), "{e}");
    }

    #[test]
    fn user_config_path_prefers_xdg() {
        // $XDG_CONFIG_HOME set → $XDG_CONFIG_HOME/mysbx/config.toml.
        assert_eq!(
            user_config_path(Path::new("/home/u"), Some("/custom/xdg")),
            PathBuf::from("/custom/xdg/mysbx/config.toml")
        );
        // Unset → ~/.config/mysbx/config.toml.
        assert_eq!(
            user_config_path(Path::new("/home/u"), None),
            PathBuf::from("/home/u/.config/mysbx/config.toml")
        );
    }

    #[test]
    fn user_config_path_ignores_relative_xdg() {
        // A non-absolute $XDG_CONFIG_HOME is treated as unset (XDG
        // base-dir spec); a relative value would resolve the user
        // config against the CWD, where nobody looks for it.
        assert_eq!(
            user_config_path(Path::new("/home/u"), Some("relative/dir")),
            PathBuf::from("/home/u/.config/mysbx/config.toml")
        );
        assert_eq!(
            user_config_path(Path::new("/home/u"), Some("")),
            PathBuf::from("/home/u/.config/mysbx/config.toml")
        );
    }

    #[test]
    fn deepest_covering_grant_decides_the_mode() {
        // Among several covering grants, the deepest (most specific)
        // decides the mode, so a narrow ro grant beside a broad rw one
        // cannot be upgraded via the broad one.
        let base = tmpdir("deepest-grant");
        let outer = dir(&base, &["outer"]);
        let secret = dir(&outer, &["secret"]);
        let sub = dir(&secret, &["sub"]);
        let other = dir(&outer, &["other"]);

        // ro-outer/rw-deeper: the narrow rw grant wins for its own path.
        let u = cfg(&format!(
            "{}{}",
            mount_toml(&outer, "ro"),
            mount_toml(&secret, "rw")
        ));
        let merged = merge(u, Config::default(), &user_file(), &sidecar_file()).unwrap();
        assert_eq!(merged.mounts.len(), 2);

        // rw-outer/ro-deeper: asking for the deeper ro path is a legal
        // downgrade, asking for rw under it is an upgrade via the
        // deepest grant and must be rejected.
        let u = cfg(&format!(
            "{}{}",
            mount_toml(&outer, "rw"),
            mount_toml(&secret, "ro")
        ));
        let merged = merge(
            u.clone(),
            cfg(&mount_toml(&secret, "ro")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 3);

        let e = merge(
            u,
            cfg(&mount_toml(&sub, "rw")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap_err();
        assert!(matches!(e, Error::MountUpgrade { .. }), "{e}");

        // A path covered only by the broad grant is unaffected by the
        // narrow one.
        let u = cfg(&format!(
            "{}{}",
            mount_toml(&outer, "rw"),
            mount_toml(&secret, "ro")
        ));
        let merged = merge(
            u,
            cfg(&mount_toml(&other, "rw")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 3);
    }

    #[test]
    fn root_grant_contains_everything() {
        // A grant of "/" contains every path (there is no parent to
        // exclude it). Pin it: a future refactor of `contains` must not
        // silently change that, and ro-at-root means nothing can be rw.
        let base = tmpdir("root-grant");
        let wanted = dir(&base, &["some", "path"]);

        let u = cfg(&mount_toml(Path::new("/"), "rw"));
        let merged = merge(
            u.clone(),
            cfg(&mount_toml(&wanted, "ro")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 2);

        let u = cfg(&mount_toml(Path::new("/"), "ro"));
        let e = merge(
            u,
            cfg(&mount_toml(&wanted, "rw")),
            &user_file(),
            &sidecar_file(),
        )
        .unwrap_err();
        assert!(matches!(e, Error::MountUpgrade { .. }), "{e}");
    }
}
