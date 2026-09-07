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
//! Both configuration files are **trusted** (docs/design/config.md D7):
//!
//! - **Mounts**: BOTH layers declare mounts directly. The sidecar needs
//!   no covering user-config entry, may name any path and either mode
//!   (`ro` and `rw` alike). The sidecar is not repo content: it lives
//!   outside the repo (D2), is never mounted into the sandbox, and the
//!   operator who cloned the repo is the one who wrote it — the same
//!   person the user config belongs to. What still holds is
//!   docs/design/config.md D9: nothing from the host filesystem is in
//!   the sandbox unless one of the two files declared it. Both layers'
//!   mounts simply concatenate — user layer first, sidecar layer second
//!   — and duplicates are kept: bubblewrap applies binds in argv order,
//!   so when both layers mount the same path with different modes the
//!   later (sidecar) bind wins inside the sandbox.
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
//!   the user config has already set `false`. A layer that does not
//!   mention `network` decided nothing — an omitted value never counts
//!   as an explicit `true` (the tri-state `Option<bool>` mirrors
//!   `backend`), so an empty/generated sidecar cannot re-enable what
//!   the user config denied; the shared-by-default `true` of
//!   docs/plan.md is applied only after the layers merged.
//!
//! A violation of the remaining rules (`[env]`, `network`) is a hard
//! error naming the offending key and both files — never a warning.
//!
//! Every path is *resolved* and canonicalized eagerly, before the merge,
//! and fails on a missing path (docs/design/config.md D8) — so the error
//! message points at the layer that wrote the path. Resolution is this
//! module's job because it is the only one that knows both `$HOME` and
//! which file each mount came from:
//!
//! - `~/…` → expanded against `$HOME` (the `home` argument), the same
//!   home for both layers: there is one invoking user.
//! - a relative path → resolved against the **directory of the config
//!   file that declared it**, so a user-config `foo` is
//!   `~/.config/mysbx/foo` while a sidecar `foo` is
//!   `<repo>.mysbx/foo`. A sidecar path never resolves against the user
//!   config's directory.
//! - an absolute path → taken as is.
//!
//! `..` is allowed in every form: canonicalization resolves it, so what
//! is stored — and later mounted — is the real target of a `../…`,
//! `~/…` or symlinked spelling, never the spelling itself.
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

use crate::config::{Config, Mount};
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
    /// The effective network sense, resolved once from the layers'
    /// tri-state values — a plain `bool` on purpose: the default (shared,
    /// docs/plan.md) is already applied, and nothing downstream of the
    /// merge may re-decide it.
    pub network: bool,
    /// Whether the interactive payload is a workmux tmux session
    /// (docs/design/config.md D16): resolved once from the layers'
    /// tri-state values, like `network`, so nothing downstream may
    /// re-decide it. Unlike `network` there is no narrowing rule —
    /// the key grants no host access, so the sidecar simply wins when
    /// both layers decide (D16).
    pub workmux: bool,
    /// User-config mounts first (in declaration order), then the sidecar
    /// mounts (in their declaration order within the sidecar file).
    /// Never sorted, never deduplicated — a repeated path is a repeated
    /// bind, and the last bind wins inside the sandbox.
    pub mounts: Vec<Mount>,
    /// Effective environment: user-config variables plus the accepted
    /// sidecar-only variables (overrides were rejected before this value
    /// existed).
    pub env: BTreeMap<String, String>,
    /// Host directories approved as git metadata targets (review-2
    /// item 1): a repo-writable `.git` FILE may only cause a bind
    /// when its resolved target is at or below one of these. Both
    /// layers contribute; the union is the approval set (a sidecar
    /// entry is trusted user policy — D5 — and a user entry is the
    /// host-wide pre-approval). All canonicalized eagerly (D8), so a
    /// dangling approval is a hard error at load time.
    pub git_dirs: Vec<PathBuf>,
    /// State directories (docs/design/config.md D15): both layers'
    /// `state-dirs` entries, user layer first, deduplicated while
    /// keeping the first occurrence (order-stable). Each entry is a
    /// path below the sandbox home (`/mysbx-home/<entry>`); the host
    /// backing directory `<sidecar>/state/<entry>` is synthesized by
    /// the CLI at run time — the merge keeps only the declared shapes.
    /// The parser has already rejected every ambiguous spelling
    /// (absolute, `~/`, `.`/`..`), so no canonicalization happens
    /// here: there is nothing on the host to resolve yet.
    pub state_dirs: Vec<String>,
}

/// How a mount source relates to the invoking user's home directory
/// (review-3 item 4): `~` is not a valid mount source, and neither is
/// any host directory that contains it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HomeRelation {
    /// The source IS the home directory (`path = "~/"` or a symlink
    /// resolving to it).
    Equal,
    /// The source CONTAINS the home directory (`path = "/home"`, or a
    /// checkout root the home lives below).
    Contains,
}

/// Why the two configuration layers could not be merged or loaded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    /// A path in one of the layers could not be canonicalized. Which file
    /// wrote the path is part of the message (D8: fail in the layer).
    Canonicalize {
        file: PathBuf,
        key: String,
        /// The path exactly as written in the file, before `~/`
        /// expansion and relative resolution — what the user has to
        /// search for in their editor.
        raw: String,
        /// The resolved (absolute, not yet canonicalized) path.
        path: PathBuf,
        source: String,
    },
    /// A mount source is — or contains — the invoking user's home
    /// directory, which the sandbox's base design keeps out entirely
    /// (docs/design/config.md D14: `HOME` is a fresh tmpfs, and the
    /// report says "the host home is not mounted"). A mount that
    /// re-exposes it would make that line false (review-3 item 4).
    HomeExposed {
        /// The mount source, canonicalized.
        source: PathBuf,
        /// How the source relates to the home directory.
        relation: HomeRelation,
    },
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
                raw,
                path,
                source,
            } => write!(
                f,
                "{}: `{}`: cannot canonicalize `{}`{} — every path must exist and resolve (docs/design/config.md D8): {}",
                file.display(),
                key,
                raw,
                if Path::new(raw) == path {
                    String::new()
                } else {
                    format!(" (resolved to {})", path.display())
                },
                source,
            ),
            Error::HomeExposed { source, relation } => write!(
                f,
                "[[mounts]] path {} {} the invoking user's home directory — \
                 the sandbox keeps the host home out entirely (`HOME` is a \
                 fresh tmpfs, and the report says it is not mounted, \
                 docs/design/config.md D14); grant the specific \
                 subdirectory instead (review-3 item 4)",
                source.display(),
                match relation {
                    HomeRelation::Equal => "is",
                    HomeRelation::Contains => "contains",
                },
            ),
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

/// Resolve one `[[mounts]]` path as written into an absolute host path
/// (docs/design/config.md D8):
///
/// - `~/…` → against `home` (the invoking user's home; both layers see
///   the same one),
/// - relative → against `config_dir`, the directory of the config file
///   that declared the path,
/// - absolute → unchanged.
///
/// The result is not canonicalized yet (`..` and symlinks survive); that
/// is the caller's next step.
fn resolve_path(raw: &str, home: &Path, config_dir: &Path) -> PathBuf {
    if let Some(rest) = raw.strip_prefix("~/") {
        // `~/` alone cannot occur: the parser rejects every `~` spelling
        // but the `~/` prefix (config.rs).
        home.join(rest)
    } else if Path::new(raw).is_absolute() {
        PathBuf::from(raw)
    } else {
        config_dir.join(raw)
    }
}

/// Resolve and canonicalize every `[[mounts]]` path of one layer,
/// eagerly, failing on a missing path (docs/design/config.md D8); the
/// error points at the file that wrote the path, so it is produced
/// before the merge starts.
///
/// Relative paths resolve against `file`'s own directory — the per-layer
/// rule: the sidecar's `../secrets` is relative to `<repo>.mysbx/`, never
/// to the user config's directory.
///
/// `dest` is deliberately NOT canonicalized: it is an in-sandbox path,
/// so canonicalizing it against the host filesystem would be wrong. Do
/// not "fix" this.
fn canonicalize_layer(
    cfg: &Config,
    file: &Path,
    home: &Path,
) -> Result<Vec<(PathBuf, Mount)>, Error> {
    let config_dir = file.parent().unwrap_or(Path::new("."));
    cfg.mounts
        .iter()
        .enumerate()
        .map(|(i, m)| {
            let resolved = resolve_path(&m.path, home, config_dir);
            let canon = std::fs::canonicalize(&resolved).map_err(|e| Error::Canonicalize {
                file: file.to_owned(),
                key: format!("[[mounts]] #{}", i + 1),
                raw: m.path.clone(),
                path: resolved.clone(),
                source: e.to_string(),
            })?;
            Ok((canon, m.clone()))
        })
        .collect()
}

/// Resolve and canonicalize one layer's `git-dirs` approval entries
/// (review-2 item 1) — same D8 treatment as `[[mounts]]` paths: `~/…`
/// expands against `home`, relative resolves against the config file's
/// own directory, and a path that does not resolve is a hard error
/// naming the file that wrote it.
fn canonicalize_git_dirs(cfg: &Config, file: &Path, home: &Path) -> Result<Vec<PathBuf>, Error> {
    let config_dir = file.parent().unwrap_or(Path::new("."));
    cfg.git_dirs
        .iter()
        .enumerate()
        .map(|(i, raw)| {
            let resolved = resolve_path(raw, home, config_dir);
            std::fs::canonicalize(&resolved).map_err(|e| Error::Canonicalize {
                file: file.to_owned(),
                key: format!("git-dirs #{}", i + 1),
                raw: raw.clone(),
                path: resolved.clone(),
                source: e.to_string(),
            })
        })
        .collect()
}

/// Merge the two loaded layers (docs/design/config.md D6, D7).
///
/// `user_file` and `sidecar_file` are the paths the configs were loaded
/// from; they appear in every violation message so the user can see which
/// file to fix.
///
/// Either file may be absent (then that layer contributes nothing) —
/// absence was already turned into the empty default layer by
/// `load_layers`. An absent user config is NOT a restriction on the
/// sidecar: the sidecar declares its mounts by itself (D7).
///
/// `home` is the invoking user's home, used to expand `~/…` paths in
/// *both* layers; relative paths resolve against each layer's own config
/// file directory (see the module docs).
pub fn merge(
    user: Config,
    sidecar: Config,
    user_file: &Path,
    sidecar_file: &Path,
    home: &Path,
) -> Result<Merged, Error> {
    // D8: resolve and canonicalize every path eagerly, before the merge,
    // so error messages point at the layer that wrote the path, and so
    // what is stored is the real target of a `~/`, `../` or symlinked
    // spelling.
    let user_canon = canonicalize_layer(&user, user_file, home)?;
    let sidecar_canon = canonicalize_layer(&sidecar, sidecar_file, home)?;
    let approved_git_dirs = canonicalize_git_dirs(&user, user_file, home)?
        .into_iter()
        .chain(canonicalize_git_dirs(&sidecar, sidecar_file, home)?)
        .collect::<Vec<PathBuf>>();

    // Review-3 item 4: the host home is not mounted — not whole, not
    // through an ancestor. `HOME` inside the sandbox is a fresh tmpfs
    // and the report literally says the host home is not mounted
    // (config.md D14); a `path = "~/"` (or a source containing the
    // home, like `/home`) would make that claim false in either
    // layer — this runs on BOTH layers, because a user-config entry
    // is just as capable of breaking the report's truth as a sidecar
    // one. The comparison must be canonicalized on BOTH sides: the
    // caller passes the raw `$HOME` value, which may be a symlink
    // (`/var/usrhome` → `/home/mhuber`), while a `~/` source
    // canonicalizes to the real directory — comparing against the
    // raw value would let a whole-home grant slip past. If `home`
    // itself cannot be canonicalized (a stale `$HOME`), compare
    // against the raw value: the sources are canonical either way, so
    // only the exotic symlinked-home case narrows, never widens.
    let home_canon = std::fs::canonicalize(home).unwrap_or_else(|_| home.to_path_buf());
    for (canon, _m) in user_canon.iter().chain(sidecar_canon.iter()) {
        if *canon == home_canon {
            return Err(Error::HomeExposed {
                source: canon.clone(),
                relation: HomeRelation::Equal,
            });
        }
        if home_canon.starts_with(canon) {
            return Err(Error::HomeExposed {
                source: canon.clone(),
                relation: HomeRelation::Contains,
            });
        }
    }

    // network: the sidecar may deny (false), not re-enable (D7). A layer
    // that does not mention `network` decided nothing (None) — an
    // omitted sidecar value must not re-enable what the user config
    // denied, and an omitted user value grants nothing either, so the
    // merge sees only EXPLICIT values here. The shared-by-default
    // `true` of docs/plan.md is applied once, below, after the layers.
    if user.network == Some(false) && sidecar.network == Some(true) {
        return Err(Error::NetworkUpgrade {
            message: format!(
                "{}: network = true re-enables the network the user config {} denied with `network = false` — the sidecar may deny the network, never re-enable it (docs/design/config.md D7)",
                sidecar_file.display(),
                user_file.display(),
            ),
        });
    }
    // Merge rule: explicit `false` in either layer denies (D7 — the
    // sidecar may narrow); when neither denied, the default of
    // docs/plan.md applies (shared).
    let network = match (user.network, sidecar.network) {
        (Some(false), _) | (_, Some(false)) => false,
        _ => true,
    };

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

    // mounts: both layers declare directly (D7 — the sidecar is
    // trusted), so they simply concatenate. Mount order is argv order;
    // user mounts keep their order and the sidecar mounts follow in the
    // sidecar's order — no sorting, no deduplication, so when both
    // layers name the same path the later (sidecar) bind wins inside
    // the sandbox. All stored paths are the canonicalized ones (D8):
    // what gets mounted is what the path resolves to.
    let mounts: Vec<Mount> = user_canon
        .iter()
        .chain(sidecar_canon.iter())
        .map(|(canon, m)| Mount {
            path: canon.to_string_lossy().into_owned(),
            dest: m.dest.clone(),
            mode: m.mode,
        })
        .collect();

    // state-dirs (D15): both layers declare, the lists concatenate —
    // but unlike mounts a repeated entry is NOT two binds: the two
    // binds would target the same sidecar directory twice, and a
    // later one could only win with a different (host) source that
    // the schema makes impossible. Duplicates are therefore dropped,
    // keeping the FIRST occurrence so the declaration order stays
    // visible; the entries carry no host paths, so nothing needs
    // canonicalization (the parser rejected every ambiguous spelling).
    let state_dirs: Vec<String> = {
        let mut seen: Vec<&str> = Vec::new();
        user.state_dirs
            .iter()
            .chain(sidecar.state_dirs.iter())
            .filter(|e| {
                if seen.contains(&e.as_str()) {
                    false
                } else {
                    seen.push(e.as_str());
                    true
                }
            })
            .cloned()
            .collect()
    };

    Ok(Merged {
        backend: sidecar.backend.or(user.backend),
        network,
        // workmux (D16): the later layer wins where it decided,
        // exactly like `backend` — the key selects the interactive
        // payload from mysbx's own closure and exposes nothing of the
        // host, so it needs neither the network's narrow-only rule nor
        // the `[env]` override refusal. Off when neither layer said
        // anything.
        workmux: sidecar.workmux.or(user.workmux).unwrap_or(false),
        mounts,
        env,
        git_dirs: approved_git_dirs,
        state_dirs,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::{Config, Mode};

    /// A fresh temporary directory per test; hand-rolled, the crate has
    /// no dependencies.
    fn tmpdir(name: &str) -> PathBuf {
        let dir =
            std::env::temp_dir().join(format!("mysbx-merge-test-{}-{name}", std::process::id()));
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

    /// The home for tests whose paths are all absolute: `~/` expansion
    /// must not be reachable there, so a path that does not exist makes
    /// an accidental expansion fail loudly.
    fn no_home() -> PathBuf {
        PathBuf::from("/nonexistent-home")
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
        format!(
            "[[mounts]]\npath = \"{}\"\nmode = \"{mode}\"\n",
            p.display()
        )
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
            &no_home(),
        )
        .unwrap_err();
        match e {
            Error::Canonicalize {
                ref file, ref path, ..
            } => {
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
        let e = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap_err();
        match e {
            Error::Canonicalize {
                ref file, ref path, ..
            } => {
                assert_eq!(file, &sidecar_file());
                assert_eq!(path, &ghost);
            }
            other => panic!("wrong error: {other}"),
        }
        assert!(e.to_string().contains("config.toml"), "{e}");
        assert!(!e.to_string().contains("not at or below"), "{e}");
    }

    #[test]
    fn identical_mount_in_both_layers_is_kept_twice() {
        // No deduplication: the same path in both layers is two binds,
        // in layer order (the later one wins inside the sandbox).
        let base = tmpdir("identical");
        let granted = dir(&base, &["some", "path"]);
        let u = cfg(&mount_toml(&granted, "rw"));
        let s = cfg(&mount_toml(&granted, "rw"));
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert!(merged
            .mounts
            .iter()
            .all(|x| x.path == granted.to_string_lossy() && x.mode == Mode::Rw));
    }

    #[test]
    fn unrelated_sidecar_mounts_are_accepted() {
        // The trust change (D7): a sidecar mount needs no covering user
        // entry. Neither a sibling of a user mount nor a path in a
        // completely different tree is special — both are mounted.
        let base = tmpdir("unrelated");
        let user_mount = dir(&base, &["user-tree"]);
        let sibling = dir(&base, &["abc"]);
        let elsewhere = dir(&base, &["far", "away"]);

        let u = cfg(&mount_toml(&user_mount, "rw"));
        let stoml = mount_toml(&sibling, "ro") + &mount_toml(&elsewhere, "rw");
        let merged = merge(u, cfg(&stoml), &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.mounts.len(), 3);
        assert_eq!(merged.mounts[1].path, sibling.to_string_lossy());
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
        assert_eq!(merged.mounts[2].path, elsewhere.to_string_lossy());
        assert_eq!(merged.mounts[2].mode, Mode::Rw);
    }

    #[test]
    fn deep_subpath_accepted() {
        let base = tmpdir("deep");
        let outer = dir(&base, &["home", "user"]);
        let deep = dir(&outer, &["data", "sub"]);
        let u = cfg(&mount_toml(&outer, "rw"));
        let s = cfg(&mount_toml(&deep, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[1].path, deep.to_string_lossy());
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
    }

    #[test]
    fn sidecar_rw_needs_no_user_entry() {
        // The strongest form of the trust change: a sidecar `rw` mount
        // of a path the user config never mentions (not even ro) is a
        // valid configuration, not an "upgrade".
        let base = tmpdir("sidecar-rw");
        let readonly = dir(&base, &["ro-tree"]);
        let below = dir(&readonly, &["sub"]);

        let u = cfg(&mount_toml(&readonly, "ro"));
        let s = cfg(&mount_toml(&below, "rw"));
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[0].mode, Mode::Ro);
        assert_eq!(merged.mounts[1].mode, Mode::Rw);
        assert_eq!(merged.mounts[1].path, below.to_string_lossy());
    }

    #[test]
    fn sidecar_ro_under_a_user_rw_mount_is_accepted() {
        let base = tmpdir("downgrade");
        let outer = dir(&base, &["outer"]);
        let below = dir(&outer, &["sub"]);

        let u = cfg(&mount_toml(&outer, "rw"));
        let s = cfg(&mount_toml(&below, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
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

        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.env.len(), 2);
        assert_eq!(merged.env.get("SIDECAR_NEW").unwrap(), "invented");
        assert_eq!(merged.env.get("USER_ONLY").unwrap(), "u");
    }

    #[test]
    fn env_override_rejected() {
        let mut u = Config::default();
        u.env.insert("EDITOR".to_owned(), "system-nvim".to_owned());
        let s = cfg("[env]\nEDITOR = \"repo-nvim\"\n");

        let e = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap_err();
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
        u.network = Some(false);
        let mut s = Config::default();
        s.network = Some(true); // the sidecar re-enables: hard error

        let e = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap_err();
        assert!(matches!(e, Error::NetworkUpgrade { .. }), "{e}");
        let msg = e.to_string();
        assert!(msg.contains("network"), "{msg}");
        assert!(msg.contains("fake-user-config.toml"), "{msg}");
        assert!(msg.contains("config.toml"), "{msg}");
    }

    #[test]
    fn omitted_sidecar_network_is_not_an_explicit_true() {
        // The review-1 P1 case: a user config that denies plus a sidecar
        // that says NOTHING (absent file, or a comment-only `init`
        // config) must merge to denied — not trip the NetworkUpgrade
        // guard, which the old bool-with-default-true layers did.
        let mut u = Config::default();
        u.network = Some(false);
        let merged = merge(
            u.clone(),
            Config::default(),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert!(!merged.network);

        // Symmetric: a user config that says nothing plus an explicit
        // sidecar deny is still a deny (D7: the sidecar may narrow).
        let mut s = Config::default();
        s.network = Some(false);
        let merged = merge(
            Config::default(),
            s,
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert!(!merged.network);
    }

    #[test]
    fn omitted_network_in_both_layers_defaults_to_shared() {
        // Neither layer mentions `network`: the shared-by-default of
        // docs/plan.md applies — after the merge, not inside a layer.
        let merged = merge(
            Config::default(),
            Config::default(),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert!(merged.network);

        // And the fourth combination of the explicit table: both layers
        // say `true` — shared, with the guard silent (nothing was
        // re-enabled; the post-merge default already is `true`).
        let mut u = Config::default();
        u.network = Some(true);
        let mut s = Config::default();
        s.network = Some(true);
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert!(merged.network);
    }

    #[test]
    fn workmux_is_off_unless_a_layer_says_so_and_the_sidecar_wins() {
        // docs/design/config.md D16: off by default, either layer may
        // decide, the sidecar wins when both do (like `backend`) —
        // there is no narrowing rule, because the key grants no host
        // access, it only selects the interactive payload.
        let m = |u: Option<bool>, s: Option<bool>| {
            let mut user = Config::default();
            user.workmux = u;
            let mut sidecar = Config::default();
            sidecar.workmux = s;
            merge(user, sidecar, &user_file(), &sidecar_file(), &no_home())
                .unwrap()
                .workmux
        };
        assert!(!m(None, None));
        assert!(m(Some(true), None));
        assert!(m(None, Some(true)));
        // The sidecar may switch it off for one repository, and on
        // where the user config said nothing — both directions, no
        // error.
        assert!(!m(Some(true), Some(false)));
        assert!(m(Some(false), Some(true)));
    }

    #[test]
    fn network_false_narrowing_accepted() {
        let mut u = Config::default();
        u.network = Some(true);
        let mut s = Config::default();
        s.network = Some(false);

        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert!(!merged.network);
    }

    #[test]
    fn network_false_in_both_stays_false() {
        let mut u = Config::default();
        u.network = Some(false);
        let mut s = Config::default();
        s.network = Some(false);

        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert!(!merged.network);
    }

    #[test]
    fn missing_user_config_sidecar_mount_accepted() {
        // The reported scenario: NO user config at all (the empty
        // default layer), a sidecar that mounts an unrelated host
        // directory ro. That is a valid configuration since D7 — the
        // sidecar is trusted and declares mounts by itself.
        let base = tmpdir("no-user");
        let wanted = dir(&base, &["some", "data"]);

        for mode in ["ro", "rw"] {
            let merged = merge(
                Config::default(),
                cfg(&mount_toml(&wanted, mode)),
                &user_file(),
                &sidecar_file(),
                &no_home(),
            )
            .unwrap();
            assert_eq!(merged.mounts.len(), 1);
            assert_eq!(merged.mounts[0].path, wanted.to_string_lossy());
            assert_eq!(
                merged.mounts[0].mode,
                if mode == "ro" { Mode::Ro } else { Mode::Rw }
            );
        }
    }

    #[test]
    fn missing_user_config_env_and_network_deny_allowed() {
        // With an absent user config, env introduction and network denial
        // stay allowed too — the empty user layer restricts nothing.
        let mut s = Config::default();
        s.network = Some(false);
        let merged = merge(
            Config::default(),
            cfg("[env]\nFOO = \"1\"\n"),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.env.get("FOO").unwrap(), "1");

        let merged = merge(
            Config::default(),
            s,
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert!(!merged.network);
        assert!(merged.mounts.is_empty());
    }

    #[test]
    fn a_symlinked_mount_path_is_stored_resolved() {
        // D8: what is mounted is what the path RESOLVES to, in both
        // layers. A symlink is followed before the path reaches
        // `Merged`, so no later stage ever sees the link.
        let base = tmpdir("symlink");
        let real = dir(&base, &["real", "data"]);
        let elsewhere = dir(&base, &["links"]);
        let link = elsewhere.join("data-link");
        std::os::unix::fs::symlink(&real, &link).unwrap();

        let u = cfg(&mount_toml(&link, "rw"));
        let s = cfg(&mount_toml(&link, "ro"));
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[0].path, real.to_string_lossy());
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
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
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
        let merged = merge(
            u,
            Config::default(),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.backend.as_deref(), Some("bubblewrap"));

        let mut s = Config::default();
        s.backend = Some("qemu".to_owned());
        let merged = merge(
            Config::default(),
            s,
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.backend.as_deref(), Some("qemu"));

        let mut u = Config::default();
        u.backend = Some("bubblewrap".to_owned());
        let mut s = Config::default();
        s.backend = Some("microvm".to_owned());
        let merged = merge(u, s, &user_file(), &sidecar_file(), &no_home()).unwrap();
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
            &home,
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
        assert_eq!(layers.sidecar.0.network, Some(false));

        // An XDG_CONFIG_HOME pointing at a directory without a mysbx
        // config: also an empty user layer.
        let layers =
            load_layers(&home, Some(&home.join(".config").to_string_lossy()), &sd).unwrap();
        assert_eq!(layers.user.0, Config::default());
        assert_eq!(layers.sidecar.0.network, Some(false));
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
    fn nested_mounts_of_both_layers_keep_their_modes() {
        // No mode is derived from a surrounding entry any more (D7):
        // each entry carries its own mode into argv, whatever encloses
        // it in either layer. Order decides what wins in the sandbox.
        let base = tmpdir("nested-modes");
        let outer = dir(&base, &["outer"]);
        let secret = dir(&outer, &["secret"]);
        let sub = dir(&secret, &["sub"]);

        let u = cfg(&format!(
            "{}{}",
            mount_toml(&outer, "rw"),
            mount_toml(&secret, "ro")
        ));
        let merged = merge(
            u,
            cfg(&mount_toml(&sub, "rw")),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 3);
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
        assert_eq!(merged.mounts[2].path, sub.to_string_lossy());
        assert_eq!(merged.mounts[2].mode, Mode::Rw);
    }

    // ---- the host home is never mounted (review-3 item 4) ----------

    #[test]
    fn a_tilde_grant_of_the_whole_home_is_refused() {
        // The review's exact case: `path = "~/"` (or an absolute or
        // symlinked spelling of the same tree) would mount the host
        // home while the report still says it is not. The comparison
        // is on canonicalized paths, so every spelling is caught.
        let base = tmpdir("home-exposed");
        let home = dir(&base, &["home"]);
        let u = cfg(
            "[[mounts]]\npath = \"~/\"\nmode = \"ro\"\n",
        );
        let e = merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &home,
        )
        .unwrap_err();
        assert!(
            matches!(e, Error::HomeExposed { relation: HomeRelation::Equal, .. }),
            "{e}"
        );
    }

    #[test]
    fn a_grant_containing_the_home_is_refused() {
        // `path = "/home"` — an ancestor — is the same exposure; the
        // review's report-line claim must survive this spelling too.
        let base = tmpdir("home-contained");
        let home = dir(&base, &["home"]);
        // A REAL host-`/home`-shaped ancestor: the parent of the home.
        let parent = home.parent().unwrap().to_path_buf();
        let u = cfg(&mount_toml(&parent, "ro"));
        let e = merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &home,
        )
        .unwrap_err();
        assert!(
            matches!(e, Error::HomeExposed { relation: HomeRelation::Contains, .. }),
            "{e}"
        );
    }

    #[test]
    fn a_symlink_to_the_home_grant_is_caught_by_canonicalization() {
        // D8's canonicalization is what makes the guard spelling-proof:
        // a symlink pointing AT the home resolves to the home before
        // the comparison — and the guard canonicalizes BOTH sides
        // (see the `home_canon` note in `merge`), so a symlinked
        // `$HOME` is caught too. An absolute link spelling is used
        // here; the `~/` spelling is covered by
        // `a_tilde_grant_of_the_whole_home_is_refused`.
        let base = tmpdir("home-symlink");
        let home = dir(&base, &["home"]);
        let link = base.join("elsewhere");
        std::os::unix::fs::symlink(&home, &link).unwrap();
        let u = cfg(&mount_toml(&link, "ro"));
        let e = merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &home,
        )
        .unwrap_err();
        assert!(
            matches!(e, Error::HomeExposed { relation: HomeRelation::Equal, .. }),
            "{e}"
        );
    }

    #[test]
    fn a_subdirectory_of_the_home_stays_grantable() {
        // The carve-out D6 has always relied on: `~/.config/git` and
        // friends grant subdirectories of the home, not the home. The
        // guard must not refuse the baseline pattern.
        let base = tmpdir("home-subdir");
        let home = dir(&base, &["home"]);
        let sub = dir(&home, &[".config", "git"]);
        let u = cfg(&mount_toml(&sub, "ro"));
        merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &home,
        )
        .unwrap();
    }

    #[test]
    fn a_symlinked_home_value_is_canonicalized_before_the_comparison() {
        // The reviewer hole: `merge` used to compare canonicalized
        // sources against the RAW `$HOME` value. When `$HOME` is a
        // symlink (`/var/usrhome` -> `/home/mhuber`), a `~/` grant
        // canonicalizes to the real directory and the equality
        // failed — the whole-home grant slipped through. The guard
        // now canonicalizes both sides.
        let base = tmpdir("home-raw-symlink");
        let real = dir(&base, &["home", "mhuber"]);
        let alias = base.join("usrhome");
        std::os::unix::fs::symlink(&real, &alias).unwrap();
        let u = cfg(
            "[[mounts]]\npath = \"~/\"\nmode = \"ro\"\n",
        );
        let e = merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            // `$HOME` spells the SYMLINK, not the real directory.
            &alias,
        )
        .unwrap_err();
        assert!(
            matches!(e, Error::HomeExposed { relation: HomeRelation::Equal, .. }),
            "{e}"
        );
    }

    #[test]
    fn a_root_grant_is_refused_as_a_home_exposure() {
        // Review-3 item 4 makes `path = "/"` unsatisfiable for a
        // mount: the host home is always somewhere below it, so the
        // whole-host grant can never pass — the report's "the host
        // home is not mounted" stays true.
        let u = cfg(&mount_toml(Path::new("/"), "ro"));
        let e = merge(
            u,
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap_err();
        assert!(
            matches!(e, Error::HomeExposed { relation: HomeRelation::Contains, .. }),
            "{e}"
        );
    }

    // ---- path resolution (docs/design/config.md D8) -------------------

    #[test]
    fn resolve_path_covers_the_three_spellings() {
        let home = Path::new("/home/u");
        let dir = Path::new("/etc/xdg/mysbx");
        assert_eq!(
            resolve_path("~/.config/git", home, dir),
            PathBuf::from("/home/u/.config/git")
        );
        assert_eq!(
            resolve_path("sub/data", home, dir),
            PathBuf::from("/etc/xdg/mysbx/sub/data")
        );
        assert_eq!(
            resolve_path("../secrets", home, dir),
            PathBuf::from("/etc/xdg/mysbx/../secrets")
        );
        assert_eq!(resolve_path("/abs", home, dir), PathBuf::from("/abs"));
    }

    #[test]
    fn tilde_expands_against_home_in_both_layers() {
        // One home for both layers: there is one invoking user.
        let base = tmpdir("tilde-both");
        let home = dir(&base, &["home", "u"]);
        let granted = dir(&home, &[".config", "git"]);

        let u = cfg("[[mounts]]\npath = \"~/.config/git\"\nmode = \"rw\"\n");
        let s = cfg("[[mounts]]\npath = \"~/.config/git\"\nmode = \"ro\"\n");
        let merged = merge(u, s, &user_file(), &sidecar_file(), &home).unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[0].path, granted.to_string_lossy());
        assert_eq!(merged.mounts[1].path, granted.to_string_lossy());
        assert_eq!(merged.mounts[1].mode, Mode::Ro);
    }

    #[test]
    fn relative_paths_resolve_per_layer() {
        // The key per-layer rule: the same relative string in the two
        // layers resolves against two different directories \u2014 each
        // layer's own config file directory.
        let base = tmpdir("relative-per-layer");
        let user_dir = dir(&base, &["xdg", "mysbx"]);
        let sidecar_dir = dir(&base, &["repo.mysbx"]);
        let user_rel = dir(&user_dir, &["granted"]);
        let sidecar_rel = dir(&sidecar_dir, &["granted"]);
        let ufile = user_dir.join("config.toml");
        let sfile = sidecar_dir.join("config.toml");

        // The same relative string in both layers, resolved against two
        // different directories.
        let u = cfg("[[mounts]]\npath = \"granted\"\nmode = \"rw\"\n");
        let s = cfg("[[mounts]]\npath = \"granted\"\nmode = \"ro\"\n");
        let merged = merge(u, s, &ufile, &sfile, &no_home()).unwrap();
        assert_eq!(merged.mounts[0].path, user_rel.to_string_lossy());
        assert_eq!(
            merged.mounts[1].path,
            sidecar_rel.to_string_lossy(),
            "a sidecar relative path must resolve against the sidecar directory"
        );
    }

    #[test]
    fn sidecar_relative_path_does_not_borrow_the_user_directory() {
        // The negative half of the per-layer rule: a relative path that
        // exists next to the USER config is not found next to the
        // sidecar config, and fails there \u2014 with the sidecar named.
        let base = tmpdir("relative-not-shared");
        let user_dir = dir(&base, &["xdg", "mysbx"]);
        let sidecar_dir = dir(&base, &["repo.mysbx"]);
        let only_user_side = dir(&user_dir, &["only-here"]);
        let ufile = user_dir.join("config.toml");
        let sfile = sidecar_dir.join("config.toml");

        let u = cfg("[[mounts]]\npath = \"only-here\"\nmode = \"rw\"\n");
        let s = cfg("[[mounts]]\npath = \"only-here\"\nmode = \"ro\"\n");
        let e = merge(u, s, &ufile, &sfile, &no_home()).unwrap_err();
        match e {
            Error::Canonicalize {
                ref file,
                ref raw,
                ref path,
                ..
            } => {
                assert_eq!(file, &sfile);
                assert_eq!(raw, "only-here");
                assert_eq!(path, &sidecar_dir.join("only-here"));
                assert_ne!(path, &only_user_side);
            }
            other => panic!("wrong error: {other}"),
        }
        // The message names the path as written AND the resolved one.
        let msg = e.to_string();
        assert!(msg.contains("`only-here`"), "{msg}");
        assert!(msg.contains("resolved to"), "{msg}");
    }

    #[test]
    fn a_sidecar_tilde_path_mounts_what_it_resolves_to() {
        // The `~/` spelling is expanded against the invoking user's
        // home in the sidecar layer too, and the resolved path is what
        // is mounted — no user-config entry involved (D7).
        let base = tmpdir("tilde-sidecar");
        let home = dir(&base, &["home", "u"]);
        let secret = dir(&home, &[".ssh"]);

        let s = cfg("[[mounts]]\npath = \"~/.ssh\"\nmode = \"ro\"\n");
        let merged = merge(Config::default(), s, &user_file(), &sidecar_file(), &home).unwrap();
        assert_eq!(merged.mounts.len(), 1);
        assert_eq!(merged.mounts[0].path, secret.to_string_lossy());
    }

    #[test]
    fn dotdot_is_resolved_to_the_real_target() {
        // `..` is allowed and canonicalizes away; what lands in
        // `Merged` is the real target, inside or outside the sidecar
        // directory alike.
        let base = tmpdir("dotdot");
        let sidecar_dir = dir(&base, &["repo.mysbx"]);
        let state = dir(&base, &["repo.mysbx", "state"]);
        let outside = dir(&base, &["outside"]);
        let sfile = sidecar_dir.join("config.toml");

        let inside = cfg("[[mounts]]\npath = \"../repo.mysbx/state\"\nmode = \"ro\"\n");
        let merged = merge(Config::default(), inside, &user_file(), &sfile, &no_home()).unwrap();
        assert_eq!(merged.mounts[0].path, state.to_string_lossy());

        let escaping = cfg("[[mounts]]\npath = \"../outside\"\nmode = \"ro\"\n");
        let merged = merge(
            Config::default(),
            escaping,
            &user_file(),
            &sfile,
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.mounts[0].path, outside.to_string_lossy());
    }

    #[test]
    fn load_layers_and_merge_resolve_tilde_and_relative() {
        // End-to-end through `load_layers`: the user config uses `~/`,
        // the sidecar a path relative to its own directory.
        let base = tmpdir("load-resolve");
        let home = dir(&base, &["home"]);
        let granted = dir(&home, &["data"]);
        let sd = dir(&base, &["fake-repo.mysbx"]);
        let below = dir(&granted, &["sub"]);

        std::fs::create_dir_all(home.join(".config").join("mysbx")).unwrap();
        std::fs::write(
            home.join(".config").join("mysbx").join("config.toml"),
            "[[mounts]]\npath = \"~/data\"\nmode = \"rw\"\n",
        )
        .unwrap();
        // `../home/data/sub`, relative to `<base>/fake-repo.mysbx/`.
        std::fs::write(
            sd.join("config.toml"),
            "[[mounts]]\npath = \"../home/data/sub\"\nmode = \"ro\"\n",
        )
        .unwrap();

        let layers = load_layers(&home, None, &sd).unwrap();
        let merged = merge(
            layers.user.0,
            layers.sidecar.0,
            &layers.user.1,
            &layers.sidecar.1,
            &home,
        )
        .unwrap();
        assert_eq!(merged.mounts.len(), 2);
        assert_eq!(merged.mounts[0].path, granted.to_string_lossy());
        assert_eq!(merged.mounts[1].path, below.to_string_lossy());
    }

    // ---- git-dirs approval list (review-2 item 1) ------------------------

    #[test]
    fn git_dirs_of_both_layers_are_canonicalized_and_unioned() {
        // Approval may come from either trusted layer: the host-wide
        // user config or the repo's own sidecar (which the sandboxed
        // payload cannot write, config.md D2/D3). Both are
        // canonicalized eagerly (D8), so the argv builder compares
        // symlink-resolved paths on both sides.
        let base = tmpdir("git-dirs-union");
        let a = dir(&base, &["main", ".git"]);
        let b = dir(&base, &["other", ".git"]);

        let merged = merge(
            cfg(&format!("git-dirs = [\"{}\"]\n", a.display())),
            cfg(&format!("git-dirs = [\"{}\"]\n", b.display())),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.git_dirs, vec![a, b]);
    }

    #[test]
    fn a_dangling_git_dirs_entry_is_a_hard_error() {
        // Same D8 treatment as mount paths: an approval that does not
        // resolve is a mistake in the file that wrote it, not a silent
        // no-op.
        let e = merge(
            cfg("git-dirs = [\"/nonexistent/main/.git\"]\n"),
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap_err();
        assert!(
            matches!(&e, Error::Canonicalize { key, .. } if key.starts_with("git-dirs")),
            "{e}"
        );
    }

    #[test]
    fn git_dirs_expand_the_home_prefix() {
        let base = tmpdir("git-dirs-home");
        let home = base.join("home");
        std::fs::create_dir_all(home.join("src").join(".git")).unwrap();
        let merged = merge(
            cfg("git-dirs = [\"~/src/.git\"]\n"),
            cfg(""),
            &user_file(),
            &sidecar_file(),
            &home,
        )
        .unwrap();
        assert_eq!(
            merged.git_dirs,
            vec![std::fs::canonicalize(home.join("src").join(".git")).unwrap()]
        );
    }

    // ---- state-dirs (docs/design/config.md D15) -----------------------

    #[test]
    fn state_dirs_of_both_layers_concatenate_user_first() {
        // D15: both layers declare; the user layer's entries come first
        // (the same layer order as mounts), the sidecar's follow in their
        // declaration order. Nothing is canonicalized — there is no host
        // path to resolve.
        let merged = merge(
            cfg("state-dirs = [\".local/share/opencode\"]\n"),
            cfg("state-dirs = [\".cache/hypothesis\", \".local/state/opencode\"]\n"),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(
            merged.state_dirs,
            vec![
                ".local/share/opencode",
                ".cache/hypothesis",
                ".local/state/opencode",
            ]
        );
    }

    #[test]
    fn state_dir_duplicates_are_dropped_first_occurrence_wins() {
        // Unlike mounts, a repeated entry is not two binds: both layers
        // naming the same entry is one state directory. The FIRST
        // occurrence is kept, so the order stays the declaration order.
        let merged = merge(
            cfg("state-dirs = [\".local/share/opencode\", \".cache/x\"]\n"),
            cfg("state-dirs = [\".cache/x\", \".local/share/opencode\"]\n"),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.state_dirs, vec![".local/share/opencode", ".cache/x"]);
    }

    #[test]
    fn nested_state_dirs_are_kept_as_declared() {
        // A nested entry inside another one (`share/opencode` below
        // `share`) is NOT cleaned up here: both binds are emitted (the
        // argv builder validates the layout), and the operator sees the
        // config they wrote. The useful pattern is one entry per tool
        // (`.local/share/opencode`), which is what the modules write.
        let merged = merge(
            cfg("state-dirs = [\".local/share\"]\n"),
            cfg("state-dirs = [\".local/share/opencode\"]\n"),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert_eq!(merged.state_dirs, vec![".local/share", ".local/share/opencode"]);
    }

    #[test]
    fn an_empty_state_dirs_list_contributes_nothing() {
        // The default is empty, and so is a layer that does not mention
        // the key at all (the common case: no state is persisted).
        let merged = merge(
            cfg(""),
            cfg("state-dirs = []\n"),
            &user_file(),
            &sidecar_file(),
            &no_home(),
        )
        .unwrap();
        assert!(merged.state_dirs.is_empty());
    }
}
