// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Home seeding: `/home/agent` from the activated home-manager generation
//! (docs/spec.md §11).
//!
//! Seeding must never abort `start`: the `home-files` tree regularly contains
//! symlinks whose `/nix/store` target is gone, so entries are copied
//! dereferenced (`cp -RL --no-preserve=mode` semantics) and failures are
//! reported, not fatal. Files are copies, not symlinks, because the sandbox
//! has no `/nix`.

use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::error::{die, log, warn};

/// `${XDG_STATE_HOME:-$HOME/.local/state}`.
fn state_home() -> PathBuf {
    match std::env::var("XDG_STATE_HOME") {
        Ok(v) if !v.is_empty() => PathBuf::from(v),
        _ => {
            let home = match std::env::var("HOME") {
                Ok(h) if !h.is_empty() => h,
                _ => "/".to_string(),
            };
            PathBuf::from(home).join(".local").join("state")
        }
    }
}

/// `$USER`, falling back to `id -un` (exec'd, like the bash original).
fn user_name() -> String {
    if let Ok(u) = std::env::var("USER") {
        if !u.is_empty() {
            return u;
        }
    }
    match std::process::Command::new("id").arg("-un").output() {
        Ok(o) if o.status.success() => String::from_utf8_lossy(&o.stdout).trim().to_string(),
        _ => String::new(),
    }
}

/// Resolve the activated home-manager generation's `home-files` tree:
/// `$XDG_STATE_HOME/home-manager/gcroots/current-home` …
/// `/nix/var/nix/gcroots/per-user/$USER/current-home` …
/// `$XDG_STATE_HOME/nix/profiles/home-manager` …
/// `/nix/var/nix/profiles/per-user/$USER/home-manager`, first hit wins,
/// canonicalized (docs/spec.md §11).
pub fn resolve_home_files() -> Option<PathBuf> {
    let user = user_name();
    let sh = state_home();
    let bases = [
        sh.join("home-manager/gcroots/current-home"),
        PathBuf::from(format!("/nix/var/nix/gcroots/per-user/{user}/current-home")),
        sh.join("nix/profiles/home-manager"),
        PathBuf::from(format!("/nix/var/nix/profiles/per-user/{user}/home-manager")),
    ];
    for base in &bases {
        let files = base.join("home-files");
        if !files.exists() {
            continue;
        }
        return fs::canonicalize(&files).ok();
    }
    None
}

/// One `cp` diagnostic, indented under the incomplete-copy warning.
fn cp_error(src: &Path, e: &std::io::Error) -> String {
    let reason = match e.kind() {
        std::io::ErrorKind::NotFound => "No such file or directory".to_string(),
        _ => e.to_string(),
    };
    format!("cp: cannot stat '{}': {reason}", src.display())
}

/// `cp -RL --no-preserve=mode` for one entry: copy dereferenced, recursing
/// into directories; per-file failures are collected, never fatal.
fn cp_recursive(src: &Path, dst: &Path, errs: &mut Vec<String>) {
    let meta = match fs::metadata(src) {
        // `cp -RL` dereferences: metadata() follows the symlink.
        Ok(m) => m,
        Err(e) => {
            errs.push(cp_error(src, &e));
            return;
        }
    };
    if meta.is_dir() {
        if let Err(e) = fs::create_dir_all(dst) {
            errs.push(cp_error(src, &e));
            return;
        }
        let mut entries: Vec<PathBuf> = match fs::read_dir(src) {
            Ok(rd) => rd.filter_map(|en| en.ok()).map(|en| en.path()).collect(),
            Err(e) => {
                errs.push(cp_error(src, &e));
                return;
            }
        };
        entries.sort();
        for entry in entries {
            let name = entry.file_name().unwrap_or_default();
            cp_recursive(&entry, &dst.join(name), errs);
        }
        return;
    }
    match fs::read(src) {
        Ok(bytes) => {
            if let Err(e) = fs::write(dst, &bytes) {
                errs.push(cp_error(src, &e));
            }
        }
        Err(e) => errs.push(cp_error(src, &e)),
    }
}

/// `chmod -R u+rwX`: add u+rw everywhere, u+x only to directories and files
/// that already have an execute bit.
fn chmod_recursive(path: &Path) {
    use std::os::unix::fs::PermissionsExt;
    fn apply(path: &Path) {
        let Ok(meta) = fs::metadata(path) else {
            return;
        };
        let mode = meta.permissions().mode();
        let mut new = mode;
        new |= 0o600; // u+rw
        if meta.is_dir() || (mode & 0o111) != 0 {
            new |= 0o100; // u+X
        }
        if new != mode {
            let mut p = meta.permissions();
            p.set_mode(new);
            let _ = fs::set_permissions(path, p);
        }
        if meta.is_dir() {
            if let Ok(rd) = fs::read_dir(path) {
                let mut entries: Vec<PathBuf> =
                    rd.filter_map(|en| en.ok()).map(|en| en.path()).collect();
                entries.sort();
                for e in entries {
                    apply(&e);
                }
            }
        }
    }
    apply(path);
}

/// Copy the allowlisted paths from `seed` into `home`, dereferenced, modes
/// cleared then `chmod -R u+rwX` applied; logs the summary line and every
/// warning. Never fails (bash `seed_home`).
pub fn seed_home(env: &crate::state::Env, home: &Path, seed: &Path) {
    let paths: Vec<String> = env
        .home_seed_paths
        .iter()
        .filter(|p| !p.is_empty())
        .cloned()
        .collect();
    if paths.is_empty() {
        return;
    }
    let mut copied = 0;
    let mut incomplete = 0;
    for rel in &paths {
        let src = seed.join(rel);
        // `[[ ! -e "$seed/$rel" ]]`: follows symlinks, so only DANGLING
        // ones (or absent paths) land here; tell the two apart.
        if !src.exists() {
            if let Ok(target) = fs::read_link(&src) {
                warn(&format!(
                    "skipping dangling seed path {rel} -> {}",
                    target.display()
                ));
            }
            continue;
        }
        if let Some(parent) = Path::new(rel).parent() {
            let _ = fs::create_dir_all(home.join(parent));
        }
        copied += 1;
        let mut errs: Vec<String> = Vec::new();
        cp_recursive(&src, &home.join(rel), &mut errs);
        if !errs.is_empty() {
            incomplete += 1;
            warn(&format!(
                "seed path {rel} copied incompletely (broken links in the home-manager generation):"
            ));
            let mut stderr = std::io::stderr();
            for e in &errs {
                let _ = writeln!(stderr, "  {e}");
            }
        }
    }
    // Store files are read-only; the agent must be able to rewrite its config.
    chmod_recursive(home);
    let suffix = if incomplete > 0 {
        format!(" ({incomplete} of them incomplete)")
    } else {
        String::new()
    };
    log(&format!(
        "seeded /home/agent with {copied} path(s) from {}{suffix}",
        seed.display()
    ));
    rewrite_seeded_home(env, home);
}

/// Parse one `OLD=NEW` rule; `Err` carries the `die` message.
pub fn parse_rewrite_rule(rule: &str) -> Result<(String, String), String> {
    match rule.split_once('=') {
        Some((from, to)) if !from.is_empty() => Ok((from.to_string(), to.to_string())),
        _ => Err(format!(
            "invalid home-seed rewrite rule (expected OLD=NEW): {rule}"
        )),
    }
}

/// Collect files under `root` containing `needle` (fixed-string), skipping
/// binaries (any NUL byte) and symlinked dirs — `grep -rIlZF` semantics.
fn grep_files(root: &Path, needle: &[u8], out: &mut Vec<PathBuf>) {
    let Ok(rd) = fs::read_dir(root) else {
        return;
    };
    let mut entries: Vec<PathBuf> = rd.filter_map(|en| en.ok()).map(|en| en.path()).collect();
    entries.sort();
    for path in entries {
        let Ok(meta) = fs::symlink_metadata(&path) else {
            continue;
        };
        if meta.file_type().is_symlink() {
            // `grep -r` does not follow symlinks found during traversal.
            continue;
        }
        if meta.is_dir() {
            grep_files(&path, needle, out);
        } else if let Ok(bytes) = fs::read(&path) {
            if bytes.contains(&0) {
                continue; // -I: binary files are skipped
            }
            if bytes.windows(needle.len().max(1)).any(|w| w == needle) {
                out.push(path);
            }
        }
    }
}

/// Apply the literal `OLD=NEW` rewrite rules (binary files skipped, rewritten
/// files end with exactly one newline, `grep -rIlZF` semantics); `die`s on
/// an invalid rule, logs the change count.
pub fn rewrite_seeded_home(env: &crate::state::Env, home: &Path) {
    let rules: Vec<String> = env
        .home_seed_rewrite
        .iter()
        .filter(|r| !r.is_empty())
        .cloned()
        .collect();
    if rules.is_empty() {
        return;
    }
    let mut changed = 0;
    for rule in &rules {
        let (from, to) = match parse_rewrite_rule(rule) {
            Ok(r) => r,
            Err(msg) => die(&msg),
        };
        let mut files: Vec<PathBuf> = Vec::new();
        grep_files(home, from.as_bytes(), &mut files);
        for file in files {
            let text = fs::read_to_string(&file).unwrap_or_default();
            // `$(<file)` strips ALL trailing newlines, the
            // printf re-adds exactly one.
            let stripped = text.trim_end_matches('\n');
            let rewritten = stripped.replace(&from, &to);
            let _ = fs::write(&file, format!("{rewritten}\n"));
            changed += 1;
        }
    }
    if changed > 0 {
        log(&format!(
            "applied {changed} host-endpoint rewrite(s) to the seeded configuration"
        ));
    }
}
