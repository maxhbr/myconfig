// Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! Parse the example configurations in `tests/assets/`.
//!
//! The assets double as documentation: `valid/` files are configurations a
//! user could write, `invalid/` files are the mistakes the strict parser
//! must reject (docs/design/config.md).

use mysbx::config::{Config, Error, Mode};
use std::path::{Path, PathBuf};

fn asset(rel: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/assets")
        .join(rel)
}

fn load_ok(rel: &str) -> Config {
    let path = asset(rel);
    Config::load(&path).unwrap_or_else(|e| panic!("{rel} should parse, but: {e}"))
}

fn load_err(rel: &str) -> Error {
    let path = asset(rel);
    match Config::load(&path) {
        Ok(_) => panic!("{rel} should have been rejected"),
        Err(e) => e,
    }
}

/// Every asset must be covered by a test, so a newly added file cannot sit
/// there unparsed and unnoticed.
#[test]
fn every_asset_is_exercised() {
    let mut found: Vec<String> = Vec::new();
    for dir in ["valid", "invalid"] {
        let mut entries: Vec<_> = std::fs::read_dir(asset(dir))
            .expect("assets dir exists")
            .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        entries.sort();
        for name in entries {
            found.push(format!("{dir}/{name}"));
        }
    }
    let expected = vec![
        "valid/empty.toml",
        "valid/full.toml",
        "valid/minimal.toml",
        "valid/syntax-zoo.toml",
        "valid/user-config.toml",
        "invalid/schema-bad-mode.toml",
        "invalid/schema-missing-mount-path.toml",
        "invalid/schema-relative-path.toml",
        "invalid/schema-unknown-key.toml",
        "invalid/schema-wrong-type.toml",
        "invalid/syntax-duplicate-key.toml",
        "invalid/syntax-missing-value.toml",
        "invalid/syntax-unterminated-string.toml",
    ];
    let mut expected: Vec<String> = expected.into_iter().map(String::from).collect();
    expected.sort();
    found.sort();
    assert_eq!(found, expected, "tests/assets changed: update this test");
}

#[test]
fn empty_config() {
    let c = load_ok("valid/empty.toml");
    assert_eq!(c, Config::default());
}

#[test]
fn minimal_config() {
    let c = load_ok("valid/minimal.toml");
    assert_eq!(c.repo.path.as_deref(), Some("/home/user/src/project"));
    assert_eq!(c.repo.mode, Mode::Rw);
    assert!(c.mounts.is_empty());
    assert!(!c.network, "network is denied unless declared");
}

#[test]
fn full_config() {
    let c = load_ok("valid/full.toml");
    assert_eq!(c.backend.as_deref(), Some("bwrap"));
    assert!(c.network);
    assert_eq!(c.repo.path.as_deref(), Some("/home/user/src/project"));
    assert_eq!(c.repo.mode, Mode::Rw);

    assert_eq!(c.mounts.len(), 2);
    assert_eq!(c.mounts[0].path, "/home/user/.config/pi");
    assert_eq!(c.mounts[0].dest, None);
    assert_eq!(c.mounts[0].mode, Mode::Ro);
    assert_eq!(c.mounts[1].path, "/home/user/.cache/mysbx/project");
    assert_eq!(c.mounts[1].dest.as_deref(), Some("/home/user/.cache"));
    assert_eq!(c.mounts[1].mode, Mode::Rw);

    assert_eq!(c.env.len(), 2);
    assert_eq!(c.env["TERM"], "xterm-256color");
    assert_eq!(c.env["LANG"], "C.UTF-8");
}

#[test]
fn user_config_without_repo_table() {
    let c = load_ok("valid/user-config.toml");
    assert_eq!(c.backend.as_deref(), Some("bwrap"));
    assert_eq!(c.repo.path, None, "the host-wide layer names no repo");
    assert_eq!(c.mounts.len(), 2);
    assert!(c.mounts.iter().all(|m| m.mode == Mode::Ro));
    assert_eq!(c.env["EDITOR"], "nvim");
}

#[test]
fn syntax_zoo() {
    let c = load_ok("valid/syntax-zoo.toml");
    assert_eq!(c.backend.as_deref(), Some("bwrap"));
    assert!(!c.network);
    assert_eq!(
        c.repo.path.as_deref(),
        Some("/home/user/src/pro#ject"),
        "`#` inside a string must not start a comment"
    );
    assert_eq!(c.env["WEIRD KEY"], "a\tb");
    assert_eq!(c.env["LITERAL"], r"no \t escape here");
    assert_eq!(c.env["UNICODE"], "ä");
    assert_eq!(c.env["QUOTED"], "he said \"hi\"");
    assert_eq!(c.mounts.len(), 2);
    assert_eq!(c.mounts[1].dest.as_deref(), Some("/mnt/b"));
}

#[test]
fn invalid_syntax_is_reported_with_a_location() {
    for rel in [
        "invalid/syntax-unterminated-string.toml",
        "invalid/syntax-missing-value.toml",
        "invalid/syntax-duplicate-key.toml",
    ] {
        let e = load_err(rel);
        let msg = e.to_string();
        assert!(msg.contains("invalid TOML"), "{rel}: {msg}");
        assert!(msg.contains("line "), "{rel}: {msg}");
        assert!(
            msg.contains(rel.rsplit('/').next().unwrap()),
            "{rel}: {msg}"
        );
    }
}

#[test]
fn invalid_schema_is_reported_with_the_offending_key() {
    let cases = [
        ("invalid/schema-unknown-key.toml", "unknown key `readonly`"),
        ("invalid/schema-bad-mode.toml", "invalid mode `readwrite`"),
        ("invalid/schema-wrong-type.toml", "expected a boolean"),
        ("invalid/schema-relative-path.toml", "must be absolute"),
        (
            "invalid/schema-missing-mount-path.toml",
            "missing required key `path`",
        ),
    ];
    for (rel, needle) in cases {
        let e = load_err(rel);
        assert!(
            matches!(e, Error::Schema(_)),
            "{rel}: expected a schema error, got {e}"
        );
        assert!(e.to_string().contains(needle), "{rel}: {e}");
    }
}

#[test]
fn missing_file_is_an_io_error() {
    let e = Config::load(&asset("valid/does-not-exist.toml")).unwrap_err();
    assert!(matches!(e, Error::Io(_)), "{e}");
}
