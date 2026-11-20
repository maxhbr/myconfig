// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `profile` — the start-time launcher enforcement (bd myconfig-011r,
//! config.md D30): when the merged configuration names a profile, the
//! run is refused unless the invocation actually IS that profile's
//! launcher.
//!
//! The NixOS module's `profiles.<name>` builds second wrappers
//! `mysbx-<name>` with a different tool closure (bd myconfig-xl93);
//! until now the sidecar could not say "this repo runs under the
//! assistant launcher" — the operator just had to pick it. The
//! `profile = "<name>"` config key makes the choice a runtime
//! contract: the merged config names the launcher, and a run started
//! through any OTHER launcher (the base `mysbx` included) is refused
//! with exit `70` before anything is created.
//!
//! Invocation identity (the `once.rs` lesson, src/once.rs lines
//! 6-8, 40-55): argv[0] does not survive a makeBinaryWrapper chain —
//! each `exec` resets it to the literal program path — so the
//! wrapper pins `MYSBX_PROFILE` on every launcher it builds (the
//! base pins nothing). The env pin is the PRIMARY detection; the
//! argv[0] `mysbx-<name>` parse is the FALLBACK for the unwrapped
//! crate, where argv[0] really is the symlink the operator typed.
//! Under once-mode (`once::active()`) only the env pin counts: the
//! once entry of a flavor is `mysbx-<profile>-once`, whose argv[0]
//! would parse as a profile of THAT name — a misparse, so the parse
//! is skipped and a flavor's once run passes the gate on the pin
//! alone.
//!
//! This is a consistency contract, not a security boundary: the
//! invoking user can set `MYSBX_PROFILE` themselves (the same trust
//! level as `MYSBX_ONCE`). What the gate protects is the accident
//! barrier (config.md D9) — a repo configured for the assistant
//! launcher does not silently run with the base closure — while the
//! actual tool closure is the wrapper's embedded `MYSBX_TOOLS_PATH`
//! either way: pinning the env var on the base wrapper changes
//! nothing about what runs inside the sandbox.
//!
//! `MYSBX_PROFILE` is unrelated to `MYSBX_NONO_PROFILE` (a nono
//! operator knob naming the backend's own profile file): the two
//! share a word, nothing else.

use crate::EXIT_INFRASTRUCTURE;

/// The env pin the Nix wrapper sets on every flavor launcher
/// (bd myconfig-011r, config.md D30): `MYSBX_PROFILE = <name>`. The
/// base `mysbx` wrapper pins nothing, so an unset variable means
/// "the base launcher" — exactly the identity the gate compares the
/// configured profile against. An empty value counts as unset, the
/// same treatment every `MYSBX_*` pin gets.
pub const PROFILE_ENV: &str = "MYSBX_PROFILE";

/// The argv[0] prefix of a profile launcher: `mysbx-<name>`, the
/// binary the Nix package installs beside `mysbx`. Only consulted
/// for the UNWRAPPED crate (a makeBinaryWrapper chain resets argv[0]
/// to the literal program path) and never under once-mode (a
/// flavor's once entry `mysbx-<profile>-once` would misparse — the
/// `once` name itself is refused by the grammar, but `<profile>-once`
/// is not, so the parse is skipped outright under once-mode).
const PROFILE_ARGV0_PREFIX: &str = "mysbx-";

/// Whether `name` is a valid profile spelling (config.md D30): the
/// same grammar the module layer's `validProfileName` uses —
/// `[a-z][a-z0-9-]*`, lowercase alphanumeric with internal hyphens —
/// plus the `once` exclusion (the base once entry's argv[0] would
/// collide). Shared by the schema validation (config.rs) and the
/// argv[0] parse below, so the two can never disagree about what a
/// profile name is.
pub fn valid_name(name: &str) -> bool {
    name != "once"
        && !name.is_empty()
        && name.chars().next().is_some_and(|c| c.is_ascii_lowercase())
        && name
            .chars()
            .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '-')
}

/// Which profile launcher THIS invocation is, if any: the
/// `MYSBX_PROFILE` env pin first (empty = unset), then — only when
/// once-mode is NOT active — the argv[0] basename parse
/// `mysbx-<name>` with the profile grammar. `None` means the base
/// launcher (or an unparseable spelling, which counts as the base:
/// a name outside the grammar cannot be a configured profile either,
/// so admitting it changes nothing the gate decides).
pub fn invoked() -> Option<String> {
    if let Some(raw) = std::env::var_os(PROFILE_ENV) {
        if !raw.is_empty() {
            // The pin is the wrapper's own — trusted spelling, but
            // validated anyway so a hand-set garbage value cannot
            // pose as a profile it cannot be.
            let name = raw.to_string_lossy().into_owned();
            return if valid_name(&name) { Some(name) } else { None };
        }
    }
    if crate::once::active() {
        // A flavor's once entry is `mysbx-<profile>-once` — argv[0]
        // would parse as a profile named `<profile>-once` or worse;
        // the pin alone is the identity under once-mode.
        return None;
    }
    from_argv0(&std::env::args_os().next().unwrap_or_default())
}

/// The pure argv[0] half of [`invoked`]: parse a profile name out of
/// the basename `mysbx-<name>`, with the profile grammar. The
/// `mysbx-once` spelling needs no extra exclusion: its basename
/// suffix is `once`, which [`valid_name`] refuses (the reserved
/// name, shared with the schema validation so the two can never
/// disagree). Unit-testable without touching the process's own
/// argv.
pub fn from_argv0(arg: &std::ffi::OsStr) -> Option<String> {
    let name = std::path::Path::new(arg).file_name()?.to_str()?;
    let rest = name.strip_prefix(PROFILE_ARGV0_PREFIX)?;
    if !valid_name(rest) {
        return None;
    }
    Some(rest.to_owned())
}

/// The refusal of the profile gate (config.md D30): `mysbx: …` on
/// stderr naming the configured profile, the launcher it requires
/// and the one the invocation actually was (a mismatch names both),
/// exit `70` (cli.md D8 — the command line was fine, the
/// configuration demands a launcher this invocation is not). Printed
/// by the run pipeline at step 4b, so a `--dry-run` audits the
/// refusal too.
pub fn refuse(configured: &str, invoked: Option<&str>) -> i32 {
    match invoked {
        Some(name) => {
            eprintln!(
                "mysbx: this configuration requires the `mysbx-{configured}` launcher \
                 (profile = \"{configured}\"), but the run started through `mysbx-{name}`"
            );
        }
        None => {
            eprintln!(
                "mysbx: this configuration requires the `mysbx-{configured}` launcher \
                 (profile = \"{configured}\"), but the run started through plain `mysbx`"
            );
        }
    }
    eprintln!(
        "  start the run with `mysbx-{configured}`, or remove/comment the `profile` key \
         in the sidecar config"
    );
    EXIT_INFRASTRUCTURE
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_name_accepts_the_grammar() {
        assert!(valid_name("assistant"));
        assert!(valid_name("a"));
        assert!(valid_name("a-b-c"));
        assert!(valid_name("agent-42"));
    }

    #[test]
    fn valid_name_refuses_the_non_grammar() {
        assert!(!valid_name(""));
        assert!(!valid_name("Assistant"));
        assert!(!valid_name("assistant "));
        assert!(!valid_name("a b"));
        assert!(!valid_name("-x"));
        assert!(!valid_name("x_"));
        assert!(!valid_name("once"));
        assert!(!valid_name("1x"));
    }

    #[test]
    fn from_argv0_parses_a_profile_launcher() {
        assert_eq!(
            from_argv0(std::ffi::OsStr::new("/nix/store/bin/mysbx-assistant")),
            Some("assistant".to_owned())
        );
        assert_eq!(
            from_argv0(std::ffi::OsStr::new("mysbx-agent-42")),
            Some("agent-42".to_owned())
        );
    }

    #[test]
    fn from_argv0_refuses_the_non_launchers() {
        // The base binary and the once entry are not profile
        // launchers.
        assert_eq!(from_argv0(std::ffi::OsStr::new("mysbx")), None);
        assert_eq!(from_argv0(std::ffi::OsStr::new("/bin/mysbx-once")), None);
        // Neither is a misspelling the grammar refuses.
        assert_eq!(from_argv0(std::ffi::OsStr::new("mysbx-")), None);
        assert_eq!(from_argv0(std::ffi::OsStr::new("mysbx-Upper")), None);
        assert_eq!(from_argv0(std::ffi::OsStr::new("mysbx-a b")), None);
    }

    #[test]
    fn a_flavor_once_entry_parses_as_its_own_name_and_once_mode_skips_argv0() {
        // `mysbx-assistant-once` parses as the profile name
        // `assistant-once` — a name the grammar accepts. That is
        // harmless in practice: once-mode never reaches the argv[0]
        // parse (`invoked` consults the env pin only under
        // `once::active()`), and no wrapper installs a NON-once
        // binary with an `-once` suffix. The case is pinned so a
        // future edit of the exclusion list knows the trade-off.
        assert_eq!(
            from_argv0(std::ffi::OsStr::new("mysbx-assistant-once")),
            Some("assistant-once".to_owned())
        );
    }
}
