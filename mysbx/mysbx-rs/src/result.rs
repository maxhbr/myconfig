// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The machine-readable result of a one-shot run (docs/design/cli.md
//! D8, extended by bd myconfig-0ql).
//!
//! `mysbx run --result -- CMD...` does not `exec` the backend: it
//! starts it as a child, waits for its outcome, records that outcome
//! in `<sidecar>/result.json` and exits by the *interpreted* contract
//! (`0` completed, `1` failed, `124` timed out, `130`/`143` cancelled,
//! `70` infrastructure error) instead of passing the payload's own
//! code through. A batch driver therefore gets one closed set from
//! the run itself and the payload's exact fate from the file.
//!
//! The result is a FILE in the sidecar, not JSON on stdout, because
//! stdout is the payload's (cli.md D9): a driver capturing the run's
//! stdout must find exactly what the payload printed, so the pointer
//! to the file goes to stderr and nothing else is added. The sidecar
//! is also the one place the payload cannot write (config.md D7 —
//! and the policy-file-writable and state-tree-writable refusals of
//! the argv builder keep it that way), so a result found there is a
//! result mysbx wrote.
//!
//! This module is pure: the caller collects the outcome (spawn,
//! wait, timeout, cancellation) and hands it over as plain data, so
//! the serialization is unit-testable byte for byte. Zero
//! dependencies, like the rest of the crate: the JSON is written by
//! hand, the timestamps are computed from the Unix epoch with the
//! usual days-to-civil-date algorithm (Howard Hinnant's
//! `civil_from_days`).

use std::time::{SystemTime, UNIX_EPOCH};

/// The name of the result file inside the sidecar. One file per
/// repository: a waited run atomically REPLACES it, so the sidecar
/// cannot grow without bound and the file always names the LATEST
/// run. A driver that wants history copies the file after each run —
/// the pointer line on stderr (cli.md D9) tells it where.
pub const FILE_NAME: &str = "result.json";

/// The schema version of the result file, the `"version"` field: a
/// future format change bumps this and a driver that does not know
/// the number refuses the file instead of misreading it.
pub const SCHEMA_VERSION: i64 = 1;

/// How a one-shot run ended (cli.md D8). The state decides the exit
/// code, so the two can never disagree: both come from this enum.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum State {
    /// The payload exited `0`.
    Completed,
    /// The payload exited non-zero, or died by a signal.
    Failed,
    /// The `--timeout` budget was exhausted and the backend was
    /// killed.
    TimedOut,
    /// `mysbx` itself received `signum` (SIGINT/SIGTERM) while
    /// waiting; it killed the backend and recorded this result.
    Cancelled {
        /// The signal number that cancelled the run (2 = SIGINT,
        /// 15 = SIGTERM).
        signum: i32,
    },
    /// The run never really happened: the backend could not be
    /// started or waited for. Earlier refusals (an unresolved repo,
    /// an uninitialized sidecar, a broken configuration) write NO
    /// file at all — there was nothing to run and nothing to record.
    InfrastructureError,
}

impl State {
    pub fn as_str(&self) -> &'static str {
        match self {
            State::Completed => "completed",
            State::Failed => "failed",
            State::TimedOut => "timed-out",
            State::Cancelled { .. } => "cancelled",
            State::InfrastructureError => "infrastructure-error",
        }
    }

    /// The exit code this state maps to (cli.md D8): `1` failed,
    /// `124` timed out, `130`/`143` cancelled by SIGINT/SIGTERM (the
    /// shell's `128 + signum` convention), `70` infrastructure.
    pub fn exit_code(&self) -> i32 {
        match self {
            State::Completed => 0,
            State::Failed => 1,
            State::TimedOut => 124,
            State::Cancelled { signum } => 128 + signum,
            State::InfrastructureError => 70,
        }
    }
}

/// One finished run, as plain data — pure, so [`render`] is testable
/// byte for byte. The field set is closed and ordered; a consumer of
/// the file never has to guess which optional keys a state carries:
///
/// - always present: `version`, `state`, `exitCode`, `repo`,
///   `sidecar`, `payload`, `startedAt`, `finishedAt`, `durationMs`
/// - `completed`/`failed`: the payload's own fate —
///   `payloadExitCode`, or `payloadSignal` when it died by one
/// - `timed-out`: `timeoutSec` (the exhausted budget)
/// - `cancelled`: `cancelledBy` (the signal mysbx received)
/// - `infrastructure-error`: `error` (the `mysbx: ` diagnosis)
/// - `timeoutSec` is also present on a cancelled run that had a
///   budget — it was given, so it is recorded.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Record {
    pub state: State,
    pub repo: String,
    pub sidecar: String,
    pub payload: Vec<String>,
    /// Seconds since the Unix epoch (see [`epoch_secs`]).
    pub started_at: u64,
    pub finished_at: u64,
    pub duration_ms: u64,
    pub timeout: Option<u64>,
    /// The code the backend (and through it, the payload) exited
    /// with — present only when it ran to completion.
    pub payload_exit: Option<i32>,
    /// The signal the backend process died by, as `SIG…` — present
    /// only when the payload ran and died by one.
    pub payload_signal: Option<String>,
    /// The `mysbx: ` diagnosis of an infrastructure error.
    pub error: Option<String>,
}

impl Record {
    /// The exit code of the run this record describes — derived from
    /// the state, never stored separately, so the file and the
    /// process status can never disagree.
    pub fn exit_code(&self) -> i32 {
        self.state.exit_code()
    }
}

/// Render `r` as the `result.json` text: pretty-printed with two
/// spaces, a fixed key order, one trailing newline. The order is
/// fixed so byte comparisons (tests, driver caches) are stable, and
/// the file ends with a newline like every text file a POSIX tool
/// writes.
pub fn render(r: &Record) -> String {
    let mut fields: Vec<String> = Vec::new();
    let mut s = |v: String| fields.push(v);
    s(format!("  \"version\": {SCHEMA_VERSION}"));
    s(format!("  \"state\": {}", quote(r.state.as_str())));
    s(format!("  \"exitCode\": {}", r.exit_code()));
    s(format!("  \"repo\": {}", quote(&r.repo)));
    s(format!("  \"sidecar\": {}", quote(&r.sidecar)));
    s(format!(
        "  \"payload\": [{}]",
        r.payload
            .iter()
            .map(|a| quote(a))
            .collect::<Vec<_>>()
            .join(", ")
    ));
    s(format!(
        "  \"startedAt\": {}",
        quote(&iso8601(r.started_at))
    ));
    s(format!(
        "  \"finishedAt\": {}",
        quote(&iso8601(r.finished_at))
    ));
    s(format!("  \"durationMs\": {}", r.duration_ms));
    if let Some(secs) = r.timeout {
        s(format!("  \"timeoutSec\": {secs}"));
    }
    if let Some(code) = r.payload_exit {
        s(format!("  \"payloadExitCode\": {code}"));
    }
    if let Some(signal) = &r.payload_signal {
        s(format!("  \"payloadSignal\": {}", quote(signal)));
    }
    if let State::Cancelled { signum } = &r.state {
        s(format!(
            "  \"cancelledBy\": {}",
            quote(&signal_name(*signum))
        ));
    }
    if let Some(error) = &r.error {
        s(format!("  \"error\": {}", quote(error)));
    }
    format!("{{\n{}\n}}\n", fields.join(",\n"))
}

/// A minimal JSON string encoder: the two characters JSON REQUIRES
/// escaping (`"`, `\`) plus the control characters, spelled the
/// short way where JSON has a short form and `\u00xx` otherwise.
/// Non-ASCII passes through untouched — the file is UTF-8, and the
/// paths and payload arguments it carries are `String`s already.
pub fn quote(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len() + 2);
    out.push('"');
    for c in raw.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            '\u{08}' => out.push_str("\\b"),
            '\u{0c}' => out.push_str("\\f"),
            c if (c as u32) < 0x20 => {
                out.push_str(&format!("\\u{:04x}", c as u32));
            }
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

/// `SystemTime` as seconds since the Unix epoch — the number
/// [`iso8601`] formats and the record stores. The clock is only ever
/// read after 1970 in practice; a time before the epoch clamps to `0`
/// rather than panicking in a failure path that still has a result to
/// write.
pub fn epoch_secs(t: SystemTime) -> u64 {
    t.duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Seconds since the Unix epoch as ISO-8601 UTC, second precision:
/// `1970-01-01T00:00:00Z`. The `Z` is the timezone — the result is
/// always UTC, so a driver never needs the writer's zone to order two
/// runs. Hand-rolled (`civil_from_days`, the standard
/// proleptic-Gregorian conversion), the crate has no dependencies.
pub fn iso8601(secs: u64) -> String {
    let days = (secs / 86_400) as i64;
    let rem = secs % 86_400;
    let (year, month, day) = civil_from_days(days);
    format!(
        "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}Z",
        year,
        month,
        day,
        rem / 3_600,
        (rem % 3_600) / 60,
        rem % 60
    )
}

/// Days since 1970-01-01 to (year, month, day) — Howard Hinnant's
/// `civil_from_days`: the proleptic Gregorian calendar in O(1) with no
/// tables, valid past 2100 (unlike a `year/4`-style shortcut).
fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097; // [0, 146096]
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365; // [0, 1459]
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100); // [0, 365]
    let mp = (5 * doy + 2) / 153; // [0, 11]
    let d = doy - (153 * mp + 2) / 5 + 1; // [1, 31]
    let m = if mp < 10 { mp + 3 } else { mp - 9 }; // [1, 12]
    (if m <= 2 { y + 1 } else { y }, m, d)
}

/// A signal number as `SIG…`, the spelling a human recognizes. The
/// table covers the signals a backend can realistically die by; an
/// unknown number stays honest as `SIG<n>` instead of being guessed.
pub fn signal_name(signum: i32) -> String {
    match signum {
        1 => "SIGHUP".into(),
        2 => "SIGINT".into(),
        3 => "SIGQUIT".into(),
        4 => "SIGILL".into(),
        6 => "SIGABRT".into(),
        9 => "SIGKILL".into(),
        11 => "SIGSEGV".into(),
        13 => "SIGPIPE".into(),
        15 => "SIGTERM".into(),
        n => format!("SIG{n}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn completed_record() -> Record {
        Record {
            state: State::Completed,
            repo: "/home/u/repo".into(),
            sidecar: "/home/u/repo.mysbx".into(),
            payload: vec!["ls".into(), "-l".into()],
            started_at: 0,
            finished_at: 2,
            duration_ms: 2_000,
            timeout: None,
            payload_exit: Some(0),
            payload_signal: None,
            error: None,
        }
    }

    #[test]
    fn iso8601_of_known_epochs() {
        // 1970-01-01T00:00:00Z
        assert_eq!(iso8601(0), "1970-01-01T00:00:00Z");
        // 2000-02-29T00:00:00Z — 2000 IS a leap year (divisible by 400).
        assert_eq!(iso8601(951_782_400), "2000-02-29T00:00:00Z");
        // 2100-03-01T00:00:00Z — 2100 is NOT a leap year (divisible by
        // 100, not by 400): the day before is 2100-02-28.
        assert_eq!(iso8601(4_107_542_400), "2100-03-01T00:00:00Z");
        // A time of day: 23:59:59 of 2020-12-31.
        assert_eq!(iso8601(1_609_459_199), "2020-12-31T23:59:59Z");
    }

    #[test]
    fn quote_escapes_the_json_mandatory_set() {
        assert_eq!(quote("plain"), "\"plain\"");
        assert_eq!(quote("a\"b"), "\"a\\\"b\"");
        assert_eq!(quote("a\\b"), "\"a\\\\b\"");
        assert_eq!(quote("a\nb\tc"), "\"a\\nb\\tc\"");
        // The rest of C0: no short form, so \u00xx.
        assert_eq!(quote("\u{01}"), "\"\\u0001\"");
    }

    #[test]
    fn the_state_decides_the_exit_code() {
        assert_eq!(State::Completed.exit_code(), 0);
        assert_eq!(State::Failed.exit_code(), 1);
        assert_eq!(State::TimedOut.exit_code(), 124);
        // The shell convention: 128 + signum.
        assert_eq!(State::Cancelled { signum: 2 }.exit_code(), 130);
        assert_eq!(State::Cancelled { signum: 15 }.exit_code(), 143);
        assert_eq!(State::InfrastructureError.exit_code(), 70);
    }

    #[test]
    fn a_completed_run_renders_byte_for_byte() {
        assert_eq!(
            render(&completed_record()),
            "{\n  \"version\": 1,\n  \"state\": \"completed\",\n  \"exitCode\": 0,\
             \n  \"repo\": \"/home/u/repo\",\n  \"sidecar\": \"/home/u/repo.mysbx\",\
             \n  \"payload\": [\"ls\", \"-l\"],\n  \"startedAt\": \"1970-01-01T00:00:00Z\",\
             \n  \"finishedAt\": \"1970-01-01T00:00:02Z\",\n  \"durationMs\": 2000,\
             \n  \"payloadExitCode\": 0\n}\n"
        );
    }

    #[test]
    fn a_signalled_run_carries_the_signal_not_a_code() {
        let r = Record {
            state: State::Failed,
            payload_exit: None,
            payload_signal: Some("SIGTERM".into()),
            error: None,
            timeout: None,
            ..completed_record()
        };
        let text = render(&r);
        assert!(text.contains("\"state\": \"failed\""), "{text}");
        assert!(text.contains("\"exitCode\": 1"), "{text}");
        assert!(text.contains("\"payloadSignal\": \"SIGTERM\""), "{text}");
        assert!(!text.contains("payloadExitCode"), "{text}");
    }

    #[test]
    fn a_cancelled_run_names_its_signal_and_keeps_the_budget() {
        let r = Record {
            state: State::Cancelled { signum: 2 },
            payload_exit: None,
            payload_signal: None,
            error: None,
            timeout: Some(300),
            ..completed_record()
        };
        let text = render(&r);
        assert!(text.contains("\"state\": \"cancelled\""), "{text}");
        assert!(text.contains("\"exitCode\": 130"), "{text}");
        assert!(text.contains("\"cancelledBy\": \"SIGINT\""), "{text}");
        assert!(text.contains("\"timeoutSec\": 300"), "{text}");
        assert!(!text.contains("payloadExitCode"), "{text}");
        assert!(!text.contains("payloadSignal"), "{text}");
    }

    #[test]
    fn a_timed_out_run_records_no_payload_fate() {
        // The payload was killed BY the timeout — its own exit status
        // is ours, not its own, so the file does not claim one.
        let r = Record {
            state: State::TimedOut,
            payload_exit: None,
            payload_signal: None,
            error: None,
            timeout: Some(1),
            ..completed_record()
        };
        let text = render(&r);
        assert!(text.contains("\"state\": \"timed-out\""), "{text}");
        assert!(text.contains("\"exitCode\": 124"), "{text}");
        assert!(text.contains("\"timeoutSec\": 1"), "{text}");
        assert!(!text.contains("payloadExitCode"), "{text}");
    }

    #[test]
    fn an_infrastructure_error_carries_the_diagnosis() {
        let r = Record {
            state: State::InfrastructureError,
            payload_exit: None,
            payload_signal: None,
            error: Some("cannot exec bwrap: not found".into()),
            timeout: None,
            ..completed_record()
        };
        let text = render(&r);
        assert!(
            text.contains("\"state\": \"infrastructure-error\""),
            "{text}"
        );
        assert!(text.contains("\"exitCode\": 70"), "{text}");
        assert!(
            text.contains("\"error\": \"cannot exec bwrap: not found\""),
            "{text}"
        );
        assert!(!text.contains("payloadExitCode"), "{text}");
    }

    #[test]
    fn paths_and_payload_arguments_are_json_escaped() {
        let r = Record {
            repo: "/weird/\"repo\"".into(),
            payload: vec!["echo".into(), "a\"b\\c".into()],
            ..completed_record()
        };
        let text = render(&r);
        assert!(text.contains("\"repo\": \"/weird/\\\"repo\\\"\""), "{text}");
        assert!(text.contains("\"a\\\"b\\\\c\""), "{text}");
    }

    #[test]
    fn unknown_signals_stay_numeric_not_guessed() {
        assert_eq!(signal_name(15), "SIGTERM");
        assert_eq!(signal_name(32), "SIG32");
    }
}
