// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! `mysbx diagnose egress` — read the `network = "proxy-only"` broker's
//! decision log back (docs/design/egress.md E1/E4, bd myconfig-n8kv.1.4).
//!
//! The broker writes one JSON object per decision to
//! `<sidecar>/egress/<pid>/decisions.jsonl`; the payload cannot reach
//! that file (it is outside the directory the sandbox binds). This verb
//! is the other half of that design: what the broker recorded must be
//! readable afterwards, or the log is a write-only artefact.
//!
//! The verb only READS. It starts no sandbox, mutates nothing and needs no
//! pins, so it works on a host where the forwarder is stopped — which is
//! exactly when someone runs it.
//!
//! ## What it reports
//!
//! Per run (newest first): the pid, the first `listen` line (the channel
//! it opened), how many connections were admitted, how many hit an
//! unreachable upstream or an I/O failure, and — with `--verbose` — the
//! last few lines verbatim. `--json` prints the same as one JSON object
//! per run, so a script can consume it.
//!
//! The exit code is `0` whether or not there is anything to report: like
//! `status`, this verb reports, it does not judge. A missing sidecar is
//! `0` with a line saying so; a sidecar whose log directory cannot be read
//! is exit `70` (a real I/O failure, not an empty answer).

use std::path::{Path, PathBuf};

/// How many trailing decision lines `--verbose` shows per run.
const TRAILING_LINES: usize = 10;

/// One parsed decision line.
#[derive(Debug, PartialEq, Eq)]
pub struct Decision {
    pub ts: Option<u64>,
    pub pid: Option<u32>,
    pub event: String,
    pub why: String,
}

/// One run's log.
#[derive(Debug, PartialEq, Eq)]
pub struct RunLog {
    pub pid: u32,
    pub path: PathBuf,
    pub decisions: Vec<Decision>,
}

impl RunLog {
    fn count(&self, event: &str) -> usize {
        self.decisions.iter().filter(|d| d.event == event).count()
    }

    /// The channel the run opened, from its first `listen` line.
    fn channel(&self) -> Option<&str> {
        self.decisions
            .iter()
            .find(|d| d.event == "listen")
            .map(|d| d.why.as_str())
    }
}

pub fn run(args: &[String], verbose_flag: bool) -> i32 {
    let mut json = false;
    let mut only: Option<u32> = None;
    // `--verbose` is a GLOBAL flag: the global parser stops at the verb, so
    // `mysbx diagnose egress --verbose` arrives here in the sub-verb's own
    // argv while `mysbx --verbose diagnose egress` arrives as the flag.
    let mut verbose = verbose_flag;
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--json" => {
                json = true;
                i += 1;
            }
            "--verbose" => {
                verbose = true;
                i += 1;
            }
            // `--dry-run` is accepted and ignored: the verb has no side
            // effects, so there is nothing to preview (the `status`
            // precedent). Refusing it would suggest a side effect that
            // does not exist.
            "--dry-run" => i += 1,
            "--run" => {
                let raw = match args.get(i + 1) {
                    Some(raw) => raw,
                    None => {
                        eprintln!("mysbx diagnose egress: `--run` needs a pid");
                        return 2;
                    }
                };
                match raw.parse::<u32>() {
                    Ok(pid) => only = Some(pid),
                    Err(e) => {
                        eprintln!("mysbx diagnose egress: `--run {raw}` is not a pid: {e}");
                        return 2;
                    }
                }
                i += 2;
            }
            other => {
                eprintln!("mysbx diagnose egress: unexpected argument: {other}");
                eprintln!("{USAGE}");
                return 2;
            }
        }
    }
    let repo = match crate::repo::resolve_cwd() {
        Ok(repo) => repo,
        Err(e) => {
            eprintln!("mysbx: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let log_root = repo.sidecar.join("egress");
    let logs = match read_logs(&log_root) {
        Ok(logs) => logs,
        Err(e) => {
            eprintln!("mysbx diagnose egress: {e}");
            return crate::EXIT_INFRASTRUCTURE;
        }
    };
    let selected: Vec<&RunLog> = logs
        .iter()
        .filter(|log| only.map(|pid| log.pid == pid).unwrap_or(true))
        .collect();
    if json {
        for log in &selected {
            println!("{}", render_json(log));
        }
        return 0;
    }
    if selected.is_empty() {
        match only {
            Some(pid) => println!(
                "no egress decisions recorded for run {pid} (nothing at {}/ — a \
                 network = \"proxy-only\" run with a broker writes there)",
                log_root.display()
            ),
            None => println!(
                "no egress decisions recorded (nothing at {}); a network = \
                 \"proxy-only\" run with the per-run broker writes its decisions \
                 there",
                log_root.display()
            ),
        }
        return 0;
    }
    for (index, log) in selected.iter().enumerate() {
        if index > 0 {
            println!();
        }
        print_run(log, verbose);
    }
    0
}

/// The usage the verb prints on a bad command line.
pub const USAGE: &str =
    "usage: mysbx diagnose egress [--run <pid>] [--json] [--verbose] [--dry-run]";

fn print_run(log: &RunLog, verbose: bool) {
    println!(
        "run {pid}  ({path})",
        pid = log.pid,
        path = log.path.display()
    );
    match log.channel() {
        Some(channel) => println!("  channel:      {channel}"),
        None => println!("  channel:      (the log carries no listen line)"),
    }
    println!(
        "  decisions:    {total} ({allow} admissions, {unreachable} unreachable, {io} io, {fatal} fatal)",
        total = log.decisions.len(),
        allow = log.count("allow"),
        unreachable = log.count("unreachable"),
        io = log.count("io"),
        fatal = log.count("fatal"),
    );
    if log.count("unreachable") > 0 {
        println!(
            "  note:         the upstream could not be reached — the model proxy \
             (litellm.service) or the forwarder may be down"
        );
    }
    if !verbose {
        return;
    }
    let start = log.decisions.len().saturating_sub(TRAILING_LINES);
    for decision in &log.decisions[start..] {
        println!(
            "  {ts} {event}  {why}",
            ts = decision
                .ts
                .map(|ts| ts.to_string())
                .unwrap_or_else(|| "-".to_owned()),
            event = decision.event,
            why = decision.why,
        );
    }
}

/// One run as a single JSON object: what `--json` prints.
fn render_json(log: &RunLog) -> String {
    format!(
        "{{\"pid\":{pid},\"log\":{log_path},\"channel\":{channel},\
         \"decisions\":{total},\"allow\":{allow},\"unreachable\":{unreachable},\
         \"io\":{io},\"fatal\":{fatal}}}",
        pid = log.pid,
        log_path = crate::result::quote(&log.path.to_string_lossy()),
        channel = match log.channel() {
            Some(channel) => crate::result::quote(channel),
            None => "null".to_owned(),
        },
        total = log.decisions.len(),
        allow = log.count("allow"),
        unreachable = log.count("unreachable"),
        io = log.count("io"),
        fatal = log.count("fatal"),
    )
}

/// Read every `<pid>/decisions.jsonl` under `root`, newest pid first.
/// A missing root is an empty list (no run has written one yet); any
/// other read failure is an error the caller reports.
fn read_logs(root: &Path) -> Result<Vec<RunLog>, String> {
    let entries = match std::fs::read_dir(root) {
        Ok(entries) => entries,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(e) => return Err(format!("cannot read {}: {e}", root.display())),
    };
    let mut logs = Vec::new();
    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let Ok(pid) = name.parse::<u32>() else {
            continue;
        };
        let path = entry.path().join("decisions.jsonl");
        let text = match std::fs::read_to_string(&path) {
            Ok(text) => text,
            // A run directory whose log was never written (the broker
            // refused before it opened one) carries no decisions.
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => continue,
            Err(e) => return Err(format!("cannot read {}: {e}", path.display())),
        };
        logs.push(RunLog {
            pid,
            decisions: text.lines().filter_map(parse_line).collect(),
            path,
        });
    }
    logs.sort_by(|a, b| b.pid.cmp(&a.pid));
    Ok(logs)
}

/// Parse one JSONL line. The broker writes a flat object of scalars, so a
/// full JSON parser is not needed; a line that does not parse is SKIPPED
/// rather than failing the verb (a truncated final line from a killed
/// broker is expected).
pub fn parse_line(line: &str) -> Option<Decision> {
    let body = line.trim().strip_prefix('{')?.strip_suffix('}')?;
    let mut ts = None;
    let mut pid = None;
    let mut event = None;
    let mut why = None;
    for field in split_fields(body) {
        let (key, value) = field.split_once(':')?;
        let key = key.trim();
        let value = value.trim();
        match key {
            "\"ts\"" => ts = value.parse::<u64>().ok(),
            "\"pid\"" => pid = value.parse::<u32>().ok(),
            "\"event\"" => event = unquote(value),
            "\"why\"" => why = unquote(value),
            _ => {}
        }
    }
    Some(Decision {
        ts,
        pid,
        event: event?,
        why: why.unwrap_or_default(),
    })
}

/// Split a flat JSON object body at the commas that are NOT inside a
/// string, so a `why` containing a comma survives.
fn split_fields(body: &str) -> Vec<&str> {
    let mut fields = Vec::new();
    let mut start = 0;
    let mut in_string = false;
    let mut escaped = false;
    for (index, byte) in body.bytes().enumerate() {
        if escaped {
            escaped = false;
            continue;
        }
        match byte {
            b'\\' if in_string => escaped = true,
            b'"' => in_string = !in_string,
            b',' if !in_string => {
                fields.push(&body[start..index]);
                start = index + 1;
            }
            _ => {}
        }
    }
    fields.push(&body[start..]);
    fields
}

/// A JSON string value with its escapes resolved. Anything that is not a
/// quoted string yields `None`, which the caller treats as absent.
fn unquote(value: &str) -> Option<String> {
    let inner = value.strip_prefix('"')?.strip_suffix('"')?;
    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        match chars.next()? {
            '"' => out.push('"'),
            '\\' => out.push('\\'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            'u' => {
                let hex: String = chars.by_ref().take(4).collect();
                let code = u32::from_str_radix(&hex, 16).ok()?;
                out.push(char::from_u32(code)?);
            }
            other => out.push(other),
        }
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_listen_line_parses() {
        let line = r#"{"ts":1794583068,"pid":9671,"event":"listen","why":"/run/user/1000/mysbx-egress/9669/litellm.sock -> 127.0.0.1:4777, confined by Landlock to that one port"}"#;
        let decision = parse_line(line).expect("parses");
        assert_eq!(decision.ts, Some(1794583068));
        assert_eq!(decision.pid, Some(9671));
        assert_eq!(decision.event, "listen");
        assert!(decision.why.contains("-> 127.0.0.1:4777"));
    }

    #[test]
    fn a_comma_inside_the_reason_survives() {
        let line =
            r#"{"ts":1,"pid":2,"event":"io","why":"pid 3 uid 0: cannot split the client, twice"}"#;
        let decision = parse_line(line).expect("parses");
        assert_eq!(decision.event, "io");
        assert_eq!(decision.why, "pid 3 uid 0: cannot split the client, twice");
    }

    #[test]
    fn escaped_quotes_and_newlines_round_trip() {
        let line = r#"{"ts":1,"pid":2,"event":"deny","why":"said \"no\"\nthen left"}"#;
        let decision = parse_line(line).expect("parses");
        assert_eq!(decision.why, "said \"no\"\nthen left");
    }

    #[test]
    fn a_truncated_or_foreign_line_is_skipped_not_fatal() {
        // The shape a broker killed mid-write leaves behind.
        assert!(parse_line(r#"{"ts":1,"pid":2,"event":"all"#).is_none());
        assert!(parse_line("not json at all").is_none());
        assert!(parse_line("{}").is_none());
    }

    #[test]
    fn the_counts_and_the_channel_come_from_the_lines() {
        let log = RunLog {
            pid: 7,
            path: PathBuf::from("/x/decisions.jsonl"),
            decisions: vec![
                Decision {
                    ts: Some(1),
                    pid: Some(7),
                    event: "listen".to_owned(),
                    why: "/run/user/1000/mysbx-egress/7/litellm.sock -> 127.0.0.1:4000".to_owned(),
                },
                Decision {
                    ts: Some(2),
                    pid: Some(7),
                    event: "allow".to_owned(),
                    why: "pid 9 uid 0 -> 127.0.0.1:4000".to_owned(),
                },
                Decision {
                    ts: Some(3),
                    pid: Some(7),
                    event: "allow".to_owned(),
                    why: "pid 10 uid 0 -> 127.0.0.1:4000".to_owned(),
                },
                Decision {
                    ts: Some(4),
                    pid: Some(7),
                    event: "unreachable".to_owned(),
                    why: "pid 11 uid 0 -> 127.0.0.1:4000: cannot connect".to_owned(),
                },
            ],
        };
        assert_eq!(log.count("allow"), 2);
        assert_eq!(log.count("unreachable"), 1);
        assert_eq!(log.count("deny"), 0);
        assert_eq!(
            log.channel(),
            Some("/run/user/1000/mysbx-egress/7/litellm.sock -> 127.0.0.1:4000")
        );
        let json = render_json(&log);
        assert!(json.starts_with("{\"pid\":7,"), "{json}");
        assert!(json.contains("\"allow\":2"), "{json}");
        assert!(json.contains("\"unreachable\":1"), "{json}");
        assert!(json.ends_with('}'), "{json}");
    }

    #[test]
    fn a_missing_log_root_is_an_empty_list_not_an_error() {
        let root = std::env::temp_dir().join(format!("mysbx-diagnose-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(read_logs(&root).unwrap(), Vec::new());
    }

    #[test]
    fn runs_are_listed_newest_first_and_bad_names_skipped() {
        let root = std::env::temp_dir().join(format!("mysbx-diagnose-runs-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&root);
        for name in ["11", "99", "notapid"] {
            let dir = root.join(name);
            std::fs::create_dir_all(&dir).unwrap();
            std::fs::write(
                dir.join("decisions.jsonl"),
                r#"{"ts":1,"pid":1,"event":"listen","why":"x"}"#,
            )
            .unwrap();
        }
        // A pid directory whose broker refused before opening a log.
        std::fs::create_dir_all(root.join("50")).unwrap();
        let logs = read_logs(&root).unwrap();
        let _ = std::fs::remove_dir_all(&root);
        assert_eq!(
            logs.iter().map(|log| log.pid).collect::<Vec<_>>(),
            vec![99, 11]
        );
    }
}
