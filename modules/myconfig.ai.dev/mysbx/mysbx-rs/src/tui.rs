// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The session picker (bd myconfig-dm7b, docs/design/tui.md): the
//! full-screen form shown before an interactive launch, one row per
//! decision a one-run override flag already carries. Three rows
//! (T2): backend, multiplexer, default-agent — closed sets drawn
//! from the registries, never free text. Left/Right cycle the
//! focused row, Up/Down move the focus, Enter dispatches into
//! `sandbox()`, Esc or `q` exits `0` (nothing was started). Enter
//! composes a `Flags` where a row the operator never TOUCHED stays
//! `None`, so the configuration layers decide (T4, config.md D6) —
//! the picker adds no decision of its own, and every refusal (a
//! disabled backend, an unpinned multiplexer) surfaces through the
//! pipeline's exit-`70` messages, never a new surface here.
//!
//! Zero new dependencies (T6): raw mode via libc
//! `tcgetattr`/`tcsetattr` with the crate's raw `extern "C"` idiom
//! (lib.rs's `isatty`), rendering through ANSI escapes only. The
//! value layer — the row sets, the seeding, the cycling, the
//! `Flags` composition — is pure and unit-tested without a
//! terminal; only the key loop is impure.
//!
//! Public entry (the dispatcher bead myconfig-dm7b.3 calls this
//! with the repo context it resolved):
//!
//! ```text
//! pub(crate) fn run(flags: Flags, repo: &repo::Repo, mode: RunMode) -> i32
//! ```
//!
//! `0` on Esc/`q` (nothing was started), `sandbox()`'s own code on
//! Enter. The repo/sidecar state check (T5's `mysbx init`
//! confirmation, Enter runs init / any other key aborts) happens
//! INSIDE, and the loop is re-enterable: after `init` runs the same
//! call re-seeds (T3) and renders the rows the fresh sidecar now
//! decides. `RunMode::Once` (T8) skips the D13 gate and seeds from
//! the user layer alone (the sidecar layer is empty by
//! construction, D22); the temp root is `sandbox()`'s Once arm's
//! to create only after Enter — Esc exits `0` before any temp
//! root exists, and the once lifecycle stays entirely the
//! pipeline's.

use crate::config::Multiplexer;
use crate::merge::{self, Merged};
use crate::repo;
use crate::{backends, doctor, AgentSelection, Flags, RunMode, EXIT_INFRASTRUCTURE};
use std::io::{Read, Write};
use std::path::PathBuf;

/// The rows in focus order (T2): each label is the flag family the
/// row stands for (`--backend`, `--multiplexer`, `--agent`).
const ROW_LABELS: [&str; 3] = ["backend", "multiplexer", "default-agent"];

/// The `none` pseudo-value of the agent row — the `--no-agent`
/// spelling (cli.md D32), not a kind.
const AGENT_NONE: &str = "none";

/// One picker row: a closed set of values with one selected (T2).
/// An empty `values` renders the row disabled with `empty_hint`,
/// never hidden — the operator sees the decision does not exist
/// here, instead of discovering the missing choice after the
/// session started.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Row {
    values: Vec<String>,
    /// `None` on a disabled row, and on a row whose merged value no
    /// layer of the set carries (T3): nothing is bracketed, Enter
    /// composes no flag for it, the pipeline's own refusal decides.
    selected: Option<usize>,
    empty_hint: &'static str,
}

impl Row {
    /// The current value's string, or the empty-registry hint when
    /// the row carries no values at all (T2: visible, disabled).
    fn current(&self) -> &str {
        match (self.values.first(), self.selected) {
            (Some(_), Some(i)) => &self.values[i % self.values.len()],
            (Some(v), None) => v,
            (None, _) => self.empty_hint,
        }
    }

    /// Cycle the selection `delta` steps within the set (Left/Right)
    /// and return whether it MOVED — a no-op on a disabled or
    /// single-valued row, which can therefore never be TOUCHED:
    /// Enter leaves its flag `None` and the layers decide (D6),
    /// the pipeline's own refusal included (T4). A first cycle
    /// from an unselected row starts at position 0 and can never
    /// land there (Right → 1, Left → the last) — the skipped
    /// position is the default the merge already showed. Moving is
    /// what makes `to_flags` emit the row's flag: a row the
    /// operator never cycled stays `None` and the layers decide
    /// (T4).
    fn cycle(&mut self, delta: i64) -> bool {
        let n = self.values.len() as i64;
        if n < 2 {
            return false;
        }
        let base = self.selected.unwrap_or(0) as i64;
        let next = (base + delta).rem_euclid(n) as usize;
        if next == base as usize {
            return false;
        }
        self.selected = Some(next);
        true
    }
}

/// The three rows with their closed sets (T2): backend is
/// `backends::NAMES` ∩ the host's enabled set — only what a run on
/// THIS host can start; multiplexer is the config key's universe;
/// default-agent is the build's pinned kinds plus the `none`
/// pseudo-value. Pure: the env reads happen in the caller.
fn rows(enabled: &[&'static str], pinned_agents: &[String]) -> [Row; 3] {
    let backend = Row {
        values: backends::NAMES
            .iter()
            .copied()
            .filter(|n| enabled.contains(n))
            .map(str::to_owned)
            .collect(),
        selected: None,
        empty_hint: "(no backend enabled on this host)",
    };
    let mux = Row {
        values: Multiplexer::NAMES
            .iter()
            .copied()
            .map(str::to_owned)
            .collect(),
        selected: None,
        empty_hint: "(no multiplexer this build carries)",
    };
    let agent = Row {
        values: pinned_agents
            .iter()
            .cloned()
            .chain([AGENT_NONE.to_owned()])
            .collect(),
        selected: None,
        empty_hint: "(no agent pinned on this build)",
    };
    [backend, mux, agent]
}

/// Preselect what a bare `mysbx` would do (T3): the merged values
/// seed the rows, and a key no layer decided seeds the pipeline's
/// default — an omitted `multiplexer` is the plain shell, a
/// `default-agent` that resolves to nothing selects the `none`
/// pseudo-value. The backend row is the one that may end up with
/// NOTHING selected: no layer named one, or the merged value is not
/// in the host's set (a disabled backend, an unpinned agent — the
/// same treatment for every row). Enter then composes no flag and
/// the pipeline's own refusal names the value, instead of the
/// picker guessing (T4).
fn seed(mut rows: [Row; 3], merged: &Merged) -> [Row; 3] {
    let at = |row: &Row, value: &str| row.values.iter().position(|v| v == value);
    rows[0].selected = merged
        .backend
        .as_deref()
        .and_then(|name| at(&rows[0], name));
    rows[1].selected = at(&rows[1], merged.multiplexer.name());
    // The `none` pseudo-value is a POSITION (the last element
    // `rows()` appends), never a name: a pinned kind literally
    // spelled `none` must not shadow the `--no-agent` spelling.
    rows[2].selected = match merged.default_agent.as_deref() {
        Some(kind) => at(&rows[2], kind),
        None => Some(rows[2].values.len() - 1),
    };
    rows
}

/// Which rows the operator touched — set by a cycle that MOVED
/// (T4). An untouched row leaves its flag `None`, so the
/// configuration layers decide (config.md D6); the picker never
/// freezes a decision the configuration owns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Touched([bool; 3]);

/// Compose the rows into the `Flags` a bare run parses (T4): a
/// touched row becomes its one-run override flag, an untouched one
/// stays `None`. This is the picker's whole output — it writes
/// nothing else.
fn to_flags(touched: Touched, rows: &[Row; 3]) -> Flags {
    let mut flags = Flags::default();
    if touched.0[0] {
        if let Some(i) = rows[0].selected {
            flags.backend = Some(rows[0].values[i].clone());
        }
    }
    if touched.0[1] {
        if let Some(i) = rows[1].selected {
            // `parse_cli` is the same mapping the `--multiplexer`
            // flag itself uses (cli.md D14) — the row strings are
            // its spellings.
            flags.multiplexer = rows[1]
                .values
                .get(i)
                .and_then(|v| Multiplexer::parse_cli(v).ok());
        }
    }
    if touched.0[2] {
        if let Some(i) = rows[2].selected {
            // The pseudo-value by index, like `seed` — never by name.
            if i == rows[2].values.len() - 1 {
                flags.agent = Some(AgentSelection::None);
            } else {
                flags.agent = Some(AgentSelection::Kind(rows[2].values[i].clone()));
            }
        }
    }
    flags
}

/// The session picker (T1): the entry the dispatcher calls. Runs
/// the D13 gate (T5, skipped by `RunMode::Once`, T8), loads and
/// merges the layers (T3), then the terminal loop — Enter composes
/// a `Flags` and dispatches into `sandbox()`, Esc/`q` returns `0`.
/// Not a tty on stdin or stdout refuses with `70` suggesting the
/// bare form (T6) — the once form suggests `mysbx-once run -- CMD`
/// instead, because there the bare form IS the picker.
#[allow(dead_code)] // the dispatcher arm (myconfig-dm7b.3) is the first caller
pub(crate) fn run(flags: Flags, repo: &repo::Repo, mode: RunMode) -> i32 {
    // T7/T8's contract: the dispatcher refuses every run-scoped flag
    // with the verb, so the three fields the picker owns are `None`
    // on entry — the overwrite in the Enter arm must never silently
    // swallow a flag a dispatcher bug let through.
    debug_assert!(flags.backend.is_none() && flags.multiplexer.is_none() && flags.agent.is_none());
    let once = matches!(mode, RunMode::Once);
    if !stdin_stdout_tty() {
        if once {
            eprintln!("mysbx-once: the picker needs a terminal on stdin and stdout");
            eprintln!("  use `mysbx-once run -- CMD` for the scripted form");
        } else {
            eprintln!("mysbx: the picker needs a terminal on stdin and stdout");
            eprintln!("  use the bare `mysbx` form — it dispatches straight into the sandbox");
        }
        return EXIT_INFRASTRUCTURE;
    }
    loop {
        // T5: the gate a bare run meets, verbatim. Enter runs the
        // idempotent `mysbx init` flow and the loop re-seeds from the
        // fresh sidecar; any other key aborts to `0` with the repo as
        // it was. Once-mode's temp root carries its own empty config
        // (T8), so the gate does not exist there.
        if !once {
            if let Err(msg) = crate::require_initialized_sidecar(repo) {
                match confirm_not_inited(&msg) {
                    Outcome::RanInit => continue,
                    Outcome::Abort => return 0,
                }
            }
        }
        let merged = match load_merged(repo) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("mysbx: {e}");
                return EXIT_INFRASTRUCTURE;
            }
        };
        let picker = seed(
            rows(&doctor::enabled_backends(), &crate::pinned_agent_names()),
            &merged,
        );
        return match picker_loop(picker, once) {
            Some((touched, rows)) => {
                let mut run_flags = flags;
                let selected = to_flags(touched, &rows);
                run_flags.backend = selected.backend;
                run_flags.multiplexer = selected.multiplexer;
                run_flags.agent = selected.agent;
                // T4: raw mode is already left (the guard dropped with
                // the loop) — the payload owns the terminal from here,
                // and a pipeline refusal surfaces as its own
                // `mysbx: ` message with exit `70`.
                crate::sandbox(run_flags, crate::bwrap::Payload::Shell, mode)
            }
            None => 0,
        };
    }
}

/// What the not-inited confirmation decided (T5).
enum Outcome {
    RanInit,
    Abort,
}

/// T5's confirmation: print the D13 refusal text verbatim, wait for
/// one key — Enter runs the idempotent `mysbx init` flow (its
/// `## created:` lines print), any other key aborts to `0` with the
/// repo as it was.
fn confirm_not_inited(msg: &str) -> Outcome {
    println!("mysbx: {msg}");
    println!("  Enter runs `mysbx init`, any other key aborts");
    let mut stdin = std::io::stdin();
    match (RawMode::enter(), read_key(&mut stdin)) {
        (Some(guard), Key::Enter) => {
            drop(guard);
            let _ = std::io::stdout().flush();
            // `init` re-resolves the repo through `resolve_cwd()`
            // while `run` holds an explicit `repo` — the two agree
            // because the dispatcher resolved the cwd's repo; a
            // future non-cwd caller must keep them tied or the
            // gate and `init` split brains.
            let _ = crate::init(&[]);
            Outcome::RanInit
        }
        _ => Outcome::Abort,
    }
}

/// The merged layers of `repo` (T3): the same load a run performs —
/// `load_layers` + `merge`, the `status` precedent. In once-mode
/// the sidecar layer is empty by construction (T8/D22: the temp
/// root's own `config.toml`, created only by `sandbox()`'s Once
/// arm after Enter), so the user layer alone decides (config.md
/// D6).
fn load_merged(repo: &repo::Repo) -> Result<Merged, String> {
    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    let layers =
        merge::load_layers(&home, xdg.as_deref(), &repo.sidecar).map_err(|e| e.to_string())?;
    merge::merge(
        layers.user.0,
        layers.sidecar.0,
        &layers.user.1,
        &layers.sidecar.1,
        &home,
    )
    .map_err(|e| e.to_string())
}

/// One decoded key of the loop — the subset the picker binds.
#[derive(Debug, PartialEq, Eq)]
enum Key {
    Up,
    Down,
    Left,
    Right,
    Enter,
    Esc,
    Char(u8),
}

/// The one impure part: the tty loop. `Some((touched, rows))` is
/// the Enter state, `None` an Esc/`q` abort (exit `0` — nothing was
/// started). The `RawMode` guard restores the terminal on every
/// exit path, Enter included: it drops HERE, before `run` dispatches.
fn picker_loop(mut rows: [Row; 3], once: bool) -> Option<(Touched, [Row; 3])> {
    let guard = RawMode::enter()?;
    let mut touched = Touched::default();
    let mut focus = 0usize;
    let mut stdin = std::io::stdin();
    loop {
        render(&rows, focus, once);
        let key = read_key(&mut stdin);
        let delta = match key {
            Key::Up => -1,
            Key::Down => 1,
            Key::Left => {
                if rows[focus].cycle(-1) {
                    touched.0[focus] = true;
                }
                continue;
            }
            Key::Right => {
                if rows[focus].cycle(1) {
                    touched.0[focus] = true;
                }
                continue;
            }
            Key::Enter => {
                drop(guard);
                clear_screen();
                return Some((touched, rows));
            }
            // ISIG is off in raw mode, so ^C arrives as byte 3 —
            // bound to the abort like Esc and `q` so the operator
            // can always interrupt (exit `0`, nothing started).
            Key::Esc | Key::Char(b'q') | Key::Char(0x03) => {
                drop(guard);
                clear_screen();
                return None;
            }
            Key::Char(_) => continue,
        };
        focus = ((focus as i64 + delta).rem_euclid(3)) as usize;
    }
}

/// Read and decode one key from a raw terminal: the arrow keys in
/// both spellings — CSI (`\x1b[A`…`\x1b[D`) and SS3 (`\x1bOA`…
/// `\x1bOD`, DECCKM's application cursor keys, left set by a
/// parent line editor, tmux or vim `:sh`) — a bare `\x1b` (Esc),
/// `\r`/`\n` (Enter), every other byte as [`Key::Char`] — `q`
/// and byte 3 are the ones the loop binds. A sequence may arrive
/// split across reads; once an introducer (`[` or `O`) showed up
/// the rest is certainly coming, so one more blocking read is
/// safe — a split BEFORE the introducer (`\x1b` alone) stays Esc,
/// the one ambiguity only a timeout could buy. EOF and io errors
/// decode as Esc: a closed or broken stdin aborts the picker
/// instead of spinning. No `\x1b[?1l` is emitted — emulator state
/// is outside what `tcsetattr` restores.
fn read_key(stdin: &mut impl Read) -> Key {
    let mut buf = [0u8; 3];
    let n = match stdin.read(&mut buf) {
        Ok(n) if n > 0 => n,
        _ => return Key::Esc,
    };
    if buf[0] != 0x1b {
        return match buf[0] {
            b'\r' | b'\n' => Key::Enter,
            other => Key::Char(other),
        };
    }
    if n == 1 || !matches!(buf[1], b'[' | b'O') {
        return Key::Esc;
    }
    let final_byte = if n >= 3 {
        buf[2]
    } else {
        let mut rest = [0u8; 1];
        match stdin.read(&mut rest) {
            Ok(1) => rest[0],
            _ => return Key::Esc,
        }
    };
    match final_byte {
        b'A' => Key::Up,
        b'B' => Key::Down,
        b'C' => Key::Right,
        b'D' => Key::Left,
        _ => Key::Char(0x1b),
    }
}

/// Full-screen ANSI render (T6): a redraw per key is the whole point
/// of three rows — no line editing. The focused row is inverted,
/// the selected value bracketed.
fn render(rows: &[Row; 3], focus: usize, once: bool) {
    let mut out = std::io::stdout();
    let _ = write!(out, "\x1b[2J\x1b[H");
    let verb = if once { "mysbx-once" } else { "mysbx tui" };
    let _ = writeln!(out, "{verb} — pick this session's sandbox\n");
    for (i, row) in rows.iter().enumerate() {
        if i == focus {
            let _ = write!(out, "\x1b[7m>{:<14}\x1b[0m", ROW_LABELS[i]);
        } else {
            let _ = write!(out, " {:<14}", ROW_LABELS[i]);
        }
        for (j, v) in row.values.iter().enumerate() {
            if j > 0 {
                let _ = write!(out, " ");
            }
            if row.selected == Some(j) {
                let _ = write!(out, "[{v}]");
            } else {
                let _ = write!(out, "{v}");
            }
        }
        if row.values.is_empty() {
            // The disabled-row spelling (T2): visible, never hidden —
            // the hint already announces it.
            let _ = write!(out, "{}", row.current());
        }
        let _ = writeln!(out);
    }
    let _ = writeln!(
        out,
        "\n  ←/→ cycle · ↑/↓ move · Enter start · Esc or q abort"
    );
    let _ = out.flush();
}

fn clear_screen() {
    let mut out = std::io::stdout();
    let _ = write!(out, "\x1b[2J\x1b[H");
    let _ = out.flush();
}

/// Whether stdin AND stdout are terminals (T6): the picker reads
/// keys and renders full-screen, so either one missing refuses with
/// `70`. Stdin reuses `lib.rs`'s `stdin_is_tty`; stdout the same raw
/// `extern "C"` idiom.
fn stdin_stdout_tty() -> bool {
    if !crate::stdin_is_tty() {
        return false;
    }
    extern "C" {
        fn isatty(fd: i32) -> i32;
    }
    // SAFETY: `isatty` only inspects the fd, it has no side effects.
    // The return is nonzero on a terminal — not specified to be 1.
    unsafe { isatty(1) != 0 }
}

/// The libc `termios` shape the kernel writes for `tcgetattr` on
/// Linux — opaque bytes to this crate, spelled out for the call.
#[repr(C)]
#[derive(Clone, Copy)]
struct Termios {
    c_iflag: u32,
    c_oflag: u32,
    c_cflag: u32,
    c_lflag: u32,
    c_line: u8,
    c_cc: [u8; 32],
    c_ispeed: u32,
    c_ospeed: u32,
}

extern "C" {
    fn tcgetattr(fd: i32, termios_p: *mut Termios) -> i32;
    fn tcsetattr(fd: i32, optional_actions: i32, termios_p: *const Termios) -> i32;
}

/// Raw mode via libc (T6, zero dependencies): a RAII guard that
/// restores the terminal on EVERY exit path — abort and Enter
/// alike (T4: the payload owns the terminal once the dispatch
/// starts, so raw mode is gone before `sandbox()` runs). The recipe
/// is cfmakeraw's essence on the Linux constants, minus `OPOST`:
/// no echo, no canonical line buffering, no signal generation, one
/// byte per keypress — while `\n` of the renders still maps to CRLF.
struct RawMode {
    saved: Termios,
}

impl RawMode {
    fn enter() -> Option<RawMode> {
        let mut saved = Termios {
            c_iflag: 0,
            c_oflag: 0,
            c_cflag: 0,
            c_lflag: 0,
            c_line: 0,
            c_cc: [0; 32],
            c_ispeed: 0,
            c_ospeed: 0,
        };
        // SAFETY: `tcgetattr` writes only the struct at `saved`.
        if unsafe { tcgetattr(0, &mut saved) } != 0 {
            eprintln!("mysbx: cannot put the terminal in raw mode (tcgetattr)");
            return None;
        }
        let mut raw = saved;
        // bits/termios.h, octal as the headers spell them —
        // cfmakeraw's input half: IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|
        // IGNCR|ICRNL|IXON. cfmakeraw's CS8 half (clear
        // CSIZE|PARENB, set CS8) is deliberately skipped: the
        // terminal already runs 8-bit, and c_cflag is no business
        // of a key loop.
        raw.c_iflag &= !(0o1 | 0o2 | 0o10 | 0o40 | 0o100 | 0o200 | 0o400 | 0o2000);
        // ECHO|ECHONL|ICANON|ISIG|IEXTEN
        raw.c_lflag &= !(0o10 | 0o100 | 0o2 | 0o1 | 0o100000);
        // VMIN=1 (one byte blocks), VTIME=0 (no inter-byte timer).
        raw.c_cc[6] = 1;
        raw.c_cc[5] = 0;
        // SAFETY: `tcsetattr` reads only the struct at `raw`; 0 is
        // TCSANOW — apply immediately.
        if unsafe { tcsetattr(0, 0, &raw) } != 0 {
            eprintln!("mysbx: cannot put the terminal in raw mode (tcsetattr)");
            return None;
        }
        Some(RawMode { saved })
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        // SAFETY: same as `enter`, writing the saved state back.
        unsafe { tcsetattr(0, 0, &self.saved) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    /// A minimal merged config with the three keys the picker reads
    /// overridden — the merge's own defaults elsewhere.
    fn merged(backend: Option<&str>, mux: Multiplexer, agent: Option<&str>) -> Merged {
        let base = crate::merge::merge(
            crate::config::Config::default(),
            crate::config::Config::default(),
            Path::new("/tmp/fake-user.toml"),
            Path::new("/tmp/fake-sidecar.toml"),
            Path::new("/nonexistent-home"),
        )
        .unwrap();
        Merged {
            backend: backend.map(str::to_owned),
            multiplexer: mux,
            default_agent: agent.map(str::to_owned),
            ..base
        }
    }

    fn pins(names: &[&str]) -> Vec<String> {
        names.iter().map(|n| n.to_string()).collect()
    }

    #[test]
    fn the_backend_row_is_the_registry_intersected_with_the_host_gate() {
        // The ∩ keeps the REGISTRY's order and drops what the gate
        // did not name — never a name a run here cannot start.
        let r = rows(&["podman-krun", "zzz-not-a-backend", "nono"], &pins(&[]));
        assert_eq!(
            r[0].values,
            vec!["nono".to_owned(), "podman-krun".to_owned()]
        );
    }

    #[test]
    fn an_empty_backend_registry_renders_one_disabled_row() {
        let r = rows(&["zzz-not-a-backend"], &pins(&[]));
        assert!(r[0].values.is_empty());
        assert_eq!(r[0].selected, None);
        assert_eq!(r[0].current(), "(no backend enabled on this host)");
    }

    #[test]
    fn the_multiplexer_row_lists_the_config_key_universe() {
        let r = rows(&[], &pins(&[]));
        assert_eq!(
            r[1].values,
            Multiplexer::NAMES
                .iter()
                .map(|n| n.to_string())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn the_agent_row_is_the_pins_plus_the_none_pseudo_value() {
        let r = rows(&[], &pins(&["pi", "opencode"]));
        assert_eq!(
            r[2].values,
            vec!["pi".to_owned(), "opencode".to_owned(), "none".to_owned()]
        );
    }

    #[test]
    fn a_build_without_pins_carries_only_the_none_pseudo_value() {
        let r = rows(&[], &pins(&[]));
        assert_eq!(r[2].values, vec!["none".to_owned()]);
    }

    #[test]
    fn seeding_preselects_what_a_bare_run_would_do() {
        let r = rows(&["nono", "bubblewrap"], &pins(&["pi", "opencode"]));
        let seeded = seed(
            r,
            &merged(Some("nono"), Multiplexer::Herdr, Some("opencode")),
        );
        assert_eq!(seeded[0].current(), "nono");
        assert_eq!(seeded[1].current(), "herdr");
        assert_eq!(seeded[2].current(), "opencode");
    }

    #[test]
    fn an_undecided_multiplexer_seeds_the_pipeline_default() {
        // T3: an omitted key is the plain shell — the merged value
        // is already `Multiplexer::None`, and the row selects `none`.
        let r = rows(&[], &pins(&[]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None));
        assert_eq!(seeded[1].current(), "none");
    }

    #[test]
    fn a_pinned_agent_literally_named_none_does_not_shadow_the_pseudo_value() {
        // The pseudo-value is a position (the appended last element),
        // never a name: `none` as a KIND stays selectable and is
        // composed as `--agent none`, while no default resolves to
        // the `--no-agent` spelling.
        let r = rows(&[], &pins(&["none"]));
        assert_eq!(r[2].values, vec!["none".to_owned(), "none".to_owned()]);
        let seeded = seed(r, &merged(None, Multiplexer::None, Some("none")));
        assert_eq!(seeded[2].selected, Some(0));
        let mut touched = Touched::default();
        touched.0[2] = true;
        match to_flags(touched, &seeded).agent {
            Some(crate::AgentSelection::Kind(k)) => assert_eq!(k, "none"),
            other => panic!("wrong agent flag: {other:?}"),
        }
        let r = rows(&[], &pins(&["none"]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None));
        assert_eq!(seeded[2].selected, Some(1));
        assert_eq!(seeded[2].current(), "none");
    }

    #[test]
    fn a_default_agent_that_resolves_to_nothing_preselects_none() {
        let r = rows(&[], &pins(&["pi"]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None));
        assert_eq!(seeded[2].current(), AGENT_NONE);
    }

    #[test]
    fn a_backend_no_layer_names_is_the_row_with_nothing_preselected() {
        let r = rows(&["nono", "bubblewrap"], &pins(&[]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None));
        assert_eq!(seeded[0].selected, None);
        // `current()` of a selected-less row shows the first value
        // unbracketed; Enter composes no backend flag either way
        // (`to_flags` keys on `selected`), so the pipeline's own
        // "no backend configured" refusal decides — the picker never
        // guesses one.
        assert_eq!(seeded[0].current(), "bubblewrap");
    }

    #[test]
    fn a_merged_value_outside_the_row_set_stays_unselected() {
        // A disabled backend, an unpinned agent: the row shows
        // nothing selected and Enter composes no flag — the
        // pipeline's refusal names the value (T4), never the picker.
        let r = rows(&["nono"], &pins(&["pi"]));
        let seeded = seed(r, &merged(Some("krun"), Multiplexer::None, Some("orca")));
        assert_eq!(seeded[0].selected, None);
        assert_eq!(seeded[1].current(), "none");
        assert_eq!(seeded[2].selected, None);
    }

    #[test]
    fn an_untouched_row_leaves_its_flag_unset() {
        let r = rows(&["nono"], &pins(&["pi"]));
        let seeded = seed(r, &merged(Some("nono"), Multiplexer::Tmux, None));
        let flags = to_flags(Touched::default(), &seeded);
        assert_eq!(flags.backend, None);
        assert_eq!(flags.multiplexer, None);
        assert_eq!(flags.agent, None);
    }

    #[test]
    fn a_touched_row_composes_its_flag() {
        let r = rows(&["nono"], &pins(&["pi", "opencode"]));
        let seeded = seed(r, &merged(Some("nono"), Multiplexer::None, None));
        let mut touched = Touched::default();
        touched.0[0] = true;
        touched.0[2] = true;
        let flags = to_flags(touched, &seeded);
        assert_eq!(flags.backend.as_deref(), Some("nono"));
        assert_eq!(flags.multiplexer, None);
        match flags.agent {
            Some(crate::AgentSelection::None) => {}
            other => panic!("wrong agent flag: {other:?}"),
        }
    }

    #[test]
    fn cycling_moves_within_the_closed_set_and_wraps() {
        let r = rows(&["nono", "bubblewrap"], &pins(&["pi", "opencode"]));
        let mut row = r[2].clone();
        assert!(row.cycle(1));
        assert_eq!(row.current(), "opencode");
        assert!(row.cycle(1));
        assert_eq!(row.current(), AGENT_NONE);
        assert!(row.cycle(1));
        // Wraps around the closed set.
        assert_eq!(row.current(), "pi");
        assert!(row.cycle(-1));
        assert_eq!(row.current(), AGENT_NONE);
    }

    #[test]
    fn cycling_a_disabled_or_single_valued_row_moves_nothing() {
        // And a cycle that does not move leaves the row untouched:
        // only a real selection is a decision (T4).
        let r = rows(&["zzz"], &pins(&[]));
        let mut disabled = r[0].clone();
        assert!(!disabled.cycle(1));
        assert_eq!(disabled.selected, None);

        let r = rows(&["nono"], &pins(&[]));
        let mut single = r[0].clone();
        assert!(single.selected.is_none()); // rows() never preselects
        assert!(!single.cycle(1));
        assert_eq!(single.selected, None);
    }

    #[test]
    fn a_cycled_row_composes_the_cycled_value() {
        let r = rows(&[], &pins(&["pi", "opencode"]));
        let mut seeded = seed(r, &merged(None, Multiplexer::None, None));
        assert!(seeded[2].cycle(1)); // none → pi
        assert!(seeded[2].cycle(1)); // pi → opencode
        let mut touched = Touched::default();
        touched.0[2] = true;
        let flags = to_flags(touched, &seeded);
        match flags.agent {
            Some(crate::AgentSelection::Kind(k)) => assert_eq!(k, "opencode"),
            other => panic!("wrong agent flag: {other:?}"),
        }
    }

    #[test]
    fn a_touched_multiplexer_row_maps_the_row_string_to_the_enum() {
        let r = rows(&[], &pins(&[]));
        let mut seeded = seed(r, &merged(None, Multiplexer::None, None));
        // `none` sits last in `Multiplexer::NAMES`; three rights wrap
        // to `herdr`.
        assert!(seeded[1].cycle(3));
        assert_eq!(seeded[1].current(), "herdr");
        let mut touched = Touched::default();
        touched.0[1] = true;
        let flags = to_flags(touched, &seeded);
        assert_eq!(flags.multiplexer, Some(Multiplexer::Herdr));
    }

    #[test]
    fn a_first_cycle_from_an_unselected_row_never_lands_on_the_first_value() {
        // The quirk is deliberate: `cycle` bases on position 0 and
        // skips it (Right → 1, Left → the last), so a first touch can
        // never SELECT the value the merge already showed — it can
        // only compose a decision that differs from the layers'.
        let r = rows(&[], &pins(&["pi", "opencode"]));
        let mut row = r[2].clone();
        row.selected = None;
        assert!(row.cycle(1));
        assert_eq!(row.current(), "opencode");
        let mut row = r[2].clone();
        row.selected = None;
        assert!(row.cycle(-1));
        assert_eq!(row.current(), AGENT_NONE);
    }

    #[test]
    fn a_touched_row_without_a_selection_composes_no_flag() {
        // The undecided-backend case: the operator cannot make a
        // choice a disabled registry does not offer, so the flag
        // stays `None` and the pipeline's refusal decides.
        let r = rows(&[], &pins(&[]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None));
        let mut touched = Touched::default();
        touched.0[0] = true;
        let flags = to_flags(touched, &seeded);
        assert_eq!(flags.backend, None);
    }

    /// Feed `bytes` to `read_key` through a `Cursor`, so the decode
    /// needs no terminal.
    fn key(bytes: &[u8]) -> Key {
        read_key(&mut std::io::Cursor::new(bytes.to_vec()))
    }

    #[test]
    fn csi_arrow_keys_decode_to_their_arrows() {
        assert_eq!(key(b"\x1b[A"), Key::Up);
        assert_eq!(key(b"\x1b[B"), Key::Down);
        assert_eq!(key(b"\x1b[C"), Key::Right);
        assert_eq!(key(b"\x1b[D"), Key::Left);
    }

    #[test]
    fn ss3_arrow_keys_decode_to_their_arrows() {
        // DECCKM's application cursor keys (`\x1bO A`…): a parent
        // line editor, tmux or vim `:sh` can leave the mode set —
        // failing to bind them turns every arrow into an abort.
        assert_eq!(key(b"\x1bOA"), Key::Up);
        assert_eq!(key(b"\x1bOB"), Key::Down);
        assert_eq!(key(b"\x1bOC"), Key::Right);
        assert_eq!(key(b"\x1bOD"), Key::Left);
    }

    #[test]
    fn an_escape_without_a_final_byte_decodes_as_esc() {
        assert_eq!(key(b"\x1b"), Key::Esc);
        // `\x1b\x1b`: the second byte is no introducer.
        assert_eq!(key(b"\x1b\x1b"), Key::Esc);
    }

    #[test]
    fn a_split_sequence_waits_for_the_rest_of_it() {
        // The pty may deliver the introducer pair and the final byte
        // in separate reads — once the introducer showed up the rest
        // is certainly coming, so the re-read decides instead of an
        // abort. `Chunks` serves its bytes one chunk per read; a
        // reader that dies after the introducer still decodes as Esc
        // (a lone `\x1b` alone cannot be told from Esc without a
        // timeout, and the picker buys none).
        struct Chunks {
            chunks: Vec<Vec<u8>>,
        }
        impl Read for Chunks {
            fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                match self.chunks.pop() {
                    Some(chunk) => {
                        buf[..chunk.len()].copy_from_slice(&chunk);
                        Ok(chunk.len())
                    }
                    None => Ok(0),
                }
            }
        }
        fn split(chunks: &[&[u8]]) -> Key {
            read_key(&mut Chunks {
                chunks: chunks.iter().rev().map(|c| c.to_vec()).collect(),
            })
        }
        assert_eq!(split(&[b"\x1b[", b"D"]), Key::Left);
        assert_eq!(split(&[b"\x1bO", b"A"]), Key::Up);
        // The introducer with nothing behind it: the re-read hits
        // EOF and the whole prefix decodes as Esc.
        assert_eq!(split(&[b"\x1b["]), Key::Esc);
        assert_eq!(split(&[b"\x1bO"]), Key::Esc);
    }

    #[test]
    fn carriage_returns_and_line_feeds_decode_as_enter() {
        assert_eq!(key(b"\r"), Key::Enter);
        assert_eq!(key(b"\n"), Key::Enter);
    }

    #[test]
    fn plain_bytes_decode_as_chars() {
        assert_eq!(key(b"q"), Key::Char(b'q'));
        // ISIG is off, so ^C is a plain byte the loop binds.
        assert_eq!(key(b"\x03"), Key::Char(0x03));
        assert_eq!(key(b"x"), Key::Char(b'x'));
    }

    #[test]
    fn an_unknown_sequence_final_byte_decodes_as_a_bare_escape_char() {
        // Not Esc — a `\x1b[5…` CSI the loop does not bind must not
        // abort; it decodes as the raw byte the loop ignores.
        assert_eq!(key(b"\x1b[5"), Key::Char(0x1b));
        assert_eq!(key(b"\x1bOZ"), Key::Char(0x1b));
    }

    #[test]
    fn eof_and_io_errors_decode_as_esc() {
        // A closed or broken stdin aborts the picker instead of
        // spinning on zero-byte reads.
        assert_eq!(key(b""), Key::Esc);
        struct Broken;
        impl Read for Broken {
            fn read(&mut self, _: &mut [u8]) -> std::io::Result<usize> {
                Err(std::io::Error::new(std::io::ErrorKind::Other, "broken"))
            }
        }
        assert_eq!(read_key(&mut Broken), Key::Esc);
    }
}
