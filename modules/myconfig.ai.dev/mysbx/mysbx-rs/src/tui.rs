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
//! Terminal stewardship (the review fixes, same bead): the picker
//! BORROWS the caller's terminal and leaves it exactly as it was,
//! on every path —
//!
//! * the **alternate screen** (`\x1b[?1049h` in, `\x1b[?1049l` out)
//!   keeps the caller's scrollback: a picker that merely printed
//!   over the primary screen would destroy it. The mode is entered
//!   WITHOUT capability detection — a `~$` cursor-position probe
//!   would need a second blocking read with a timeout of its own.
//!   Instead the clear and the mode switch COMPOSE: the entry
//!   writes `?1049h` FIRST and clears AFTER, so the clear lands on
//!   the alternate screen a capable terminal just switched to (the
//!   fresh buffer) while an incapable one clears the primary —
//!   today's behavior. The exit clears BEFORE `?1049l`, so a
//!   capable terminal discards a cleared alternate buffer and
//!   restores the untouched primary, while an incapable one keeps
//!   exactly the old clear-on-exit. No terminal is left with
//!   picker leftovers, no capable one loses its scrollback.
//! * **raw mode** is a RAII guard again (termios restore on every
//!   drop, Enter included — the payload owns the terminal from
//!   `sandbox()`'s first byte, T4).
//! * **catchable termination** (SIGTERM, SIGINT, SIGHUP) is caught
//!   while the picker owns the TTY: the handler restores the saved
//!   termios and the alternate-screen leave sequence, then restores
//!   the default disposition and re-raises, so the exit status stays
//!   the killed-by-signal one and the shell's terminal survives the
//!   kill. The dispositions are restored to `SIG_DFL` before the
//!   Enter dispatch, so `sandbox()`'s payload inherits a clean slate.
//!
//! Public entry (the dispatcher bead myconfig-dm7b.3 calls this
//! with the repo context it resolved):
//!
//! ```text
//! pub(crate) fn run(flags: Flags, repo: &repo::Repo, mode: RunMode) -> i32
//! ```
//!
//! `0` on Esc/`q` (nothing was started), `sandbox()`'s own code on
//! Enter, `70` when the terminal itself is unusable (no tty, or the
//! raw-mode setup failed — an infrastructure failure, never an
//! abort-shaped `0`). The repo/sidecar state check (T5's `mysbx
//! init` confirmation, Enter runs init / any other key aborts)
//! happens INSIDE, and the loop is re-enterable: after `init` runs
//! the same call re-seeds (T3) and renders the rows the fresh sidecar
//! now decides — and only on SUCCESS; a failed init propagates its
//! own exit code after the one Enter, with no second prompt and no
//! re-seed. `RunMode::Once` (T8) skips the D13 gate and seeds from
//! the user layer alone (the sidecar layer is empty by construction,
//! D22); the temp root is `sandbox()`'s Once arm's to create only
//! after Enter — Esc exits `0` before any temp root exists, and the
//! once lifecycle stays entirely the pipeline's.

use crate::config::Multiplexer;
use crate::merge::{self, Merged};
use crate::repo;
use crate::sig;
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
/// Not a tty on stdin or stdout — or a terminal whose raw mode
/// cannot be set — refuses with `70` suggesting the bare form (T6)
/// — the once form suggests `mysbx-once run -- CMD` instead,
/// because there the bare form IS the picker.
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
                    Ok(()) => continue, // init succeeded — re-seed from the fresh sidecar
                    Err(code) => return code,
                }
            }
        }
        let merged = match load_merged(repo, once) {
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
            Ok(Some((touched, rows))) => {
                let mut run_flags = flags;
                let selected = to_flags(touched, &rows);
                run_flags.backend = selected.backend;
                run_flags.multiplexer = selected.multiplexer;
                run_flags.agent = selected.agent;
                // T4: raw mode is already left and the alternate
                // screen left — the display restore completed before
                // the dispatch, so the payload owns the terminal from
                // `sandbox()`'s first byte, and a pipeline refusal
                // surfaces as its own `mysbx: ` message with exit
                // `70`.
                crate::sandbox(run_flags, crate::bwrap::Payload::Shell, mode)
            }
            // Esc/q: nothing was started.
            Ok(None) => 0,
            // The terminal itself is broken — an infrastructure
            // refusal (`70`), never the abort's `0`.
            Err(()) => EXIT_INFRASTRUCTURE,
        };
    }
}

/// T5's confirmation: print the D13 refusal text verbatim, wait for
/// one key — Enter runs the idempotent `mysbx init` flow (its
/// `## created:` lines print), any other key aborts to `0` with the
/// repo as it was. `Ok(())` means init SUCCEEDED and the loop
/// re-seeds from the fresh sidecar; `Err(code)` is the outcome to
/// propagate instead: `0` for the abort, or — a FAILED init — its
/// own exit code, after this one Enter, with no second prompt, no
/// re-seed and no launch. A terminal whose raw mode cannot be set
/// is an infrastructure refusal (`70`), never a silent abort.
fn confirm_not_inited(msg: &str) -> Result<(), i32> {
    println!("mysbx: {msg}");
    println!("  Enter runs `mysbx init`, any other key aborts");
    let guard = RawMode::enter().ok_or(EXIT_INFRASTRUCTURE)?;
    // ONE reader for the prompt — but its queue DIES with it at
    // the return, and that is DELIBERATE: bytes already read off
    // fd 0 (a key-repeat Enter, a burst behind the aborting key)
    // must not leak past the confirm boundary into the re-seeded
    // picker — a second Enter from a held key would dispatch in
    // the picker before the operator saw it render.
    let mut reader = tty_key_reader();
    let key = read_key(&mut reader);
    match key {
        Key::Enter => {
            drop(guard);
            let _ = std::io::stdout().flush();
            // `init` re-resolves the repo through `resolve_cwd()`
            // while `run` holds an explicit `repo` — the two agree
            // because the dispatcher resolved the cwd's repo; a
            // future non-cwd caller must keep them tied or the
            // gate and `init` split brains.
            let code = crate::init(&[]);
            match code {
                0 => Ok(()),
                // The failure propagated, not re-seeded: a sidecar
                // the operator could not create (an unwritable
                // parent, a permissions disaster) must surface with
                // init's own exit code, once — never a second prompt
                // over the same broken state.
                code => Err(code),
            }
        }
        // ISIG is off in raw mode, so ^C arrives as byte 3 — bound
        // to the abort like Esc and `q`, so the operator can always
        // interrupt the confirmation too.
        _ => Err(0),
    }
}

/// The merged layers of `repo` (T3): the same load a run performs —
/// `load_layers` + `merge`, the `status` precedent. In once-mode
/// the sidecar layer is empty by construction (T8/D22: the temp
/// root's own `config.toml`, created only by `sandbox()`'s Once arm
/// after Enter) — so the HOST sidecar is IGNORED here, never
/// read: the `doctor.rs` `effective_config` precedent ("an
/// existing host `<repo>.mysbx/config.toml` is ignored"). A
/// preselect from a sidecar the Enter run will not use would lie
/// (the sidecar says podman-gvisor, the once run runs the user
/// layer's backend), and a touched agent row would import
/// host-sidecar policy into a once run. The user layer loads WITH
/// its parent chain (config.md D29) against `Config::default()`;
/// the repo-root containment check is skipped for the same reason
/// a real once run skips it — no repo root is derivable from the
/// temp sidecar a run carries.
fn load_merged(repo: &repo::Repo, once: bool) -> Result<Merged, String> {
    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    if !once {
        let layers =
            merge::load_layers(&home, xdg.as_deref(), &repo.sidecar).map_err(|e| e.to_string())?;
        return merge::merge(
            layers.user.0,
            layers.sidecar.0,
            &layers.user.1,
            &layers.sidecar.1,
            &home,
        )
        .map_err(|e| e.to_string());
    }
    let user_path = merge::user_config_path(&home, xdg.as_deref());
    let user =
        merge::load_layer_chain(&user_path, "user", &home, None).map_err(|e| e.to_string())?;
    merge::merge(
        user.config,
        crate::config::Config::default(),
        &user.top,
        &user.top,
        &home,
    )
    .map_err(|e| e.to_string())
}

/// One decoded key of the loop — the subset the picker binds,
/// plus the shapes it must consume without binding.
#[derive(Debug, PartialEq, Eq)]
enum Key {
    Up,
    Down,
    Left,
    Right,
    Enter,
    /// The escape key AND every decode-dead-end the loop treats as
    /// an abort (EOF, a broken stdin): the lone-`\x1b` budget
    /// expiry is the one true keypress spelling — see [`KeyReader`].
    /// An unsupported COMPLETE sequence is [`Key::Ignored`], never
    /// Esc.
    Esc,
    /// A complete sequence the picker does not bind — an F-key, a
    /// Home/End, a Delete, a modifier arrow — decoded as one unit
    /// and bound to NOTHING: the loop continues. Brushing Home must
    /// not abort the session (Esc is reserved for the operator's
    /// key).
    Ignored,
    Char(u8),
}

/// Read and decode one key from a raw terminal: the arrow keys in
/// both spellings — CSI (`\x1b[A`…`\x1b[D`) and SS3 (`\x1bOA`…
/// `\x1bOD`, DECCKM's application cursor keys, left set by a
/// parent line editor, tmux or vim `:sh`) — a bare `\x1b` (Esc),
/// `\r`/`\n` (Enter), every other byte as [`Key::Char`] — `q`
/// and byte 3 are the ones the loop binds. A COMPLETE sequence the
/// picker does not bind decodes as [`Key::Ignored`] — one unit,
/// never keys, never Esc.
///
/// The pure decoder: [`KeyReader`] holds the byte queue and the
/// per-decode continuation budget, so the decode is unit-testable
/// with any `impl Read`; the wired loops hold ONE reader for
/// their lifetime (the queue survives across keys) built on a
/// small raw-`read(2)` fd-0 source — no std buffering between
/// the terminal and the queue — with a deadline-backed budget
/// (`select(2)` on fd 0, same raw extern idiom as the test
/// crate's `pty_readable`; VMIN=1/VTIME=0 keeps the first byte
/// blocking and only the CONTINUATION is bounded).
///
/// A sequence may arrive split across reads; a lone `\x1b` or an
/// introducer pair with nothing more buffered consumes ONE
/// budget unit waiting for the rest — the budget exists exactly
/// for that wait, so the pure tests inject it. Budget expiry with
/// a bare `\x1b` decodes as Esc (the escape key, not the sequence
/// introducer — bare Esc must stay responsive); expiry after an
/// introducer treats the sequence as unsupported and consumes its
/// tail WITHOUT surfacing it as keys. EOF and io errors decode as
/// Esc: a closed or broken stdin aborts the picker instead of
/// spinning.
fn read_key(reader: &mut KeyReader<'_, Fd0>) -> Key {
    reader.next_key()
}

/// The wired key source: fd 0 through the raw `read(2)` extern —
/// NO buffering. `std::io::stdin()`'s internal BufReader can hold
/// bytes the continuation deadline's `select(2)` cannot see, so a
/// mid-burst drained queue would spuriously time out; the raw fd
/// has exactly one buffer — the decoder's queue — and `select`
/// sees the truth. EINTR is retried (the handler window installs
/// `SA_RESTART`, but a race remains).
struct Fd0;

impl Read for Fd0 {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        loop {
            let n = unsafe { libc_read(0, buf.as_mut_ptr(), buf.len()) };
            if n >= 0 {
                return Ok(n as usize);
            }
            if unsafe { errno() } == EINTR {
                continue;
            }
            return Err(std::io::Error::last_os_error());
        }
    }
}

extern "C" {
    fn read(fd: i32, buf: *mut std::ffi::c_void, count: usize) -> isize;
}

/// `read(2)` for [`Fd0`] — spelled separately because the extern
/// block's `read` and the `Read::read` trait method would shadow
/// each other inside the impl.
///
/// # Safety
///
/// `buf` must name `count` writable bytes; the fd is the caller's.
unsafe fn libc_read(fd: i32, buf: *mut u8, count: usize) -> isize {
    unsafe { read(fd, buf.cast(), count) }
}

/// `EINTR` — the errno a raw fd read is retried on (a signal the
/// handler window did not install `SA_RESTART` for, or a race).
const EINTR: i32 = 4;

unsafe fn errno() -> i32 {
    unsafe {
        extern "C" {
            fn __errno_location() -> *mut i32;
        }
        *__errno_location()
    }
}

/// The tty key source — the ONE persistent [`KeyReader`] the
/// wired loops ([`picker_loop`], [`confirm_not_inited`]) hold for
/// their whole lifetime: a fresh reader per keypress would drop
/// the queue on every decode and lose the coalesced keys (`xq` in
/// one read, an arrow and Enter in another burst). The budget is
/// per-DECODE, not per-reader (reset at the top of each
/// [`KeyReader::next_key`]), so persistence does not leak budget.
fn tty_key_reader() -> KeyReader<'static, Fd0> {
    // `Fd0` is a zero-sized handle — the state it names is fd 0,
    // which IS process-static — so a `static` location may stand
    // behind the reader's `'static` borrow. The exit paths drop
    // the reader; the location stays.
    static mut FD0: Fd0 = Fd0;
    // SAFETY: the picker is single-threaded, and — the
    // load-bearing invariant — no two `tty_key_reader()` readers
    // are ever alive at once: every call manufactures a fresh
    // `&'static mut` to the SAME location, so overlapping
    // lifetimes would alias it. The two callers are sequential
    // (`confirm_not_inited` returns — reader dropped — before
    // `run` re-enters `picker_loop`, whose reader drops on its
    // return). A future caller interleaving them must switch to
    // ONE shared reader instead.
    KeyReader::new(unsafe { &mut *std::ptr::addr_of_mut!(FD0) }, 8).with_deadline(25)
}

/// Where continuation bytes come from when the queue runs dry
/// mid-sequence: `Blocking` re-reads the source directly (the unit
/// tests' reader — however it behaves), `Deadline` waits for fd 0
/// to become readable first through the raw `select(2)` extern and
/// gives up when it does not within `millis`. The budget caps BOTH
/// shapes' read attempts, and the deadline is what keeps a split
/// introducer from blocking forever on a live tty.
enum Continue {
    Blocking,
    Deadline { millis: u64 },
}

/// The byte cap of a bracketed-paste consume — the backstop of
/// [`KeyReader::consume_paste`]'s budget-exempt refills. Real
/// pastes aimed at the picker are kilobytes at most; the cap is
/// what bounds a terminator that never comes from a hostile or
/// broken emulator streaming paste content forever.
const PASTE_CAP_BYTES: usize = 64 * 1024;

/// The pure key-stream decoder behind [`read_key`]: reads bytes
/// from any [`Read`] into an internal queue and decodes one key
/// at a time, so a read carrying several keys (`xq`) yields first
/// `Char(x)`, then `Char(q)` — nothing is dropped. Bytes after a
/// decoded key stay queued, across [`next_key`](Self::next_key)
/// calls too: the wired loops hold ONE reader for their lifetime
/// and the queue is what keeps a burst from losing its tail.
/// The continuation budget is per-DECODE — reset at the top of
/// every `next_key` — so it bounds one split sequence's tail
/// waits, never the reader's lifetime.
struct KeyReader<'a, R: Read> {
    stdin: &'a mut R,
    /// Undecoded bytes carried across `next_key` calls — the queue
    /// that keeps coalesced keys from being lost.
    buf: Vec<u8>,
    /// How many more continuation attempts an incomplete sequence
    /// may spend before it decodes as Esc (bare `\x1b`) or is
    /// dropped as unsupported (an introducer). Reset per decode —
    /// `initial_budget` is the value every `next_key` restores.
    budget: u32,
    /// The budget each decode starts with — what `next_key` resets
    /// `budget` to.
    initial_budget: u32,
    /// Whether this decode already CONSUMED a byte (taken one from
    /// the queue — `take` sets it). The budget and the deadline
    /// gate ONLY the fills after that point, so the LEADING fill of
    /// every decode — the one that waits for the operator's next
    /// keypress — blocks on the raw read (VMIN=1) and an idle
    /// picker never times out into a spurious abort. Marking on
    /// CONSUMED (not on read-from-source) also bounds the lone
    /// `\x1b` at a burst's tail: it was consumed, so the wait for
    /// a possible introducer behind it is a continuation, not the
    /// unbounded leading fill.
    consumed_in_decode: bool,
    cont: Continue,
}

impl<'a, R: Read> KeyReader<'a, R> {
    fn new(stdin: &'a mut R, budget: u32) -> KeyReader<'a, R> {
        KeyReader {
            stdin,
            buf: Vec::new(),
            budget,
            initial_budget: budget,
            consumed_in_decode: false,
            cont: Continue::Blocking,
        }
    }

    /// The tty shape: continuation reads wait for fd 0 to become
    /// readable within `millis`, through `select(2)` — the same
    /// raw-extern idiom the rest of the crate uses. A continuation
    /// that never arrives does not block the loop. The LEADING
    /// fill never waits: it is the operator's keypress, blocking
    /// until it comes (an idle picker must sit in that read
    /// indefinitely, never abort on a deadline).
    fn with_deadline(mut self, millis: u64) -> KeyReader<'a, R> {
        self.cont = Continue::Deadline { millis };
        self
    }

    /// Whether the queue holds at least one more byte, reading
    /// from the source when it does not — a budgeted continuation
    /// under the rules below.
    fn fill(&mut self) -> Result<bool, ()> {
        if !self.buf.is_empty() {
            return Ok(true);
        }
        self.read_chunk(true)
    }

    /// ONE source read, appended to the queue — the shared
    /// primitive of the continuation fills. `Ok(true)` means
    /// byte(s) were appended; `Ok(false)` the source is at EOF;
    /// `Err(())` an io error, the deadline spent, or — `budgeted`
    /// — the budget ran out mid-sequence. The LEADING read of a
    /// decode (no byte consumed yet in it) blocks on the raw
    /// read, budget-free and deadline-free — the budget and the
    /// deadline bound only the continuation reads of an
    /// incomplete sequence, so a fully-buffered key (the
    /// coalesced case) spends nothing and an idle picker sits in
    /// the blocking read indefinitely instead of timing out into
    /// a spurious abort. `budgeted: false` is the paste consume's
    /// exemption: a real bracketed paste is kilobytes, and a
    /// budget of a few refills would cut the consume mid-content
    /// and leak the REST as keystrokes — its bounds are the byte
    /// cap ([`PASTE_CAP_BYTES`]) and the per-refill deadline
    /// instead.
    fn read_chunk(&mut self, budgeted: bool) -> Result<bool, ()> {
        if self.consumed_in_decode {
            if budgeted {
                if self.budget == 0 {
                    return Err(());
                }
                self.budget -= 1;
            }
            if let Continue::Deadline { millis } = self.cont {
                if !fd0_readable_within(millis) {
                    return Err(());
                }
            }
        }
        let mut chunk = [0u8; 32];
        match self.stdin.read(&mut chunk) {
            Ok(0) => Ok(false),
            Ok(n) => {
                self.buf.extend_from_slice(&chunk[..n]);
                Ok(true)
            }
            Err(_) => Err(()),
        }
    }

    /// Take the next byte if one is queued, without reading.
    fn peek(&self) -> Option<u8> {
        self.buf.first().copied()
    }

    fn take(&mut self) {
        if !self.buf.is_empty() {
            self.buf.remove(0);
            // A byte consumed in THIS decode: every fill after it
            // is a budgeted, deadlined continuation. Only a decode
            // that consumed nothing yet gets the unbounded leading
            // fill — the idle-picker wait.
            self.consumed_in_decode = true;
        }
    }

    /// Consume the paste payload of a bracketed paste — everything
    /// up to and including the `\x1b[201~` terminator — as ONE
    /// [`Key::Ignored`]. Without this the paste's content would
    /// decode key-by-key after the `\x1b[200~` introducer and a
    /// pasted `\r` would dispatch, a pasted `q` abort —
    /// keystrokes the operator never pressed, never saw.
    ///
    /// The bounds: paste refills are budget-EXEMPT (a real paste
    /// is kilobytes; the continuation budget would cut the
    /// consume at a few refills and leak the REST as keys), each
    /// refill still carries the deadline (an unterminated paste
    /// must end the wait), EOF ends it, and the byte cap
    /// ([`PASTE_CAP_BYTES`]) backstops a stream that never ends.
    /// On ANY of those exits everything consumed so far is
    /// discarded — content bytes can never surface as keys from
    /// this decode; the terminator `201~`, not a budget, is what
    /// a well-formed paste ends on.
    fn consume_paste(&mut self) {
        // The last bytes seen, most recent last — the window the
        // `\x1b[201~` terminator is matched against from inside
        // the stream (its `\x1b` and `[` may have been consumed
        // reads ago).
        let mut tail: Vec<u8> = Vec::new();
        let mut bytes: usize = 0;
        loop {
            let Some(b) = self.peek() else {
                match self.read_chunk(false) {
                    Ok(true) => continue,
                    // EOF, the refill deadline spent, or an io
                    // error: the paste is over however it ends,
                    // and everything consumed is gone — an
                    // unterminated paste's content must never
                    // decode as keys.
                    _ => return,
                }
            };
            self.take();
            bytes += 1;
            if bytes >= PASTE_CAP_BYTES {
                // A "paste" past 64 KiB is emulator misbehavior
                // — no operator pastes that much into a three-row
                // picker. Give up: the residue decodes as plain
                // keys (documented residual, accepted — the
                // alternative is an unbounded consume that pins
                // the picker forever).
                return;
            }
            tail.push(b);
            if tail.len() > 4 {
                tail.remove(0);
            }
            if b == b'~' && tail.ends_with(b"201~") {
                return;
            }
        }
    }

    /// Decode the next key from the stream. See [`read_key`] for
    /// the grammar and the budget/expiry rules. The budget and the
    /// consumed-source flag reset HERE — one decode's continuations
    /// never eat into the next decode's budget, and every decode's
    /// leading fill blocks (the idle-picker guarantee).
    fn next_key(&mut self) -> Key {
        self.budget = self.initial_budget;
        self.consumed_in_decode = false;
        match self.fill() {
            Err(()) => return Key::Esc,
            Ok(false) => return Key::Esc,
            _ => {}
        }
        let b0 = match self.peek() {
            Some(b) => b,
            None => return Key::Esc,
        };
        if b0 != 0x1b {
            self.take();
            return match b0 {
                b'\r' | b'\n' => Key::Enter,
                other => Key::Char(other),
            };
        }
        // `\x1b` — Esc or the introducer of a longer sequence. A
        // lone `\x1b` with nothing more buffered spends one budget
        // unit WAITING for a possible introducer: a fast terminal
        // delivers `\x1b[A` in one piece, a slow one splits it,
        // and only a budget expiry decides it was a bare Esc
        // keypress after all — bare Esc stays responsive because
        // the wait is short, never unbounded.
        self.take();
        if self.peek().is_none() && self.fill().is_err() {
            return Key::Esc;
        }
        let introducer = match self.peek() {
            Some(b @ (b'[' | b'O')) => {
                self.take();
                b
            }
            // Anything else behind the `\x1b` is no introducer,
            // and Esc now: a second `\x1b` (some terminals repeat
            // the byte for a bare Esc keypress) — and an
            // Alt-modified key (`\x1bq`, Alt-q), which xterm
            // spells as `\x1b` plus the key's byte. The leftover
            // byte STAYS QUEUED: for a real Esc keypress the queue
            // is empty and nothing leaks; for an Alt-key the next
            // decode sees the bare key byte alone (the Alt
            // modifier is lost — the picker binds no Alt keys, so
            // an Alt-q would abort like `q` alone does either
            // way).
            _ => return Key::Esc,
        };
        // The sequence's final byte (or, for CSI, the byte that
        // continues it). Both spellings' arrows are exactly three
        // bytes, so one continuation decides the bound keys.
        let final_byte = match self.peek() {
            Some(b) => {
                self.take();
                b
            }
            None => {
                if self.fill().is_err() {
                    // The introducer arrived but its rest never
                    // did within the budget (or EOF): an
                    // unsupported sequence's head — consumed,
                    // never interpreted as keys, and never the
                    // operator's Esc (a bare-Esc keypress is the
                    // lone-`\x1b` path above; this one HAD an
                    // introducer behind it).
                    return Key::Ignored;
                }
                match self.peek() {
                    Some(b) => {
                        self.take();
                        b
                    }
                    None => return Key::Ignored,
                }
            }
        };
        match (introducer, final_byte) {
            (b'[', b'A') | (b'O', b'A') => Key::Up,
            (b'[', b'B') | (b'O', b'B') => Key::Down,
            (b'[', b'C') | (b'O', b'C') => Key::Right,
            (b'[', b'D') | (b'O', b'D') => Key::Left,
            // A bracketed paste (`\x1b[200~` … `\x1b[201~`): the
            // CONTENT is not keystrokes — a pasted `\r` must not
            // dispatch, a pasted `q` must not abort — so the whole
            // payload up to and including the terminator is
            // consumed as one Ignored. The guard FILLS first
            // (budgeted, deadlined continuations — the normal
            // rules): a read boundary inside `\x1b[200` (`\x1b[2`
            // now, `00~` in the next read) must still take the
            // paste arm, not fall through to the generic CSI
            // consume and decode the content as keys.
            (b'[', b'2')
                if {
                    // `read_chunk`, NOT `fill`: `fill` short-
                    // circuits on a non-empty queue, and the split
                    // head may have left ONE `0` queued — an
                    // infinite no-read loop. `read_chunk` reads.
                    while self.buf.len() < 2 {
                        match self.read_chunk(true) {
                            Ok(true) => {}
                            _ => break,
                        }
                    }
                    self.peek() == Some(b'0') && self.buf.get(1) == Some(&b'0')
                } =>
            {
                self.take(); // the first `0`
                let _ = self.peek().map(|_| self.take()); // the second
                let _ = self.peek().map(|_| self.take()); // the `~`
                self.consume_paste();
                Key::Ignored
            }
            // A CSI the picker does not bind (a modifier arrow
            // `\x1b[1;5C`, a Home `\x1b[H`, a Delete `\x1b[3~`):
            // consume the rest of the sequence — every byte
            // through the final one, `@` through `~` (0x40–0x7e) —
            // WITHOUT decoding them as keys (the `;`, `5`, `C` of
            // the modifier arrow never surface), as ONE Ignored:
            // brushing Home must not abort the session.
            (b'[', _) => {
                if !is_csi_final(final_byte) {
                    while self.fill().is_ok() {
                        let Some(b) = self.peek() else { break };
                        self.take();
                        if is_csi_final(b) {
                            break;
                        }
                    }
                }
                Key::Ignored
            }
            // An SS3 final is `A`–`Z` or `a`–`z`; anything else
            // (e.g. `\x1bOP`, a function key) is an unsupported
            // sequence consumed whole — the one byte read past the
            // introducer is already taken above — and reported as
            // one Ignored, never Esc (Esc is the operator's abort).
            _ => Key::Ignored,
        }
    }
}

/// Whether `b` may terminate a CSI sequence (ECMA-48: `@`..`~`,
/// 0x40–0x7e) — the minimal terminator rule the unsupported-sequence
/// consume uses, enough for the sequences a terminal sends.
fn is_csi_final(b: u8) -> bool {
    (0x40..=0x7e).contains(&b)
}

/// `select(2)` on fd 0 with a timeout — the continuation deadline
/// of [`read_key`] on a live tty, the same raw-extern idiom the
/// test crate's `pty_readable` uses. `false` when fd 0 is not
/// readable within `millis`: a split sequence that never completes
/// gives up instead of blocking. `EINTR` is RETRIED — the picker's
/// handler window installs `SA_RESTART`, but a signal a handler
/// catches can still land between the syscall and the flag; a
/// `SIGWINCH` (which the picker does not handle at all) would
/// otherwise spend the whole deadline and report a spurious
/// not-readable, decoding a split arrow into a spurious Esc.
fn fd0_readable_within(millis: u64) -> bool {
    #[repr(C)]
    struct FdSet {
        fds_bits: [u64; 16],
    }
    #[repr(C)]
    struct TimeVal {
        secs: i64,
        micros: i64,
    }
    extern "C" {
        fn select(
            n: i32,
            readfds: *mut FdSet,
            writefds: *mut FdSet,
            exceptfds: *mut FdSet,
            timeout: *mut TimeVal,
        ) -> i32;
    }
    // The deadline as an ABSOLUTE start — the retry after EINTR
    // re-arms with the REMAINING time, never the full window
    // again (a signal storm must not extend the wait forever).
    let started = std::time::Instant::now();
    loop {
        let mut set = FdSet { fds_bits: [0; 16] };
        set.fds_bits[0] = 1; // fd 0
        let left = millis.saturating_sub((std::time::Instant::now() - started).as_millis() as u64);
        // SAFETY: `select` reads the set and the timeout and writes
        // only the locals they point at.
        let n = unsafe {
            select(
                1,
                &mut set,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                &mut TimeVal {
                    secs: (left / 1000) as i64,
                    micros: ((left % 1000) * 1000) as i64,
                },
            )
        };
        if n > 0 {
            return true;
        }
        if n == 0 {
            return false; // the deadline spent, fd 0 still quiet
        }
        // `n < 0`: an error — EINTR retries with the remaining
        // window, anything else (EBADF and friends) gives up.
        if unsafe { errno() } != EINTR {
            return false;
        }
    }
}

/// The one impure part: the tty loop. `Ok(Some((touched, rows)))`
/// is the Enter state, `Ok(None)` an Esc/`q` abort (exit `0` —
/// nothing was started), `Err(())` a terminal whose raw mode
/// cannot be entered — mapped by `run` to `70`
/// (EXIT_INFRASTRUCTURE), never to the abort's `0`. The `RawMode`
/// guard restores the termios on every exit path, Enter included,
/// and the alternate-screen leave goes out FIRST — still inside
/// the armed window — so both restores complete here, before `run`
/// dispatches and `sandbox()` writes its first byte.
fn picker_loop(mut rows: [Row; 3], once: bool) -> Result<Option<(Touched, [Row; 3])>, ()> {
    // A terminal whose raw mode cannot be entered is `Err(())` —
    // `run` maps it to `70` (EXIT_INFRASTRUCTURE): a broken
    // terminal is infrastructure, never the operator's abort, so
    // `Ok(None)` stays reserved for Esc/`q`'s exit-`0` abort.
    let guard = RawMode::enter().ok_or(())?;
    // The picker draws on the ALTERNATE screen: the caller's primary
    // screen — scrollback included — is restored by `leave_screen()`
    // on every exit below (and by the termination handler on a
    // kill). A terminal without the mode ignores the sequences and
    // keeps the clear-based behavior.
    enter_screen();
    let mut touched = Touched::default();
    let mut focus = 0usize;
    // ONE reader for the loop's whole life: the queue survives
    // across keypresses, so a coalesced burst (`xq`, an arrow and
    // Enter in one read) decodes key by key, nothing lost — a
    // fresh reader per keypress would drop the queue every time.
    let mut reader = tty_key_reader();
    loop {
        render(&rows, focus, once);
        let key = read_key(&mut reader);
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
                leave_screen();
                drop(guard);
                return Ok(Some((touched, rows)));
            }
            // ISIG is off in raw mode, so ^C arrives as byte 3 —
            // bound to the abort like Esc and `q` so the operator
            // can always interrupt (exit `0`, nothing started).
            Key::Esc | Key::Char(b'q') | Key::Char(0x03) => {
                leave_screen();
                drop(guard);
                return Ok(None);
            }
            // A complete unbound sequence (Home, End, Delete, an
            // F-key, a modifier arrow, a bracketed paste): one
            // unit, consumed, bound to nothing — the render loop
            // simply redraws. Esc stays the operator's abort.
            Key::Ignored | Key::Char(_) => continue,
        };
        focus = ((focus as i64 + delta).rem_euclid(3)) as usize;
    }
}

/// Full-screen ANSI render (T6): a redraw per key is the whole point
/// of three rows — no line editing. The focused row is inverted,
/// the selected value bracketed. The clear homes the render; the
/// picker runs on the alternate screen ([`enter_screen`]), so on
/// a capable terminal every clear hits the alternate buffer, and
/// an incapable one gets today's clear-per-redraw behavior.
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

/// Enter the alternate screen and clear it: `?1049` FIRST (a
/// capable terminal switches to the alternate buffer — the
/// caller's primary screen, scrollback included, stays untouched),
/// then the `2J`/`H` clear, which lands on whichever screen is
/// now active. A terminal without the mode ignores the switch and
/// gets exactly the clear the picker always did — the fallback is
/// composed into the sequence, not detected.
fn enter_screen() {
    let mut out = std::io::stdout();
    let _ = write!(out, "\x1b[?1049h\x1b[2J\x1b[H");
    let _ = out.flush();
}

/// The alternate-screen leave, as the byte string both the exit
/// paths and the termination handler write: clear + home FIRST —
/// on a capable terminal that clears the ALTERNATE buffer, which
/// `?1049l` then discards, restoring the untouched primary — then
/// the mode-off. A terminal without the mode sees the clear alone,
/// today's clear-on-exit. Written before the raw-mode restore on
/// every exit (Enter and abort alike), and raw by the signal
/// handler.
const LEAVE_SCREEN_SEQ: &[u8] = b"\x1b[2J\x1b[H\x1b[?1049l";

/// Leave the picker's screen (see [`LEAVE_SCREEN_SEQ`]) — the
/// caller's terminal reappears exactly as it was on a capable
/// terminal, cleared as before on an incapable one. The sequence
/// is flushed before the caller proceeds: on Enter it must be
/// complete before `sandbox()`'s first byte.
fn leave_screen() {
    let mut out = std::io::stdout();
    let _ = out.write_all(LEAVE_SCREEN_SEQ);
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

/// The saved termios of the tty the picker took over — what
/// [`RawMode::drop`] and the termination handler write back.
/// A `static mut`, NOT a lock: the handler must read it without
/// acquiring anything (a `Mutex::lock` in a handler deadlocks
/// when the signal interrupts the very code that holds the lock),
/// and the picker is single-threaded — a handler never runs
/// CONCURRENTLY with the code that mutates the slot, it can only
/// interrupt it, and every mutation is bracketed by the arm/
/// disarm lifecycle: written before the handlers are installed,
/// cleared after they are restored to defaults, so the handler
/// only ever reads a stable value.
static mut SAVED_TERMIOS: Option<Termios> = None;

/// Publish `saved` for the termination handler — called BEFORE
/// the handlers are installed (see [`SAVED_TERMIOS`]).
///
/// # Safety
///
/// Single-threaded caller, handlers not yet installed (or already
/// disarmed) — the [`SAVED_TERMIOS`] invariant.
unsafe fn publish_saved_termios(saved: Termios) {
    unsafe { SAVED_TERMIOS = Some(saved) }
}

/// Clear the published termios — called after the handlers are
/// disarmed.
///
/// # Safety
///
/// As [`publish_saved_termios`].
unsafe fn clear_saved_termios() {
    unsafe { SAVED_TERMIOS = None }
}

/// The catchable-termination set (SIGTERM, SIGINT, SIGHUP): the
/// signals whose default disposition kills the process WITHOUT
/// running the `RawMode` drop, leaving the shell with disabled
/// echo and canonical input.
const CAUGHT_SIGNALS: [i32; 3] = [sig::TERM, sig::INT, sig::HUP];

/// The termination handler: restore the termios and the display,
/// then restore the default disposition and re-raise — so the
/// process dies with the killed-by-signal status the caller
/// expects, on a restored terminal. Async-signal-safe: the termios
/// write is a `tcsetattr` on the stable published copy, the display
/// restore is one raw `write(2)` of escape-sequence bytes, and
/// `sigaction`/`kill` are safe — never buffered IO (the flush of
/// [`leave_screen`] is not), never a lock.
unsafe extern "C" fn on_terminate(signum: i32, _info: *const u8, _ctx: *mut u8) {
    unsafe {
        // A plain read of the published slot — no lock, no
        // allocation, a fixed-size copy: async-signal-safe because
        // the slot is stable for the handler's whole armed life.
        if let Some(saved) = SAVED_TERMIOS {
            tcsetattr(0, 0, &saved);
        }
        // The alternate-screen leave, spelled as one raw write —
        // buffered IO is not async-signal-safe, so the handler
        // borrows the fd directly.
        let _ = libc_write(1, LEAVE_SCREEN_SEQ.as_ptr(), LEAVE_SCREEN_SEQ.len());
        sig::to_default(signum);
        sig::kill_pid(std::process::id() as i32, signum);
    }
}

extern "C" {
    fn write(fd: i32, buf: *const std::ffi::c_void, count: usize) -> isize;
}

/// `write(2)` for the termination handler — the one raw extern the
/// async-signal-safe display restore needs.
///
/// # Safety
///
/// `buf` must name `count` readable bytes; the fd is the caller's.
unsafe fn libc_write(fd: i32, buf: *const u8, count: usize) -> isize {
    unsafe { write(fd, buf.cast(), count) }
}

/// Install the termination handlers for the picker's terminal
/// window: while the picker owns the TTY, a SIGTERM/SIGINT/SIGHUP
/// must restore the terminal before the default kill. Installs
/// [`on_terminate`] through the crate's shared `sigaction` binding
/// ([`crate::sig::on_info`] — zero new externs). A failure to
/// install is an infrastructure refusal, reported by the caller.
fn arm_termination() -> Result<(), std::io::Error> {
    for signum in CAUGHT_SIGNALS {
        if let Err(e) = unsafe { sig::on_info(signum, on_terminate) } {
            // Lifecycle symmetry: a failed install must not leave
            // the EARLIER signals armed — the caller treats an Err
            // as "no handlers installed" and the window is closed
            // by disarm; a half-armed set would restore on some
            // signals and die mid-restore on others.
            disarm_termination();
            return Err(e);
        }
    }
    Ok(())
}

/// Restore the default disposition for the caught set — the
/// window's close. Runs before the Enter dispatch (so the pipeline
/// inherits a clean disposition: `sandbox()`'s own SIGINT/SIGTERM
/// handling, lib.rs's `libc_signal_catch`, installs ITS handlers on
/// a default slate, and the payload after it inherits default) and
/// on every abort path.
fn disarm_termination() {
    for signum in CAUGHT_SIGNALS {
        unsafe { sig::to_default(signum) };
    }
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
    /// Enter raw mode, or `None` with the refusal printed — the
    /// caller maps that to `EXIT_INFRASTRUCTURE` (`70`), never to
    /// the abort's `0`: a terminal whose state cannot be saved or
    /// set is broken infrastructure, not an operator's change of
    /// mind. Also publishes the saved termios to the termination
    /// handler's static and arms the catchable-termination window,
    /// so a SIGTERM under raw mode restores before dying; the
    /// arm failure is the same infrastructure refusal (a process
    /// that cannot promise to restore the terminal must not take
    /// it over in the first place).
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
        // The termination window: publish the saved termios for
        // the handler, then install the handlers. A failed install
        // reverts raw mode and refuses — a process that cannot
        // promise to restore the terminal must not take it over
        // in the first place.
        unsafe { publish_saved_termios(saved) };
        if arm_termination().is_err() {
            unsafe { tcsetattr(0, 0, &saved) };
            unsafe { clear_saved_termios() };
            eprintln!("mysbx: cannot install the terminal restoration handlers");
            return None;
        }
        Some(RawMode { saved })
    }
}

impl Drop for RawMode {
    fn drop(&mut self) {
        // SAFETY: same as `enter`, writing the saved state back.
        unsafe { tcsetattr(0, 0, &self.saved) };
        // The terminal is restored; the termination window closes
        // with it — a signal past this point is the pipeline's or
        // the shell's business, on default dispositions.
        disarm_termination();
        unsafe { clear_saved_termios() };
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

    /// Feed `bytes` to a fresh [`KeyReader`] through a `Cursor`, so
    /// the decode needs no terminal. Budget 0: everything the tests
    /// feed arrives in the buffer or not at all.
    fn key(bytes: &[u8]) -> Key {
        KeyReader::new(&mut std::io::Cursor::new(bytes.to_vec()), 0).next_key()
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

    /// A reader serving its bytes one chunk per read, then EOF —
    /// the split-read shape the pty delivers.
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

    fn split_reader(chunks: &[&[u8]], budget: u32) -> KeyReader<'static, Chunks> {
        // The reader is leaked into a 'static slot the KeyReader can
        // borrow — the tests' convenience; nothing outlives them.
        let boxed: Box<Chunks> = Box::new(Chunks {
            chunks: chunks.iter().rev().map(|c| c.to_vec()).collect(),
        });
        KeyReader::new(Box::leak(boxed), budget)
    }

    #[test]
    fn a_split_sequence_waits_for_the_rest_of_it() {
        // The pty may deliver the introducer pair and the final byte
        // in separate reads — the budget spends one continuation on
        // the second read and the re-read decides instead of an
        // abort.
        assert_eq!(split_reader(&[b"\x1b[", b"D"], 1).next_key(), Key::Left);
        assert_eq!(split_reader(&[b"\x1bO", b"A"], 1).next_key(), Key::Up);
    }

    #[test]
    fn an_introducer_with_nothing_behind_it_is_not_interpreted() {
        // Budget expiry after `\x1b[`/`\x1bO` — or EOF behind the
        // introducer — treats the sequence as unsupported: it does
        // NOT decode as anything (Ignored, never the operator's
        // Esc: an introducer WAS seen, so it cannot have been the
        // escape key), and a follow-up read starts from a clean
        // queue (no spin: the bytes are consumed).
        let mut r = split_reader(&[b"\x1b["], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        // The queue is empty — the next read is a fresh blocking
        // read, not a spin on the half-sequence.
        let mut r = split_reader(&[b"\x1b["], 1);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the half-sequence was consumed");
        let mut r = split_reader(&[b"\x1bO"], 1);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the half-sequence was consumed");
        // EOF behind the introducer: consumed, ignored, no spin.
        let mut r = split_reader(&[b"\x1b[", b""], 1);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
    }

    #[test]
    fn a_bare_escape_with_nothing_behind_it_decodes_as_esc() {
        // Budget expiry on a lone `\x1b` decodes as Esc — bare Esc
        // must stay responsive, which is what the budget buys.
        assert_eq!(split_reader(&[b"\x1b"], 0).next_key(), Key::Esc);
        assert_eq!(split_reader(&[b"\x1b"], 1).next_key(), Key::Esc);
    }

    #[test]
    fn coalesced_keys_in_one_read_decode_in_order() {
        // `x` then `q` in one read: both surface, in order — the old
        // one-key-per-read dropped the `q`.
        let mut r = split_reader(&[b"xq"], 0);
        assert_eq!(r.next_key(), Key::Char(b'x'));
        assert_eq!(r.next_key(), Key::Char(b'q'));
        // Two arrows in one read: both decode, none lost.
        let mut r = split_reader(&[b"\x1b[B\x1b[A"], 0);
        assert_eq!(r.next_key(), Key::Down);
        assert_eq!(r.next_key(), Key::Up);
        // An arrow and a key in one read — the queued key after a
        // decoded sequence stays buffered.
        let mut r = split_reader(&[b"\x1b[Cz"], 0);
        assert_eq!(r.next_key(), Key::Right);
        assert_eq!(r.next_key(), Key::Char(b'z'));
    }

    #[test]
    fn split_csi_and_ss3_arrows_across_reads() {
        // Every split point: `\x1b` alone, then `[`/`O`, then the
        // final byte — one key decoded per arrow, both spellings.
        let mut r = split_reader(&[b"\x1b", b"[", b"A"], 2);
        assert_eq!(r.next_key(), Key::Up);
        let mut r = split_reader(&[b"\x1b", b"O", b"D"], 2);
        assert_eq!(r.next_key(), Key::Left);
    }

    #[test]
    fn unsupported_sequence_tails_do_not_surface_as_keys() {
        // `\x1b[1;5C` (ctrl-right): the `1`, `;`, `5` and the final
        // `C` are CONSUMED with the sequence — none of them may
        // surface as a Char key (the `;` would do nothing, but a
        // future binding could make the tail a live key) — and the
        // whole unit decodes as Ignored, never Esc: brushing the
        // key must not abort the session.
        let mut r = split_reader(&[b"\x1b[1;5C"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the whole sequence was consumed");
        // The same sequence SPLIT across reads: the consume works
        // through continuations too.
        let mut r = split_reader(&[b"\x1b[1;5", b"C"], 1);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
        // Home (`\x1b[H`) and Delete (`\x1b[3~`) — CSI finals the
        // picker does not bind, complete in one piece.
        let mut r = split_reader(&[b"\x1b[H"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        let mut r = split_reader(&[b"\x1b[3~"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        // An SS3 final outside A–Z/a–z: `\x1bOP` (F1) is consumed
        // whole too.
        let mut r = split_reader(&[b"\x1bOP"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
    }

    #[test]
    fn an_unsupported_complete_sequence_never_aborts() {
        // The removed `an_unknown_sequence_final_byte_decodes_as_a_
        // bare_escape_char` pinned that an unknown sequence is NOT
        // an abort — restated against `Key::Ignored`, its exact
        // spelling: F-keys, Home/End, Delete, modifier arrows and
        // `\x1bOP` all decode as Ignored, and the loop binds
        // Ignored to nothing (Esc is the operator's abort, and
        // only the lone-`\x1b` budget expiry may spell it).
        for seq in [
            b"\x1bOP".as_slice(),
            b"\x1bOQ",
            b"\x1b[H",
            b"\x1b[F",
            b"\x1b[3~",
            b"\x1b[1;5C",
            b"\x1b[5~",
        ] {
            assert_eq!(key(seq), Key::Ignored, "{seq:?} is ignored, not an abort");
        }
    }

    #[test]
    fn a_bracketed_paste_is_consumed_whole_as_ignored() {
        // A bracketed paste's CONTENT is not keystrokes: a pasted
        // `\r` must not dispatch and a pasted `q` must not abort
        // — the whole payload through the `\x1b[201~` terminator
        // decodes as ONE Ignored, and nothing of it surfaces.
        let mut r = split_reader(&[b"\x1b[200~q\rq\x1b[201~"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the whole paste was consumed");
        // The paste terminator SPLIT across reads: the budget
        // exemption carries the consume through the refills, the
        // deadline still bounds each one.
        let mut r = split_reader(&[b"\x1b[200~x", b"y\x1b[20", b"1~"], 2);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
        // A paste with keys BEHIND it: the following keypress
        // still decodes after the paste.
        let mut r = split_reader(&[b"\x1b[200~\x1b[201~q"], 1);
        assert_eq!(r.next_key(), Key::Ignored);
        assert_eq!(r.next_key(), Key::Char(b'q'));
    }

    #[test]
    fn a_large_paste_is_consumed_whole_despite_the_budget() {
        // MAJOR-1 regression: a paste bigger than the continuation
        // budget's worth of refills (8 units x 32 bytes) must still
        // be consumed WHOLE — the terminator ends the consume, not
        // the budget. 4 KiB with `\r` and `q` sprinkled through
        // every chunk, the terminator split across the last reads:
        // a budget-capped consume would expire mid-content and the
        // rest would decode as keystrokes (an Enter dispatching, a
        // q aborting); the exemption makes the one Ignored cover
        // all of it, and the key BEHIND the paste still decodes.
        let mut chunks: Vec<Vec<u8>> = Vec::new();
        chunks.push(b"\x1b[200~".to_vec());
        for i in 0..136 {
            // ~32-byte content chunks, Enter and q sprinkled in
            // every one of them — none may ever surface.
            let mut c = Vec::new();
            for j in 0..28 {
                c.push(if j % 9 == 0 { b'\r' } else { b'x' });
            }
            c.push(b'q');
            c.push((i % 10) as u8 + b'0');
            chunks.push(c);
        }
        chunks.push(b"\x1b[20".to_vec());
        chunks.push(b"1~".to_vec());
        chunks.push(b"z".to_vec()); // the key behind the paste
        let refs: Vec<&[u8]> = chunks.iter().map(|c| c.as_slice()).collect();
        let mut r = split_reader(&refs, 8);
        assert_eq!(r.next_key(), Key::Ignored);
        assert_eq!(
            r.next_key(),
            Key::Char(b'z'),
            "the key behind the paste decodes; no content byte leaked"
        );
        assert!(r.buf.is_empty());
    }

    #[test]
    fn a_split_paste_introducer_still_takes_the_paste_arm() {
        // MINOR-1 regression: a read boundary INSIDE the `\x1b[200`
        // head (`\x1b[2` now, `00~...` in the next read) must fill
        // (budgeted, deadlined — the normal continuation rules)
        // before deciding paste vs generic CSI; falling through to
        // the generic consume would let the content decode as keys.
        let mut r = split_reader(&[b"\x1b[2", b"00~q\rq\x1b[201~"], 2);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the whole paste was consumed");
        // The head split one byte LATER still works too.
        let mut r = split_reader(&[b"\x1b[20", b"0~q\x1b[201~"], 2);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
    }

    #[test]
    fn an_unterminated_paste_ends_the_wait_without_leaking_keys() {
        // An unterminated paste (EOF behind the content in the unit
        // tests; the refill deadline on a tty) ends the consume and
        // returns Ignored — the content it already swallowed (the
        // `\r`, the `q`) must NEVER surface as keystrokes.
        let mut r = split_reader(&[b"\x1b[200~q\rq"], 0);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty(), "the partial paste was consumed");
        // EOF split one read later: same outcome.
        let mut r = split_reader(&[b"\x1b[200~q", b""], 8);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(r.buf.is_empty());
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
        let mut broken = Broken;
        assert_eq!(KeyReader::new(&mut broken, 8).next_key(), Key::Esc);
    }

    #[test]
    fn the_once_seed_ignores_a_host_sidecar_that_disagrees_with_the_user_layer() {
        // T8/D22 regression: the once picker's seed must come from
        // the user layer ALONE — a host sidecar the Enter run will
        // not use must not preselect (it would lie: the sidecar says
        // podman-gvisor, the once run runs the user layer's
        // backend), and a touched agent row must not import
        // host-sidecar policy into a once run. The sidecar here
        // disagrees on every row: backend, multiplexer, agent.
        //
        // `load_merged` reads $HOME and $XDG_CONFIG_HOME, so the
        // test owns them for its duration and restores them after
        // (the doctor.rs egress-test precedent); no other lib test
        // writes either name, so the ownership cannot race.
        let base = std::env::temp_dir().join(format!("mysbx-tui-once-seed-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&base);
        let home = base.join("home");
        let xdg = base.join("xdg");
        std::fs::create_dir_all(xdg.join("mysbx")).unwrap();
        std::fs::write(
            xdg.join("mysbx").join("config.toml"),
            "backend = \"bubblewrap\"\nmultiplexer = \"none\"\n",
        )
        .unwrap();
        let repo = repo::Repo {
            root: base.join("repo"),
            sidecar: base.join("repo.mysbx"),
            git_dirs: Vec::new(),
            worktrees: None,
        };
        std::fs::create_dir_all(&repo.sidecar).unwrap();
        std::fs::write(
            repo.sidecar.join("config.toml"),
            "backend = \"podman-gvisor\"\nmultiplexer = \"herdr\"\ndefault-agent = \"opencode\"\n",
        )
        .unwrap();

        let saved_home = std::env::var_os("HOME");
        let saved_xdg = std::env::var_os("XDG_CONFIG_HOME");
        unsafe { std::env::set_var("HOME", &home) };
        unsafe { std::env::set_var("XDG_CONFIG_HOME", &xdg) };
        let once_merged = load_merged(&repo, true);
        let plain_merged = load_merged(&repo, false);
        match (saved_home, saved_xdg) {
            (Some(h), Some(x)) => {
                unsafe { std::env::set_var("HOME", h) };
                unsafe { std::env::set_var("XDG_CONFIG_HOME", x) };
            }
            (Some(h), None) => {
                unsafe { std::env::set_var("HOME", h) };
                unsafe { std::env::remove_var("XDG_CONFIG_HOME") };
            }
            (None, Some(x)) => {
                unsafe { std::env::remove_var("HOME") };
                unsafe { std::env::set_var("XDG_CONFIG_HOME", x) };
            }
            (None, None) => {
                unsafe { std::env::remove_var("HOME") };
                unsafe { std::env::remove_var("XDG_CONFIG_HOME") };
            }
        }

        // The plain mode loads both layers: the sidecar's
        // podman-gvisor wins the merge (the run would use it).
        let plain = plain_merged.expect("the plain load succeeds");
        assert_eq!(plain.backend.as_deref(), Some("podman-gvisor"));
        assert_eq!(plain.default_agent.as_deref(), Some("opencode"));
        // The once mode ignores the sidecar entirely: the user
        // layer's backend, and NO agent — never the sidecar's
        // `default-agent` a touched row would have carried over.
        let once = once_merged.expect("the once load succeeds");
        assert_eq!(once.backend.as_deref(), Some("bubblewrap"));
        assert_eq!(once.default_agent, None);
        assert_eq!(once.multiplexer, Multiplexer::None);

        let _ = std::fs::remove_dir_all(&base);
    }
}
