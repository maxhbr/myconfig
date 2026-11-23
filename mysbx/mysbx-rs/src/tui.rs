// Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
// SPDX-License-Identifier: MIT
//! The session picker (bd myconfig-dm7b, docs/design/tui.md): the
//! full-screen form shown before an interactive launch, one row per
//! decision a one-run override flag already carries. Three launch
//! rows (T2): backend, multiplexer, default-agent — closed sets
//! drawn from the registries, never free text — plus the property
//! rows: a read-only `network` row (T9), the typed `cpus`/
//! `memory`/`pids-limit` rows (T10, drafted under a byte grammar and
//! validated at Enter through config.rs's OWN parsers), the
//! tri-state `nested-virt`/`store` cycle rows (T11/T12), and the
//! launch trio dry-run/detachable/session (T13–T15): two `off`/
//! `on` cycles over flags with no TOML surface (`off` always the
//! preselect) and one typed row drafting a session NAME under the
//! argv grammar, validated at Enter through `session::valid_name`.
//! Left/Right
//! cycle the focused row, Up/Down move the focus, typing drafts
//! into the focused typed row, Backspace edits, Enter dispatches
//! into `sandbox()`, Esc or `q` exits `0` (nothing was started). Enter
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

use crate::config::{Multiplexer, Store};
use crate::merge::{self, Merged};
use crate::repo;
use crate::sig;
use crate::toml;
use crate::{backends, doctor, AgentSelection, Flags, RunMode, EXIT_INFRASTRUCTURE};
use std::io::{Read, Write};
use std::path::PathBuf;

/// The rows in focus order: the three launch rows (T2), then the
/// property rows — the read-only `network` (T9), the typed trio
/// (T10), the tri-state cycles (T11/T12) — then the dry-run/
/// detachable/session trio (T13–T15), appended after the store
/// row. Each launch-row label is the flag family the row stands
/// for (`--backend`, `--multiplexer`, `--agent`); the property
/// rows carry the config key's own spelling (`network`, `cpus`,
/// `memory`, `pids-limit`, `nested-virt`, `store`, config.md
/// D20/D23/D27, guest-store.md); the trio carries the flags'
/// own spellings (`--dry-run`, `--detachable`, `--session`).
const ROW_LABELS: [&str; 12] = [
    "backend",
    "multiplexer",
    "default-agent",
    "network",
    "cpus",
    "memory",
    "pids-limit",
    "nested-virt",
    "store",
    "dry-run",
    "detachable",
    "session",
];

/// The row count — `ROW_LABELS`' length, spelled for the arrays.
const ROWS: usize = ROW_LABELS.len();

/// The `none` pseudo-value of the agent row — the `--no-agent`
/// spelling (cli.md D32), not a kind.
const AGENT_NONE: &str = "none";

/// The `inherit` spelling of the tri-state rows (T11/T12): the
/// layers (and the key's own default) decide — composed as no
/// `Flags` field, exactly like an untouched row.
const INHERIT: &str = "inherit";

/// The session row's display seed (T15): the mode a bare run uses
/// — the live checkout, the default `--session` leaves untouched.
/// Display-only like every typed seed (a seed never composes).
const LIVE: &str = "live";

/// The once-mode disabled-row hint of the session and detachable
/// rows (T13.1/T14.1/T15.1): the reason D22 refuses the flags —
/// the temp root cannot carry what the row would compose.
const ONCE_REFUSED_HINT: &str = "(refused in once-mode — the temp root cannot carry it)";

/// The `MYSBX_DETACHED` disabled-row hint of the detachable row
/// (T14.1): the dispatcher's nested-detach guard, mirrored — a
/// second detached run for the same workspace is not valid inside
/// the one this picker runs in.
const NESTED_DETACHED_HINT: &str = "(refused inside a detached run — MYSBX_DETACHED is set)";

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

/// The value layer of one picker row (T2/T9/T10/T11/T12, and
/// T13–T15): the launch rows and the tri-state cycles are a closed
/// set of values with one selected; the typed rows are a draft
/// under a byte grammar. `ReadOnly` is the network row (T9):
/// rendered, never cycled, never composed. `Disabled` is the
/// environment-refused row (the once-mode session/detachable rows,
/// the MYSBX_DETACHED detachable row, T13.1/T14.1/T15.1): visible
/// never hidden (the T2 disabled-row precedent), annotated with the
/// reason, no key changes it, it never contributes to the Enter
/// state — the picker must not compose what the CLI form would
/// refuse.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Value {
    /// A closed set with one selected — the launch rows and the
    /// tri-state cycles. `None` on a disabled row, and on a row
    /// whose merged value no layer of the set carries (T3).
    Set(Row),
    /// A typed row (T10/T15): `draft` is `None` while the row is
    /// untouched (empty — Enter composes no flag, the layers
    /// decide) and `Some` once the operator typed a key the
    /// grammar admitted; `seed` is the merged value's display
    /// spelling (the fact the layers decided, T3's preselect
    /// translated to a typed row — context, never a composed
    /// value: an untouched row's seed must not reach `Flags`).
    Typed {
        row: TypedRow,
        draft: Option<String>,
        seed: String,
    },
    /// The read-only network row (T9): Left/Right are no-ops, no
    /// key contributes, Enter composes nothing.
    ReadOnly { text: String, hint: &'static str },
    /// An environment-refused row (T13.1/T14.1/T15.1): the once-mode
    /// session/detachable rows, the `MYSBX_DETACHED` detachable row
    /// — visible, annotated with `hint`, untouchable, never
    /// composed. The disabled-row spelling of T2's empty
    /// registries: the operator sees the decision exists but is not
    /// theirs to make here.
    Disabled { hint: &'static str },
}

/// The typed row's grammar and Enter-time parser (T10/T15): which
/// keys the draft accepts byte by byte, and which parser the Enter
/// composition runs — the SAME grammar a config file's value (or
/// the argv flag's own value) passes through, so the picker cannot
/// accept what a layer or the parser could not.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TypedRow {
    /// `cpus`: digits and at most one dot — podman's `--cpus` shape.
    Cpus,
    /// `memory`: digits plus one optional `b`/`k`/`m`/`g` suffix
    /// (any case) — the config parser's grammar.
    Memory,
    /// `pids-limit`: digits only, a positive `u32`.
    PidsLimit,
    /// `session` (T15): the `--session NAME` argv grammar's own
    /// byte set — `[A-Za-z0-9._-]`, capped at 64 chars (the
    /// validator's own bound). The `q` CONFLICT is resolved at the
    /// LOOP, never here: a grammar-side refusal would turn the
    /// abort key into an ignored keystroke at the row's steady
    /// state, and the loop's routing keeps `q` the abort at an
    /// EMPTY draft (the row's steady state, the untouched row)
    /// while admitting it into a non-empty one — the record's
    /// decision (tui.md T15).
    Session,
}

impl TypedRow {
    /// Whether `b` may extend `draft` (T10/T15): the grammar filters
    /// key by key — a key outside it is an ignored keystroke, never
    /// an error mid-draft.
    fn admits(&self, draft: &str, b: u8) -> bool {
        if matches!(self, Self::Session) {
            return Self::session_admits(draft, b);
        }
        let letters = self.letters();
        let last = draft.as_bytes().last();
        // memory: the suffix is the LAST byte — nothing may follow
        // it, not even a digit (`8g2` is not a size).
        if self == &Self::Memory && last.is_some_and(|c| letters.contains(&c)) {
            return false;
        }
        match (b.is_ascii_digit(), last) {
            (true, _) => true,
            // The one optional suffix byte — after at least one
            // digit (a bare `g` is not a size).
            (false, Some(c)) if letters.contains(&b) => c.is_ascii_digit(),
            // cpus admits exactly one dot, between digits (podman's
            // `--cpus` shape: `2`, `1.5` — never a leading or double
            // dot).
            (false, _) if self == &Self::Cpus && b == b'.' => {
                !draft.is_empty() && !draft.contains('.')
            }
            _ => false,
        }
    }

    /// The suffix letters the grammar admits after digits (`memory`'s
    /// `b`/`k`/`m`/`g`, any case; none for the digit-only rows).
    fn letters(&self) -> &'static [u8] {
        match self {
            Self::Cpus | Self::PidsLimit | Self::Session => &[],
            Self::Memory => b"bkmgBKMG",
        }
    }

    /// The byte grammar of the session row (T15): the argv
    /// grammar's own set `[A-Za-z0-9._-]`, capped at 64 chars —
    /// `session::valid_name`'s own bound, so a draft can never
    /// grow past what the validator could accept. `q` is NOT
    /// special-cased here — the loop routes the abort conflict
    /// (see [`TypedRow::Session`], the record's T15 decision).
    fn session_admits(draft: &str, b: u8) -> bool {
        if draft.len() >= 64 {
            return false;
        }
        matches!(b, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'.' | b'_' | b'-')
    }

    /// Parse the finished draft through config.rs's OWN parser at
    /// Enter (T10): the draft is a number for `cpus`, a string for
    /// `memory` and an integer for `pids-limit`, so each grammar
    /// builds the TOML value shape its parser reads — a draft the
    /// parser refuses is a picker error naming the row, never a
    /// dispatch. The return is the `Flags` spelling: the string the
    /// parser canonicalized (`cpus`/`memory`) and, for `pids-limit`,
    /// the parsed `u32`.
    fn parse(&self, draft: &str) -> Result<(String, Option<u32>), String> {
        // T15: the session draft is validated through the SAME
        // validator the `--session` argv parser uses
        // (`session::valid_name`, lib.rs's parse arm) — the picker
        // cannot accept what the flag could not. An invalid draft
        // is the picker error line's own message: the argv parser's
        // refusal names the grammar, so the picker's does too (the
        // reserved `__worktrees` suffix included — the validator's
        // own rule, never re-derived here).
        if matches!(self, Self::Session) {
            if crate::session::valid_name(draft) {
                return Ok((draft.to_owned(), None));
            }
            // The argv parser's message names the grammar; the
            // PICKER's error line adds the validator's OTHER rule a
            // draft can trip — the reserved `__worktrees` suffix —
            // so a draft like `a__worktrees` fails with a message
            // that names why (the draft satisfies the byte grammar
            // the plain message cites). The argv parser's own
            // message stays untouched.
            return Err("session: invalid session name — the grammar is \
                 [A-Za-z0-9][A-Za-z0-9._-]{0,63} — no slashes, no leading dot, \
                 and it must not end in the reserved `__worktrees` suffix"
                .to_owned());
        }
        let value = match self {
            // (Session never reaches this construction — its parse
            // arm above returns before it — but the match must stay
            // exhaustive; the String shape is what a NAME is.)
            Self::Session => toml::Value::String(draft.to_owned()),
            // The cpus grammar's two shapes keep their EXACT value:
            // a dot-free draft goes through `i64` directly (an
            // overflow is an error — the f64 detour would saturate
            // a 17-digit draft into i64::MAX silently), a dotted
            // whole one through `f64` CAPPED at 2^53 before the
            // Integer conversion — below 2^53 every integer is
            // exactly representable, so `n as i64` is the value the
            // operator typed; at or above it the conversion could
            // round and the draft is refused instead.
            Self::Cpus if !draft.contains('.') => match draft.parse::<i64>() {
                Ok(n) => toml::Value::Integer(n),
                Err(_) => return Err("cpus: expected a positive number".to_owned()),
            },
            Self::Cpus => match draft.parse::<f64>() {
                Ok(n) if n.fract() == 0.0 => {
                    const EXACT_I64_CAP: f64 = 9_007_199_254_740_992.0; // 2^53
                    if n.abs() >= EXACT_I64_CAP {
                        return Err("cpus: the value is too large to keep exactly".to_owned());
                    }
                    toml::Value::Integer(n as i64)
                }
                Ok(n) => toml::Value::Float(n),
                Err(_) => return Err("cpus: expected a positive number".to_owned()),
            },
            Self::Memory => toml::Value::String(draft.to_owned()),
            Self::PidsLimit => match draft.parse::<i64>() {
                Ok(n) => toml::Value::Integer(n),
                Err(_) => return Err("pids-limit: expected a positive integer".to_owned()),
            },
        };
        match self {
            Self::Cpus => crate::config::cpus(&value)
                .map(|s| (s, None))
                .map_err(|e| e.to_string()),
            Self::Memory => crate::config::memory(&value)
                .map(|s| (s, None))
                .map_err(|e| e.to_string()),
            Self::PidsLimit => crate::config::pids_limit(&value)
                .map(|n| (n.to_string(), Some(n)))
                .map_err(|e| e.to_string()),
            Self::Session => Ok((draft.to_owned(), None)),
        }
    }
}

/// The environment the Enter run dispatches into (T13.1/T14.1/
/// T15.1): the two facts that decide whether the session and
/// detachable rows are offerable or disabled — the once mode and
/// the nested-detached marker. Both mirror the dispatcher's OWN
/// refusal scope exactly: `once_gate` refuses `--session` and
/// `--detach`/`--detachable` (D22), and the nested guard refuses
/// `--detach`/`--detachable` under `MYSBX_DETACHED` (lib.rs, B4
/// step 3) — session and dry-run are UNAFFECTED by the marker, the
/// guard's exact scope. The picker must not compose what the CLI
/// form would refuse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Env {
    /// The once picker (T8): the session and detachable rows are
    /// disabled — a clone would vanish with the temp root, and a
    /// detached run would outlive it with no owner to tear it down.
    once: bool,
    /// `MYSBX_DETACHED` set and non-empty (the dispatcher's own
    /// probe spelling): the detachable row is disabled — a nested
    /// `--detachable` is not valid inside the detached run this
    /// picker runs in.
    nested_detached: bool,
}

impl Env {
    /// The env of a picker about to run in `once` mode, reading the
    /// nested-detached marker the way the dispatcher's guard does.
    fn detect(once: bool) -> Env {
        Env {
            once,
            nested_detached: std::env::var_os("MYSBX_DETACHED").is_some_and(|v| !v.is_empty()),
        }
    }
}

/// [`rows_in`] for the persistent, not-nested environment — the
/// test fixture's default (all rows offerable) and the spelling
/// the plain-mode picker's rows are built through before `Env`
/// decides the trio's disabled-ness.
fn rows(enabled: &[&'static str], pinned_agents: &[String]) -> [Value; ROWS] {
    rows_in(enabled, pinned_agents, Env::default())
}

fn rows_in(enabled: &[&'static str], pinned_agents: &[String], env: Env) -> [Value; ROWS] {
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
    // The property rows carry no registry reads here: their values
    // come from the merge in `seed` (the network text, the typed
    // rows' seeds, the tri-state cycles), so `rows` builds them
    // unseeded and `seed` fills them in — the same split the three
    // launch rows have.
    let network = Value::ReadOnly {
        text: String::new(),
        hint: "(read-only — the merged network policy)",
    };
    let cpus = Value::Typed {
        row: TypedRow::Cpus,
        draft: None,
        seed: String::new(),
    };
    let memory = Value::Typed {
        row: TypedRow::Memory,
        draft: None,
        seed: String::new(),
    };
    let pids_limit = Value::Typed {
        row: TypedRow::PidsLimit,
        draft: None,
        seed: String::new(),
    };
    let nested_virt = Value::Set(Row {
        values: vec![INHERIT.to_owned(), "on".to_owned(), "off".to_owned()],
        selected: None,
        empty_hint: "(no nested-virt row)",
    });
    let store = Value::Set(Row {
        values: [INHERIT]
            .iter()
            .map(|n| n.to_string())
            .chain(Store::NAMES.iter().map(|n| n.to_string()))
            .collect(),
        selected: None,
        empty_hint: "(no store row)",
    });
    // T13: the dry-run row — `off` ALWAYS the preselect (no layer
    // can ask for a dry run, D9's promise has no TOML surface),
    // `on` the only other value. Offerable in BOTH modes (D22
    // allows `--dry-run` for once).
    let dry_run = Value::Set(Row {
        values: vec!["off".to_owned(), "on".to_owned()],
        selected: Some(0),
        empty_hint: "(no dry-run row)",
    });
    // T14: the detachable row — same shape as dry-run (`off` always
    // the preselect), but DISABLED in the two environments whose
    // CLI form the dispatcher itself refuses: once-mode (D22 — a
    // detached run outlives the temp root) and inside a detached
    // run (the nested guard, `MYSBX_DETACHED`). NEVER `Flags::detach`
    // — detach.md B2: the picker's shape is the attach-the-terminal
    // one, `mysbx tui` runs ON a terminal (T6).
    let detachable = if env.once || env.nested_detached {
        Value::Disabled {
            hint: if env.once {
                ONCE_REFUSED_HINT
            } else {
                NESTED_DETACHED_HINT
            },
        }
    } else {
        Value::Set(Row {
            values: vec!["off".to_owned(), "on".to_owned()],
            selected: Some(0),
            empty_hint: "(no detachable row)",
        })
    };
    // T15: the session row — typed (the flag has no TOML surface,
    // nothing to cycle over), seeded with the live mode's own
    // spelling, an empty draft the untouched live mode. DISABLED in
    // once-mode (D22: `--session` is refused — the clone would
    // vanish with the temp root); UNAFFECTED by `MYSBX_DETACHED`
    // (the nested guard scopes to the detach flags alone).
    let session = if env.once {
        Value::Disabled {
            hint: ONCE_REFUSED_HINT,
        }
    } else {
        Value::Typed {
            row: TypedRow::Session,
            draft: None,
            seed: LIVE.to_owned(),
        }
    };
    [
        Value::Set(backend),
        Value::Set(mux),
        Value::Set(agent),
        network,
        cpus,
        memory,
        pids_limit,
        nested_virt,
        store,
        dry_run,
        detachable,
        session,
    ]
}

/// The tri-state `nested-virt` seed T11 needs: the LAYERS' decision
/// before the merge applies the D27 off-default — `Some(true)` seeds
/// `on`, `Some(false)` seeds `off`, `None` seeds `inherit`. The
/// plain mode reads the folded chains (`LoadedLayers::user_files`/
/// `sidecar_files`, sidecar-over-user like the merge itself); the
/// once mode reads the folded user chain alone (T8/D22 — the host
/// sidecar the Enter run will not use must not seed, the same
/// reason `load_merged` ignores it).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NestedVirtSeed {
    /// The user chain's folded tri-state.
    user: Option<bool>,
    /// The sidecar layer's tri-state — `None` in once-mode.
    sidecar: Option<bool>,
}

impl NestedVirtSeed {
    /// The tri-state the merge would resolve — `merge::merge`'s own
    /// table, case for case: ANY-enable-wins (a user `true` stays
    /// `on` under a silent sidecar, a sidecar `true` wins over a
    /// silent user — the key is not a grant, either layer may
    /// enable it), any explicit `false` denies when nothing
    /// enabled, the one widening pair the merge REFUSES shows as
    /// undecided. The seed must never resolve differently from the
    /// run: the row is the fact the merge acts on.
    fn resolved(&self) -> Option<bool> {
        match (self.user, self.sidecar) {
            // The merge's hard error — unreachable through `run`
            // (the merge refused the config before the picker
            // rendered), shown as `inherit` for the same reason.
            (Some(false), Some(true)) => None,
            (Some(true), _) | (_, Some(true)) => Some(true),
            (Some(false), _) | (_, Some(false)) => Some(false),
            (None, None) => None,
        }
    }
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
///
/// The property rows (T9–T12): the network row's read-only text is
/// the RESOLVED merged value (`Merged::network` is never
/// tri-state); the typed rows seed their draft from the merged
/// value (a draft IS a touch — a seeded draft that was never typed
/// into must not compose, so the seed is display-only and the
/// draft starts empty/None); the tri-state cycles seed from
/// `NestedVirtSeed::resolved()` and `merged.store`.
///
/// The trio (T13–T15) needs no seeding: the dry-run and
/// detachable rows preselect `off` at build time (the flags have
/// no TOML surface — there is nothing a layer could decide for
/// them), and the session row's display seed is `live`, the
/// mode a bare run uses, set by `rows()` for the same reason.
fn seed(mut rows: [Value; ROWS], merged: &Merged, nv: NestedVirtSeed) -> [Value; ROWS] {
    let at = |row: &Row, value: &str| row.values.iter().position(|v| v == value);
    let Value::Set(backend) = &mut rows[0] else {
        unreachable!("rows() builds row 0 as a Set")
    };
    backend.selected = merged.backend.as_deref().and_then(|name| at(backend, name));
    let Value::Set(mux) = &mut rows[1] else {
        unreachable!("rows() builds row 1 as a Set")
    };
    mux.selected = at(mux, merged.multiplexer.name());
    // The `none` pseudo-value is a POSITION (the last element
    // `rows()` appends), never a name: a pinned kind literally
    // spelled `none` must not shadow the `--no-agent` spelling.
    let Value::Set(agent) = &mut rows[2] else {
        unreachable!("rows() builds row 2 as a Set")
    };
    agent.selected = match merged.default_agent.as_deref() {
        Some(kind) => at(agent, kind),
        None => Some(agent.values.len() - 1),
    };
    // T9: the resolved merged network — the row is read-only, the
    // text is the fact.
    if let Value::ReadOnly { text, .. } = &mut rows[3] {
        *text = merged.network.name().to_owned();
    }
    // T10: the typed rows' display seed — the merged value the
    // row would run with (`—` when no layer set one). The draft
    // stays untouched: a seed must never compose (T4), it is the
    // fact the operator types OVER.
    if let Value::Typed { seed, .. } = &mut rows[4] {
        *seed = merged.cpus.clone().unwrap_or_else(|| "—".to_owned());
    }
    if let Value::Typed { seed, .. } = &mut rows[5] {
        *seed = merged.memory.clone().unwrap_or_else(|| "—".to_owned());
    }
    if let Value::Typed { seed, .. } = &mut rows[6] {
        *seed = merged
            .pids_limit
            .map(|n| n.to_string())
            .unwrap_or_else(|| "—".to_owned());
    }
    // T11: the tri-state seed — `on`/`off` from the layers'
    // decision, `inherit` when none made one.
    if let Value::Set(nv_row) = &mut rows[7] {
        nv_row.selected = match nv.resolved() {
            Some(true) => at(nv_row, "on"),
            Some(false) => at(nv_row, "off"),
            None => at(nv_row, INHERIT),
        };
    }
    // T12: the store tri-state — `resolved_store()` is NOT consulted:
    // the row seeds what the LAYERS said, and `inherit` is the
    // honest spelling when no layer decided (the backend-shaped
    // default the resolution would apply is the pipeline's, shown
    // nowhere in the picker).
    if let Value::Set(st) = &mut rows[8] {
        st.selected = match merged.store {
            Some(Store::Host) => at(st, "host"),
            Some(Store::Baked) => at(st, "baked"),
            None => at(st, INHERIT),
        };
    }
    rows
}

/// Which rows the operator touched — set by a cycle that MOVED, a
/// typed byte a grammar admitted, or a Backspace that left a
/// draft standing (T4). An untouched row leaves its flag `None`, so
/// the configuration layers decide (config.md D6); the picker
/// never freezes a decision the configuration owns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Touched([bool; ROWS]);

/// Compose the touched rows into the `Flags` a bare run parses
/// (T4): a touched row becomes its one-run override flag, an
/// untouched one stays `None`. The typed rows' drafts were already
/// validated at Enter (the picker error path); the cycle rows'
/// `inherit` composes NO field — the layers decide, the same as
/// untouched. This is the picker's whole output — it writes nothing
/// else.
fn to_flags(touched: Touched, rows: &[Value; ROWS]) -> Flags {
    let mut flags = Flags::default();
    if touched.0[0] {
        if let Value::Set(row) = &rows[0] {
            if let Some(i) = row.selected {
                flags.backend = Some(row.values[i].clone());
            }
        }
    }
    if touched.0[1] {
        if let Value::Set(row) = &rows[1] {
            if let Some(i) = row.selected {
                // `parse_cli` is the same mapping the `--multiplexer`
                // flag itself uses (cli.md D14) — the row strings are
                // its spellings.
                flags.multiplexer = row
                    .values
                    .get(i)
                    .and_then(|v| Multiplexer::parse_cli(v).ok());
            }
        }
    }
    if touched.0[2] {
        if let Value::Set(row) = &rows[2] {
            if let Some(i) = row.selected {
                // The pseudo-value by index, like `seed` — never by name.
                if i == row.values.len() - 1 {
                    flags.agent = Some(AgentSelection::None);
                } else {
                    flags.agent = Some(AgentSelection::Kind(row.values[i].clone()));
                }
            }
        }
    }
    // T10: the typed rows — drafts validated at Enter (`run`'s
    // error line fired on the invalid one, and the loop
    // continued, so the compose below never sees a draft a
    // parser refuses). The composition is the parser's canonical
    // spelling, and a parse failure here — unreachable — stays
    // `None` rather than dispatch a value no parser accepts.
    if touched.0[4] {
        if let Value::Typed {
            row: TypedRow::Cpus,
            draft: Some(d),
            ..
        } = &rows[4]
        {
            if let Ok((canonical, _)) = TypedRow::Cpus.parse(d) {
                flags.cpus = Some(canonical);
            }
        }
    }
    if touched.0[5] {
        if let Value::Typed {
            row: TypedRow::Memory,
            draft: Some(d),
            ..
        } = &rows[5]
        {
            if let Ok((canonical, _)) = TypedRow::Memory.parse(d) {
                flags.memory = Some(canonical);
            }
        }
    }
    if touched.0[6] {
        if let Value::Typed {
            row: TypedRow::PidsLimit,
            draft: Some(d),
            ..
        } = &rows[6]
        {
            if let Ok((_, pids)) = TypedRow::PidsLimit.parse(d) {
                flags.pids_limit = pids;
            }
        }
    }
    // T11: `on`/`off` compose the bool; `inherit` composes nothing.
    if touched.0[7] {
        if let Value::Set(row) = &rows[7] {
            if let Some(i) = row.selected {
                flags.nested_virt = match row.values[i].as_str() {
                    "on" => Some(true),
                    "off" => Some(false),
                    _ => None,
                };
            }
        }
    }
    // T12: `host`/`baked` compose the store; `inherit` nothing.
    if touched.0[8] {
        if let Value::Set(row) = &rows[8] {
            if let Some(i) = row.selected {
                flags.store = match row.values[i].as_str() {
                    "host" => Some(Store::Host),
                    "baked" => Some(Store::Baked),
                    _ => None,
                };
            }
        }
    }
    // T13: `on` composes the dry-run promise — the EXISTING field
    // the argv parser's `--dry-run` sets. A picker composed
    // `dry_run` composes a DRY RUN: Enter prints the argv and exits
    // `0`, nothing started (the audit contract of D9, the picker's
    // Enter included). `off` composes nothing (the untouched
    // direction — a dry run is never the default).
    if touched.0[9] {
        if let Value::Set(row) = &rows[9] {
            if let Some(i) = row.selected {
                flags.dry_run = row.values[i] == "on";
            }
        }
    }
    // T14: `on` composes `--detachable` — and NEVER `Flags::detach`
    // (detach.md B2: the two are two ways to start ONE detached
    // run, never both, and the picker's shape is the
    // attach-the-terminal one). A `Value::Disabled` row is never
    // `Set`, so a disabled row composes nothing even if a bug set
    // its touched bit — the disabled spelling is a different
    // variant, not a frozen selection.
    if touched.0[10] {
        if let Value::Set(row) = &rows[10] {
            if let Some(i) = row.selected {
                flags.detachable = row.values[i] == "on";
            }
        }
    }
    // T15: a valid session draft composes `Flags::session` — the
    // EXISTING field the argv parser's `--session NAME` sets,
    // validated through the same `session::valid_name` at Enter
    // (the validation lives in the loop's Enter arm; this arm
    // only composes). An empty draft is the untouched row: the
    // live mode, the layers' and the flag's own default.
    if touched.0[11] {
        if let Value::Typed {
            row: TypedRow::Session,
            draft: Some(name),
            ..
        } = &rows[11]
        {
            if crate::session::valid_name(name) {
                flags.session = Some(name.clone());
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
    // with the verb, and the T9–T12 fields have no CLI spelling at
    // all — so every field the picker owns is `None` on entry, and
    // the overwrite in the Enter arm must never silently swallow a
    // flag a dispatcher bug let through.
    debug_assert!(
        flags.backend.is_none()
            && flags.multiplexer.is_none()
            && flags.agent.is_none()
            && flags.cpus.is_none()
            && flags.memory.is_none()
            && flags.pids_limit.is_none()
            && flags.nested_virt.is_none()
            && flags.store.is_none()
            && !flags.dry_run
            && !flags.detachable
            && flags.session.is_none()
    );
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
        let (merged, nv) = match load_merged(repo, once) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("mysbx: {e}");
                return EXIT_INFRASTRUCTURE;
            }
        };
        let picker = seed(
            rows_in(
                &doctor::enabled_backends(),
                &crate::pinned_agent_names(),
                Env::detect(once),
            ),
            &merged,
            nv,
        );
        return match picker_loop(picker, once) {
            Ok(Some((touched, rows))) => {
                // T10: the touched typed rows' drafts are parsed
                // HERE, before any flag is composed — an invalid
                // draft is a PICKER error (the loop showed the
                // error line and continued), so a draft that
                // reaches the compose is one a config.rs parser
                // accepted. The validation lives in the loop's
                // Enter arm; this arm only composes.
                let mut run_flags = flags;
                let selected = to_flags(touched, &rows);
                run_flags.backend = selected.backend;
                run_flags.multiplexer = selected.multiplexer;
                run_flags.agent = selected.agent;
                run_flags.cpus = selected.cpus;
                run_flags.memory = selected.memory;
                run_flags.pids_limit = selected.pids_limit;
                run_flags.nested_virt = selected.nested_virt;
                run_flags.store = selected.store;
                run_flags.dry_run = selected.dry_run;
                run_flags.detachable = selected.detachable;
                run_flags.session = selected.session;
                // T14: the row's `on` maps to `RunMode::Detachable`
                // HERE, mirroring the dispatcher's own bare-form
                // arm (lib.rs ~258) — the mode is what routes the
                // pipeline to `detach::launch_outer`, where the
                // guards T14 promises live (B9's podman refusal,
                // B10's dtach pin, B3's runtime root); a composed
                // `detachable` flag with the mode unchanged would
                // be accept-and-ignore, the exact anti-pattern
                // the dispatcher's comment warns about. The
                // dispatcher's own tty guard
                // (`detachable && !dry_run && !stdin_is_tty`, lib.rs
                // ~249) is dispatcher-side and not hit by this path,
                // but its precondition holds by construction: the
                // picker already refused at `stdin_stdout_tty()`
                // (T6), and once-mode and `MYSBX_DETACHED` disable
                // the row, so only `RunMode::Exec` can arrive here
                // with the flag set. dry-run+detachable composes
                // fine: `preflight_only` covers Detachable, so the
                // Enter run prints the argv and exits `0`, the
                // same as the argv spelling.
                let mode = if run_flags.detachable && matches!(mode, crate::RunMode::Exec) {
                    crate::RunMode::Detachable
                } else {
                    mode
                };
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
fn load_merged(repo: &repo::Repo, once: bool) -> Result<(Merged, NestedVirtSeed), String> {
    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .unwrap_or_default();
    let xdg = std::env::var("XDG_CONFIG_HOME").ok();
    if !once {
        let layers =
            merge::load_layers(&home, xdg.as_deref(), &repo.sidecar).map_err(|e| e.to_string())?;
        // T11: the tri-state seed from the folded chains — the
        // user chain's fold first, the sidecar layer's own value
        // over it, exactly the two inputs the merge resolves from
        // (the upgrade hard error stays the merge's; the seed shows
        // `inherit` for the pair the merge would refuse).
        let nv = NestedVirtSeed {
            user: layers.user.0.nested_virt,
            sidecar: layers.sidecar.0.nested_virt,
        };
        return merge::merge(
            layers.user.0,
            layers.sidecar.0,
            &layers.user.1,
            &layers.sidecar.1,
            &home,
        )
        .map(|m| (m, nv))
        .map_err(|e| e.to_string());
    }
    let user_path = merge::user_config_path(&home, xdg.as_deref());
    let user =
        merge::load_layer_chain(&user_path, "user", &home, None).map_err(|e| e.to_string())?;
    // Once-mode (T8/D22): the user chain ALONE — the host sidecar's
    // tri-state must not seed a row whose Enter run will not read
    // it, the same isolation `load_merged`'s merge half keeps.
    let nv = NestedVirtSeed {
        user: user.config.nested_virt,
        sidecar: None,
    };
    merge::merge(
        user.config,
        crate::config::Config::default(),
        &user.top,
        &user.top,
        &home,
    )
    .map(|m| (m, nv))
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
                // — no operator pastes that much into a
                // row-per-decision picker. Give up, and DROP the
                // residue: everything still QUEUED drains here, so
                // no content byte of the oversized paste ever
                // decodes as a key. That was a documented residual
                // when the picker only cycled — a leaked `q`
                // aborted, harmless — but the typed rows draft
                // byte by byte, so residue would type itself into
                // a draft. Only the QUEUE drains: bytes the source
                // delivers later are fresh keypresses the picker
                // cannot tell from new input, and draining the
                // source itself would be the unbounded consume the
                // cap exists to prevent.
                while self.peek().is_some() {
                    self.take();
                }
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
fn picker_loop(
    mut picker: [Value; ROWS],
    once: bool,
) -> Result<Option<(Touched, [Value; ROWS])>, ()> {
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
    // The one mutable render state beyond the rows (T10): the
    // Enter-time validation error line — shown until any key
    // redraws, so the operator reads it and edits the draft.
    let mut error: Option<String> = None;
    // ONE reader for the loop's whole life: the queue survives
    // across keypresses, so a coalesced burst (`xq`, an arrow and
    // Enter in one read) decodes key by key, nothing lost — a
    // fresh reader per keypress would drop the queue every time.
    let mut reader = tty_key_reader();
    loop {
        render(&picker, focus, once, error.as_deref());
        let key = read_key(&mut reader);
        // Any key but Enter clears the error line — the operator
        // has seen it; the next render shows the rows alone.
        let delta = match key {
            Key::Up => -1,
            Key::Down => 1,
            Key::Left => {
                if cycle_value(&mut picker, focus, -1) {
                    touched.0[focus] = true;
                }
                error = None;
                continue;
            }
            Key::Right => {
                if cycle_value(&mut picker, focus, 1) {
                    touched.0[focus] = true;
                }
                error = None;
                continue;
            }
            Key::Enter => {
                // T10: the typed rows' touched drafts are validated
                // HERE, through config.rs's own parsers — an invalid
                // draft is a PICKER error: the line renders inside
                // the picker, Enter dispatches NOTHING (neither the
                // valid rows nor this one), and the loop continues
                // with every draft intact and editable. The
                // read-only and cycle rows validate trivially.
                match validate_enter(&picker, &touched) {
                    Ok(()) => {
                        leave_screen();
                        drop(guard);
                        return Ok(Some((touched, picker)));
                    }
                    Err(msg) => {
                        error = Some(msg);
                        continue;
                    }
                }
            }
            Key::Char(0x7f) => {
                // Backspace (T10): delete the focused typed row's
                // last draft byte — an empty draft returns the row
                // to untouched (Enter composes no flag).
                let had_draft = draft_of(&picker[focus]).is_some();
                backspace_value(&mut picker, focus);
                if had_draft {
                    touched.0[focus] = true;
                }
                error = None;
                continue;
            }
            // ISIG is off in raw mode, so ^C arrives as byte 3 —
            // bound to the abort like Esc and `q` so the operator
            // can always interrupt (exit `0`, nothing started).
            //
            // The `q` conflict of the session row (T15, the
            // record's decision): `q` is a draft byte ONLY while
            // the focused session row's draft is non-empty — at an
            // EMPTY draft (the row's steady state, the untouched
            // row) it is the abort, exactly as everywhere else in
            // the picker. The abort set stays reachable in ONE
            // keypress from every state: Esc and ^C abort
            // unconditionally (no grammar admits byte `0x1b` or
            // byte `3`), and `q` aborts the common case. The cost
            // is named in the record: a session name whose FIRST
            // byte is `q` aborts — that name is spelled on the
            // argv, the full-grammar surface.
            Key::Esc | Key::Char(0x03) => {
                leave_screen();
                drop(guard);
                return Ok(None);
            }
            Key::Char(b'q')
                if matches!(
                    &picker[focus],
                    Value::Typed {
                        row: TypedRow::Session,
                        draft: Some(_),
                        ..
                    },
                ) && draft_of(&picker[focus])
                    .is_some_and(|d| TypedRow::Session.admits(d, b'q')) =>
            {
                // A `q` INSIDE a non-empty session draft: the loop
                // routes it to the grammar before the abort arm —
                // the gate the record decided, never a shadowing of
                // the abort (the empty draft, the steady state,
                // falls through to the abort arm below). The guard
                // checks the GRAMMAR too: a `q` the grammar refuses
                // (the 64-char cap at a full draft) falls through
                // to the abort as well — a gate that swallowed it
                // there would leave the row in a state where `q`
                // is neither draft nor abort nor error, a silent
                // no-op; the invariant "the abort reachable in one
                // keypress from every state" then holds for `q`
                // literally, not just via Esc/^C.
                if type_byte_value(&mut picker, focus, b'q') {
                    touched.0[focus] = true;
                }
                error = None;
                continue;
            }
            Key::Char(b'q') => {
                leave_screen();
                drop(guard);
                return Ok(None);
            }
            // A complete unbound sequence (Home, End, Delete, an
            // F-key, a modifier arrow, a bracketed paste): one
            // unit, consumed, bound to nothing — the render loop
            // simply redraws. Esc stays the operator's abort.
            Key::Ignored => {
                error = None;
                continue;
            }
            // Every other plain byte is offered to the focused
            // typed row's grammar (T10/T15): admitted bytes draft,
            // keys outside the grammar — letters at the digit-only
            // rows, a second dot, a suffix before digits, bytes
            // past the session cap — are ignored keystrokes, never
            // errors. `q`, `^C` and the aborts were matched above;
            // the digit grammars admit no letters but the memory
            // suffix, and the session grammar's `q` is routed at
            // the arm above, so no binding is shadowed.
            Key::Char(b) => {
                if type_byte_value(&mut picker, focus, b) {
                    touched.0[focus] = true;
                }
                error = None;
                continue;
            }
        };
        focus = ((focus as i64 + delta).rem_euclid(ROWS as i64)) as usize;
        error = None;
    }
}

/// The row operations the loop and the tests share — each spelled
/// ONCE, on the raw `[Value; ROWS]` array `picker_loop` owns:
/// `cycle_value` (Left/Right on a Set row, a no-op on the others),
/// `type_byte_value` (the focused typed row's grammar filter),
/// `backspace_value` (the draft's last byte), `draft_of` (the
/// tests' window into a typed row) and `validate_enter` (the
/// Enter-time parse).
fn cycle_value(rows: &mut [Value; ROWS], focus: usize, delta: i64) -> bool {
    match &mut rows[focus] {
        Value::Set(row) => row.cycle(delta),
        Value::Typed { .. } | Value::ReadOnly { .. } | Value::Disabled { .. } => false,
    }
}

fn type_byte_value(rows: &mut [Value; ROWS], focus: usize, b: u8) -> bool {
    match &mut rows[focus] {
        Value::Typed { row, draft, .. } => {
            let current = draft.clone().unwrap_or_default();
            if !row.admits(&current, b) {
                return false;
            }
            let mut next = current;
            next.push(b as char);
            *draft = Some(next);
            true
        }
        _ => false,
    }
}

fn backspace_value(rows: &mut [Value; ROWS], focus: usize) {
    if let Value::Typed { draft, .. } = &mut rows[focus] {
        if let Some(d) = draft.as_mut() {
            d.pop();
            if d.is_empty() {
                *draft = None;
            }
        }
    }
}

fn draft_of(value: &Value) -> Option<&str> {
    match value {
        Value::Typed { draft, .. } => draft.as_deref(),
        _ => None,
    }
}

/// The Enter-time validation (T10): every TOUCHED typed row's draft
/// must parse through its config.rs grammar parser; the cycle and
/// read-only rows validate trivially (their spellings are the
/// closed sets). `Err` is the picker error line — the loop renders
/// it and continues; `Ok` is the dispatch.
fn validate_enter(rows: &[Value; ROWS], touched: &Touched) -> Result<(), String> {
    // The typed rows: cpus/memory/pids-limit (4–6, T10) and the
    // session row (11, T15) — every TOUCHED row's draft must parse
    // through its own validator (config.rs's parsers, the argv
    // grammar's `session::valid_name`), so the picker cannot accept
    // what a config layer or the flag could not. The cycle and
    // read-only rows validate trivially (their spellings are the
    // closed sets).
    for idx in [4usize, 5, 6, 11] {
        if !touched.0[idx] {
            continue;
        }
        if let Value::Typed {
            row,
            draft: Some(d),
            ..
        } = &rows[idx]
        {
            if let Err(msg) = row.parse(d) {
                // The config parser's message already names its key
                // (`memory: expected a size …`) — the two shapes
                // the pre-conversion checks refuse (a draft the
                // grammar cannot hand the parser at all) carry the
                // picker's own message; everything else is the
                // parser's text, what a config layer would print.
                // The session row's message names the argv grammar,
                // the `--session` parser's own refusal shape.
                return Err(msg);
            }
        }
    }
    Ok(())
}

/// Full-screen ANSI render (T6): a redraw per key is the whole point
/// of three rows — no line editing. The focused row is inverted,
/// the selected value bracketed. The clear homes the render; the
/// picker runs on the alternate screen ([`enter_screen`]), so on
/// a capable terminal every clear hits the alternate buffer, and
/// an incapable one gets today's clear-per-redraw behavior.
fn render(rows: &[Value; ROWS], focus: usize, once: bool, error: Option<&str>) {
    let mut out = std::io::stdout();
    let _ = write!(out, "\x1b[2J\x1b[H");
    let verb = if once { "mysbx-once" } else { "mysbx tui" };
    let _ = writeln!(out, "{verb} — pick this session's sandbox\n");
    for (i, value) in rows.iter().enumerate() {
        if i == focus {
            let _ = write!(out, "\x1b[7m>{:<14}\x1b[0m", ROW_LABELS[i]);
        } else {
            let _ = write!(out, " {:<14}", ROW_LABELS[i]);
        }
        match value {
            Value::Set(row) => {
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
            }
            // T10: the draft over the seed — the merged value the
            // row would run with is the context (bracketed, the
            // preselect spelling), the draft the override being
            // typed after it; `—` when no layer set one.
            Value::Typed { draft, seed, .. } => {
                let _ = write!(out, "[{seed}]");
                if let Some(d) = draft {
                    let _ = write!(out, " {d}");
                }
            }
            // T9: the read-only row — the disabled-row spelling
            // (T2's precedent): visible, the hint announcing the
            // read-only-ness, Left/Right never moving it.
            Value::ReadOnly { text, hint } => {
                if text.is_empty() {
                    let _ = write!(out, "{hint}");
                } else {
                    let _ = write!(out, "{text} {hint}");
                }
            }
            // T13.1/T14.1/T15.1: the environment-refused row — the
            // disabled-row spelling (T2's empty-registry precedent,
            // the same greyed treatment): visible, never hidden, the
            // hint announcing the reason, no key changes it.
            Value::Disabled { hint } => {
                let _ = write!(out, "{hint}");
            }
        }
        let _ = writeln!(out);
    }
    // T10: the Enter-time validation error — the one line the
    // picker adds of its own, inside its own screen.
    if let Some(msg) = error {
        let _ = writeln!(out, "\n  error: {msg}");
    }
    let _ = writeln!(
        out,
        "\n  ←/→ cycle · ↑/↓ move · type to draft · Enter start · Esc or q abort"
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

    /// The neutral tri-state seed (`NestedVirtSeed` with both layers
    /// undecided) — the seed of every test that does not exercise
    /// the T11 row.
    fn nv() -> NestedVirtSeed {
        NestedVirtSeed {
            user: None,
            sidecar: None,
        }
    }

    /// The `Value::Set` of a seeded row — the tests' view into the
    /// launch/cycle rows.
    fn set_of(v: &Value) -> &Row {
        match v {
            Value::Set(r) => r,
            _ => panic!("the test asked for a Set row"),
        }
    }

    /// A picker built and seeded the way `run` builds it — the
    /// default test fixture.
    fn seeded_picker(
        backend: Option<&str>,
        mux: Multiplexer,
        agent: Option<&str>,
    ) -> [Value; ROWS] {
        seed(rows(&[], &pins(&[])), &merged(backend, mux, agent), nv())
    }

    #[test]
    fn the_backend_row_is_the_registry_intersected_with_the_host_gate() {
        // The ∩ keeps the REGISTRY's order and drops what the gate
        // did not name — never a name a run here cannot start.
        let r = rows(&["podman-krun", "zzz-not-a-backend", "nono"], &pins(&[]));
        assert_eq!(
            set_of(&r[0]).values,
            vec!["nono".to_owned(), "podman-krun".to_owned()]
        );
    }

    #[test]
    fn an_empty_backend_registry_renders_one_disabled_row() {
        let r = rows(&["zzz-not-a-backend"], &pins(&[]));
        assert!(set_of(&r[0]).values.is_empty());
        assert_eq!(set_of(&r[0]).selected, None);
        assert_eq!(set_of(&r[0]).current(), "(no backend enabled on this host)");
    }

    #[test]
    fn the_multiplexer_row_lists_the_config_key_universe() {
        let r = rows(&[], &pins(&[]));
        assert_eq!(
            set_of(&r[1]).values,
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
            set_of(&r[2]).values,
            vec!["pi".to_owned(), "opencode".to_owned(), "none".to_owned()]
        );
    }

    #[test]
    fn a_build_without_pins_carries_only_the_none_pseudo_value() {
        let r = rows(&[], &pins(&[]));
        assert_eq!(set_of(&r[2]).values, vec!["none".to_owned()]);
    }

    #[test]
    fn seeding_preselects_what_a_bare_run_would_do() {
        let r = rows(&["nono", "bubblewrap"], &pins(&["pi", "opencode"]));
        let seeded = seed(
            r,
            &merged(Some("nono"), Multiplexer::Herdr, Some("opencode")),
            nv(),
        );
        assert_eq!(set_of(&seeded[0]).current(), "nono");
        assert_eq!(set_of(&seeded[1]).current(), "herdr");
        assert_eq!(set_of(&seeded[2]).current(), "opencode");
    }

    #[test]
    fn an_undecided_multiplexer_seeds_the_pipeline_default() {
        // T3: an omitted key is the plain shell — the merged value
        // is already `Multiplexer::None`, and the row selects `none`.
        let seeded = seeded_picker(None, Multiplexer::None, None);
        assert_eq!(set_of(&seeded[1]).current(), "none");
    }

    #[test]
    fn a_pinned_agent_literally_named_none_does_not_shadow_the_pseudo_value() {
        // The pseudo-value is a position (the appended last element),
        // never a name: `none` as a KIND stays selectable and is
        // composed as `--agent none`, while no default resolves to
        // the `--no-agent` spelling.
        let r = rows(&[], &pins(&["none"]));
        assert_eq!(
            set_of(&r[2]).values,
            vec!["none".to_owned(), "none".to_owned()]
        );
        let seeded = seed(r, &merged(None, Multiplexer::None, Some("none")), nv());
        assert_eq!(set_of(&seeded[2]).selected, Some(0));
        let mut touched = Touched::default();
        touched.0[2] = true;
        match to_flags(touched, &seeded).agent {
            Some(crate::AgentSelection::Kind(k)) => assert_eq!(k, "none"),
            other => panic!("wrong agent flag: {other:?}"),
        }
        let r = rows(&[], &pins(&["none"]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None), nv());
        assert_eq!(set_of(&seeded[2]).selected, Some(1));
        assert_eq!(set_of(&seeded[2]).current(), "none");
    }

    #[test]
    fn a_default_agent_that_resolves_to_nothing_preselects_none() {
        let r = rows(&[], &pins(&["pi"]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None), nv());
        assert_eq!(set_of(&seeded[2]).current(), AGENT_NONE);
    }

    #[test]
    fn a_backend_no_layer_names_is_the_row_with_nothing_preselected() {
        let r = rows(&["nono", "bubblewrap"], &pins(&[]));
        let seeded = seed(r, &merged(None, Multiplexer::None, None), nv());
        assert_eq!(set_of(&seeded[0]).selected, None);
        // `current()` of a selected-less row shows the first value
        // unbracketed; Enter composes no backend flag either way
        // (`to_flags` keys on `selected`), so the pipeline's own
        // "no backend configured" refusal decides — the picker never
        // guesses one.
        assert_eq!(set_of(&seeded[0]).current(), "bubblewrap");
    }

    #[test]
    fn a_merged_value_outside_the_row_set_stays_unselected() {
        // A disabled backend, an unpinned agent: the row shows
        // nothing selected and Enter composes no flag — the
        // pipeline's refusal names the value (T4), never the picker.
        let r = rows(&["nono"], &pins(&["pi"]));
        let seeded = seed(
            r,
            &merged(Some("krun"), Multiplexer::None, Some("orca")),
            nv(),
        );
        assert_eq!(set_of(&seeded[0]).selected, None);
        assert_eq!(set_of(&seeded[1]).current(), "none");
        assert_eq!(set_of(&seeded[2]).selected, None);
    }

    #[test]
    fn an_untouched_row_leaves_its_flag_unset() {
        let r = rows(&["nono"], &pins(&["pi"]));
        let seeded = seed(r, &merged(Some("nono"), Multiplexer::Tmux, None), nv());
        let flags = to_flags(Touched::default(), &seeded);
        assert_eq!(flags.backend, None);
        assert_eq!(flags.multiplexer, None);
        assert_eq!(flags.agent, None);
        // T9–T12: the property rows compose nothing either — the
        // untouched direction spans every row the picker carries.
        assert_eq!(flags.cpus, None);
        assert_eq!(flags.memory, None);
        assert_eq!(flags.pids_limit, None);
        assert_eq!(flags.nested_virt, None);
        assert_eq!(flags.store, None);
    }

    #[test]
    fn a_touched_row_composes_its_flag() {
        let r = rows(&["nono"], &pins(&["pi", "opencode"]));
        let seeded = seed(r, &merged(Some("nono"), Multiplexer::None, None), nv());
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
        let mut row = set_of(&r[2]).clone();
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
        let mut disabled = set_of(&r[0]).clone();
        assert!(!disabled.cycle(1));
        assert_eq!(disabled.selected, None);

        let r = rows(&["nono"], &pins(&[]));
        let mut single = set_of(&r[0]).clone();
        assert!(single.selected.is_none()); // rows() never preselects
        assert!(!single.cycle(1));
        assert_eq!(single.selected, None);
    }

    #[test]
    fn a_cycled_row_composes_the_cycled_value() {
        let r = rows(&[], &pins(&["pi", "opencode"]));
        let mut seeded = seed(r, &merged(None, Multiplexer::None, None), nv());
        assert!(cycle_value(&mut seeded, 2, 1)); // none → pi
        assert!(cycle_value(&mut seeded, 2, 1)); // pi → opencode
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
        let mut seeded = seeded_picker(None, Multiplexer::None, None);
        // `none` sits last in `Multiplexer::NAMES`; three rights wrap
        // to `herdr`.
        assert!(cycle_value(&mut seeded, 1, 3));
        assert_eq!(set_of(&seeded[1]).current(), "herdr");
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
        let mut row = set_of(&r[2]).clone();
        row.selected = None;
        assert!(row.cycle(1));
        assert_eq!(row.current(), "opencode");
        let mut row = set_of(&r[2]).clone();
        row.selected = None;
        assert!(row.cycle(-1));
        assert_eq!(row.current(), AGENT_NONE);
    }

    #[test]
    fn a_touched_row_without_a_selection_composes_no_flag() {
        // The undecided-backend case: the operator cannot make a
        // choice a disabled registry does not offer, so the flag
        // stays `None` and the pipeline's refusal decides.
        let seeded = seeded_picker(None, Multiplexer::None, None);
        let mut touched = Touched::default();
        touched.0[0] = true;
        let flags = to_flags(touched, &seeded);
        assert_eq!(flags.backend, None);
    }

    // ---- T9: the read-only network row ----

    #[test]
    fn the_network_row_shows_the_resolved_merged_value() {
        let seeded = seeded_picker(None, Multiplexer::None, None);
        match &seeded[3] {
            Value::ReadOnly { text, hint } => {
                // The merge's default: `shared` (D20's built-in).
                assert_eq!(text, "shared");
                assert!(!hint.is_empty(), "the read-only-ness is visible");
            }
            other => panic!("the network row is read-only: {other:?}"),
        }
    }

    #[test]
    fn left_and_right_on_the_network_row_move_nothing_and_never_touch() {
        let mut seeded = seeded_picker(None, Multiplexer::None, None);
        assert!(!cycle_value(&mut seeded, 3, 1));
        assert!(!cycle_value(&mut seeded, 3, -1));
        // No key contributes a draft either — typing at it is an
        // ignored keystroke, never a touch.
        assert!(!type_byte_value(&mut seeded, 3, b'7'));
        // And Enter with the row "touched" (which cannot happen —
        // nothing sets the bit) would compose no flag anyway: there
        // is no `Flags` field for network.
        let mut touched = Touched::default();
        touched.0[3] = true;
        let flags = to_flags(touched, &seeded);
        assert_eq!(flags.backend, None, "no network flag exists to compose");
    }

    // ---- T10: the typed rows ----

    #[test]
    fn the_typed_row_grammars_admit_only_their_shapes() {
        // cpus: digits and one dot.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 4, b'1'));
        assert!(type_byte_value(&mut v, 4, b'.'));
        assert!(type_byte_value(&mut v, 4, b'5'));
        assert_eq!(draft_of(&v[4]), Some("1.5"));
        assert!(!type_byte_value(&mut v, 4, b'.'), "a second dot is ignored");
        assert!(!type_byte_value(&mut v, 4, b'g'), "letters are ignored");
        // A leading dot is ignored too.
        let mut v = rows(&[], &pins(&[]));
        assert!(!type_byte_value(&mut v, 4, b'.'));

        // memory: digits plus one optional suffix, after digits.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 5, b'8'));
        assert!(type_byte_value(&mut v, 5, b'g'));
        assert_eq!(draft_of(&v[5]), Some("8g"));
        assert!(!type_byte_value(&mut v, 5, b'm'), "a second suffix byte");
        assert!(!type_byte_value(&mut v, 5, b'2'), "digits after the suffix");
        let mut v = rows(&[], &pins(&[]));
        assert!(!type_byte_value(&mut v, 5, b'g'), "a bare suffix");
        // Any case.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 5, b'4'));
        assert!(type_byte_value(&mut v, 5, b'M'));

        // pids-limit: digits only.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 6, b'4'));
        assert!(type_byte_value(&mut v, 6, b'2'));
        assert!(!type_byte_value(&mut v, 6, b'.'), "no dot");
        assert!(!type_byte_value(&mut v, 6, b'g'), "no suffix");
        assert_eq!(draft_of(&v[6]), Some("42"));
    }

    #[test]
    fn backspace_deletes_and_an_empty_draft_is_untouched() {
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 4, b'2'));
        assert!(type_byte_value(&mut v, 4, b'.'));
        assert_eq!(draft_of(&v[4]), Some("2."));
        backspace_value(&mut v, 4);
        assert_eq!(draft_of(&v[4]), Some("2"));
        backspace_value(&mut v, 4);
        // The empty draft is the untouched row: `None`, Enter composes
        // no flag.
        assert_eq!(draft_of(&v[4]), None);
        // Backspace on an empty draft is a no-op.
        backspace_value(&mut v, 4);
        assert_eq!(draft_of(&v[4]), None);
        // And on a non-typed row it is a no-op too.
        backspace_value(&mut v, 0);
        backspace_value(&mut v, 3);
    }

    #[test]
    fn a_typed_draft_composes_its_flag_through_the_config_parser() {
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 4, b'1'));
        assert!(type_byte_value(&mut v, 4, b'2'));
        assert!(type_byte_value(&mut v, 5, b'5'));
        assert!(type_byte_value(&mut v, 5, b'1'));
        assert!(type_byte_value(&mut v, 5, b'2'));
        assert!(type_byte_value(&mut v, 5, b'm'));
        assert!(type_byte_value(&mut v, 6, b'6'));
        assert!(type_byte_value(&mut v, 6, b'4'));
        // The drafts parse — validate_enter accepts.
        let mut touched = Touched::default();
        touched.0[4] = true;
        touched.0[5] = true;
        touched.0[6] = true;
        assert_eq!(validate_enter(&v, &touched), Ok(()));
        // And compose their flags.
        let flags = to_flags(touched, &v);
        assert_eq!(flags.cpus.as_deref(), Some("12"));
        assert_eq!(flags.memory.as_deref(), Some("512m"));
        assert_eq!(flags.pids_limit, Some(64));
    }

    #[test]
    fn the_cpus_grammar_keeps_the_exact_value_no_saturation() {
        // m1 regression: the dot-free draft must go through i64
        // DIRECTLY — the f64 detour saturated a 17-digit draft
        // into i64::MAX silently, and a 18-digit one rounded.
        let mut v = rows(&[], &pins(&[]));
        for _ in 0..19 {
            assert!(type_byte_value(&mut v, 4, b'9'));
        }
        let mut touched = Touched::default();
        touched.0[4] = true;
        assert!(
            validate_enter(&v, &touched).is_err(),
            "a 19-digit draft (past i64::MAX) is an overflow error, never a saturated i64::MAX"
        );
        assert_eq!(to_flags(touched, &v).cpus, None);
        // 2^53 and above: a dotted whole draft the f64 cannot keep
        // exactly is refused, never rounded.
        let mut v = rows(&[], &pins(&[]));
        for b in b"9007199254740992.0" {
            assert!(
                type_byte_value(&mut v, 4, *b),
                "byte {:?} drafted",
                *b as char
            );
        }
        let mut touched = Touched::default();
        touched.0[4] = true;
        assert!(
            validate_enter(&v, &touched).is_err(),
            "a dotted whole draft at 2^53 is refused, never rounded"
        );
        // The last exact values still compose: 2^53 - 1 dot-free,
        // and an ordinary dotted one.
        let mut v = rows(&[], &pins(&[]));
        for b in b"9007199254740991" {
            assert!(type_byte_value(&mut v, 4, *b));
        }
        let mut touched = Touched::default();
        touched.0[4] = true;
        assert_eq!(validate_enter(&v, &touched), Ok(()));
        assert_eq!(
            to_flags(touched, &v).cpus.as_deref(),
            Some("9007199254740991")
        );
        let mut v = rows(&[], &pins(&[]));
        for b in b"2.5" {
            assert!(type_byte_value(&mut v, 4, *b));
        }
        let mut touched = Touched::default();
        touched.0[4] = true;
        assert_eq!(validate_enter(&v, &touched), Ok(()));
        assert_eq!(to_flags(touched, &v).cpus.as_deref(), Some("2.5"));
    }

    #[test]
    fn an_invalid_draft_is_a_picker_error_and_never_a_dispatch() {
        // `0` is digits-only valid per the grammar, but the config
        // parser refuses it (a positive number is required) — the
        // Enter validation catches it and the loop would show the
        // error line; `to_flags` (post-validation) composes nothing
        // for a draft its parser refuses.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 4, b'0'));
        let mut touched = Touched::default();
        touched.0[4] = true;
        assert!(validate_enter(&v, &touched).is_err(), "cpus = 0 is refused");
        let flags = to_flags(touched, &v);
        assert_eq!(flags.cpus, None, "a refused draft composes nothing");
        // The memory row: `0m` — same refusal shape.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 5, b'0'));
        assert!(type_byte_value(&mut v, 5, b'm'));
        let mut touched = Touched::default();
        touched.0[5] = true;
        assert!(validate_enter(&v, &touched).is_err());
        // pids-limit = 0: refused (positive).
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 6, b'0'));
        let mut touched = Touched::default();
        touched.0[6] = true;
        assert!(validate_enter(&v, &touched).is_err());
        // And a huge pids draft the u32 cannot hold — the grammar
        // admits the digits, the parser refuses at Enter.
        let mut v = rows(&[], &pins(&[]));
        for _ in 0..11 {
            assert!(type_byte_value(&mut v, 6, b'9'));
        }
        let mut touched = Touched::default();
        touched.0[6] = true;
        assert!(validate_enter(&v, &touched).is_err());
    }

    #[test]
    fn the_typed_rows_seed_the_merged_value_as_display_only() {
        // The merged value shows as the row's context (`seed`), but
        // an untouched row composes no flag — the seed must never
        // reach `Flags`.
        let base = crate::merge::merge(
            crate::config::Config::default(),
            crate::config::Config::default(),
            Path::new("/tmp/fake-user.toml"),
            Path::new("/tmp/fake-sidecar.toml"),
            Path::new("/nonexistent-home"),
        )
        .unwrap();
        let m = Merged {
            cpus: Some("2.5".to_owned()),
            memory: Some("8g".to_owned()),
            pids_limit: Some(1024),
            ..base
        };
        let seeded = seed(rows(&[], &pins(&[])), &m, nv());
        for (idx, expected) in [(4usize, "2.5"), (5, "8g"), (6, "1024")] {
            match &seeded[idx] {
                Value::Typed { draft, seed, .. } => {
                    assert_eq!(seed, expected);
                    assert_eq!(*draft, None, "the seed is display-only");
                }
                other => panic!("row {idx} is typed: {other:?}"),
            }
        }
        // Untouched Enter composes nothing.
        let flags = to_flags(Touched::default(), &seeded);
        assert_eq!(flags.cpus, None);
        assert_eq!(flags.memory, None);
        assert_eq!(flags.pids_limit, None);
    }

    // ---- T11: the nested-virt cycle ----

    #[test]
    fn the_nested_virt_row_seeds_from_the_tri_state_and_cycles() {
        // None → inherit.
        let seeded = seeded_picker(None, Multiplexer::None, None);
        assert_eq!(set_of(&seeded[7]).current(), INHERIT);
        // Some(true) → on.
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            NestedVirtSeed {
                user: Some(true),
                sidecar: None,
            },
        );
        assert_eq!(set_of(&seeded[7]).current(), "on");
        // Some(false) → off.
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            NestedVirtSeed {
                user: Some(false),
                sidecar: None,
            },
        );
        assert_eq!(set_of(&seeded[7]).current(), "off");
        // The merge's own rule is ANY-ENABLE-WINS (merge.rs, D27):
        // either layer's `true` enables, either layer's explicit
        // `false` denies only when nothing enabled.
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            NestedVirtSeed {
                user: Some(false),
                sidecar: Some(true),
            },
        );
        // The user deny + sidecar enable is the merge's hard error —
        // the seed shows inherit, never a resolution the merge
        // refuses to make.
        assert_eq!(set_of(&seeded[7]).current(), INHERIT);
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            NestedVirtSeed {
                user: Some(true),
                sidecar: Some(false),
            },
        );
        // Any-enable-wins: the user's `true` stays `on` under the
        // silent-denying sidecar — the row shows exactly what the
        // run would do (the sidecar's `false` denies nothing here;
        // only a USER deny is un-liftable, D27).
        assert_eq!(set_of(&seeded[7]).current(), "on");
        // And the symmetrical direction: the sidecar enables, the
        // user layer stayed silent.
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            NestedVirtSeed {
                user: None,
                sidecar: Some(true),
            },
        );
        assert_eq!(set_of(&seeded[7]).current(), "on");
    }

    #[test]
    fn the_nested_virt_cycle_composes_the_bool_and_inherit_composes_nothing() {
        let seeded = seeded_picker(None, Multiplexer::None, None);
        let mut v = seeded;
        assert!(cycle_value(&mut v, 7, 1)); // inherit → on
        let mut touched = Touched::default();
        touched.0[7] = true;
        assert_eq!(to_flags(touched, &v).nested_virt, Some(true));
        assert!(cycle_value(&mut v, 7, 1)); // on → off
        assert_eq!(to_flags(touched, &v).nested_virt, Some(false));
        assert!(cycle_value(&mut v, 7, 1)); // off → inherit (wraps)
        assert_eq!(
            to_flags(touched, &v).nested_virt,
            None,
            "inherit composes no field — the layers decide"
        );
    }

    // ---- T12: the store cycle ----

    #[test]
    fn the_store_row_seeds_from_the_layer_decision_and_cycles() {
        // No layer → inherit.
        let seeded = seeded_picker(None, Multiplexer::None, None);
        assert_eq!(set_of(&seeded[8]).current(), INHERIT);
        // The row's set is the config key's own spellings.
        assert_eq!(
            set_of(&seeded[8]).values,
            vec!["inherit".to_owned(), "host".to_owned(), "baked".to_owned()]
        );
    }

    #[test]
    fn the_store_cycle_composes_the_store_and_inherit_composes_nothing() {
        let mut v = seeded_picker(None, Multiplexer::None, None);
        assert!(cycle_value(&mut v, 8, 1)); // inherit → host
        let mut touched = Touched::default();
        touched.0[8] = true;
        assert_eq!(to_flags(touched, &v).store, Some(Store::Host));
        assert!(cycle_value(&mut v, 8, 1)); // host → baked
        assert_eq!(to_flags(touched, &v).store, Some(Store::Baked));
        assert!(cycle_value(&mut v, 8, 1)); // baked → inherit (wraps)
        assert_eq!(to_flags(touched, &v).store, None);
    }

    #[test]
    fn a_seeded_store_row_cycled_to_inherit_composes_nothing() {
        // A layer said `baked`, the operator cycled away and back:
        // the row shows the layer's value again but `inherit` (the
        // wrapped position) is still no field — the layers decide.
        let base = crate::merge::merge(
            crate::config::Config::default(),
            crate::config::Config::default(),
            Path::new("/tmp/fake-user.toml"),
            Path::new("/tmp/fake-sidecar.toml"),
            Path::new("/nonexistent-home"),
        )
        .unwrap();
        let m = Merged {
            store: Some(Store::Baked),
            ..base
        };
        let mut v = seed(rows(&[], &pins(&[])), &m, nv());
        assert_eq!(set_of(&v[8]).current(), "baked");
        assert!(cycle_value(&mut v, 8, 1)); // baked → inherit
        assert!(cycle_value(&mut v, 8, 1)); // inherit → host
        assert!(cycle_value(&mut v, 8, 1)); // host → baked (back)
        let mut touched = Touched::default();
        touched.0[8] = true;
        assert_eq!(to_flags(touched, &v).store, Some(Store::Baked));
    }

    // ---- T13: the dry-run cycle ----

    #[test]
    fn the_dry_run_row_preselects_off_in_both_modes_and_cycles() {
        // T13: no layer can ask for a dry run (D9's promise has no
        // TOML surface), so `off` is ALWAYS the preselect — in the
        // persistent picker and the once picker alike.
        for env in [
            Env::default(),
            Env {
                once: true,
                nested_detached: false,
            },
        ] {
            let seeded = seed(
                rows_in(&[], &pins(&[]), env),
                &merged(None, Multiplexer::None, None),
                nv(),
            );
            assert_eq!(set_of(&seeded[9]).current(), "off");
            assert_eq!(
                set_of(&seeded[9]).values,
                vec!["off".to_owned(), "on".to_owned()]
            );
            // The untouched row composes nothing — a dry run is
            // never the default.
            let flags = to_flags(Touched::default(), &seeded);
            assert!(!flags.dry_run);
        }
    }

    #[test]
    fn the_dry_run_cycle_composes_the_flag_and_back_to_off() {
        let mut v = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            nv(),
        );
        assert!(cycle_value(&mut v, 9, 1)); // off → on
        let mut touched = Touched::default();
        touched.0[9] = true;
        assert!(to_flags(touched, &v).dry_run);
        assert!(cycle_value(&mut v, 9, 1)); // on → off (wraps)
        assert!(!to_flags(touched, &v).dry_run);
    }

    // ---- T14: the detachable cycle and its two disabled envs ----

    #[test]
    fn the_detachable_row_preselects_off_and_composes_only_detachable() {
        // T14: `on` composes `Flags::detachable` and NEVER
        // `Flags::detach` — B2: two ways to start ONE detached run,
        // and the picker's shape is the attach-the-terminal one.
        let seeded = seed(
            rows(&[], &pins(&[])),
            &merged(None, Multiplexer::None, None),
            nv(),
        );
        assert_eq!(set_of(&seeded[10]).current(), "off");
        let mut v = seeded;
        assert!(cycle_value(&mut v, 10, 1)); // off → on
        let mut touched = Touched::default();
        touched.0[10] = true;
        let flags = to_flags(touched, &v);
        assert!(flags.detachable);
        assert!(!flags.detach, "--detach is never composed");
        assert!(cycle_value(&mut v, 10, 1)); // on → off (wraps)
        assert!(!to_flags(touched, &v).detachable);
        // Untouched composes nothing.
        assert!(!to_flags(Touched::default(), &v).detachable);
    }

    #[test]
    fn the_once_mode_disables_the_session_and_detachable_rows() {
        // T14.1/T15.1: D22 refuses `--session` and
        // `--detach`/`--detachable` for the bare once form — the
        // picker must not compose what the CLI form would refuse.
        // The rows stay VISIBLE (the T2 disabled-row precedent):
        // a Disabled value, the once hint, no key can touch them.
        let v = rows_in(
            &[],
            &pins(&[]),
            Env {
                once: true,
                nested_detached: false,
            },
        );
        assert!(matches!(
            &v[10],
            Value::Disabled {
                hint: ONCE_REFUSED_HINT
            }
        ));
        assert!(matches!(
            &v[11],
            Value::Disabled {
                hint: ONCE_REFUSED_HINT
            }
        ));
        // No key changes them: cycles are no-ops, typing is a
        // no-op, backspace is a no-op — and the touched bit cannot
        // even be set through the row operations.
        let mut v = v;
        assert!(!cycle_value(&mut v, 10, 1));
        assert!(!cycle_value(&mut v, 10, -1));
        assert!(!cycle_value(&mut v, 11, 1));
        assert!(!type_byte_value(&mut v, 10, b'o'));
        assert!(!type_byte_value(&mut v, 11, b'x'));
        backspace_value(&mut v, 10);
        backspace_value(&mut v, 11);
        assert!(draft_of(&v[11]).is_none());
        // And even a FORGED touched bit composes nothing: the
        // disabled spelling is a different variant, not a frozen
        // selection — `to_flags` matches `Set`/`Typed` only.
        let mut touched = Touched::default();
        touched.0[10] = true;
        touched.0[11] = true;
        let flags = to_flags(touched, &v);
        assert!(!flags.detachable);
        assert_eq!(flags.session, None);
        // The dry-run row stays offerable in once-mode (D22 allows
        // it) — it cycles and composes.
        assert!(cycle_value(&mut v, 9, 1)); // off → on
        let mut touched = Touched::default();
        touched.0[9] = true;
        assert!(to_flags(touched, &v).dry_run);
    }

    #[test]
    fn a_nested_detached_environment_disables_only_the_detachable_row() {
        // T14.1: `MYSBX_DETACHED` mirrors the dispatcher's nested
        // guard — which scopes to `--detach`/`--detachable` ALONE:
        // the session and dry-run rows stay offerable.
        let v = rows_in(
            &[],
            &pins(&[]),
            Env {
                once: false,
                nested_detached: true,
            },
        );
        assert!(matches!(
            &v[10],
            Value::Disabled {
                hint: NESTED_DETACHED_HINT
            }
        ));
        // Session stays typed and offerable, dry-run stays a cycle.
        assert!(matches!(
            &v[11],
            Value::Typed {
                row: TypedRow::Session,
                draft: None,
                seed,
            } if seed == LIVE
        ));
        assert!(matches!(&v[9], Value::Set(_)));
    }

    #[test]
    fn env_detect_reads_the_nested_detached_marker_like_the_guard() {
        // The dispatcher's own probe spelling: set AND non-empty.
        let saved = std::env::var_os("MYSBX_DETACHED");
        unsafe { std::env::set_var("MYSBX_DETACHED", "") };
        assert!(!Env::detect(false).nested_detached, "empty is unset");
        unsafe { std::env::set_var("MYSBX_DETACHED", "/run/user/0/mysbx/x/live.sock") };
        assert!(Env::detect(false).nested_detached);
        assert!(
            Env::detect(true).once,
            "the once fact comes from the mode, not the env"
        );
        match saved {
            Some(v) => unsafe { std::env::set_var("MYSBX_DETACHED", v) },
            None => unsafe { std::env::remove_var("MYSBX_DETACHED") },
        }
    }

    // ---- T15: the session typed row ----

    #[test]
    fn the_session_row_grammar_admits_the_argv_byte_set_capped_at_64() {
        // The argv grammar's own set `[A-Za-z0-9._-]`, capped at the
        // validator's own bound (64). A leading dot IS drafted —
        // the no-leading-dot rule is an ENTER validation (the
        // error names it), never a mid-draft mangling.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 11, b'f'));
        assert!(type_byte_value(&mut v, 11, b'i'));
        assert!(type_byte_value(&mut v, 11, b'x'));
        assert!(type_byte_value(&mut v, 11, b'-'));
        assert!(type_byte_value(&mut v, 11, b'1'));
        assert!(type_byte_value(&mut v, 11, b'.'));
        assert!(type_byte_value(&mut v, 11, b'_'));
        assert!(type_byte_value(&mut v, 11, b'q')); // inside a draft
        assert_eq!(draft_of(&v[11]), Some("fix-1._q"));
        // Outside the grammar: a slash, a space, a byte-3 ^C shape.
        assert!(!type_byte_value(&mut v, 11, b'/'));
        assert!(!type_byte_value(&mut v, 11, b' '));
        assert!(!type_byte_value(&mut v, 11, 0x03));
        assert_eq!(draft_of(&v[11]), Some("fix-1._q"));
        // The 64 cap: a 64-char draft is the validator's own bound.
        let mut v = rows(&[], &pins(&[]));
        for _ in 0..63 {
            assert!(type_byte_value(&mut v, 11, b'a'));
        }
        assert!(type_byte_value(&mut v, 11, b'a'), "64 chars is in");
        assert!(!type_byte_value(&mut v, 11, b'a'), "the 65th is out");
        assert_eq!(draft_of(&v[11]).map(str::len), Some(64));
        // A leading dot drafts (the Enter validation refuses it).
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 11, b'.'));
        assert_eq!(draft_of(&v[11]), Some("."));
    }

    #[test]
    fn a_session_draft_validates_through_the_argv_validator_at_enter() {
        // `session::valid_name` — the SAME validator the
        // `--session` argv parser uses — decides at Enter: a
        // valid draft composes `Flags::session`, an invalid one is
        // the picker error line and NEVER a dispatch.
        let mut v = rows(&[], &pins(&[]));
        for b in b"fix-1" {
            assert!(type_byte_value(&mut v, 11, *b));
        }
        let mut touched = Touched::default();
        touched.0[11] = true;
        assert_eq!(validate_enter(&v, &touched), Ok(()));
        assert_eq!(to_flags(touched, &v).session.as_deref(), Some("fix-1"));
        // A leading dot: drafted, refused at Enter (the typed-row
        // precedent — the error line, nothing dispatched).
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 11, b'.'));
        let mut touched = Touched::default();
        touched.0[11] = true;
        let err = validate_enter(&v, &touched).expect_err("a leading dot is refused");
        assert!(err.contains("session"), "the error names the row: {err}");
        assert!(err.contains("no leading dot"), "{err}");
        assert_eq!(to_flags(touched, &v).session, None, "never composed");
        // The reserved `__worktrees` suffix: the validator's own
        // rule, never re-derived here.
        let mut v = rows(&[], &pins(&[]));
        for b in b"a__worktrees" {
            assert!(type_byte_value(&mut v, 11, *b));
        }
        let mut touched = Touched::default();
        touched.0[11] = true;
        assert!(validate_enter(&v, &touched).is_err());
        assert_eq!(to_flags(touched, &v).session, None);
        // The empty draft is the untouched row: the live mode.
        let v = rows(&[], &pins(&[]));
        assert_eq!(draft_of(&v[11]), None);
        let flags = to_flags(Touched::default(), &v);
        assert_eq!(flags.session, None, "empty draft = unset = live mode");
    }

    #[test]
    fn session_backspace_deletes_and_the_emptied_draft_is_untouched() {
        let mut v = rows(&[], &pins(&[]));
        for b in b"ab" {
            assert!(type_byte_value(&mut v, 11, *b));
        }
        backspace_value(&mut v, 11);
        assert_eq!(draft_of(&v[11]), Some("a"));
        backspace_value(&mut v, 11);
        assert_eq!(draft_of(&v[11]), None, "empty is untouched again");
        backspace_value(&mut v, 11); // a no-op on empty
        assert_eq!(draft_of(&v[11]), None);
        let flags = to_flags(Touched::default(), &v);
        assert_eq!(flags.session, None);
    }

    #[test]
    fn the_session_row_seeds_the_live_mode_as_display_only() {
        // T15: the display seed is `live`, the mode a bare run uses
        // — context, never a composed value (a seed never reaches
        // `Flags`, the T10 rule).
        let v = rows(&[], &pins(&[]));
        match &v[11] {
            Value::Typed { row, draft, seed } => {
                assert_eq!(*row, TypedRow::Session);
                assert_eq!(seed, LIVE);
                assert_eq!(*draft, None);
            }
            other => panic!("the session row is typed: {other:?}"),
        }
        assert_eq!(to_flags(Touched::default(), &v).session, None);
    }

    #[test]
    fn the_q_conflict_resolves_at_the_grammar_gate_not_the_abort() {
        // T15's record decision: `q` is a draft byte ONLY while the
        // session draft is non-empty. The GRAMMAR admits it (the
        // loop's routing is what keeps the empty-draft `q` the
        // abort — pinned by the pty test); a grammar-side refusal
        // would leak into the other typed rows' rule and turn the
        // abort key into an ignored keystroke at the row.
        assert!(TypedRow::Session.admits("fix-", b'q'));
        assert!(TypedRow::Session.admits("", b'x'));
        assert!(TypedRow::Session.admits("fix", b'Q'));
        // The loop's side of the contract (the gate lives in
        // `picker_loop`, but the value-layer half is pinned here):
        // an EMPTY draft still accepts every non-`q` grammar byte,
        // so the operator can always get past the first byte and
        // then draft `q` freely — backspace can rebuild the draft.
        let mut v = rows(&[], &pins(&[]));
        assert!(type_byte_value(&mut v, 11, b'x'));
        assert!(type_byte_value(&mut v, 11, b'q'));
        assert_eq!(draft_of(&v[11]), Some("xq"));
        backspace_value(&mut v, 11);
        assert_eq!(draft_of(&v[11]), Some("x"));
        // NIT-1 regression: at a 64-char (capped) draft the grammar
        // refuses `q` — so the loop's gate arm must NOT match and
        // the key falls through to the ABORT, never a silent
        // no-op. The loop's guard checks `admits` itself; this
        // pins the value-layer half of that contract: at a full
        // draft `admits` is false, so the gate cannot swallow the
        // key and the abort invariant holds for `q` literally,
        // from every state.
        let mut v = rows(&[], &pins(&[]));
        for _ in 0..64 {
            assert!(type_byte_value(&mut v, 11, b'a'));
        }
        let full = draft_of(&v[11]).unwrap().to_owned();
        assert_eq!(full.len(), 64);
        assert!(
            !TypedRow::Session.admits(&full, b'q'),
            "a capped draft refuses q — the loop routes it to the abort, never a no-op"
        );
        assert!(!type_byte_value(&mut v, 11, b'q'));
        assert_eq!(draft_of(&v[11]), Some(full.as_str()), "nothing drafted");
        // And the same corner at the message level: the reserved
        // `__worktrees` suffix is named by the PICKER's error line
        // (the argv parser's message is untouched).
        let mut v = rows(&[], &pins(&[]));
        for b in b"a__worktrees" {
            assert!(type_byte_value(&mut v, 11, *b));
        }
        let mut touched = Touched::default();
        touched.0[11] = true;
        let err = validate_enter(&v, &touched).expect_err("the reserved suffix is refused");
        assert!(
            err.contains("`__worktrees` suffix"),
            "the picker's error names the reserved suffix: {err}"
        );
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
    fn the_paste_cap_drops_the_residue_instead_of_leaking_keys() {
        // m2 regression: a "paste" past PASTE_CAP_BYTES is emulator
        // misbehavior; the give-up used to return the residue to
        // the stream (harmless when the picker only cycled — a
        // leaked `q` aborted), but the typed rows draft byte by
        // byte, so residue would type itself into a draft. The cap
        // now DROPS everything still queued: no content byte of
        // the oversized paste decodes as a key, and the paste
        // still counts as ONE Ignored.
        let mut chunks: Vec<Vec<u8>> = Vec::new();
        chunks.push(b"\x1b[200~".to_vec());
        // (cap + 4) content bytes — the cap trips before the
        // terminator, with `\r`, `q` and digits sprinkled through
        // the residue: none may surface (an Enter would dispatch,
        // a `q` abort, a digit would draft). The content arrives
        // in 32-byte chunks — the read shape the reader asks for.
        let over: Vec<u8> = std::iter::repeat_n(b'x', PASTE_CAP_BYTES + 4)
            .enumerate()
            .map(|(i, b)| {
                if i % 1000 == 0 {
                    b'\r'
                } else if i % 1000 == 1 {
                    b'q'
                } else if i % 1000 == 2 {
                    b'7'
                } else {
                    b
                }
            })
            .collect();
        for piece in over.chunks(32) {
            chunks.push(piece.to_vec());
        }
        chunks.push(b"\x1b[201~".to_vec()); // never reached
        let refs: Vec<&[u8]> = chunks.iter().map(|c| c.as_slice()).collect();
        let mut r = split_reader(&refs, 8);
        assert_eq!(r.next_key(), Key::Ignored);
        assert!(
            r.buf.is_empty(),
            "the residue was dropped, not returned to the stream"
        );
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
            "backend = \"podman-gvisor\"\nmultiplexer = \"herdr\"\ndefault-agent = \"opencode\"\nnested-virt = true\n",
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
        let (plain, plain_nv) = plain_merged.expect("the plain load succeeds");
        assert_eq!(plain.backend.as_deref(), Some("podman-gvisor"));
        assert_eq!(plain.default_agent.as_deref(), Some("opencode"));
        // The tri-state seed reads both layers (T11): the sidecar's
        // `nested-virt = true` over the user chain's silence.
        assert_eq!(plain_nv.sidecar, Some(true));
        assert_eq!(plain_nv.user, None);
        // The once mode ignores the sidecar entirely: the user
        // layer's backend, and NO agent — never the sidecar's
        // `default-agent` a touched row would have carried over.
        let (once, once_nv) = once_merged.expect("the once load succeeds");
        assert_eq!(once.backend.as_deref(), Some("bubblewrap"));
        assert_eq!(once.default_agent, None);
        assert_eq!(once.multiplexer, Multiplexer::None);
        // Once-mode seeds the tri-state from the user chain ALONE
        // (T8/D22): the host sidecar's decision must not reach the
        // row, the same isolation the merge half keeps.
        assert_eq!(once_nv.sidecar, None);
        assert_eq!(once_nv.user, None);

        let _ = std::fs::remove_dir_all(&base);
    }
}
