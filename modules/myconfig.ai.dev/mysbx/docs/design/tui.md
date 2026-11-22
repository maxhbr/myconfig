<!--
Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
SPDX-License-Identifier: MIT
-->

# Design: the session picker — `mysbx tui` before a launch

Status: agreed design (bd myconfig-dm7b; this record is
myconfig-dm7b.1, written before any code exists — `src/tui.rs`, the
dispatcher arm, the once-mode change, the usage/completions
contract and the validation are the implementation beads
myconfig-dm7b.2–.6).

This is the decision record for the one interactive surface of the
mysbx CLI: a minimal full-screen terminal form, shown before a
sandbox is launched, that preselects the three interactive-session
decisions from the merged configuration layers, lets the operator
cycle values with Left/Right and move rows with Up/Down, and starts
the sandbox with Enter. The picker composes the one-run override
flags that already exist (cli.md D14/D18/D32); it writes nothing,
and every key it does not carry (network, mounts, store, cpu,
memory, …) is out of scope for the MVP.

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **session picker** — the full-screen selection of the three
  interactive-session decisions, shown before a launch and composed
  into the one-run override flags on Enter: the `mysbx tui` form (T1)
  and the bare `mysbx-once` form (T8). "TUI" alone is avoided — it
  already names the aoe interface ([detach.md](./detach.md) B7).
- **row** — one of the picker's three decisions: backend,
  multiplexer, default-agent (T2). A row is a closed set with one
  preselected value, never free text.

## The problem

The decisions an interactive session varies per run — which backend,
which multiplexer, which agent — are one-run override flags today:
`--backend` (cli.md D18), `--multiplexer` (D14), `--agent`/
`--no-agent` (D32). An operator flipping one of them types it from
memory, with no view of what the layers already decided: the
effective configuration is visible only in `--verbose` (D10) or
`status` (D19), and a guess that names a value this build does not
carry is a refused run (exit `70`, D8) the operator reads after the
fact. The picker shows each decision, its alternatives and the merged
value it would run with at once — and Enter composes exactly the
flags that already exist, so the picker adds no decision of its own
to the pipeline.

## Decisions

### T1: a verb, not a bare form

`mysbx tui` is a top-level verb — the single-verb shape of D3, no
nested noun group (none of D3's closed-set exceptions). The
picker is OPT-IN interactive: the bare `mysbx` still dispatches
straight into the sandbox (D2), `run -- CMD` stays a one-shot, and
the "no interactive prompts" non-goal of cli.md holds for every
surface but this one — there is no `--tui` flag and no prompt inside
the bare form, because a flag would make the primary action
conditional on a spelling, and a prompt inside it is exactly what
the non-goal forbids. The one form whose bare invocation opens the
picker is `mysbx-once` (T8).

Keys: Left/Right cycle the focused row's value, Up/Down move the
focus, Enter dispatches, Esc or `q` exits `0`. `q` never dispatches.

### T2: the rows are closed sets from the registries

Each row lists the closed set its flag accepts — never free text,
never a command (config.md D4):

- **backend**: `backends::NAMES` ∩ `doctor::enabled_backends()` —
  the registry's universe (`backends.rs`) narrowed to what this
  host's wrapper enables (`MYSBX_ENABLED_BACKENDS`, the set `doctor`
  reports), so the row shows only backends a run here
  can start.
- **multiplexer**: `config::Multiplexer::NAMES` — the six values of
  the config key (config.md D17), `none` included.
- **agent**: `pinned_agent_names()` (`MYSBX_AGENT_NAMES`, config.md
  D32) plus ONE pseudo-value, `none` — the `--no-agent` spelling
  (cli.md D32). The registry's commands and arguments never reach
  the picker.

An empty registry renders a single DISABLED row that says so — `(no
backend enabled on this host)`, `(no agent pinned on this build)` —
never a hidden one: the operator sees that the decision does not
exist here, instead of discovering the missing choice after the
session started (the D11/config.md D17 argument for refusing over
falling back).

### T3: seeded from the merge — preselect what a bare run would do

The rows preselect what a bare `mysbx` would do: the picker seeds
from `load_layers`/`merge` (`merge.rs`), the same merge a run performs
(the `status` precedent, cli.md D19). Repo resolution is unchanged
(D1): a cwd that is not a repo is the same `70` refusal every other
verb gives, before any row renders.

A key no layer decides seeds what the pipeline would resolve, not a
guess: an omitted `multiplexer` preselects `none` (config.md D17's
default: an omitted key is the plain shell), a `default-agent` that
resolves to nothing preselects the `none` pseudo-value, and a
backend no layer names is the one row with nothing preselected —
Enter then meets the same "no backend configured" refusal (`70`,
D8) a bare run meets. Seeding never widens what a run may do: it
shows the merged values (both layers are trusted, config.md D7) and
offers only the sets of T2.

### T4: Enter builds a `Flags` and calls `sandbox()`

Enter composes the rows into the `Flags` a bare run parses — the
one-run override flags the rows stand for (`--backend` D18,
`--multiplexer` D14, `--agent`/`--no-agent` D32) — and calls the
same `sandbox()` entry. The picker is a front end for exactly those
flags and nothing else: it writes no file, adds no key, and every
downstream check — the backend check (D7's no-fallback rule), the
missing-entry refusals (D11/config.md D17), the mount and home
guards — fires
as if the flags had been typed. A row the operator never TOUCHED
leaves its `flags.*` unset, so the layers decide (cli.md D6
precedence: flags > sidecar > user > defaults); the picker never
freezes a decision the configuration owns — and a row whose set
carries a single value can never be touched at all (cycling it
moves nothing), so Enter leaves it to the layers on every path,
the pipeline's own refusals included (a backend-only host meets
the same "no backend configured" `70` the bare form would).

The terminal leaves raw mode (T6) BEFORE dispatch: the payload — or
the refusing site — owns the terminal from there, and a refusal
surfaces as the same `mysbx: ` message with exit `70` (D8) it
prints today. The picker adds no refusal surface of its own.

### T5: a not-inited repo confirms `mysbx init`

The D13 gate (`require_initialized_sidecar`, `lib.rs`) runs before
the rows: in a repo without its sidecar config the picker shows
that function's refusal text verbatim and offers the confirmation —
**Enter runs `mysbx init`** (its `## created:` lines print, D9),
then the picker re-seeds (T3) and renders the rows the fresh sidecar
now decides; **Esc aborts** the confirmation and exits `0`, leaving
the repo as it was. `init` is idempotent (config.md D12), so
repeating the confirmation is safe. The flow does not exist in
once-mode: `RunMode::Once` skips the D13 gate (T8).

### T6: no picker in pipes; zero new dependencies

The picker needs a terminal on BOTH stdin and stdout — it reads keys
and renders full-screen. Either one not a tty (`isatty`) is refused
with exit `70` suggesting the bare form: the command line was fine,
the terminal it needs is not there (D8's boundary; the
`--detachable` precedent, detach.md B2). Scripts and agents keep
the bare form. The once form's refusal suggests `mysbx-once run --
CMD` instead — there the bare form IS the picker (T8), so it
cannot be the suggestion.

Zero new dependencies: the crate is zero-dep by design (cli.md D5),
and the picker keeps it — raw mode through libc
`tcgetattr`/`tcsetattr` with the same raw `extern "C"` idiom
`isatty` already uses (`lib.rs`), rendering through ANSI escapes
only, one ~300-line `src/tui.rs`. No crossterm, no ratatui: three
rows and a key loop do not pay for a framework.

### T7: run-scoped flags are refused with the verb

The dispatcher's shared rejection arm lists `tui`: `--dry-run`,
`--backend`, `--multiplexer`, `--agent` and every other run-scoped
flag are usage errors (`2`, D8) with the verb. The picker IS the
interactive replacement for exactly those flags — `mysbx tui
--backend krun` is the same decision typed twice, and
accept-and-ignore is the failure mode the arm exists to prevent.
`--dry-run` is refused for its own reason: the picker's whole action
is interactive, so there is no argv to print and no side effect to
preview.

`tui` with `--detach`/`--detachable` is deferred and refused by the
same arm: "pick, then detach" reopens detach.md B4's two-process
handshake for a flow nobody has asked for; it is decided when
someone wants it.

### T8: bare `mysbx-once` launches the picker

The once binary's bare form (no arguments) opens the picker,
superseding the straight dispatch into `RunMode::Once` that form
had — D22 listed it only as allowed — seeded from the user layer
alone (config.md D6; the sidecar layer is empty by construction,
D22). Enter dispatches into `RunMode::Once`; Esc or `q` exits `0`
before any temp root exists.
The temp-root lifecycle — the data-loss guard, the teardown, the
worktree registration surgery — is untouched: the picker only fills
`Flags`. The once gate (`once_gate`, D22's refusal surface)
allows the verb, so the spelled-out `mysbx-once tui` behaves the
same, and the not-inited confirmation of T5 does not exist here —
`RunMode::Once` carries its own empty `config.toml` in the temp
root and skips the D13 gate, so there is nothing to confirm.

A once bare form WITH flags (`mysbx-once --backend krun`) never
opens the picker: D22's allowed list keeps the run-scoped flags,
and the flags ARE the decisions — it dispatches straight into
`RunMode::Once`.

**Why the once form and not the plain bare form.** A once run starts
from the empty sidecar layer: its three interactive decisions are
made per run, never inherited from a repo's policy, and the once
binary is the ad-hoc, try-this-checkout form — the one place where
a human is deciding on the spot. The plain bare form stays instant
(D2): it is the primary action, and a daily action must not grow a
keypress.

## Resolved open questions

1. **Esc aborts the not-inited confirmation** (T5) — Enter is the
   only key that runs `init`.
2. **Verb only**: no `--tui` flag, no prompt in the bare form (T1).
3. **The agent row preselects `none`** when no `default-agent`
   resolves (T3).
4. **`mysbx gui tui` works by D15's existing tail pass-through** —
   the tail is never parsed, so the window's inner invocation runs
   the verb and the picker renders in the window like any inner
   run. Nothing about `gui` adapts to it (no window title, no size
   hint); it is recorded here as working by construction, and
   polishing it is a later change.
5. **`tui` + `--detachable` is deferred** (T7): refused with the
   verb until the composition is decided.

## Non-goals

- No later keys in the MVP: network, mounts, store, cpu, memory, …
  are out of scope (the intro); the picker shows the
  three decisions the one-run flags already carry.
- No state anywhere: the picker writes nothing (T4) and remembers no
  last selection.
- No new dependencies (T6).
