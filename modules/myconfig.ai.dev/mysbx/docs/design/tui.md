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
sandbox is launched, that preselects the interactive-session
decisions from the merged configuration layers, lets the operator
cycle values with Left/Right, move rows with Up/Down, type into the
typed rows (T10) and start the sandbox with Enter. The picker
composes the one-run override flags that already exist
(cli.md D14/D18/D32); it writes nothing, and every key it does not
carry (mounts, allowlists, ports, …) is out of scope for the MVP.

Vocabulary, on top of [`../../CONTEXT.md`](../../CONTEXT.md):

- **session picker** — the full-screen selection of the three
  interactive-session decisions, shown before a launch and composed
  into the one-run override flags on Enter: the `mysbx tui` form (T1)
  and the bare `mysbx-once` form (T8). "TUI" alone is avoided — it
  already names the aoe interface ([detach.md](./detach.md) B7).
- **row** — one of the picker's decisions: backend, multiplexer,
  default-agent (T2), network (T9), cpus/memory/pids-limit (T10),
  nested-virt (T11), store (T12). A row is a closed set with one
  preselected value — or, for the typed rows, a byte grammar and a
  draft — never free-form text.

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

The three launch rows list the closed set their flag accepts —
never free text, never a command (config.md D4). The later property
rows keep the SHAPE with their own source: T10's typed rows replace
the closed set with a byte grammar, T11/T12 cycle their config
key's tri-state, T9 renders read-only.

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

### T9: the network row is read-only

The merged `network` (T3's merge, resolved: `none` < `proxy-only` <
`public` < `shared`, never tri-state by the time a run sees it)
renders as a fourth row — READ-ONLY, the disabled-row precedent of
T2: Left/Right on it are no-ops (the row never moves, so it can
never be TOUCHED), it never contributes a `Flags` field (there is
none — `network` has no one-run override flag, and the row invents
nothing), and its read-only-ness is VISIBLE the way T2's empty
registries are: the row renders with the disabled hint, not the
cycling values, so the operator sees the decision exists but is
not theirs to make here. The row exists for one reason: the
network is the one fact an operator about to launch most needs to
see (D20's boundary) and could not — `status` shows it, but the
picker is the surface the launch happens on.

### T10: the typed rows — cpus, memory, pids-limit

Three rows are TYPED ENTRY, not cycling: `cpus`, `memory`,
`pids-limit` (config.md D23) have no closed set to cycle — any
positive number is a valid value — so the row shows the merged
value (or `—` when no layer set one) as a seed and accepts typing.
Each row carries a DRAFT, invisible until the first key it
accepts; the grammar filters key by key (T2's closed-set rule
translated to a byte grammar):

- **cpus**: digits and one dot — podman's `--cpus` shape
  (`2`, `1.5`); a second dot, a leading dot, letters are ignored
  keystrokes.
- **memory**: digits plus ONE optional `b`/`k`/`m`/`g` suffix
  (any case) — the config parser's own grammar (`8g`, `512m`).
- **pids-limit**: digits only — a positive integer (`u32`).

Keys outside the grammar are ignored like `Key::Ignored` — never
an error mid-draft, never a beep surface; `q`, `^C` and Esc keep
their loop bindings (a `q` typed at a typed row is the abort,
never a draft byte — the rows' grammars admit no letters).
Backspace (`0x7f`) deletes the draft's last byte; an empty draft
is an UNTOUCHED row (T4): Enter composes no flag and the layers
decide.

At Enter the draft is parsed through config.rs's OWN parsers —
`cpus`/`memory`/`pids_limit`, the same code path a config file's
value takes — so the picker cannot accept what a layer could not.
An invalid draft is a PICKER error: the error line renders inside
the picker (the only new surface it has), Enter dispatches nothing,
and the loop continues (the draft stays, editable). A valid draft
becomes the one-run override — `Flags::cpus`/`memory`/`pids_limit`
(the fields exist for the picker alone; there is no CLI spelling,
and the argv parser, the T7 refusal arm and `usage.txt` are
untouched). The pipeline applies them where `--backend` already
overrides the merge, and the per-backend gates — the unenforced-
limits warnings, the krun `PidsLimit` refusal — stay the single
decision surface: the picker carries NO per-backend support
matrix (T4's argument applied to limits: a memory on a backend
that cannot enforce it is the pipeline's warning, not the
picker's guess).

### T11: the nested-virt row cycles inherit/on/off

`nested-virt` (config.md D27) is tri-state in the LAYERS
(`Some(true)`/`Some(false)`/omitted) but resolved to a bool in
the merge — so the row shows the resolution and the seed: `on`
when the resolved value is true, `off` when false, and `inherit`
for the omitted case — which requires the tri-state BEFORE the
merge applies the off-default, so the seed reads the folded
layer configs, not the merged bool. Cycling moves
`inherit → on → off → inherit`; `inherit` composes NO field (an
untouched row's behavior — the layers and the D27 default
resolve), `on` composes `Flags::nested_virt = Some(true)`, `off`
`Some(false)`. The seed mirrors the merge's own resolution table
(ANY-enable-wins — either layer's `true` enables, either layer's
explicit `false` denies only when nothing enabled), so the row
never shows a resolution the run would not make.

`on` over a user-layer deny is the one shape the picker's
override can WIDEN, and the merge's own `NestedVirtUpgrade`
guard cannot catch it — the guard sees only the two layers, and
it runs BEFORE the override exists. So the override site
re-checks exactly that one shape: `Flags::nested_virt =
Some(true)` while the USER layer's chain folded to
`nested-virt = false` is a refusal composed like `--backend`'s
validation — the merge's own message shape, exit `70`, nothing
dispatched. NOT a per-backend matrix (T4's argument holds): the
warn-and-drop and host pre-flight gates stay untouched and stay
the only backend-shaped decisions; the re-check is a DENY
re-check, the same D27 rule the merge applies to a sidecar,
applied to the one layer the merge already consumed.

### T12: the store row cycles inherit/host/baked

`store` (guest-store.md) is tri-state in the layers (`host`/
`baked`/omitted) and stays an `Option` in the merge — only
`resolved_store()` collapses the backend-shaped default. The row
cycles `inherit → host → baked → inherit` with the config
key's own spellings (`config::Store::NAMES`); `inherit` composes
no field, `host`/`baked` compose `Flags::store` overriding the
layer decision for this run. `baked` on a backend that cannot
attach a disk is the pipeline's warn-and-fall-back — not the
picker's: no per-backend matrix (T4).

## Resolved open questions

1. **Esc aborts the not-inited confirmation** (T5) — Enter is the
   only key that runs `init`.
2. **Verb only**: no `--tui` flag, no prompt in the bare form (T1).
3. **The agent row preselects `none`** when no `default-agent`
   resolves (T3).
4. **`mysbx gui tui` works by D15's existing tail pass-through** —
   the tail is never parsed, so the window's inner invocation runs
   the verb and the picker renders in the window like any inner
   run. The same construction covers the once form: `mysbx-once
   gui` with an EMPTY tail re-invokes the bare once form in the
   window, which is the picker itself (T8) — the window opens on
   the rows. Nothing about `gui` adapts to it (no window title, no
   size hint); it is recorded here as working by construction,
   and polishing it is a later change.
5. **`tui` + `--detachable` is deferred** (T7): refused with the
   verb until the composition is decided.

## Non-goals

- No later keys beyond T9–T12: mounts, allowlists, ports, … remain
  out of scope; the picker shows the decisions the one-run override
  pipeline can carry.
- No CLI spelling for the T9–T12 fields: `Flags` gains picker-only
  fields, and the argv parser, the T7 refusal arm, `usage.txt` and
  the completions stay untouched — a property the shell must set
  goes in a config layer, never a new flag.
- No state anywhere: the picker writes nothing (T4) and remembers no
  last selection.
- No new dependencies (T6).
