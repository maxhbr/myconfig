# beads-server — implementation plan

Status: planned, not started. Nothing but documentation exists in this
directory yet.

Tracker epic: **myconfig-93lp** (`bd show myconfig-93lp`). Each phase below is
a child bead of that epic; the bead is the work item, this file is the design.

| Phase | Bead | Deliverable |
|---|---|---|
| 0 | `myconfig-ts6g` | spike, alternatives verdict |
| 1 | `myconfig-fofw` | `nix run .#beads-server` shows the board |
| 2 | `myconfig-nvvu` | hierarchy and dependencies |
| 3 | `myconfig-b9yh` | live updates |
| 4 | `myconfig-pu7z` | filters and `mysbx` preset |
| 5 | `myconfig-jfvk` | editing behind `--allow-write` |
| 6 | `myconfig-y294` | tests, checks, docs |
| 7 | `myconfig-z7rr` | delete `misc/beads-viewer/` |

## Goal

`misc/beads-server/` is the successor to `misc/beads-viewer/`: a kanban board
over the repository's `.beads` (embedded Dolt) issue database.

Requirements:

- kanban view of the current bead state
- live updates while other agents and humans write
- readable hierarchy and dependency structure
- filtering, so that e.g. only the `mysbx` subset is visible
- editing beads and their descriptions
- runnable as `nix run .#beads-server`

## Verified facts

Measured against this repository, `bd 1.3.4 (dev)`:

| Fact | Value |
|---|---|
| `bd export --all` | ~1.2 s, 1.2 MB, 384 records, all `_type: "issue"` |
| status spread | 315 closed, 64 open, 4 in_progress, 1 deferred |
| labels | 14 total (`bd label list-all`); `mysbx` = 52 issues |
| sparse labels | 326 of 384 issues have an **empty** label list; the `labels` key is omitted entirely, not `null` |
| dependency types | blocks 162, parent-child 162, discovered-from 161, related 24, relates-to 19, depends-on 11, supersedes 4 |
| issue types | bug 152, task 127, feature 72, epic 17, chore 8, decision 7, spike 1 |
| `bd serve` (HTTP API, OpenAPI at `/v0`) | exists, but fails here: `cannot resolve workspace context: BEADS_DIR points to unsafe location`; `bd context` fails the same way |
| `bd sql` | not available on the embedded-Dolt backend |
| `bd events` journal | exists, `events-journal = false`; enabling it writes to the shared DB |
| `auto_commit` | `true` — set in `modules/myconfig.ai.dev/programs/programs.beads/default.nix` (lines 149, 173) |
| write surface | `bd update --body-file -/--design-file -/--notes/--append-notes/--acceptance/--if-status/--if-assignee` (guard mismatch exits 13), `bd comment --stdin`, `bd create --body-file -`, `bd dep add/remove`, `bd batch` |

Consequences that shape the whole design:

1. All reads and writes go through the `bd` CLI. Never direct Dolt/SQL, never
   `.beads/issues.jsonl` — upstream documents that file as a passive export
   and writes to it are lost.
2. `bd serve` cannot be used as a data layer, so the server does its own
   subprocess fan-out and its own change detection.
3. Because `auto_commit = true`, a UI edit can create a git commit in the
   user's working repository. Editing is therefore opt-in.

## Technology (decision)

Keep the viewer's stack: **Python 3 standard library backend + vanilla
JavaScript ES modules frontend, no build step**, with Server-Sent Events for
live updates.

| Criterion | Python stdlib + vanilla JS | Node/React/Vite |
|---|---|---|
| Nix packaging | `stdenv.mkDerivation` for assets + `writeShellApplication`, the exact 48-line recipe already in `misc/beads-viewer/default.nix` | `buildNpmPackage` plus a fixed-output hash to maintain |
| dependencies | none; dagre is already vendored | lockfile and node_modules vendor tree |
| live push | SSE is ~40 lines on `ThreadingHTTPServer` | native, but pays the above |
| effort | most board logic exists and is ported, not rewritten | rewrite |

Rejected alternatives:

- **FastAPI/uvicorn** — adds `python3.withPackages`, an ASGI lifetime model
  and a second process model, for no benefit at 384 issues.
- **A Go or Rust single binary** — reimplements everything; the `bd`
  subprocess is the only data path either way.
- **TypeScript** — no compile step means no TS. Use plain JS with JSDoc
  typedefs where typing helps.

The frontend is split into ES modules (`<script type="module">`) so the
870-line `misc/beads-viewer/index.html` can be broken up without a bundler.

## Existing alternatives (Phase 0 gate)

Timeboxed to about two hours; the rule is pre-committed: adopt an alternative
only if it satisfies **all three** of *live update*, *label-driven filter*,
*edit through `bd`*.

| Candidate | Verdict |
|---|---|
| `bd serve` upstream HTTP API | blocked: `unsafe location` on the embedded backend; and it is an API, not a board UI |
| `bd` TUI, community beads viewers (`bv`, `beads-ui`) | must be checked; adopt only if it reads embedded Dolt through `bd`, edits, and packages without a node bundle |
| Focalboard, Planka, Vikunja, Wekan | reject: own data model and own database; a bridge needs both an importer and an exporter that re-enters through `bd` with CAS semantics, which is more work than this plan and adds a second stateful service |
| graph-only exporters (graphviz, obsidian, cytoscape) | reject for the UI (no live updates, no edits); the idea is reused since the data source is the same |
| NocoDB, Baserow, Apitable | reject: `bd sql` is unavailable and direct DB writes are forbidden |

The verdict is recorded in `README.md` when Phase 0 closes.

## Architecture

### Reading state

- One bulk read: `bd list --all --limit 0 --json` (~1.2 s, 1.1 MB, 384
  issues). This is the only expensive call and it is never issued per
  request.
- Details lazily: `bd show <id> --json --include-comments`, cached per
  `(id, revision)`.
- Invocations run with `cwd=<repo>` and `BEADS_DIR=<repo>/.beads` (what the
  viewer does today), with one retry without `BEADS_DIR` if bd answers
  `unsafe location`. The active mode is reported by `/api/health`.
- Derived indices, rebuilt per snapshot (cheap pure functions):
  - `labelSet` — from `labels || []`, never `labels.length`
  - `clusterRoot` — port of `parentOf` / `computeClusters` from
    `misc/beads-viewer/index.html`
  - `blocked` — status open and at least one non-closed `blocks` in-edge
  - `reverseDeps` — id to the issues that depend on it
  - counts per status, type and label for the UI badges

### Live updates

A single `Watcher` thread owns the snapshot cache; SSE fans deltas out.

1. **Cheap probe** every `--poll-interval` (default 2 s): `stat()`
   fingerprint of `.beads/embeddeddolt/<db>/.dolt/noms/manifest` and
   `.beads/last-touched`. Unchanged means zero subprocesses. Every 15th tick,
   or when those paths are missing, fall back to `bd count --json` (0.32 s) as
   the authoritative probe. The active probe mode is exposed by
   `/api/health`.
2. **Single-flight refetch** on change; diff by `id -> (updated_at, status,
   revision)` into `{upserted, removed}` plus a monotonic `revision`.
3. **`GET /api/events`** SSE: `hello`, `patch`, `reset`, `stale`,
   `retry: 2000`, a 15 s heartbeat comment. The last ~50 revisions are
   retained; a client whose `?rev=` is older gets `reset` and refetches.
   Needs `daemon_threads = True` and a per-connection write lock so one
   stalled client cannot block the watcher.
4. **Client** (`static/live.js`): `EventSource`, patch application,
   `requestAnimationFrame`-coalesced render preserving selection and scroll
   offset, refetch on reconnect-with-gap and on `visibilitychange`.
5. **Optional `--events-journal`**: `bd events tail --since N --follow`.
   Never auto-enabled, because `bd config set events-journal true` mutates
   shared DB state. Detected at startup; silently degrades to polling.

### Filtering

Client-side over the full snapshot (384 issues is trivial), the same pipeline
shape as `visibleIssues()` in the viewer.

Axes: label (multi-select with counts), status, issue type, priority, lane,
assignee, blocked-only, free text over title and id (descriptions optional).

Filter state lives in the URL hash, so `#labels=mysbx&status=open` is a
shareable "mysbx subset" view. Presets: `mysbx`, all, mine, plus user presets
in localStorage. `showClosed` defaults to off.

Two traps that the UI must make visible:

- **`labels` is sparse.** 326 of 384 issues have no labels and the key is
  absent, so `issue.labels.length` throws. Every consumer uses `|| []`, and a
  unit test covers exactly this.
- **label is not text.** About 326 issues mention "mysbx" somewhere in their
  text, but only 52 carry the label. The label filter and the search box must
  look different, or the subset view reads as broken.

Server-side prefiltering (`bd list --label mysbx`, or `GET
/api/beads?label=mysbx`) is documented as the escape hatch for a database
with thousands of issues, but is not built first: one snapshot keeps lanes and
dependencies coherent.

### Editing

All writes are `bd` subprocesses built from an **argv allowlist**. No shell
string concatenation, no direct Dolt/SQL, no `.beads/*.jsonl` writes.

| UI action | Endpoint | bd invocation |
|---|---|---|
| status, priority, assignee, type, title, parent | `POST /api/beads/issue/<id>/update` | `bd update <id> [--status/--priority/--assignee/--type/--title/--parent]` |
| description | same | `bd update <id> --body-file -` with the text on stdin |
| design, notes, acceptance | same | `--design-file -`, `--notes`, `--append-notes`, `--acceptance` |
| labels | same | `--add-label`, `--remove-label`, `--set-labels` |
| comment | `POST /api/beads/issue/<id>/comment` | `bd comment <id> --stdin` |
| create | `POST /api/beads` | `bd create --title … --description-file - …` |
| dependency | `POST /api/beads/deps` | `bd dep add/remove` |
| bulk move | `POST /api/beads/batch` | `bd batch` (one Dolt transaction) |

- **Conflict handling.** The client sends the `if_status` / `if_assignee` it
  last saw; the server maps them to `--if-status` / `--if-assignee`. A lost
  guard makes bd exit 13, which maps to HTTP 409 with the fresh issue, so the
  UI reloads the card instead of silently overwriting an agent's claim. The
  snapshot `revision` is checked as well.
- **`auto_commit = true`.** Every write may create a git and a Dolt commit.
  Editing is therefore **off by default** behind `--allow-write`,
  `/api/capabilities` advertises whether it is on, the edit pane carries a
  permanent notice, and a toast shows bd's commit output after each save.
  Description autosave is debounced to at least 1.5 s idle and only commits on
  blur or `Ctrl-S`.
- **Attribution.** `BEADS_ACTOR=beads-server` so bd history shows where the
  write came from.
- **Local-only hardening.** Bind `127.0.0.1`, reject non-loopback `Host` and
  `Origin`, issue a random per-run token at `GET /api/session` and require it
  as `X-Beads-Server-Token` on every mutating request. Without that, any web
  page in a local browser can drive the API.
- **After a write** refetch immediately (do not wait for the poll) and reply
  with `{revision, issue}` so the caller renders instantly.

### Layout

```
misc/beads-server/
  default.nix              assets derivation + writeShellApplication wrapper
  server.py                arg parsing, ThreadingHTTPServer, routing, SSE, auth token
  bdcache.py               run_bd, snapshot, derived indices, delta, Watcher
  static/
    index.html             shell, toolbar, board/graph/details containers
    style.css              lifted from misc/beads-viewer/index.html
    api.js                 fetch wrappers, SSE client
    board.js               COLUMNS, lanes, renderBoard, card (ported)
    graph.js               dagre graph, pan/zoom (ported)
    detail.js              details pane (ported, made editable)
    filters.js             filter model, URL hash sync, label picker, presets
    live.js                patch application, render coalescing
    vendor/
      dagre-0.8.5.js       third-party layout library (excluded from formatters)
      dagre-0.8.5.LICENSE
  tests/
    fake-bd.sh             shim emitting canned list/show/update JSON
    test_bdcache.py        snapshot, delta, sparse-label, BEADS_DIR fallback
    test_http.py           API, SSE framing, 409, read-only
  docs/PLAN.md             this file
  README.md                verdict, usage, API contract, troubleshooting
```

`dagre-0.8.5.js` is copied from `misc/beads-viewer/` during Phases 0–6 (one
coupling point, deliberately temporary) and moved into `vendor/` in Phase 7
when the viewer is deleted, so the finished project is self-contained.

## Phases

Each phase is a child bead of **myconfig-93lp**; the IDs below are the ones
created with this plan.

### Phase 0 — `myconfig-ts6g` — spike and record the decision

No production code. Re-run the alternatives survey, verify the write surface
in this repo (`--if-status` returning 13, `--body-file -`, `bd comment
--stdin`, `bd batch`, `bd dep`), confirm the probe paths actually move on a
write, and check whether `bd events tail` needs the journal enabled. Output is
`README.md` with the verdict and the probe transcripts.

Blocks Phase 1.

### Phase 1 — `myconfig-fofw` — skeleton on the flake, read-only parity

`default.nix`, the two `flake.nix` lines, `server.py`, `bdcache.py`, and the
viewer's board/graph/details ported into ES modules. Endpoints `/api/beads`,
`/api/beads/issue/<id>`, `/api/health`, `/api/capabilities`. Default port
8339; the viewer keeps 8338.

Acceptance: `nix build .#beads-server` and `nix build .#beads-viewer` both
succeed, and `nix run .#beads-server` from the repo root serves the board on
`http://127.0.0.1:8339`.

### Phase 2 — `myconfig-nvvu` — hierarchy and dependencies

Derived `clusterRoot` / `blocked` / `reverseDeps`, collapsible lanes with
counts, a lane filter, a blocker chip on the card, a "blocks" section next to
"depends on" in the details pane with all seven dependency types labelled,
Blocked and Deferred columns, and a graph view that can focus one issue's
neighbourhood.

Acceptance: every card reveals its parent chain and both dependency
directions; a blocked bead is visually distinct from a merely open one.

### Phase 3 — `myconfig-b9yh` — live updates

`Watcher` with the two-tier probe, single-flight refetch, delta computation
and monotonic revision; `/api/events` SSE; `live.js` with reconnect and a
stale indicator; the optional journal path.

Acceptance: `bd create "test"` in another shell makes a card appear within
2 s; `bd close <id>` removes it; a killed SSE connection reconnects with no
stale cards.

### Phase 4 — `myconfig-pu7z` — filtering and presets

Label multi-select with counts, priority and assignee filters, blocked-only,
search-scope toggle, URL-hash state, presets.

Acceptance: `#labels=mysbx` shows exactly the 52 labelled beads (39 closed,
12 open, 1 deferred) with closed shown; reloading restores the view; incoming
SSE events respect the filter.

### Phase 5 — `myconfig-jfvk` — editing

The write endpoints, guards and 409 mapping, capabilities gating, CSRF token,
and the UI controls: inline status/priority/assignee/title, editable
description, comment composer, label editor, new-bead dialog, drag-and-drop
between columns.

Acceptance: a description edit round-trips into `bd show <id>`; a drag
persists the status; two tabs editing one bead produce a 409 rather than a
silent overwrite; without `--allow-write` mutations return 403.

### Phase 6 — `myconfig-y294` — tests, checks, docs

`tests/fake-bd.sh` plus the stdlib `unittest` suites, `checks.beads-server-tests`
in `flake.nix`, finishing `README.md`.

Acceptance: the check passes and the suite runs without a database.

The viewer is not touched here; it is retired in Phase 7, which depends on
this bead, so the fallback client stays usable while the suite is written.

### Phase 7 — `myconfig-z7rr` — retire `misc/beads-viewer/`

Its own commit, so the deletion is reviewable in isolation.

1. Create `misc/beads-server/vendor/`, move `dagre-0.8.5.js` and
   `dagre-0.8.5.LICENSE` there, and repoint the `cp` in `default.nix`. This
   is the only thing the server builds from the viewer; `flake.nix:541`
   already sets `formatterExcludeDirs = [ "vendor" ]`, so the move needs no
   formatter change — keep only third-party code in that directory.
2. Delete `packages.beads-viewer` and `apps.beads-viewer` plus their comment
   from `flake.nix` (the pair around line 555-558).
3. `git rm -r misc/beads-viewer/`.
4. `rg -n 'beads-viewer'` must then return only historical prose in this
   plan and `README.md`; fix anything else.
5. `git add -A`, then `nix build .#beads-server` and `nix flake check` to
   prove nothing else referenced the viewer.

Acceptance: the viewer is gone, no flake output references it, `nix flake
check` passes, and `nix run .#beads-server` still serves the board with dagre
loaded from `misc/beads-server/vendor/`.

## Nix integration

`misc/beads-server/default.nix` copies the shape of
`misc/beads-viewer/default.nix`: an assets derivation into
`$out/share/beads-server/`, then a wrapper.

```nix
writeShellApplication {
  name = "beads-server";
  runtimeInputs = [ beads python3 ];
  runtimeEnv = {
    BEADS_SERVER_APP = "${app}/share/beads-server";
    BD_DISABLE_METRICS = "1";
  };
  text = ''exec python3 -u "$BEADS_SERVER_APP/server.py" "$@"'';
}
```

`BD_DISABLE_METRICS` is set here because the `pkgs.beads` that a `nix run`
gets is the unwrapped package; the wrapper from the NixOS module is not in
play. The value matches the module so both paths behave identically.

`flake.nix`, directly after the `beads-viewer` pair (around line 558):

```nix
packages.beads-server = nixpkgs.legacyPackages.${system}.callPackage ./misc/beads-server { };
apps.beads-server = inputs.flake-utils.lib.mkApp { drv = self.packages.${system}.beads-server; };
```

Verification order:

```bash
git add misc/beads-server          # Nix only sees tracked files
nix build .#beads-server
nix build .#beads-viewer           # must not regress
nix run .#beads-server -- --repo . --allow-write
```

## Risks

1. **`auto_commit = true`** — a UI edit creates git and Dolt commits in the
   user's repository. Mitigated by the read-only default, the edit banner,
   actor attribution and the conflict guards.
2. **Embedded-Dolt fragility** — `bd serve` and `bd sql` are unavailable and
   the `unsafe location` refusals show that path handling is strict. The
   `BEADS_DIR` fallback plus an honest `/api/health` report is the guard.
3. **Poll cost** — 1.1 MB and 1.2 s per refetch. Fine at a few per minute,
   not per second: fingerprint probe first, single-flight, minimum interval
   clamp, and a timeout surfaces as `stale`, not as an error.
4. **Sparse records** — `labels` is absent on 326 of 384 issues;
   `is_blocked` is not in `list --json` and must be derived. Unit-tested.
5. **`bd` version drift** — `pkgs.beads` in the pinned nixpkgs may lag the
   1.3.4 verified here. Every bd feature the server relies on is probed at
   startup and degrades instead of dying.
6. **SSE on `ThreadingHTTPServer`** — one thread per client; needs
   `daemon_threads`, a per-connection write lock, heartbeats and a bounded
   client list. Tested with a deliberately non-reading client.
7. **Nix git tree** — this directory was untracked and empty, which is
   invisible to the flake. `git add` before every eval, and again after each
   new file.
8. **dagre coupling during the transition** — until Phase 7, `default.nix`
   references the viewer's copy, so deleting the viewer earlier breaks the
   build. Phase 7 moves the file first and verifies with `nix flake check`.
9. **Losing the fallback viewer** — once `misc/beads-viewer/` is gone there
   is no second read-only client to cross-check against. Phases 0–6 keep it
   working on purpose so the board, cluster grouping and graph rendering can
   be diffed side by side before the deletion.

## Decisions

- **Editing is gated behind `--allow-write`** and is off by default. No
  ``on by default`` mode is built, because `auto_commit = true` lets a UI edit
  create git commits in the user's repository.
- **`misc/beads-viewer/` is dropped completely** in Phase 7, once Phase 6
  has proven the server. The server subsumes it: same stack, same board, plus
  live updates, filtering and editing. Until Phase 7 it stays untouched and
  working, so the two can be compared side by side.
