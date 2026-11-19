# beads-server

A kanban board over the repository's `.beads` (embedded Dolt) issue
database. Served as a local web app via `nix run .#beads-server`.
It replaces the former `misc/beads-viewer/`.

Design and phase plan: `docs/PLAN.md` (epic: bd `myconfig-93lp`).

## Phase 0 verdict (bd `myconfig-ts6g`)

**Build it.** No existing alternative satisfies all three gate
criteria (live update, label-driven filter, edit through `bd`).

The write-surface probes below ran against a scratch `bd init`
directory (`/tmp`, `bd 1.3.4 (dev)`); the productive database was
never touched — writes on this repo would have created git + Dolt
commits. The `BEADS_DIR`/`bd serve` probes additionally ran against
this repo read-only.

### Alternatives survey

| Candidate | Verdict |
|---|---|
| `bd serve` (upstream HTTP API) | **blocked**: "operation not supported by the embedded-dolt backend" on this repo; also just an API, no board UI |
| `bd` TUI, community viewers (`bv`, `beads-ui`) | **unavailable**: not in nixpkgs, none installed, no `nix run`-able upstream flake found |
| Focalboard, Planka, Vikunja, Wekan | **reject**: own data model + own database; a bridge needs importer AND exporter re-entering through `bd` with CAS semantics — more work than this plan, plus a second stateful service |
| graph-only exporters (graphviz, obsidian, cytoscape) | **reject for the UI** (no live updates, no edits); the data-source idea is reused |
| NocoDB, Baserow, Apitable | **reject**: `bd sql` unavailable on the embedded backend; direct DB writes forbidden |

### Probe transcripts (bd 1.3.4, scratch directory)

Conflict guard:

```
$ bd update <id> --if-status closed --priority 0
Error updating issue: status mismatch: <id> has status "in_progress", expected "closed"
exit=13
```

Description via stdin (both body and design variants):

```
$ printf 'body via stdin' | bd update <id> --body-file -
✓ Updated issue: <id> — Test two
exit=0
$ printf 'design via stdin' | bd update <id> --design-file -   # and bd show confirms the text
✓ Updated issue: <id> — Test two
```

Comment, dependency, batch:

```
$ echo "hello comment" | bd comment <id> --stdin
✓ Comment added to <id> — Test two
exit=0
$ bd dep add <from> <to>
✓ Added dependency: <from> (Test two) depends on <to> (Test one) (blocks)
```

`bd batch` exists; grammar is `close/update/create/dep add/dep remove`
(one command per line, one Dolt transaction).

Events journal:

```
$ bd events tail --since 0
note: the events journal is disabled for this workspace (enable with
'bd config set events-journal true'); any records shown were written
while it was enabled, and new mutations are not being recorded
exit=0
```

→ `bd events tail` runs but records nothing without the journal; the
server never auto-enables it (that mutates shared DB state), polling is
the primary mechanism.

Probe paths move on write (`stat()` mtime before → after a
`bd update`):

```
$ stat -c '%Y %s %n' .beads/embeddeddolt/<db>/.dolt/noms/manifest .beads/last-touched
1795117726 …manifest      →  1795117730 …manifest
1795117721 …last-touched  →  1795117730 …last-touched
```

Both fingerprints change on every write — a `stat()` fingerprint
probe detects writes with zero subprocesses.

### BEADS_DIR handling (this repo vs scratch)

On the **productive repo** (`bd where` → prefix `myconfig`,
database `/home/mhuber/myconfig/myconfig/.beads/embeddeddolt`):

```
$ BEADS_DIR=/home/mhuber/myconfig/myconfig/.beads bd serve
Error: cannot resolve workspace context: BEADS_DIR points to unsafe location: /home/mhuber/myconfig/myconfig/.beads
$ bd serve                            # no BEADS_DIR
Error: operation "serve" not supported by the embedded-dolt backend: bd serve requires a Dolt SQL server; this workspace uses embedded Dolt
$ bd count --json                     # plain workspace discovery
{ "count": 413, "schema_version": 1 }
```

On a **scratch directory** `BEADS_DIR=<dir>/.beads` is accepted.

**Correction of the plan** (supersedes the BEADS_DIR sentence in
PLAN.md → *Architecture → Reading state*): on this repo `BEADS_DIR` is
*refused*, and plain workspace discovery with `cwd=<repo>` works. The
server therefore runs every invocation with `cwd=<repo>` and no
`BEADS_DIR` first, and sets `BEADS_DIR` only as a fallback when
workspace discovery fails. `/api/health` reports which mode is active.
(PLAN.md's *Verified facts* row records the pre-spike assumption;
this section is the verified behavior.)

## Usage

```
nix run .#beads-server [-- --repo PATH --allow-write --port 8339]
```

- `--repo` — repository whose `.beads/` DB is displayed
  (default: `$PWD`)
- `--allow-write` — opt-in editing (off by default, because
  `auto_commit = true` lets a UI edit create git + Dolt commits)
- `--poll-interval` — seconds between change probes (default 2)
- `--events-journal` — tail the bd events journal instead of stat
  polling; only works when the journal is enabled for the workspace,
  and it never is by this server (enabling it mutates shared DB state)
- binds `127.0.0.1` only; mutating requests require a per-run token
  from `GET /api/session` sent as `X-Beads-Server-Token`

The board: lanes = epics (parent-child clusters), columns =
status, cards carry type/priority/blocker chips. The details pane
shows the parent chain and both dependency directions. Drag cards
between columns to change status (with `--allow-write`); the URL
hash (e.g. `#labels=mysbx`) is a bookmarkable filter view; presets
(mysbx / all / mine / saved) live in the toolbar.

## API

| Endpoint | Method | Purpose |
|---|---|---|
| `/api/beads` | GET | full snapshot `{revision, issues}` |
| `/api/beads/issue/<id>` | GET | one issue with comments |
| `/api/indices` | GET | reverse deps, blocker counts, label counts |
| `/api/health` | GET | revision, issue count, bd mode, error count |
| `/api/capabilities` | GET | `{write: bool}` |
| `/api/session` | GET | per-run token (403 unless `--allow-write`) |
| `/api/events` | GET | SSE: `hello`/`patch`/`reset`/`stale`, `retry: 2000`, 15 s heartbeat |
| `/api/beads/issue/<id>/update` | POST | fields map to `bd update` flags; `if_status`/`if_assignee` guards; exit 13 → 409 with the fresh issue |
| `/api/beads/issue/<id>/comment` | POST | `bd comment --stdin` |
| `/api/beads` | POST | `bd create` |
| `/api/beads/deps` | POST | `bd dep add/remove` |
| `/api/beads/batch` | POST | `bd batch` (one Dolt transaction) |

## Tests

`tests/fake-bd.sh` is a drop-in `bd` shim over a plain JSON file, so
`tests/test_bdcache.py` and `tests/test_http.py` (stdlib `unittest`)
need neither Dolt nor network. In CI:

```
nix build .#checks.x86_64-linux.beads-server-tests
```

Locally:

```
python3 misc/beads-server/tests/test_bdcache.py
python3 misc/beads-server/tests/test_http.py
```

## Troubleshooting

- **`bd serve` / `bd sql` unusable** — this workspace uses the
  embedded-Dolt backend; there is no SQL server. The server shells out
  to the `bd` CLI instead and that is by design.
- **`BEADS_DIR points to unsafe location`** — bd on this repo refuses
  `BEADS_DIR` but accepts plain workspace discovery; the server runs
  bd with `cwd=<repo>` and no `BEADS_DIR` first, retrying with
  `BEADS_DIR` only when discovery fails. `/api/health` reports which
  mode is active.
- **`the events journal is disabled`** — `bd events` records nothing
  unless `events-journal` is enabled, and enabling it mutates shared
  DB state. Live updates therefore poll file fingerprints; the
  `--events-journal` flag is the opt-in fast path.
- **`auto_commit = true`** — every successful write may create a git
  and a Dolt commit in the target repository. That is why editing is
  off by default and every save is toasted with bd's output.
