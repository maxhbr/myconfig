# Dynamic workspace naming in niri — research notes

Research task: give niri workspaces human-readable names ("work", "chat",
"client_1") assigned dynamically based on what runs on them. Module:
`modules/myconfig.desktop.wayland.niri/`.

## Pinned niri version and where to look

The flake builds niri from the upstream flake input
(`flake.nix`: `niri.url = "github:niri-wm/niri"`), patched via
`overrideAttrs` in `modules/myconfig.desktop.wayland.niri/default.nix`
(only a `niri-session` script substitution — no IPC changes).

Locked rev: `02fdd8e7586110d6aa79d94f388756a3479091b6`, version
**26.4.0** (`Cargo.toml`). Pinned source: `./get_input.sh niri` →
`/nix/store/wicvpy00ckvhd5l4zbprm03drs85l3l8-source`.

All findings below are verified against that pinned source, not against
latest upstream docs.

## Mechanisms available

### 1. Static named workspaces in config.kdl (since 0.1.6)

```kdl
workspace "browser"
workspace "chat" { open-on-output "DP-2" }
```

Limitations (from `docs/wiki/Configuration:-Named-Workspaces.md` in the
pinned source): named workspaces *always exist* even when empty, and names
come from the config file. `window-rule { open-on-workspace "chat" }` can
pin windows to them. This is fully static — no reaction to what actually
runs. Docs explicitly state the alternative: use `set-workspace-name` /
`unset-workspace-name` actions (since 25.01) for dynamic naming.

### 2. IPC actions for renaming (since 25.01 — present in pinned 26.4.0)

`niri-ipc/src/lib.rs`, `Action` enum:

- `SetWorkspaceName { name, workspace: Option<WorkspaceReferenceArg> }`
  — CLI: `niri msg action set-workspace-name <name> [--workspace <ref>]`.
  If `workspace` is `None`, names the *focused* workspace.
  `WorkspaceReferenceArg` = `Id(u64) | Index(u8) | Name(String)`, parsed via
  `FromStr` (integer → index, anything else → name). **There is no `Id`
  variant reachable from the CLI `--workspace` flag** (plain strings parse
  as index or name only), but the raw-JSON request path supports it:
  `{"Action":{"SetWorkspaceName":{"name":"work","workspace":{"Id":8}}}}`
  sent to `$NIRI_SOCKET` (or `niri msg raw-request`).
- `UnsetWorkspaceName { reference: Option<WorkspaceReferenceArg> }` —
  same reference handling, `None` = focused workspace.

Semantics in `src/layout/mod.rs::set_workspace_name`:
- A rename is **silently ignored if another workspace already uses that
  name** (`find_workspace_by_name`). A daemon must therefore check for
  name collisions itself (e.g. suffix `_2`, `_3` — the "client_1"-style
  numbering fits this).
  - **Caveat**: rule names themselves must not end in `_[0-9]+` (e.g.
    two distinct rules named `client_1` and `client_2`): the daemon derives
    the rule name from a collision variant by stripping that suffix, so
    generated variants (`client_1_2`) and rule names
    (`client_1`, `client_2`) share one namespace and can block each
    other's renames via the duplicate-name no-op.
- Renaming the first/last workspace can trigger insertion of a new empty
  workspace (`empty-workspace-above-first` handling).
- `unset_workspace_name` on an empty workspace makes it eligible for
  garbage collection (`has_windows_or_name()` → `retain` in
  `src/layout/monitor.rs`), i.e. it disappears like a normal dynamic
  workspace once unnamed and empty.
- Workspaces are matched by name case-insensitively
  (`eq_ignore_ascii_case`).

### 3. Event stream (since 0.1.9 — present in pinned version)

`niri msg --json event-stream` (or `Request::EventStream` on
`$NIRI_SOCKET`) yields one JSON event per line; the first events carry the
full state (`WorkspacesChanged`, then `WindowsChanged`).

Relevant events (verified in `niri-ipc/src/lib.rs::Event`):

- `WorkspacesChanged { workspaces: Vec<Workspace> }` — full replacement
  state. `Workspace { id, idx, name: Option<String>, output, is_urgent,
  is_active, is_focused, active_window_id }`. Emitted on *any* rename
  (the refresh diff in `src/ipc/server.rs` compares `name`).
- `WindowOpenedOrChanged { window }` — full window snapshot;
  `Window { id, title, app_id, pid, workspace_id, is_focused, ... }`.
  Fired on window open **and** on any change of title / app_id /
  workspace_id / floating state (see `ipc_refresh_windows`).
- `WindowClosed { id }`.
- `WorkspaceActivated { id, focused }`, `WorkspaceActiveWindowChanged`.

Important protocol constraint (`niri-ipc` crate docs): after requesting an
event stream, niri **stops reading requests on that socket**. To rename
while subscribed you need a *second* IPC connection (e.g. shell out to
`niri msg action set-workspace-name ...` from the stream consumer — this is
what community scripts do).

### 4. Waybar display

The pinned waybar 0.15.0 `niri/workspaces` module (man 5
`waybar-niri-workspaces`) shows names: `format` default `{value}` renders
the name if set, index otherwise; `{name}` / `{index}` are separate
replacements; `format-icons` can map workspace names to icons. The current
module config (`modules/myconfig.desktop.wayland.niri/default.nix`) uses
`"format" = "{index}"` — switch to `{value}` (or `{icon}` +
`format-icons`) once workspaces carry names. Clicking a workspace button
targets it by name/reference, so named workspaces are easy to focus.

### 5. Community tooling

Upstream docs mention no official auto-naming daemon; the standard pattern
in the community (niri wiki, r/unixporn projects) is exactly the
subscribe-and-rename script: read `niri msg --json event-stream` with
`jq`, keep a window→workspace map, call `niri msg action
set-workspace-name NAME --workspace <index-or-name>` on changes. niri's
own `niri-ipc` Rust crate (`socket::Socket`, `Event`, `Action`) is the
cleaner basis for a Rust implementation; note the crate is not semver-stable
and must be pinned to the exact niri version (`=26.4.0`).

## Recommendation

Implement an opt-in daemon wired into
`modules/myconfig.desktop.wayland.niri/`:

- **Option** (following the module's naming style, gated):
  `myconfig.desktop.wayland.niri.dynamicWorkspaceNames` with
  `mkEnableOption`-style enable plus a rules attr/list, e.g.
  `myconfig.desktop.wayland.niri.workspaceNames.rules = [
    { app-id = "^firefox$"; title = ".*Jira.*"; name = "work"; }
    { app-id = "^(discord|telegramdesktop)$"; name = "chat"; }
  ]` (first match wins; unmatched workspaces keep their index). Allow
  multiple workspaces matching the same rule to auto-number
  (`client_1`, `client_2`) because `set-workspace-name` is a no-op on
  name collisions.
- **Daemon**: a small script (bash + jq, via `pkgs.writeShellScript`) or
  Rust helper that:
  1. spawns `niri msg --json event-stream` and reads line-by-line,
  2. maintains `window_id -> (workspace_id, app_id, title)` state,
  3. on `WindowOpenedOrChanged` / `WindowClosed` / `WorkspacesChanged`,
     recomputes the desired name for each affected workspace from its
     window set and the rule table,
  4. applies changes with `niri msg action set-workspace-name NAME
     --workspace <ref>` / `unset-workspace-name`, tolerating the
     duplicate-name no-op (retry with a suffix on collision),
  5. prefers renaming only on workspace-level changes (not every title
     change) to avoid churn.
- **Wiring**: start via the existing `spawn-at-startup` config.kdl hook
  (next to `niri-autostart`) and/or a
  `systemd.user.services.*` bound to `graphical-session.target` — the
  module already defines both patterns. The daemon needs `niri` and `jq`
  in scope; the module already produces the patched `niri` derivation to
  reference.
- **Waybar**: change `"niri/workspaces"` format from `{index}` to
  `{value}` (optionally `format-icons` keyed by name) so names become
  visible.
- **Concurrency note**: since the event-stream socket cannot carry
  further requests, the daemon must run `niri msg action ...` as a
  separate process per rename (cheap; or keep a persistent second
  connection if implemented in Rust).

## Status

Implemented in `modules/myconfig.desktop.wayland.niri/workspace-names.nix` and
wired into `modules/myconfig.desktop.wayland.niri/default.nix` as
`myconfig.desktop.wayland.niri.workspaceNames` (enable + rules + interval).
The daemon is a polling reconciler (simpler and more robust than the
event-stream variant sketched above): each interval it snapshots
`niri msg --json windows`/`workspaces`, classifies with awk, and applies
renames via raw JSON requests referencing workspaces by id.

Two gotchas discovered during end-to-end testing are worth remembering:
- Tab is in the IFS whitespace class, so `read` with `IFS=$'\t'`
  collapses empty fields — the plan is joined with `\x1f` instead.
- The apply loop only unsets names on workspaces whose current name the
  daemon could have produced (a rule name or its `_N` collision variant);
  manual or static names are never touched.
