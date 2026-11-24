# myconfig.ai.workmux

[workmux](https://github.com/raine/workmux) is "parallel development in tmux
with git worktrees" — a terminal-native companion to agentic coding harnesses
(pi, claude-code, codex, opencode, ...). This module installs workmux, owns its
*global* configuration, and wires the per-agent status-tracking hooks
declaratively.

It is auto-enabled (`mkDefault`) whenever the AI tooling, the `dev` profile and
`programs.tmux` are all active on the host. The workmux binary itself comes from
the upstream flake input `inputs.workmux.packages.${system}.default`.

## Files in this directory

| File          | Option namespace              | Purpose                                                                 |
| ------------- | ----------------------------- | ----------------------------------------------------------------------- |
| `default.nix` | `myconfig.ai.workmux`         | Install workmux, generate `~/.config/workmux/config.yaml`, status hooks |
| `mysbx.nix`   | `myconfig.ai.workmux.mysbx`   | Make the workmux session the interactive payload of every `mysbx` sandbox |

The reusable helper that turns an agent launcher into a workmux
"named agent" lives one level up in
[`../fns/workmux-worktree.nix`](../fns/workmux-worktree.nix); the coding-agent
modules (`programs.pi-coding-agent`, `programs.claude-code`, `programs.codex`,
`programs.opencode`) call it to register
themselves under `myconfig.ai.workmux.agents.<name>`.

## Concepts

### Named agents (`myconfig.ai.workmux.agents.<name>`)

Coding-agent modules register a workmux *named agent* — a launcher produced by
`fns/workmux-worktree.nix`. Each entry
renders under `agents:` in the generated global config as
`{ type; command; args?; env?; }`:

- `type` — workmux built-in behaviour for prompt injection and resume /
  skip-permission flags (`pi`, `claude`, `codex`, `opencode`, ...).
- `command` — the executable workmux launches in the worktree pane.

You select one with `workmux add --agent <name>`. On hosts that enable pi, the
top-level `agent` key defaults to `pi`.

Sandboxing is not part of the per-agent entry anymore: `mysbx` is the sandbox,
and a workmux session runs either on the host (unsandboxed) or as the
interactive payload of a `mysbx` sandbox (see `mysbx.nix` above).

### Declarative `workmux setup` (`statusTracking.enable`, default `true`)

Upstream's `workmux setup` detects installed agents and drops status-tracking
hooks into each agent's config so panes report 🤖 / 💬 / ✅ in tmux window
names, the dashboard and the sidebar. This module installs the *exact same
artefacts* declaratively, gated on which agents the host actually enables:

- **pi** — `~/.pi/agent/extensions/workmux-status.ts` (file drop).
- **opencode** — `~/.config/opencode/plugins/workmux-status/{index,status,tui}.ts`
  (file drop; the OpenCode 2 plugin layout, which upstream also installs for
  OpenCode 2 — the 1.x single-file plugin no longer loads).
- **codex** — `programs.codex.hooks` (native hook merge) + `features.hooks`.
- **claude-code** — `programs.claude-code.settings.hooks` (native merge).

Disable `statusTracking.enable` to manage the hooks yourself.

### `settings`

Free-form YAML merged into `~/.config/workmux/config.yaml` alongside the
generated `agents`. Notable pinned defaults:

- `nerdfont = true` — avoids the interactive "Nerdfont Setup" prompt that would
  otherwise block non-interactive worktree creation.
- `agent = "pi"` — only on hosts that enable pi; inside an mysbx sandbox
  the agent surface is **runtime configuration** (bd myconfig-fv4h, see
  [`../../../mysbx/README.md`](../../../mysbx/README.md)): this key is derived from
  `myconfig.ai.dev.mysbx.defaultAgent` and the in-sandbox agent map
  carries every kind of `myconfig.ai.dev.mysbx.agents` (plain binaries,
  no nested launchers), so a `mysbx --<kind>` run or a layer's
  `default-agent` selects any of them; the derived key wins over a
  host-set `settings.agent` there.
- `panes = [ { command = "<agent>"; focus = true; } ]` — a single focused pane
  running the selected agent, so `workmux add` always launches an agent instead
  of falling back to a bare shell.

See <https://workmux.raine.dev/guide/configuration> for all keys.

## Commands

### `tmux-workmux` (from `default.nix`)

Bootstraps a dedicated `workmux` tmux session driving the parallel-worktree
agents: creates the session (detached) if needed, opens the sidebar +
dashboard once (tracked via the `@workmux_bootstrapped` session option), then
`switch-client`s to it when already inside tmux or `attach`es otherwise. The
status hooks the dashboard/sidebar rely on are installed declaratively, so
there is no runtime `workmux setup` step.

### `<agent>-worktree` wrappers (from `../fns/workmux-worktree.nix`)

Thin per-agent commands (e.g. `pi-worktree`) registered by the
coding-agent modules. Each:

- **Resumes in place** when invoked with no arguments from inside an existing
  linked worktree (re-execs the inner launcher with `--continue`).
- **Refuses to run outside tmux** (workmux requires a running tmux server).
- **Slugifies** a free-form first positional into a valid git branch name, so
  `pi-worktree "Fix the parser"` still works.
- Otherwise execs `workmux add --agent <name> "$@"`, forwarding branch name,
  `--prompt`, `--base`, etc. straight through.

`workmux merge` / `workmux remove` then handle cleanup — no bespoke
resume/cleanup scripts are needed.

### The mysbx sandbox as the workmux session (`mysbx.nix`)

One `mysbx` sandbox can own the whole tmux/workmux session: the agents workmux
launches run *inside* it, un-jailed (plain binaries — mysbx is already the
sandbox). `cd <repo> && mysbx` is the entry point; on hosts that enable both
`myconfig.ai.dev.workmux` and `myconfig.ai.dev.mysbx` this is the default
interactive payload (mysbx/docs/design/config.md D16). The tmux socket lives
inside the sandbox home; `workmux add` gets the `<repo>__worktrees` sibling
bound read-write automatically.

## Detached GUI launchers

The `<name>-alacritty-tmux` wrappers from `myconfig.agentUsers` share the
shell prelude in
[`../../../lib/detached-gui-launcher.nix`](../../../lib/detached-gui-launcher.nix).
Instead of `exec alacritty …` it calls `gui_launcher_exec alacritty …`, which:

- starts the GUI via `setsid --fork` in its own session, with stdin from
  `/dev/null` and stdout/stderr appended to
  `${XDG_STATE_HOME:-~/.local/state}/myconfig-gui-launchers/<wrapper>.log`
  (falling back to `$TMPDIR`/`/tmp` if that directory cannot be created),
- returns immediately, so the calling shell is not blocked and the window
  survives the shell exiting — no manual `… &disown` needed.

Detaching only happens when it makes sense; the foreground (`exec`) path is
kept when any of the following holds:

- **stdout is not a TTY** (scripts, pipes, `.desktop` launchers, `-e`/`--command`
  invocations from a window manager),
- the first argument is **`--foreground`** (it is consumed by the prelude),
- **`MYCONFIG_GUI_LAUNCHER_FOREGROUND=1`** is exported.

All validation the wrapper performs *before* spawning (git checkout checks,
worktree checks, …) still runs in the foreground, so its error messages and
non-zero exit statuses reach the user unchanged. Once detached, the wrapper
prints the log path and exits `0`; the GUI's own output goes to the log file.
