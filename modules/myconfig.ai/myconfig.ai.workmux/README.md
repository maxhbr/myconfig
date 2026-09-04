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
| `jail.nix`    | `myconfig.ai.workmux.jail`    | Run the whole workmux/tmux session inside a single bubblewrap jail      |
| `sandbox.nix` | `myconfig.ai.workmux.sandbox` | Run the whole workmux/tmux session inside a single microvm.nix VM       |

The reusable helper that turns a jailed agent wrapper into a workmux
"named agent" lives one level up in
[`../fns/workmux-worktree.nix`](../fns/workmux-worktree.nix); the coding-agent
modules (`programs.pi-coding-agent`, `programs.claude-code`, `programs.codex`,
`programs.opencode`) call it to register
themselves under `myconfig.ai.workmux.agents.<name>`.

## Concepts

### Named agents (`myconfig.ai.workmux.agents.<name>`)

Coding-agent modules register a workmux *named agent* — usually a
jailed/sandboxed launcher produced by `fns/workmux-worktree.nix`. Each entry
renders under `agents:` in the generated global config as
`{ type; command; args?; env?; }`:

- `type` — workmux built-in behaviour for prompt injection and resume /
  skip-permission flags (`pi`, `claude`, `codex`, `opencode`, ...).
- `command` — the executable workmux launches in the worktree pane.

You select one with `workmux add --agent <name>`. On hosts that enable pi, the
top-level `agent` key defaults to `pi`.

### Declarative `workmux setup` (`statusTracking.enable`, default `true`)

Upstream's `workmux setup` detects installed agents and drops status-tracking
hooks into each agent's config so panes report 🤖 / 💬 / ✅ in tmux window
names, the dashboard and the sidebar. This module installs the *exact same
artefacts* declaratively, gated on which agents the host actually enables:

- **pi** — `~/.pi/agent/extensions/workmux-status.ts` (file drop).
- **opencode** — `~/.config/opencode/package.json` + the
  `plugins/workmux-status.ts` plugin (file drop).
- **codex** — `programs.codex.hooks` (native hook merge) + `features.hooks`.
- **claude-code** — `programs.claude-code.settings.hooks` (native merge).

Disable `statusTracking.enable` to manage the hooks yourself.

### `settings`

Free-form YAML merged into `~/.config/workmux/config.yaml` alongside the
generated `agents`. Notable pinned defaults:

- `nerdfont = true` — avoids the interactive "Nerdfont Setup" prompt that would
  otherwise block non-interactive worktree creation.
- `agent = "pi"` — only on hosts that enable pi.
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

Thin per-agent commands (e.g. `agent-bubblewrap-pi-worktree`) registered by the
coding-agent modules. Each:

- **Resumes in place** when invoked with no arguments from inside an existing
  linked worktree (re-execs the inner launcher with `--continue`).
- **Refuses to run outside tmux** (workmux requires a running tmux server).
- **Slugifies** a free-form first positional into a valid git branch name, so
  `agent-bubblewrap-pi-worktree "Fix the parser"` still works.
- Otherwise execs `workmux add --agent <name> "$@"`, forwarding branch name,
  `--prompt`, `--base`, etc. straight through.

`workmux merge` / `workmux remove` then handle cleanup — no bespoke
resume/cleanup scripts are needed.

### `agent-bubblewrap-alacritty-workmux-tmux` / `agent-bubblewrap-workmux-tmux` (from `jail.nix`)

An alternative, "sandbox-the-whole-session" approach. Instead of sandboxing one
agent binary per pane, it runs *one* bubblewrap jail containing the tmux server,
workmux, the main git repo (the CWD) and its sibling `<basename>__worktrees`
directory:

- `agent-bubblewrap-workmux-tmux` — the jail. Its entrypoint boots a `workmux` tmux
  session on a **private socket inside the jail** (`/tmp/workmux-tmux/socket`),
  wires up the sidebar + dashboard, and attaches. Because server and client
  share the same bwrap process tree, the socket never leaves the jail.
- `agent-bubblewrap-alacritty-workmux-tmux` — the user-facing launcher. Run it from your **main**
  git checkout (it refuses to run from a linked worktree): it resolves the
  `<basename>__worktrees` sibling, binds it read-write into the jail, and opens
  Alacritty running the jail.

Agents launched in panes run *inside* this shared sandbox (no nested bwrap), so
the in-jail workmux config maps `pi` to the **plain** pi binary rather than the
host's `pi`→`pi-bwrap` agent (a nested sandbox would lose pi's real
config/credentials). Everything else (nerdfont, `<agent>` pane layout, default
`agent`) is inherited verbatim from `myconfig.ai.workmux.settings`.

Enabled by default (`myconfig.ai.workmux.jail.enable`) wherever
`myconfig.ai.workmux` is enabled.

### `agent-qemu-workmux-tmux` / `agent-qemu-alacritty-workmux-tmux` (from `sandbox.nix`)

The microVM counterpart of the bubblewrap `jail.nix` above, gated behind
`myconfig.ai.workmux.sandbox.enable` (off by default; requires `/dev/kvm`):

- `agent-qemu-workmux-tmux` — the in-terminal entry point (like `agent-bubblewrap-workmux-tmux`).
  Run it from the main git checkout: it resolves the `<basename>__worktrees`
  sibling, builds the per-invocation microVM runner, boots the VM, waits for
  guest SSH, forwards LLM credentials over the SSH environment and execs the
  in-guest `workmux-sandbox-entry` (which boots the workmux tmux session on a
  private socket and attaches) in the current terminal.
- `agent-qemu-alacritty-workmux-tmux` — a thin popup that opens
  `agent-qemu-workmux-tmux` in a dedicated Alacritty window (like
  `agent-bubblewrap-alacritty-workmux-tmux` opens `agent-bubblewrap-workmux-tmux`).

Both reuse the same `mkSandboxedWorkmuxRunner` guest/runner (see
[`../../../modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix`](../../../modules/myconfig.ai/myconfig.ai.qemu-agent-sandbox/builders.nix)). The
in-terminal sandbox is the reusable entry point; the Alacritty variant is a
thin popup around it so the two wrappers stay byte-identical in everything but
the window.

## Two sandboxing approaches at a glance

| Approach                    | Sandbox unit           | tmux server | Agent in pane          |
| --------------------------- | ---------------------- | ----------- | ---------------------- |
| Per-agent worktree wrappers | one jail per agent     | host tmux   | nested `agent-bubblewrap-*` agent |
| `agent-bubblewrap-alacritty-workmux-tmux`    | one jail per session   | in-jail tmux (private socket) | plain agent (jail is the sandbox) |
| `agent-qemu-workmux-tmux`         | one microVM per session | in-VM tmux (private socket) | plain agent (VM is the sandbox)   |
