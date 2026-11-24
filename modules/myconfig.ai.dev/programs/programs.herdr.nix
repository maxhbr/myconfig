# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `herdr` is an "Agent multiplexer that lives in your terminal"
# (https://herdr.dev, nixpkgs `legacyPackages.x86_64-linux.herdr`). It is a
# companion to the agentic coding harnesses, so it is installed whenever at
# least one agentic coding agent is enabled on this host. The harness set
# mirrors `../../../modules/myconfig.ai.dev/skills/default.nix` (opencode, codex, claude-code,
# pi-coding-agent) and is extended with the remaining agentic terminal coding
# agents (`qwen-code`, `github-copilot-cli`).
#
# The generated ~/.config/herdr/config.toml mirrors the user's tmux keybindings
# (modules/shell.programs.tmux/tmux.conf): ctrl+a prefix, alt+arrow pane focus,
# alt+shift+arrow tab switching, prefix+r reload, and new tabs inheriting the
# current directory. Run `herdr --default-config` for the full key reference.
#
# It also replaces herdr's built-in "new worktree" action with a custom command
# that puts the checkout where workmux (and `git branch-to-worktree`) put
# theirs: `<parent-of-repo>/<repo>__worktrees/<slug>`. See
# ./programs.herdr.README.md for the layout and its limits.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  osconfig = config;

  agenticCodingEnabled =
    (config.myconfig.ai.dev.claude-code.enable or false)
    || (config.myconfig.ai.dev.codex.enable or false)
    || (config.myconfig.ai.dev.opencode.enable or false)
    || (config.myconfig.ai.dev.pi-coding-agent.enable or false)
    || (config.myconfig.ai.dev.qwen-code.enable or false)
    || (config.myconfig.ai.dev.github-copilot-cli.enable or false);

  # `herdr` (the multiplexer) plus the skill generated from it.
  herdr = pkgs.herdr;

  # The agent skill, generated at build time by asking the installed binary:
  # `herdr --skill` prints the frontmatter + body matching exactly this
  # version, so the skill stays in sync with the CLI instead of being a
  # stale vendored copy. Registered in the
  # `myconfig.ai.dev.skills.handcrafted` registry, which
  # ../../../modules/myconfig.ai.dev/skills/default.nix deploys to every enabled agent harness. Implicitly
  # enabled by herdr — there is no separate enable flag.
  herdrSkillSrc = pkgs.runCommand "herdr-skill" { nativeBuildInputs = [ herdr ]; } ''
    mkdir -p $out
    herdr --skill > $out/SKILL.md
  '';

  # `herdr-worktree-sibling` — the workmux-style replacement for herdr's
  # built-in "new worktree" action (`prefix+shift+g`).
  #
  # herdr itself can only be pointed at ONE global worktree root
  # (`[worktrees] directory`, default `~/.herdr/worktrees`) under which it
  # creates `<repo>/<branch-slug>` checkouts; the root is expanded once at
  # server start and knows nothing about the repository the action was
  # triggered from, so the workmux layout
  # (`<parent-of-repo>/<repo>__worktrees/<handle>`) is NOT expressible through
  # that option. What IS expressible is a custom command keybinding, and
  # `herdr worktree create --path <ABSOLUTE PATH>` accepts an arbitrary
  # checkout path while still registering the result as a herdr-managed linked
  # worktree workspace (so `remove_worktree` keeps working). This script glues
  # the two together: it computes the workmux path itself and hands it to
  # herdr. See ../../doc/TODOs/herdr-per-repo-worktree-directory.md.
  #
  # The handle slugification is deliberately identical to
  # ../../../shell.git/bin/git-branch-to-worktree.sh (which mirrors workmux's
  # `derive_handle`), so all three tools agree on the path for a branch.
  herdr-worktree-sibling = pkgs.writeShellApplication {
    name = "herdr-worktree-sibling";
    runtimeInputs = with pkgs; [
      herdr
      git
      coreutils
      # `awk` parses `git worktree list --porcelain` below. Declared
      # explicitly (like ../../../shell.git/default.nix does for
      # `git-branch-to-worktree`) so the script does not depend on an ambient
      # host installation: without it the pipeline dies with status 127
      # *before* `die` can keep the popup open.
      gawk
    ];
    text = ''
      self="herdr-worktree-sibling"

      die() {
          echo "$self: $*" >&2
          # Bound to a herdr popup, whose window disappears as soon as the
          # command exits - keep the error on screen until acknowledged.
          if [ -t 0 ]; then
              read -r -p "press enter to close " _ || true
          fi
          exit 1
      }

      usage() {
          cat <<'EOF'
      Usage: herdr-worktree-sibling [<branch>]

      Create a git worktree for <branch> in the workmux layout

          <parent-of-repo>/<repo-name>__worktrees/<handle>

      and open it as a herdr worktree workspace. Without <branch> the branch
      name is read from stdin (herdr runs this as a popup, which has a tty).

      Run from anywhere in the repository, including from a linked worktree:
      the source is always the MAIN checkout, so a new branch forks from the
      main checkout's HEAD, not from the worktree the popup was opened in.
      EOF
      }

      # Same slugification as workmux's `derive_handle` and
      # `git branch-to-worktree`: lowercase, every run of non-alphanumeric
      # characters becomes a single "-", no leading/trailing "-".
      slugify() {
          local input="''${1,,}" out="" ch i
          for ((i = 0; i < ''${#input}; i++)); do
              ch="''${input:i:1}"
              case "$ch" in
                  [a-z0-9]) out+="$ch" ;;
                  *) out+="-" ;;
              esac
          done
          while [[ $out == *--* ]]; do
              out="''${out//--/-}"
          done
          out="''${out#-}"
          out="''${out%-}"
          printf '%s\n' "$out"
      }

      branch=""
      case "''${1:-}" in
          -h | --help)
              usage
              exit 0
              ;;
          -*) die "unknown option: $1" ;;
          *) branch="''${1:-}" ;;
      esac

      # herdr exports the focused pane's working directory for custom
      # commands; fall back to the popup's own cwd.
      start_dir="''${HERDR_ACTIVE_PANE_CWD:-$PWD}"
      cd "$start_dir" || die "cannot enter $start_dir"
      git rev-parse --git-dir >/dev/null 2>&1 || die "not inside a git repository: $start_dir"

      # The MAIN working tree (first entry of `git worktree list`), so this
      # also works when triggered from inside a linked worktree - herdr
      # refuses `--cwd` inside one ("worktree actions start from the repo
      # parent workspace"). NOTE: this also fixes the BASE of a new branch to
      # the main checkout's HEAD (no `--base` is passed), which is what herdr's
      # own action does too - it does not fork from the focused worktree.
      repo_root="$(git worktree list --porcelain | awk '/^worktree /{print substr($0, 10); exit}')"
      [ -n "$repo_root" ] || die "could not determine the main working tree"

      if [ -z "$branch" ]; then
          [ -t 0 ] || die "no branch given and stdin is not a terminal"
          echo "new worktree in $(basename "$repo_root")__worktrees"
          read -r -p "branch: " branch || true
      fi
      [ -n "$branch" ] || die "no branch name given"
      git check-ref-format --branch "$branch" >/dev/null 2>&1 ||
          die "not a valid branch name: $branch"

      handle="$(slugify "$branch")"
      [ -n "$handle" ] || die "branch name slugifies to the empty string: $branch"

      worktrees_dir="$(dirname "$repo_root")/$(basename "$repo_root")__worktrees"
      target="$worktrees_dir/$handle"
      [ ! -e "$target" ] || die "target worktree path already exists: $target"
      mkdir -p "$worktrees_dir"

      # `--path` is what makes the sibling layout possible at all; `--cwd`
      # selects the source repository (the checkout the worktree forks from).
      # The CLI always answers with JSON; show it only when something failed,
      # so a successful popup just closes.
      if ! out="$(herdr worktree create \
          --cwd "$repo_root" \
          --branch "$branch" \
          --path "$target" \
          --focus 2>&1)"; then
          echo "$out" >&2
          die "herdr worktree create failed for $target"
      fi
    '';
  };

  # Keybindings mirror ~/.tmux.conf (modules/shell.programs.tmux/tmux.conf).
  # Concept mapping: tmux window->herdr tab, tmux pane->herdr pane. Bindings
  # left unset here stay at herdr defaults, several of which already match tmux:
  #   setw -g mouse on          -> [ui] mouse_capture = true (herdr default)
  #   set -g base-index 1       -> herdr tabs are 1-indexed (switch_tab 1..9)
  #   status-keys/mode-keys vi  -> herdr navigate mode uses h/j/k/l by default
  # tmux's `bind-key C-a last-window` (toggle to previous window) has no herdr
  # equivalent (only last_pane exists), so it is not replicated.
  #
  # Split in two: `herdrConfigCommon` ends *inside* the `[keys]` table, so both
  # consumers can append their own trailing `[keys]` entries and their own
  # `[worktrees]` table:
  #   * the host config (`herdrConfig`) below.
  herdrConfigCommon = ''
    # Generated by NixOS (modules/myconfig.ai.dev/programs/programs.herdr.nix).
    # Mirrors ~/.tmux.conf - do not edit by hand. After Nix changes, run
    # `herdr server reload-config` (or restart herdr) to pick them up.

    [terminal]
    # tmux: bind-key 'C' new-window -c '#{pane_current_path}'
    #       bind-key 'M-/' attach -c '#{pane_current_path}'
    # New tabs/panes/workspaces inherit the focused pane's directory.
    new_cwd = "follow"

    [keys]
    # tmux: set -g prefix C-a  (default is ctrl+b)
    prefix = "ctrl+b"

    # tmux: bind r source-file ~/.tmux.conf
    reload_config = "prefix+r"

    # tmux: bind-key 'C' new-window -c '#{pane_current_path}'
    # (with new_cwd = "follow" above, prefix+c already opens in the cwd)
    new_tab = "prefix+c"

    # tmux: bind -n M-Left/Right/Up/Down select-pane -L/-R/-U/-D
    # Direct bindings (no prefix) - matches tmux `bind -n`.
    focus_pane_left = "alt+left"
    focus_pane_down = "alt+down"
    focus_pane_up = "alt+up"
    focus_pane_right = "alt+right"

    # tmux: bind -n M-S-Left next-window
    next_tab = "alt+shift+left"
    # tmux: bind -n M-S-Right previous-window
    previous_tab = "alt+shift+right"
  '';

  herdrConfig = herdrConfigCommon + ''

    # herdr's built-in "new worktree" action can only create checkouts under
    # the single global [worktrees] root below, so it is unbound here and
    # replaced by the custom command at the end of this file, which creates
    # the checkout in the workmux layout instead.
    new_worktree = ""

    [worktrees]
    # Only a fallback: this root is used by flows that do NOT pass an explicit
    # `--path` (e.g. a bare `herdr worktree create` from an agent). herdr then
    # creates `<directory>/<repo>/<branch-slug>`, which cannot express the
    # per-repo sibling layout. Pinned to herdr's own default so such checkouts
    # stay clearly distinguishable from the workmux-style ones.
    directory = "~/.herdr/worktrees"

    # Workmux-style "new worktree": the script computes
    # `<parent-of-repo>/<repo>__worktrees/<handle>` from the focused pane's
    # repository and calls `herdr worktree create --path <that>`, which herdr
    # cannot be configured to do on its own (see the header comment).
    [[keys.command]]
    key = "prefix+shift+g"
    type = "popup"
    command = "${herdr-worktree-sibling}/bin/herdr-worktree-sibling"
    description = "new worktree (workmux layout)"
    width = "70%"
    height = "30%"
  '';

in
{
  config = lib.mkIf agenticCodingEnabled {
    # Install the herdr skill for every enabled agent harness (see
    # ../../../modules/myconfig.ai.dev/skills/default.nix); string form (the derivation's outPath), same
    # convention as the workmux and simple-english skill registrations.
    myconfig.ai.dev.skills.handcrafted.herdr = "${herdrSkillSrc}";

    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".herdr" ];

        home.packages = [
          herdr
          herdr-worktree-sibling
        ];
        xdg.configFile."herdr/config.toml".text = herdrConfig;
      }
    ];
  };
}
