# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `herdr` is an "Agent multiplexer that lives in your terminal"
# (https://herdr.dev, nixpkgs `legacyPackages.x86_64-linux.herdr`). It is a
# companion to the agentic coding harnesses, so it is installed whenever at
# least one agentic coding agent is enabled on this host. The harness set
# mirrors `./skills/default.nix` (opencode, codex, claude-code,
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
  jail,
  ...
}:
let
  osconfig = config;
  jail-app = import ./fns/bubblewrap-app.nix {
    inherit
      lib
      pkgs
      jail
      osconfig
      ;
  };

  agenticCodingEnabled =
    (config.myconfig.ai.claude-code.enable or false)
    || (config.myconfig.ai.codex.enable or false)
    || (config.myconfig.ai.opencode.enable or false)
    || (config.myconfig.ai.pi-coding-agent.enable or false)
    || (config.myconfig.ai.qwen-code.enable or false)
    || (config.myconfig.ai.github-copilot-cli.enable or false);

  # `herdr` (the multiplexer) plus the skill generated from it.
  herdr = pkgs.herdr;

  # The agent skill, generated at build time by asking the installed binary:
  # `herdr --skill` prints the frontmatter + body matching exactly this
  # version, so the skill stays in sync with the CLI instead of being a
  # stale vendored copy. Registered in the
  # `myconfig.ai.skills.handcrafted` registry, which
  # ./skills/default.nix deploys to every enabled agent harness. Implicitly
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
  # ../shell.git/bin/git-branch-to-worktree.sh (which mirrors workmux's
  # `derive_handle`), so all three tools agree on the path for a branch.
  herdr-worktree-sibling = pkgs.writeShellApplication {
    name = "herdr-worktree-sibling";
    runtimeInputs = with pkgs; [
      herdr
      git
      coreutils
      # `awk` parses `git worktree list --porcelain` below. Declared
      # explicitly (like ../shell.git/default.nix does for
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
  #   * the host config (`herdrConfig`) below, and
  #   * the config the bubblewrap jail assembles at runtime
  #     (`herdrJailConfigCommonFile`), where the worktree root is only known
  #     once the repository is known.
  herdrConfigCommon = ''
    # Generated by NixOS (modules/myconfig.ai/programs.herdr.nix).
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

  # --- agent-bubblewrap-herdr -------------------------------------------
  # The bubblewrap analogue of `agent-qemu-herdr`, and the one place where
  # herdr's *own* worktree option can be pointed at the workmux layout.
  #
  # The host config cannot do that (one global root for all repositories, see
  # ./programs.herdr.README.md), but a jail session has exactly ONE repository:
  # the wrapper resolves `<parent-of-repo>/<repo>__worktrees`, creates it and
  # binds it read-write into the jail at the identical path (identical, so the
  # absolute paths git records in `.git/worktrees/<n>/gitdir` stay valid on the
  # host), and the entrypoint writes a session config pointing `[worktrees]
  # directory` at it. herdr still appends `<repo-name>/<branch-slug>`, so
  # checkouts land in `<parent-of-repo>/<repo>__worktrees/<repo>/<slug>` — one
  # level deeper than workmux, but inside the same sibling directory.
  #
  # Because the root is correct, the jail restores herdr's BUILT-IN
  # `prefix+shift+g` instead of the host's `[[keys.command]]` popup: it needs
  # no socket round-trip, which matters with `--no-session` below.
  #
  # The session starts in the directory the wrapper was INVOKED from, not in
  # the jail's `$HOME` - which takes both a working `bwrap` cwd (`mount-cwd`)
  # and, because herdr's own first workspace is always rooted at `$HOME`, the
  # relocation the entrypoint performs over the socket API. See the entrypoint
  # below and ./programs.herdr.README.md ("Working directory of the session").
  #
  # The session config lives in the jail's tmpfs `$HOME` and is written on
  # every start, so it can never be stale, and `~/.config/herdr` is
  # deliberately NOT bound from the host.
  herdrJailConfigCommonFile = pkgs.writeText "herdr-jail-config-common.toml" (
    # `onboarding` is a top-level key, so it has to come before the first TOML
    # table. Disabled because the jail's $HOME is a tmpfs: herdr would show its
    # first-run wizard on EVERY start and - worse - come up with no workspace
    # at all ("No workspaces yet"), which also leaves the entrypoint below
    # nothing to relocate to the invocation directory.
    ''
      onboarding = false

    ''
    + herdrConfigCommon
    + ''

      # Restored here (the host config unbinds it): inside the jail the
      # [worktrees] root below is repository-local, so the built-in action
      # already creates the checkout next to the repository.
      new_worktree = "prefix+shift+g"
      # Open/remove an existing herdr-managed checkout.
      open_worktree = "prefix+shift+o"
      remove_worktree = "prefix+shift+x"
    ''
  );

  # Runs INSIDE the jail as the jail's entrypoint.
  herdr-jail-entry = pkgs.writeShellApplication {
    name = "herdr-jail-entry";
    runtimeInputs = with pkgs; [
      herdr
      git
      coreutils
      # `awk` parses `git worktree list --porcelain` below.
      gawk
      # `jq` reads the workspace ids out of the socket API's JSON answers
      # when the startup workspace is relocated below.
      jq
    ];
    text = ''
      self="agent-bubblewrap-herdr"

      # The directory the wrapper was invoked from. `bwrap` keeps the host
      # working directory (the jail binds it read-write via `mount-cwd`), so
      # this is the project directory the user ran the command in - it is the
      # directory herdr's first workspace has to be rooted at (see the
      # background job at the end of this script).
      start_dir="$PWD"

      # Session config: the shared part plus - inside a git repository - the
      # repository-local worktree root. Written fresh on every start into the
      # jail's tmpfs $HOME.
      mkdir -p "$HOME/.config/herdr"
      cat ${herdrJailConfigCommonFile} > "$HOME/.config/herdr/config.toml"

      if git rev-parse --git-dir >/dev/null 2>&1; then
          # The MAIN working tree, so the layout is identical whether the jail
          # was started from the main checkout or from one of its linked
          # worktrees. Note that the main checkout itself is NOT bound into
          # the jail when the jail was started from a linked worktree - only
          # its shared git directory and the `__worktrees` sibling are (see
          # `worktreesSiblingPerm`), which is all this path is used for.
          repo_root="$(git worktree list --porcelain | awk '/^worktree /{print substr($0, 10); exit}')"
          if [ -z "$repo_root" ]; then
              echo "$self: could not determine the main working tree" >&2
              exit 1
          fi

          worktrees="$(dirname "$repo_root")/$(basename "$repo_root")__worktrees"
          if ! mkdir -p "$worktrees" 2>/dev/null; then
              echo "$self: $worktrees is not writable inside the jail;" >&2
              echo "$self: start the jail from the main checkout so the wrapper binds it." >&2
              exit 1
          fi

          # The path goes into a TOML basic string, so `\` and `"` must be
          # escaped and control characters (which TOML forbids there)
          # rejected - otherwise a repository name containing them yields a
          # config herdr cannot parse, and herdr then silently FALLS BACK TO
          # ITS DEFAULTS (`~/.herdr/worktrees`), quietly losing the
          # repository-local root.
          case $worktrees in
              *[[:cntrl:]]*)
                  echo "$self: repository path contains control characters: $worktrees" >&2
                  exit 1
                  ;;
          esac
          worktrees_toml="''${worktrees//\\/\\\\}"
          worktrees_toml="''${worktrees_toml//\"/\\\"}"

          printf '\n[worktrees]\ndirectory = "%s"\n' "$worktrees_toml" \
              >> "$HOME/.config/herdr/config.toml"
      else
          # Not a git repository: herdr itself works fine, only the worktree
          # actions have nowhere sensible to write. Leave `[worktrees]`
          # unset, so herdr uses its own default (`~/.herdr/worktrees`) -
          # which is the jail's tmpfs $HOME and therefore ephemeral.
          echo "$self: not inside a git repository: $start_dir" >&2
          echo "$self: starting herdr anyway; worktree actions are not usable here." >&2
      fi

      # herdr's FIRST workspace is always rooted at $HOME: the `new_cwd`
      # policy only applies to workspaces created later, and there is no CLI
      # flag for the initial one. Inside the jail $HOME is a tmpfs, so this
      # would drop the user into an empty directory instead of the project.
      # Fix it up over the socket API (which `--no-session` also serves): once
      # herdr is up, create a workspace rooted at the invocation directory,
      # focus it, and close the workspace(s) that existed before. Runs in the
      # background because the API only answers after herdr has started, and
      # discards all output because the TUI owns the terminal from here on.
      # `new_cwd = "follow"` is deliberately kept (rather than "current"): new
      # tabs/panes must inherit the focused workspace's directory, which for a
      # worktree workspace is the worktree and not the invocation directory.
      (
          initial_ws=()
          for _ in $(seq 1 100); do
              if out="$(herdr workspace list 2>/dev/null)"; then
                  mapfile -t initial_ws < <(
                      printf '%s' "$out" |
                          jq -r '.result.workspaces[].workspace_id' 2>/dev/null
                  )
                  if [ "''${#initial_ws[@]}" -gt 0 ]; then
                      break
                  fi
              fi
              sleep 0.1
          done
          # No API answer (or no workspace yet) - leave herdr alone rather
          # than adding a second workspace to an unknown state.
          if [ "''${#initial_ws[@]}" -eq 0 ]; then
              exit 0
          fi
          herdr workspace create \
              --cwd "$start_dir" \
              --label "$(basename "$start_dir")" \
              --focus >/dev/null || exit 0
          for ws in "''${initial_ws[@]}"; do
              herdr workspace close "$ws" >/dev/null 2>&1 || true
          done
      ) >/dev/null 2>&1 &

      # Monolithic: no server/client split, so the session cannot attach to a
      # differently-configured server and dies with the jail.
      exec herdr --no-session "$@"
    '';
  };

  # Create the `<parent-of-repo>/<repo>__worktrees` sibling on the HOST and
  # bind it read-write at the identical path, so the checkouts herdr creates
  # inside the jail are real host directories with valid git metadata. Runs in
  # the wrapper before `bwrap` starts (`add-runtime`), where only coreutils is
  # guaranteed on PATH - hence the pure-bash walk up to the repository root
  # instead of `git rev-parse`. Unlike the equivalent in
  # ./programs.pi-coding-agent/default.nix this *creates* the directory: herdr
  # has to be able to write the first worktree into it.
  #
  # When the wrapper is started from a LINKED worktree, `<repo>/.git` is a
  # file pointing at `<main-repo>/.git/worktrees/<name>`; without that shared
  # git directory every git command inside the jail fails with "not a git
  # repository". It is therefore resolved (via the `commondir` file git writes
  # next to it) and bound read-write too, while the main CHECKOUT stays
  # invisible - the jail only ever needs the git metadata, not the other
  # working tree.
  worktreesSiblingPerm = (jail.init pkgs).combinators.add-runtime ''
    _hb_root="$PWD"
    while [ "$_hb_root" != "/" ] && [ ! -e "$_hb_root/.git" ]; do
      _hb_root="$(dirname "$_hb_root")"
    done
    if [ -e "$_hb_root/.git" ]; then
      _hb_root="$(realpath "$_hb_root")"
      _hb_home="$(realpath "$HOME")"
      # The shared guard (`rejectHomeCwd`) only checks the INITIAL $PWD, but
      # the walk above can still land on $HOME - or on a parent of it - when
      # the home directory itself is a git checkout. Binding that root
      # read-write would hand the agent the entire home directory, so refuse
      # instead. Matches $HOME itself and any ancestor of it.
      case "$_hb_home" in
        "$_hb_root" | "$_hb_root"/*)
          echo "agent-bubblewrap-herdr: the enclosing git repository is \$HOME (or contains it): $_hb_root" >&2
          echo "agent-bubblewrap-herdr: binding it read-write would expose the whole home directory." >&2
          echo "agent-bubblewrap-herdr: run from a project checkout below \$HOME instead." >&2
          exit 1
          ;;
      esac
      # Linked worktree: `.git` is a file `gitdir: <path>`. `<path>/commondir`
      # points at the MAIN repository's `.git`, which holds the shared object
      # store, refs and config. `_hb_main` is that repository's working tree,
      # so the `__worktrees` sibling below is the same directory no matter
      # which checkout the jail was started from - matching what the
      # entrypoint computes from `git worktree list`.
      _hb_gitcommon=""
      _hb_main="$_hb_root"
      if [ -f "$_hb_root/.git" ]; then
        IFS= read -r _hb_line < "$_hb_root/.git" || true
        _hb_gitdir="''${_hb_line#gitdir:}"
        _hb_gitdir="''${_hb_gitdir# }"
        case "$_hb_gitdir" in
          /*) ;;
          *) _hb_gitdir="$_hb_root/$_hb_gitdir" ;;
        esac
        if [ -f "$_hb_gitdir/commondir" ]; then
          IFS= read -r _hb_common < "$_hb_gitdir/commondir" || true
          case "$_hb_common" in
            /*) ;;
            *) _hb_common="$_hb_gitdir/$_hb_common" ;;
          esac
          if [ -d "$_hb_common" ]; then
            _hb_gitcommon="$(realpath "$_hb_common")"
            # Only a conventional `<main-repo>/.git` layout tells us where the
            # main working tree is; anything else (bare repos, separate git
            # dirs) keeps the enclosing checkout as the reference point.
            if [ "$(basename "$_hb_gitcommon")" = ".git" ]; then
              _hb_main="$(dirname "$_hb_gitcommon")"
            fi
          fi
        fi
      fi
      _hb_worktrees="$(dirname "$_hb_main")/$(basename "$_hb_main")__worktrees"
      mkdir -p "$_hb_worktrees"
      RUNTIME_ARGS+=(--bind "$_hb_root" "$_hb_root" --bind "$_hb_worktrees" "$_hb_worktrees")
      if [ -n "$_hb_gitcommon" ]; then
        RUNTIME_ARGS+=(--bind "$_hb_gitcommon" "$_hb_gitcommon")
      fi
    fi
  '';

  # Writable home state of the agents that run *inside* this jail. The jail is
  # the sandbox here (like ./myconfig.ai.workmux/jail.nix), so the panes run
  # the PLAIN agent binaries, which need their real state directories.
  agentUserDataDirsByFlag = {
    pi-coding-agent = [ ".pi" ];
    claude-code = [
      ".claude"
      ".config/claude-code"
      ".config/mcp"
    ];
    opencode = [
      ".config/opencode"
      ".local/share/opencode"
      ".local/state/opencode"
    ];
    codex = [ ".codex" ];
    qwen-code = [ ".qwen" ];
    github-copilot-cli = [
      ".config/.copilot"
      ".local/state/.copilot"
    ];
  };

  agentUserDataDirs = lib.concatLists (
    lib.attrValues (
      lib.filterAttrs (name: _: config.myconfig.ai.${name}.enable or false) agentUserDataDirsByFlag
    )
  );

  agent-bubblewrap-herdr = jail-app {
    name = "agent-bubblewrap-herdr";
    pkg = herdr-jail-entry;
    userDataDirs = agentUserDataDirs;
    # claude-code keeps its account/session index in a single home file.
    userDataFiles = lib.optional (config.myconfig.ai.claude-code.enable or false) ".claude.json";
    # ~/.agents/skills (handcrafted skills, incl. the herdr skill) read-only.
    # `.config/herdr` is deliberately NOT bound: the entrypoint writes the
    # session config into the jail's tmpfs $HOME instead.
    extraConfigDirs = [ ".agents" ];
    # Plain (un-jailed) agent binaries for the panes - no nested sandbox.
    # `fish` is added so panes can run an interactive fish shell inside the
    # jail (the base `devTools` only provide `bashInteractive`).
    extraDevTools = [
      herdr
      pkgs.fish
    ]
    ++ enabledAgentPackages;
    extraPermissions = [ worktreesSiblingPerm ];
  };

  # The coding-agent CLIs this repo can install on the host, mapped from
  # their `myconfig.ai.<name>.enable` flag to the package attribute the
  # matching host wrapper uses. Mirrors the `agentPackagesByFlag` set in
  # modules/myconfig.ai/myconfig.ai.gvisor-agent-sandbox/default.nix so the
  # `agent-qemu-herdr` guest carries exactly the same agents the host (and
  # the gVisor sandbox image) offers.
  agentPackagesByFlag = {
    pi-coding-agent = pkgs.nixos-unstable.pi-coding-agent;
    opencode = pkgs.opencode;
    claude-code = pkgs.claude-code;
    codex = pkgs.codex;
    github-copilot-cli = pkgs.github-copilot-cli;
    qwen-code = pkgs.qwen-code;
  };

  # The subset of agents actually enabled on this host, as a list of store
  # paths. Baked into the `agent-qemu-herdr` wrapper as the default value of
  # AGENT_QEMU_HERDR_AGENT_PACKAGES (a JSON array), which the impure runner
  # expression reads and passes to
  # `mkAgentQemuHerdrRunner` as `agentPackages`. Only public store paths are
  # baked in (never credentials); this matches `agent-qemu-pi`, which bakes the
  # `piPackage` store path into its runner the same way.
  enabledAgentPackages = lib.attrValues (
    lib.filterAttrs (name: _: config.myconfig.ai.${name}.enable or false) agentPackagesByFlag
  );

  agentPackagesJson = builtins.toJSON (map (p: p.outPath) enabledAgentPackages);

  # `agent-qemu-herdr` — the `herdr` analogue of `agent-qemu-pi`. Same
  # ergonomics (run it from a project subdirectory; the working directory is
  # the only writable thing the agent sees) and the same microVM machinery
  # (qemu + SLiRP user-mode networking, ephemeral root, unprivileged `agent`
  # user, virtiofs workspace share), but instead of exec'ing `pi` over SSH it
  # execs `herdr` — the agent multiplexer — so that from inside the sandbox
  # the user is dropped into a `herdr` session and can start `pi` / other
  # coding agents from within it.
  #
  # The guest closure carries `herdr` plus whatever coding-agent CLIs are
  # enabled on the building host (see `enabledAgentPackages` above), so the
  # agents `herdr` launches are available on PATH inside the VM.
  #
  # Workspace handling, credential forwarding and the refuse-$HOME guard are
  # identical to `agent-qemu-pi`. See
  # ./myconfig.ai.qemu-agent-sandbox/builders.nix (`mkAgentQemuHerdrRunner`) and
  # ./agent-qemu-herdr.README.md.
  agent-qemu-herdr = pkgs.writeShellApplication {
    name = "agent-qemu-herdr";
    runtimeInputs = with pkgs; [
      nix
      openssh
      coreutils
      gnugrep
    ];
    text = ''
      # Refuse to run in $HOME: like agent-qemu-pi, the working directory is
      # shared writable into the sandbox, and sharing the whole home
      # directory would defeat the isolation. Run from a project subdirectory.
      if [ "$PWD" = "$HOME" ]; then
        echo "agent-qemu-herdr: refusing to run in home directory ($HOME):" >&2
        echo "agent-qemu-herdr: the working directory is shared writable into the VM." >&2
        echo "agent-qemu-herdr: run from a project subdirectory instead." >&2
        exit 1
      fi

      workspace="$(realpath "$PWD")"
      if [ ! -d "$workspace" ]; then
        echo "agent-qemu-herdr: workspace is not a directory: $workspace" >&2
        exit 1
      fi

      # Per-invocation runtime state (throwaway SSH key, VM control socket).
      runtime_dir="$(mktemp -d "''${XDG_RUNTIME_DIR:-/tmp}/agent-qemu-herdr.XXXXXX")"
      # Pick a pseudo-random host-localhost port for the forwarded guest SSH.
      ssh_port=$(( (RANDOM % 20000) + 20000 ))

      vm_pid=""
      cleanup() {
        # Killing the launcher triggers its own trap, which tears down the
        # qemu VM and the virtiofsd daemon(s) it started.
        if [ -n "$vm_pid" ] && kill -0 "$vm_pid" 2>/dev/null; then
          kill "$vm_pid" 2>/dev/null || true
          wait "$vm_pid" 2>/dev/null || true
        fi
        rm -rf "$runtime_dir"
      }
      trap cleanup EXIT INT TERM

      # Throwaway SSH keypair authorizing the launcher into the guest.
      ssh-keygen -q -t ed25519 -N "" -f "$runtime_dir/id" -C agent-qemu-herdr

      export QEMU_AGENT_SANDBOX_KIND=herdr
      export AGENT_QEMU_HERDR_WORKSPACE="$workspace"
      export AGENT_QEMU_HERDR_SSH_PORT="$ssh_port"
      export AGENT_QEMU_HERDR_AUTHORIZED_KEYS="$runtime_dir/id.pub"
      export AGENT_QEMU_HERDR_NETWORK=1
      # Pin the guest `agent` user's uid/gid to ours: virtiofsd runs
      # unprivileged (no --translate-uid) and passes the workspace's real
      # host ownership straight through, so the guest kernel's permission
      # check only allows writes when the guest uid matches ours. Without
      # this, `agent`'s auto-assigned uid only accidentally matches the host
      # owner, and writes to /workspace fail with EACCES while reads (backed
      # by the usual world-readable bits) keep working. See
      # ./myconfig.ai.qemu-agent-sandbox/builders.nix (`mkSandboxedRunner`).
      agent_qemu_herdr_uid="$(id -u)"
      agent_qemu_herdr_gid="$(id -g)"
      export AGENT_QEMU_HERDR_UID="$agent_qemu_herdr_uid"
      export AGENT_QEMU_HERDR_GID="$agent_qemu_herdr_gid"
      # JSON array of enabled coding-agent store paths, baked in at build
      # time from the host's myconfig.ai.<name>.enable flags.
      export AGENT_QEMU_HERDR_AGENT_PACKAGES='${agentPackagesJson}'
      # Shared sandbox tools (myconfig.ai.sandboxTools), baked in at build
      # time as a JSON array of store paths; read by the impure
      # standalone qemu-agent-sandbox runner expression.
      export AGENT_QEMU_HERDR_EXTRA_PACKAGES='${sandboxToolsJson}'

      echo "agent-qemu-herdr: building microvm runner for workspace: $workspace" >&2
      # Evaluate the module-owned expression directly. Impure evaluation is
      # required for the transient workspace, port, key, and uid/gid values.
      runner=$(nix build --impure --no-link --print-out-paths \
        --file ${config.myconfig.ai.qemu-agent-sandbox.runnerExpression})

      # microvm.nix's qemu runner connects to the virtiofs daemons over
      # RELATIVE unix socket paths; the runner's `bin/sandboxed-launch`
      # starts virtiofsd, waits for the sockets, then runs qemu — all from the
      # current directory. Run it from $runtime_dir so the relative socket
      # paths resolve to a stable, per-invocation location.
      cd "$runtime_dir"

      echo "agent-qemu-herdr: starting microvm (guest SSH forwarded to 127.0.0.1:$ssh_port)" >&2
      "$runner/bin/sandboxed-launch" >"$runtime_dir/console.log" 2>&1 &
      vm_pid=$!

      # Wait for the guest SSH server to accept our key (or the VM to die).
      ssh_opts=(
        -p "$ssh_port"
        -i "$runtime_dir/id"
        -o StrictHostKeyChecking=no
        -o UserKnownHostsFile=/dev/null
        -o ConnectTimeout=3
        -o LogLevel=ERROR
      )
      ready=0
      for _ in $(seq 1 120); do
        if ! kill -0 "$vm_pid" 2>/dev/null; then
          echo "agent-qemu-herdr: microvm exited before SSH became ready; console log:" >&2
          tail -n 40 "$runtime_dir/console.log" >&2 || true
          exit 1
        fi
        if ssh "''${ssh_opts[@]}" agent@127.0.0.1 true 2>/dev/null; then
          ready=1
          break
        fi
        sleep 1
      done
      if [ "$ready" -ne 1 ]; then
        echo "agent-qemu-herdr: timed out waiting for guest SSH; console log:" >&2
        tail -n 40 "$runtime_dir/console.log" >&2 || true
        exit 1
      fi

      # Forward LLM credentials over the SSH environment (never in argv or the
      # store). Only forward variables that are actually set on the host.
      for var in OPENAI_API_KEY OPENAI_BASE_URL OPENROUTER_BASE_URL ANTHROPIC_API_KEY; do
        if [ -n "''${!var:-}" ]; then
          ssh_opts+=(-o "SetEnv=$var=''${!var}")
        fi
      done

      # Seed the in-guest configuration for EVERY agent `herdr` can launch
      # (pi, opencode, claude-code, codex, qwen-code, github-copilot-cli,
      # hermes) from the matching host dotfiles. The runner carries a
      # `seed-agent-config` script that copies the ALLOWLISTED,
      # denylist-filtered host configuration into the guest `/home/agent` over
      # SSH — never baking anything into the store and never copying
      # credential files (keys keep flowing over the SSH environment above).
      # See ./myconfig.ai.qemu-agent-sandbox/builders.nix (`mkSeedScript`) and
      # ../fns/seed-agent-config.nix. Run it BEFORE the interactive session so
      # the agents launched from inside `herdr` start already configured.
      if [ -x "$runner/bin/seed-agent-config" ]; then
        echo "agent-qemu-herdr: seeding guest agent config from host" >&2
        "$runner/bin/seed-agent-config" "$ssh_port" "$runtime_dir/id" 127.0.0.1 agent \
          || echo "agent-qemu-herdr: warning: config seeding reported errors (continuing)" >&2
      fi

      # Build a safely-quoted remote command: cd into the workspace and exec
      # herdr (the agent multiplexer) with the caller's argument vector
      # preserved via printf %q. From inside the herdr session the user starts
      # pi / opencode / etc.
      remote_cmd='cd /workspace && exec herdr'
      for a in "$@"; do
        remote_cmd+=" $(printf '%q' "$a")"
      done

      # Interactive session (-t for a TTY so the herdr TUI works).
      ssh -tt "''${ssh_opts[@]}" agent@127.0.0.1 "$remote_cmd"
    '';
  };

  # Shared sandbox tools (myconfig.ai.sandboxTools) as a JSON array of store
  # paths, baked into the `agent-qemu-herdr` wrapper and read (via
  # AGENT_QEMU_HERDR_EXTRA_PACKAGES) by the impure runner expression that builds
  # the per-invocation VM runner. Same pattern as `agentPackagesJson` above.
  sandboxToolsJson = builtins.toJSON (
    map (p: p.outPath) config.myconfig.ai.sandboxTools.extraPackages
  );
in
{
  config = lib.mkIf agenticCodingEnabled {
    # Install the herdr skill for every enabled agent harness (see
    # ./skills/default.nix); string form (the derivation's outPath), same
    # convention as the workmux and simple-english skill registrations.
    myconfig.ai.skills.handcrafted.herdr = "${herdrSkillSrc}";

    home-manager.sharedModules = [
      {
        myconfig.persistence.directories = [ ".herdr" ];

        home.packages = [
          herdr
          herdr-worktree-sibling
          agent-bubblewrap-herdr
          agent-qemu-herdr
        ];
        xdg.configFile."herdr/config.toml".text = herdrConfig;
      }
    ];
  };
}
