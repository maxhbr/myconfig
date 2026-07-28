# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux.jail — run the *whole* workmux/tmux session inside a
# single bubblewrap jail and pop it up in a dedicated Alacritty window.
#
# Unlike the per-agent jails (`jailed-pi`, ...) which sandbox one agent binary
# each, this wrapper takes the opposite approach: it starts *one* jail that
# contains the tmux server, workmux, the main git repository (the CWD) and its
# sibling `<basename>__worktrees` directory. Because the tmux server and the
# attaching client both live in that single bwrap process tree, they share a
# private tmux socket that never leaves the jail — hence "custom socket within
# the jail". Agents that workmux launches in panes then run *inside* this
# shared sandbox (no nested bwrap), so they must be plain (un-jailed) agents.
#
# Entry points:
#
#   * `jailed-workmux-tmux`      — the jail. Its entrypoint boots a `workmux`
#                                  tmux session on the private socket, wires up
#                                  the sidebar + dashboard, and attaches.
#   * `alacritty-workmux-here`   — user-facing launcher. Run it from the main
#                                  git checkout: it resolves the worktrees
#                                  sibling, binds it read-write into the jail,
#                                  and opens Alacritty running the jail.
{
  config,
  lib,
  pkgs,
  jail,
  ...
}:
let
  cfg = config.myconfig.ai.workmux.jail;
  wmCfg = config.myconfig.ai.workmux;
  aiCfg = config.myconfig.ai;
  osconfig = config;

  callJailLib =
    file:
    import file {
      inherit
        lib
        pkgs
        jail
        osconfig
        ;
    };
  jail-app = callJailLib ./fns/jail-app.nix;

  # tmux socket *inside* the jail. With `persistentTmp` (jail-app default) the
  # jail's /tmp is a host-backed directory private to this jail, so the socket
  # never collides with the user's normal tmux server on the host default
  # socket. Both the server and the attaching client run in the same bwrap
  # process tree, so they resolve the same socket inode.
  socketPath = "/tmp/workmux-tmux/socket";

  # Plain (un-jailed) agent binaries made available *inside* the shared jail so
  # workmux can launch them in panes. The jail itself is the sandbox here, so
  # these are the raw agent binaries, not the nested `jailed-*` wrappers.
  innerAgents = lib.optional aiCfg.pi-coding-agent.enable pkgs.nixos-unstable.pi-coding-agent;

  # Entrypoint executed as the jail's `pkg`. Mirrors the host `tmux-workmux`
  # bootstrap (see myconfig.ai.workmux.nix) but pins a private `-S <socket>`
  # and always attaches (a fresh jail is never already inside tmux).
  entry = pkgs.writeShellApplication {
    name = "workmux-tmux-jail-entry";
    runtimeInputs = [
      wmCfg.package
      pkgs.tmux
      pkgs.coreutils
      pkgs.bashInteractive
    ];
    text = ''
      session=workmux
      socket=${lib.escapeShellArg socketPath}
      mkdir -p "$(dirname "$socket")"

      # Inside the jail, tmux resolves the login shell from the jail's
      # /etc/passwd, where mhuber's shell is `nologin`. Every new pane would
      # then exec `nologin`, die immediately and take the server down
      # ("no server running"). Pin a real interactive bash instead, both via
      # SHELL (default-shell falls back to it) and the tmux option directly.
      shell=${lib.escapeShellArg (lib.getExe pkgs.bashInteractive)}
      export SHELL="$shell"

      # Pin the private socket for every tmux invocation in this script.
      tmux() { command tmux -S "$socket" "$@"; }

      # Create the session detached if it does not exist yet. Set
      # default-shell on the (freshly started) server *before* new-session so
      # the first pane already uses bash and never execs `nologin`.
      if ! tmux has-session -t "=$session" 2>/dev/null; then
        tmux start-server
        tmux set-option -g default-shell "$shell"
        tmux new-session -d -s "$session"
      fi

      # Bootstrap the sidebar + dashboard exactly once (tracked via a session
      # option, so a half-created session still gets its dashboard next run).
      # The trailing colon in `=$session:` forces session resolution for
      # send-keys and targets its active pane.
      if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
        tmux set-option -t "=$session:" @workmux_bootstrapped 1
        tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
      fi

      exec tmux attach-session -t "=$session"
    '';
  };

  # The jail. `mount-cwd` (jail-app default) binds the main repo read-write;
  # `WORKMUX_WORKTREES_DIR` binds the sibling worktrees dir read-write so
  # `workmux add` can create linked worktrees next to the main checkout.
  jailed-workmux-tmux = jail-app {
    name = "jailed-workmux-tmux";
    pkg = entry;
    # rw state: pi session/config state and the workmux config dir (workmux may
    # persist worktree metadata there).
    userDataDirs = [
      ".pi"
      ".config/workmux"
    ];
    # ~/.agents/skills (handcrafted skills) picked up read-only by the agents.
    extraConfigDirs = [ ".agents" ];
    # tmux + workmux + the plain agent binaries live inside the shared sandbox.
    extraDevTools = [
      wmCfg.package
      pkgs.tmux
    ]
    ++ innerAgents;
    # Bind the `<basename>__worktrees` sibling read-write at runtime (path
    # supplied by the host launcher below via this env var).
    extraReadWriteEnvPaths = [ "WORKMUX_WORKTREES_DIR" ];
  };

  alacritty-workmux-here = pkgs.writeShellApplication {
    name = "alacritty-workmux-here";
    runtimeInputs = [
      pkgs.alacritty
      pkgs.git
      pkgs.coreutils
      jailed-workmux-tmux
    ];
    text = ''
      # Must be run from a git checkout.
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "alacritty-workmux-here: not inside a git repository." >&2
        echo "alacritty-workmux-here: run it from your main git checkout." >&2
        exit 1
      fi

      # Refuse to run from a *linked* worktree — the jail is meant to own the
      # main checkout plus all of its worktrees. In a linked worktree the
      # per-worktree git dir differs from the shared common dir.
      git_dir="$(git rev-parse --path-format=absolute --git-dir)"
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir)"
      if [ "$git_dir" != "$git_common_dir" ]; then
        echo "alacritty-workmux-here: refusing to run from a linked worktree." >&2
        echo "alacritty-workmux-here: run it from the main checkout ($(dirname "$git_common_dir"))." >&2
        exit 1
      fi

      # Resolve and create the sibling worktrees directory workmux uses, and
      # export it so the jail binds it read-write (see extraReadWriteEnvPaths).
      worktrees="$(dirname "$top")/$(basename "$top")__worktrees"
      mkdir -p "$worktrees"
      export WORKMUX_WORKTREES_DIR="$worktrees"

      # Launch Alacritty from the main checkout so the jail's mount-cwd binds
      # the repository read-write.
      cd "$top"
      exec alacritty \
        --title "workmux: $(basename "$top")" \
        --working-directory "$top" \
        -e jailed-workmux-tmux
    '';
  };
in
{
  options.myconfig.ai.workmux.jail = with lib; {
    enable = mkOption {
      type = types.bool;
      default = wmCfg.enable;
      defaultText = literalExpression "config.myconfig.ai.workmux.enable";
      description = ''
        Provide `alacritty-workmux-here` (and the underlying
        `jailed-workmux-tmux` jail): run the whole workmux/tmux session — main
        repo, worktrees, agents — inside a single bubblewrap jail on a private
        tmux socket, opened in a dedicated Alacritty window. Defaults to on
        wherever `myconfig.ai.workmux` is enabled.
      '';
    };
  };

  config = lib.mkIf (wmCfg.enable && cfg.enable) {
    home-manager.sharedModules = [
      {
        home.packages = [
          jailed-workmux-tmux
          alacritty-workmux-here
        ];
      }
    ];
  };
}
