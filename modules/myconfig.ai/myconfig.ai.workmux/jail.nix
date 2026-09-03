# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux.jail — run the *whole* workmux/tmux session inside a
# single bubblewrap jail and pop it up in a dedicated Alacritty window.
#
# Unlike the per-agent jails (`agent-bubblewrap-pi`, ...) which sandbox one agent binary
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
#   * `agent-bubblewrap-workmux-tmux`      — the jail. Its entrypoint boots a `workmux`
#                                  tmux session on the private socket, wires up
#                                  the sidebar + dashboard, and attaches.
#   * `agent-bubblewrap-alacritty-workmux-tmux`   — user-facing launcher. Run it from the main
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
  jail-app = callJailLib ../fns/jail-app.nix;

  # Combinators used directly below (for `extraPermissions`).
  inherit (jail.init pkgs) combinators;

  yamlFormat = pkgs.formats.yaml { };

  # tmux socket *inside* the jail. With `persistentTmp` (jail-app default) the
  # jail's /tmp is a host-backed directory private to this jail, so the socket
  # never collides with the user's normal tmux server on the host default
  # socket. Both the server and the attaching client run in the same bwrap
  # process tree, so they resolve the same socket inode.
  socketPath = "/tmp/workmux-tmux/socket";

  # Plain (un-jailed) agent binaries made available *inside* the shared jail so
  # workmux can launch them in panes. The jail itself is the sandbox here, so
  # these are the raw agent binaries, not the nested `agent-bubblewrap-*` wrappers.
  innerAgents = lib.optional aiCfg.pi-coding-agent.enable pkgs.nixos-unstable.pi-coding-agent;

  # workmux config used *inside* the jail. It deliberately does NOT reuse the
  # host's `~/.config/workmux/config.yaml`: there the `pi` named agent points
  # at `pi-workmux-launch`, which execs the bubblewrap-sandboxed `pi-bwrap`.
  # Running that inside this jail would start a *nested* sandbox (fresh tmpfs
  # $HOME, its own env) and lose pi's real configuration/credentials — exactly
  # the "No models available / No API key" breakage. The jail is already the
  # sandbox, so here the `pi` agent must be the *plain* pi binary. Everything
  # else (nerdfont, the `<agent>` pane layout, the default `agent`) is inherited
  # verbatim from `myconfig.ai.workmux.settings`.
  jailWorkmuxConfig = {
    agents = lib.optionalAttrs aiCfg.pi-coding-agent.enable {
      pi = {
        type = "pi";
        command = lib.getExe pkgs.nixos-unstable.pi-coding-agent;
      };
    };
  }
  // wmCfg.settings;
  jailWorkmuxConfigFile = yamlFormat.generate "workmux-jail-config.yaml" jailWorkmuxConfig;

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

      # Create the session detached if it does not exist yet. This *must* be a
      # single tmux invocation: tmux defaults to `exit-empty on`, so a server
      # started with no sessions exits again immediately. Splitting the
      # bootstrap across separate `tmux` processes (start-server; set-option;
      # new-session) makes the server vanish between calls, so `set-option`
      # fails with "no server running" and the default-shell is never pinned.
      # Setting both default-shell and default-command *before* new-session in
      # one command sequence guarantees the very first pane (and every later
      # split created by `workmux sidebar`) execs bash and never `nologin`.
      if ! tmux has-session -t "=$session" 2>/dev/null; then
        tmux \
          set-option -g default-shell "$shell" \; \
          set-option -g default-command "$shell" \; \
          new-session -d -s "$session"
      fi

      # Bootstrap the sidebar + dashboard exactly once (tracked via a session
      # option, so a half-created session still gets its dashboard next run).
      # The trailing colon in `=$session:` forces session resolution for
      # send-keys and targets its active pane.
      if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
        tmux set-option -t "=$session:" @workmux_bootstrapped 1
        tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
      fi

      # NOTE: `exec` never runs shell functions, so it bypasses the `tmux()`
      # wrapper above (which would otherwise inject `-S "$socket"`) and would
      # attach to tmux's *default* socket. Pass `-S "$socket"` explicitly to
      # the real binary here.
      exec tmux -S "$socket" attach-session -t "=$session"
    '';
  };

  # The jail. `mount-cwd` (jail-app default) binds the main repo read-write;
  # `WORKMUX_WORKTREES_DIR` binds the sibling worktrees dir read-write so
  # `workmux add` can create linked worktrees next to the main checkout.
  agent-bubblewrap-workmux-tmux = jail-app {
    name = "agent-bubblewrap-workmux-tmux";
    pkg = entry;
    # rw state: pi session/config state. NOTE: `.config/workmux` is
    # deliberately *not* rw-bound here — the jail must not see the host's
    # `pi`->`pi-bwrap` agent config (see `jailWorkmuxConfig`). Instead a
    # jail-specific config is bound read-only via `extraPermissions` below;
    # workmux's mutable runtime state lives under `~/.local/state/workmux`,
    # which the jail's writable $HOME provides per-invocation.
    userDataDirs = [
      ".pi"
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
    # tmux reads its global configuration from `/etc/tmux.conf` (written by the
    # host's NixOS `programs.tmux`). The base jail only binds a couple of
    # specific `/etc` files, so without this the in-jail tmux server would
    # start with no keybindings/theme/plugins. `try-ro-bind` skips silently if
    # the host has no `/etc/tmux.conf`.
    #
    # The jail-specific workmux config (plain `pi` agent, see above) is bound
    # read-only over `~/.config/workmux/config.yaml`. bwrap creates the parent
    # directory automatically. This shadows the host config that would
    # otherwise map `pi` to the nested `pi-bwrap` sandbox.
    extraPermissions = [
      (combinators.try-ro-bind "/etc/tmux.conf" "/etc/tmux.conf")
      (combinators.ro-bind "${jailWorkmuxConfigFile}" (
        combinators.noescape "~/.config/workmux/config.yaml"
      ))
    ];
  };

  agent-bubblewrap-alacritty-workmux-tmux = pkgs.writeShellApplication {
    name = "agent-bubblewrap-alacritty-workmux-tmux";
    runtimeInputs = [
      pkgs.alacritty
      pkgs.git
      pkgs.coreutils
      agent-bubblewrap-workmux-tmux
    ];
    text = ''
      # Must be run from a git checkout.
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "agent-bubblewrap-alacritty-workmux-tmux: not inside a git repository." >&2
        echo "agent-bubblewrap-alacritty-workmux-tmux: run it from your main git checkout." >&2
        exit 1
      fi

      # Refuse to run from a *linked* worktree — the jail is meant to own the
      # main checkout plus all of its worktrees. In a linked worktree the
      # per-worktree git dir differs from the shared common dir.
      git_dir="$(git rev-parse --path-format=absolute --git-dir)"
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir)"
      if [ "$git_dir" != "$git_common_dir" ]; then
        echo "agent-bubblewrap-alacritty-workmux-tmux: refusing to run from a linked worktree." >&2
        echo "agent-bubblewrap-alacritty-workmux-tmux: run it from the main checkout ($(dirname "$git_common_dir"))." >&2
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
        -e agent-bubblewrap-workmux-tmux
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
        Provide `agent-bubblewrap-alacritty-workmux-tmux` (and the underlying
        `agent-bubblewrap-workmux-tmux` jail): run the whole workmux/tmux session — main
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
          agent-bubblewrap-workmux-tmux
          agent-bubblewrap-alacritty-workmux-tmux
        ];
      }
    ];
  };
}
