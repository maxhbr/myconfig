# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.gvisor-agent-sandbox.workmux — the gVisor tier of the
# "one sandbox owns the whole workmux/tmux session" family:
#
#   * bubblewrap — `agent-bubblewrap-workmux-tmux` /
#     `agent-bubblewrap-alacritty-workmux-tmux` (../myconfig.ai.workmux/jail.nix)
#   * microVM    — `agent-qemu-workmux-tmux` /
#     `agent-qemu-alacritty-workmux-tmux` (../myconfig.ai.workmux/sandbox.nix)
#   * gVisor     — `agent-gvisor-workmux-tmux` /
#     `agent-gvisor-alacritty-workmux-tmux` (this file)
#
# `agent-gvisor-workmux-tmux` is a thin wrapper around `agent-gvisor workmux`
# (rust/src/workmux.rs, docs/spec.md §16): a rootless Podman + runsc container
# that gets the REAL repository (no clone) bind-mounted at its own host path
# plus the `<repo>__worktrees` sibling, and runs the whole workmux/tmux
# session inside it. workmux does its own worktree handling in there; this
# module adds no worktree logic of its own.
#
# The in-sandbox entrypoint (`/bin/workmux-gvisor-entry`) and the tools it
# needs (workmux, tmux) are baked into the sandbox image via
# `imagePackages`, which ../myconfig.ai.gvisor-agent-sandbox/default.nix
# folds into `extraImagePackages`.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.gvisor-agent-sandbox;
  wmCfg = config.myconfig.ai.workmux;
  aiCfg = config.myconfig.ai;

  yamlFormat = pkgs.formats.yaml { };

  # workmux configuration used *inside* the sandbox. As in the bubblewrap
  # jail and the microVM (see the modules referenced above), the `pi` named
  # agent must be a PLAIN agent binary: the container is already the sandbox,
  # and a nested wrapper would lose pi's configuration/credentials. Unlike
  # those two tiers the command is a bare name resolved from the image's
  # `/bin` (the host store paths are meaningless once a `--nix` session
  # replaces /nix/store with its own volume). Everything else is inherited
  # verbatim from `myconfig.ai.workmux.settings`.
  sandboxWorkmuxConfig = {
    agents = lib.optionalAttrs aiCfg.pi-coding-agent.enable {
      pi = {
        type = "pi";
        command = "pi";
      };
    };
  }
  // wmCfg.settings;
  sandboxWorkmuxConfigFile = yamlFormat.generate "workmux-gvisor-config.yaml" sandboxWorkmuxConfig;

  # In-sandbox entrypoint, dropped into the image's `/bin`. Like
  # ./nix/agent-gvisor-init.sh it is a PLAIN `#!/bin/bash` script that only
  # uses `/bin` commands and carries its configuration inline: nothing here
  # may depend on a host store path, because a `--nix` session mounts a
  # fresh volume over /nix/store.
  #
  # It mirrors the bubblewrap jail entrypoint: a PRIVATE tmux socket (server
  # and client both live in this container, so the socket never leaves the
  # sandbox), the sidebar + dashboard bootstrap, then attach.
  workmux-gvisor-entry = pkgs.writeTextFile {
    name = "workmux-gvisor-entry";
    destination = "/bin/workmux-gvisor-entry";
    executable = true;
    text = ''
      #!/bin/bash
      set -euo pipefail

      session=workmux
      socket=/tmp/workmux-tmux/socket
      mkdir -p "$(dirname "$socket")"

      # The in-sandbox workmux configuration. Written on every start (the
      # session home is seeded from the host home-manager generation, which
      # deliberately does not carry `.config/workmux`, and the host config
      # would point at host-only launcher paths anyway).
      mkdir -p "$HOME/.config/workmux"
      cat >"$HOME/.config/workmux/config.yaml" <<'WORKMUX_GVISOR_CONFIG'
      ${lib.removeSuffix "\n" (builtins.readFile sandboxWorkmuxConfigFile)}
      WORKMUX_GVISOR_CONFIG

      # Pin a real interactive shell for every pane, both via SHELL and the
      # tmux options: the image's /etc/passwd (dockerTools.fakeNss) gives the
      # container user no usable login shell.
      shell=/bin/bash
      export SHELL="$shell"

      tmux() { command tmux -S "$socket" "$@"; }

      # One single invocation: tmux defaults to `exit-empty on`, so a server
      # started without a session exits again before the next command could
      # set its options.
      if ! tmux has-session -t "=$session" 2>/dev/null; then
        tmux \
          set-option -g default-shell "$shell" \; \
          set-option -g default-command "$shell" \; \
          new-session -d -s "$session" -c "''${AGENT_WORKTREE:-$PWD}"
      fi

      # Sidebar + dashboard exactly once, tracked via a session option.
      if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
        tmux set-option -t "=$session:" @workmux_bootstrapped 1
        tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
      fi

      # `exec` bypasses the shell function above, so pass -S explicitly.
      exec tmux -S "$socket" attach-session -t "=$session"
    '';
  };

  # `agent-gvisor-workmux-tmux` — the in-terminal entry point, the gVisor
  # counterpart of `agent-bubblewrap-workmux-tmux` / `agent-qemu-workmux-tmux`.
  # The repository checks (git repo, no linked worktree), the worktrees
  # sibling and the container itself are the CLI's job (docs/spec.md §16),
  # so this is a one-liner that keeps the naming family complete and gives
  # the Alacritty variant something to run.
  agent-gvisor-workmux-tmux = pkgs.writeShellApplication {
    name = "agent-gvisor-workmux-tmux";
    runtimeInputs = [ cfg.finalPackage ];
    text = ''
      exec agent-gvisor workmux "$@"
    '';
  };

  # `agent-gvisor-alacritty-workmux-tmux` — the same sandbox in a dedicated
  # Alacritty window, mirroring `agent-bubblewrap-alacritty-workmux-tmux` and
  # `agent-qemu-alacritty-workmux-tmux`. The repository checks are repeated
  # here (with this command's name) so a wrong working directory fails in the
  # calling terminal instead of in a window that closes immediately.
  agent-gvisor-alacritty-workmux-tmux = pkgs.writeShellApplication {
    name = "agent-gvisor-alacritty-workmux-tmux";
    runtimeInputs = [
      pkgs.alacritty
      pkgs.git
      pkgs.coreutils
      agent-gvisor-workmux-tmux
    ];
    text = ''
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "agent-gvisor-alacritty-workmux-tmux: not inside a git repository." >&2
        echo "agent-gvisor-alacritty-workmux-tmux: run it from your main git checkout." >&2
        exit 1
      fi

      git_dir="$(git rev-parse --path-format=absolute --git-dir)"
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir)"
      if [ "$git_dir" != "$git_common_dir" ]; then
        echo "agent-gvisor-alacritty-workmux-tmux: refusing to run from a linked worktree." >&2
        echo "agent-gvisor-alacritty-workmux-tmux: run it from the main checkout ($(dirname "$git_common_dir"))." >&2
        exit 1
      fi
      top="$(realpath "$top")"

      cd "$top"
      exec alacritty \
        --title "agent-gvisor-workmux-tmux: $(basename "$top")" \
        --working-directory "$top" \
        -e agent-gvisor-workmux-tmux
    '';
  };
in
{
  options.myconfig.ai.gvisor-agent-sandbox.workmux = with lib; {
    enable = mkOption {
      type = types.bool;
      default = wmCfg.enable;
      defaultText = literalExpression "config.myconfig.ai.workmux.enable";
      description = ''
        Provide `agent-gvisor-workmux-tmux` (in-terminal) and
        `agent-gvisor-alacritty-workmux-tmux` (Alacritty popup): run the whole
        workmux/tmux session — the REAL main checkout, its
        `<repo>__worktrees` sibling and the agents workmux launches — inside a
        single rootless Podman + gVisor container, via `agent-gvisor workmux`.
        Nothing is cloned and no worktree is created here: workmux manages the
        worktrees itself inside the sandbox. Defaults to on wherever
        `myconfig.ai.workmux` is enabled.
      '';
    };

    imagePackages = mkOption {
      type = types.listOf types.package;
      internal = true;
      default = lib.optionals cfg.workmux.enable [
        wmCfg.package
        pkgs.tmux
        workmux-gvisor-entry
      ];
      defaultText = literalExpression "workmux, tmux and the in-sandbox entrypoint when workmux is enabled";
      description = ''
        Packages the workmux sandbox needs INSIDE the image; folded into
        `myconfig.ai.gvisor-agent-sandbox.extraImagePackages` by
        ./default.nix.
      '';
    };
  };

  config = lib.mkIf (cfg.enable && cfg.workmux.enable && wmCfg.enable) {
    home-manager.sharedModules = [
      {
        home.packages = [
          agent-gvisor-workmux-tmux
          agent-gvisor-alacritty-workmux-tmux
        ];
      }
    ];
  };
}
