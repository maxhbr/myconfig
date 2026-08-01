# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux.sandbox — the microVM counterpart of
# `myconfig.ai.workmux.jail` (`alacritty-workmux-here`).
#
# `alacritty-sandboxed-workmux-here` runs the *whole* workmux/tmux session —
# main git checkout, its `<basename>__worktrees` sibling, tmux, workmux and the
# agents it launches — inside a single microvm.nix VM (its own kernel), popped
# up in a dedicated Alacritty window. This is the same "one sandbox owns the
# whole session" model as the bubblewrap `alacritty-workmux-here`, but with the
# stronger isolation of a real VM: an ephemeral, discarded-on-exit root
# filesystem and an unprivileged `agent` user. Only the main checkout
# (/workspace) and the worktrees sibling (/workspace__worktrees) are writable;
# the host home directory, credentials and sockets are never exposed.
#
# See ../../../flake.sandboxed-pi.nix (`mkSandboxedWorkmuxRunner`) for the
# guest and the rationale for qemu + user-mode networking over
# cloud-hypervisor. The session is reached over SSH on a host-localhost
# forwarded port using a throwaway keypair generated per invocation.
{
  config,
  lib,
  pkgs,
  flake,
  ...
}:
let
  cfg = config.myconfig.ai.workmux.sandbox;
  wmCfg = config.myconfig.ai.workmux;
  aiCfg = config.myconfig.ai;

  system = pkgs.stdenv.hostPlatform.system;

  yamlFormat = pkgs.formats.yaml { };

  # Workmux config used *inside* the VM. Like the bubblewrap jail
  # (myconfig.ai.workmux/jail.nix), the `pi` named agent must be the *plain*
  # pi binary, not the bubblewrap-wrapped one — the VM is already the sandbox,
  # so a nested bwrap would lose pi's configuration/credentials. Everything
  # else is inherited verbatim from `myconfig.ai.workmux.settings`.
  sandboxWorkmuxConfig = {
    agents = lib.optionalAttrs aiCfg.pi-coding-agent.enable {
      pi = {
        type = "pi";
        command = lib.getExe pkgs.nixos-unstable.pi-coding-agent;
      };
    };
  }
  // wmCfg.settings;
  sandboxWorkmuxConfigFile = yamlFormat.generate "workmux-sandbox-config.yaml" sandboxWorkmuxConfig;

  # Host tmux configuration, exposed read-only inside the guest so the in-VM
  # tmux server picks up the same keybindings/theme. Empty string when the
  # host has no /etc/tmux.conf (the guest then uses tmux defaults).
  tmuxConf = config.environment.etc."tmux.conf".source or "";

  alacritty-sandboxed-workmux-here = pkgs.writeShellApplication {
    name = "alacritty-sandboxed-workmux-here";
    runtimeInputs = [
      pkgs.alacritty
      pkgs.git
      pkgs.coreutils
      pkgs.nix
      pkgs.openssh
    ];
    text = ''
      # Must be run from a git checkout.
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "alacritty-sandboxed-workmux-here: not inside a git repository." >&2
        echo "alacritty-sandboxed-workmux-here: run it from your main git checkout." >&2
        exit 1
      fi

      # Refuse to run from a *linked* worktree — the VM owns the main checkout
      # plus all of its worktrees.
      git_dir="$(git rev-parse --path-format=absolute --git-dir)"
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir)"
      if [ "$git_dir" != "$git_common_dir" ]; then
        echo "alacritty-sandboxed-workmux-here: refusing to run from a linked worktree." >&2
        echo "alacritty-sandboxed-workmux-here: run it from the main checkout ($(dirname "$git_common_dir"))." >&2
        exit 1
      fi

      top="$(realpath "$top")"
      # Resolve and create the sibling worktrees directory workmux uses.
      worktrees="$(dirname "$top")/$(basename "$top")__worktrees"
      mkdir -p "$worktrees"

      # Per-invocation runtime state (throwaway SSH key, console log).
      runtime_dir="$(mktemp -d "''${XDG_RUNTIME_DIR:-/tmp}/sandboxed-workmux.XXXXXX")"
      ssh_port=$(( (RANDOM % 20000) + 20000 ))

      vm_pid=""
      cleanup() {
        if [ -n "$vm_pid" ] && kill -0 "$vm_pid" 2>/dev/null; then
          kill "$vm_pid" 2>/dev/null || true
          wait "$vm_pid" 2>/dev/null || true
        fi
        rm -rf "$runtime_dir"
      }
      trap cleanup EXIT INT TERM

      ssh-keygen -q -t ed25519 -N "" -f "$runtime_dir/id" -C sandboxed-workmux

      export SANDBOXED_WORKMUX_REPO="$top"
      export SANDBOXED_WORKMUX_WORKTREES="$worktrees"
      export SANDBOXED_WORKMUX_SSH_PORT="$ssh_port"
      export SANDBOXED_WORKMUX_AUTHORIZED_KEYS="$runtime_dir/id.pub"
      export SANDBOXED_WORKMUX_CONFIG=${lib.escapeShellArg "${sandboxWorkmuxConfigFile}"}
      export SANDBOXED_WORKMUX_TMUXCONF=${lib.escapeShellArg (toString tmuxConf)}
      export SANDBOXED_WORKMUX_NETWORK=1

      echo "alacritty-sandboxed-workmux-here: building microvm runner for $top" >&2
      # `path:` flakeref (not a bare store path): a bare `/nix/store/...-source`
      # argument is re-resolved by Nix to its originating `git+file://`
      # flakeref, which libgit2 refuses because /nix/store is not owned by the
      # current user (dubious-ownership, error 7). `path:` forces the path
      # fetcher and copies the tree, bypassing the git ownership check. The
      # runner is still built impurely per invocation because it embeds the
      # per-launch main repo, worktrees dir, SSH port, keys and config via the
      # SANDBOXED_WORKMUX_* environment variables.
      runner=$(nix build --impure --no-link --print-out-paths \
        "path:${flake.outPath}#packages.${system}.sandboxed-workmux-runner")

      echo "alacritty-sandboxed-workmux-here: starting microvm (guest SSH on 127.0.0.1:$ssh_port)" >&2
      "$runner/bin/microvm-run" >"$runtime_dir/console.log" 2>&1 &
      vm_pid=$!

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
          echo "alacritty-sandboxed-workmux-here: microvm exited before SSH; console log:" >&2
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
        echo "alacritty-sandboxed-workmux-here: timed out waiting for guest SSH; console log:" >&2
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

      # Open Alacritty running an interactive SSH into the guest that boots the
      # workmux tmux session. Runs in the foreground: when the window is
      # closed, the EXIT trap tears the VM down.
      cd "$top"
      exec alacritty \
        --title "sandboxed-workmux: $(basename "$top")" \
        --working-directory "$top" \
        -e ssh -tt "''${ssh_opts[@]}" agent@127.0.0.1 -- exec workmux-sandbox-entry
    '';
  };
in
{
  options.myconfig.ai.workmux.sandbox = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Provide `alacritty-sandboxed-workmux-here`: run the whole workmux/tmux
        session — main repo, worktrees, agents — inside a single microvm.nix
        VM (its own kernel, ephemeral root, unprivileged `agent` user), opened
        in a dedicated Alacritty window. This is the microVM counterpart of
        `myconfig.ai.workmux.jail` (`alacritty-workmux-here`). Off by default;
        requires KVM (`/dev/kvm`) on the host. See ./sandbox.nix.
      '';
    };
  };

  config = lib.mkIf (wmCfg.enable && cfg.enable) {
    home-manager.sharedModules = [
      {
        home.packages = [
          alacritty-sandboxed-workmux-here
        ];
      }
    ];
  };
}
