# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# myconfig.ai.workmux.sandbox — the microVM counterpart of
# `myconfig.ai.workmux.jail` (`agent-bubblewrap-alacritty-workmux-tmux` / `agent-bubblewrap-workmux-tmux`).
#
# `agent-qemu-workmux-tmux` runs the *whole* workmux/tmux session — main git
# checkout, its `<basename>__worktrees` sibling, tmux, workmux and the agents
# it launches — inside a single microvm.nix VM (its own kernel), attaching in
# the current terminal. `agent-qemu-alacritty-workmux-tmux` opens the same
# sandbox in a dedicated Alacritty window. This is the same "one sandbox owns
# the whole session" model as the bubblewrap `agent-bubblewrap-alacritty-workmux-tmux` /
# `agent-bubblewrap-workmux-tmux`, but with the stronger isolation of a real VM: an
# ephemeral, discarded-on-exit root filesystem and an unprivileged `agent`
# user. Only the main checkout (/workspace) and the worktrees sibling
# (/workspace__worktrees) are writable; the host home directory, credentials
# and sockets are never exposed.
#
# `agent-qemu-workmux-tmux` is to `agent-qemu-alacritty-workmux-tmux` what
# `agent-bubblewrap-workmux-tmux` is to `agent-bubblewrap-alacritty-workmux-tmux`: the in-terminal sandbox
# is the reusable entry point, and the Alacritty variant is a thin popup
# around it. Both reuse the same `mkSandboxedWorkmuxRunner` guest/runner (see
# ../myconfig.ai.qemu-agent-sandbox/builders.nix).
#
# See ../myconfig.ai.qemu-agent-sandbox/builders.nix (`mkSandboxedWorkmuxRunner`) for the
# guest and the rationale for qemu + user-mode networking over
# cloud-hypervisor. The session is reached over SSH on a host-localhost
# forwarded port using a throwaway keypair generated per invocation.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.workmux.sandbox;
  wmCfg = config.myconfig.ai.workmux;
  aiCfg = config.myconfig.ai;

  yamlFormat = pkgs.formats.yaml { };

  # Shell prelude that makes the GUI launcher detach from the calling shell.
  detachedGuiLauncher = import ../../../lib/detached-gui-launcher.nix { inherit lib pkgs; };

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

  # Shared sandbox tools (myconfig.ai.sandboxTools) as a JSON array of store
  # paths, baked into the `agent-qemu-alacritty-workmux-tmux` wrapper and read
  # (via SANDBOXED_WORKMUX_EXTRA_PACKAGES) by the impure runner expression that
  # builds the per-invocation VM runner. Same pattern as
  # AGENT_QEMU_HERDR_AGENT_PACKAGES in ../programs.herdr.nix.
  sandboxToolsJson = builtins.toJSON (
    map (p: p.outPath) config.myconfig.ai.sandboxTools.extraPackages
  );

  # Host tmux configuration, exposed read-only inside the guest so the in-VM
  # tmux server picks up the same keybindings/theme. Empty string when the
  # host has no /etc/tmux.conf (the guest then uses tmux defaults).
  tmuxConf = config.environment.etc."tmux.conf".source or "";

  # `agent-qemu-workmux-tmux` — the in-terminal microVM counterpart of
  # `agent-bubblewrap-workmux-tmux`. Run it from the main git checkout: it resolves the
  # `<basename>__worktrees` sibling, builds the per-invocation microVM runner
  # (the same `mkSandboxedWorkmuxRunner` the Alacritty variant uses), boots the
  # VM, waits for guest SSH, forwards LLM credentials over the SSH environment
  # and execs the in-guest `workmux-sandbox-entry` (which boots the workmux tmux
  # session on a private socket and attaches) *in the current terminal*. On
  # exit the VM is torn down and the runtime dir removed.
  #
  # This is to `agent-qemu-alacritty-workmux-tmux` what `agent-bubblewrap-workmux-tmux` is
  # to `agent-bubblewrap-alacritty-workmux-tmux`: the same sandbox without the Alacritty window,
  # so it can be run from an existing terminal / tmux pane / SSH session.
  agent-qemu-workmux-tmux = pkgs.writeShellApplication {
    name = "agent-qemu-workmux-tmux";
    runtimeInputs = [
      pkgs.git
      pkgs.coreutils
      pkgs.nix
      pkgs.openssh
    ];
    text = ''
      # Must be run from a git checkout.
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "agent-qemu-workmux-tmux: not inside a git repository." >&2
        echo "agent-qemu-workmux-tmux: run it from your main git checkout." >&2
        exit 1
      fi

      # Refuse to run from a *linked* worktree — the VM owns the main checkout
      # plus all of its worktrees.
      git_dir="$(git rev-parse --path-format=absolute --git-dir)"
      git_common_dir="$(git rev-parse --path-format=absolute --git-common-dir)"
      if [ "$git_dir" != "$git_common_dir" ]; then
        echo "agent-qemu-workmux-tmux: refusing to run from a linked worktree." >&2
        echo "agent-qemu-workmux-tmux: run it from the main checkout ($(dirname "$git_common_dir"))." >&2
        exit 1
      fi

      top="$(realpath "$top")"
      # Resolve and create the sibling worktrees directory workmux uses.
      worktrees="$(dirname "$top")/$(basename "$top")__worktrees"
      mkdir -p "$worktrees"

      # Per-invocation runtime state (throwaway SSH key, console log).
      runtime_dir="$(mktemp -d "''${XDG_RUNTIME_DIR:-/tmp}/agent-qemu-workmux-tmux.XXXXXX")"
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

      ssh-keygen -q -t ed25519 -N "" -f "$runtime_dir/id" -C agent-qemu-workmux-tmux

      export QEMU_AGENT_SANDBOX_KIND=workmux
      export SANDBOXED_WORKMUX_REPO="$top"
      export SANDBOXED_WORKMUX_WORKTREES="$worktrees"
      export SANDBOXED_WORKMUX_SSH_PORT="$ssh_port"
      export SANDBOXED_WORKMUX_AUTHORIZED_KEYS="$runtime_dir/id.pub"
      export SANDBOXED_WORKMUX_CONFIG=${lib.escapeShellArg "${sandboxWorkmuxConfigFile}"}
      export SANDBOXED_WORKMUX_TMUXCONF=${lib.escapeShellArg (toString tmuxConf)}
      export SANDBOXED_WORKMUX_NETWORK=1
      # Shared sandbox tools (myconfig.ai.sandboxTools), baked in at build
      # time as a JSON array of store paths; read by the impure
      # standalone qemu-agent-sandbox runner expression.
      export SANDBOXED_WORKMUX_EXTRA_PACKAGES='${sandboxToolsJson}'

      echo "agent-qemu-workmux-tmux: building microvm runner for $top" >&2
      # Evaluate the module-owned expression directly. Impure evaluation is
      # required for the transient repository, worktrees, port, key, and
      # generated configuration paths.
      runner=$(nix build --impure --no-link --print-out-paths \
        --file ${config.myconfig.ai.qemu-agent-sandbox.runnerExpression})

      # Start the rootless virtiofsd daemon(s) + qemu via the runner's
      # combined `sandboxed-launch` entry point, from $runtime_dir so the
      # RELATIVE virtiofs socket paths resolve to a stable per-invocation
      # directory. (Plain `microvm-run` would exit immediately because
      # nothing would have created the virtiofs share sockets.)
      cd "$runtime_dir"
      echo "agent-qemu-workmux-tmux: starting microvm (guest SSH on 127.0.0.1:$ssh_port)" >&2
      "$runner/bin/sandboxed-launch" >"$runtime_dir/console.log" 2>&1 &
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
          echo "agent-qemu-workmux-tmux: microvm exited before SSH; console log:" >&2
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
        echo "agent-qemu-workmux-tmux: timed out waiting for guest SSH; console log:" >&2
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

      # Interactive SSH into the guest that boots the workmux tmux session via
      # the in-guest `workmux-sandbox-entry`. Runs in the foreground (current
      # terminal): when the SSH session exits, the EXIT trap tears the VM down.
      exec ssh -tt "''${ssh_opts[@]}" agent@127.0.0.1 -- exec workmux-sandbox-entry
    '';
  };

  # `agent-qemu-alacritty-workmux-tmux` — open `agent-qemu-workmux-tmux` in a dedicated
  # Alacritty window. This mirrors `agent-bubblewrap-alacritty-workmux-tmux` (which opens
  # `agent-bubblewrap-workmux-tmux` in Alacritty): the in-terminal sandbox is the reusable
  # entry point, and the Alacritty variant is a thin popup around it. Running
  # `agent-qemu-workmux-tmux` inside Alacritty (rather than duplicating the VM boot
  # here) keeps the two wrappers byte-identical in everything but the window.
  agent-qemu-alacritty-workmux-tmux = pkgs.writeShellApplication {
    name = "agent-qemu-alacritty-workmux-tmux";
    runtimeInputs = [
      pkgs.alacritty
      pkgs.git
      pkgs.coreutils
    ];
    text = ''
      ${detachedGuiLauncher { name = "agent-qemu-alacritty-workmux-tmux"; }}

      # Must be run from a git checkout (forward the rest to agent-qemu-workmux-tmux
      # so the error messages name the right command).
      if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        echo "agent-qemu-alacritty-workmux-tmux: not inside a git repository." >&2
        echo "agent-qemu-alacritty-workmux-tmux: run it from your main git checkout." >&2
        exit 1
      fi
      top="$(realpath "$top")"

      # Open Alacritty running the in-terminal sandbox wrapper. Alacritty
      # itself stays as the primary user so it can reach the display; the VM
      # boot and SSH attach happen inside the new window. When the window is
      # closed, the VM is torn down (agent-qemu-workmux-tmux's EXIT trap).
      cd "$top"
      gui_launcher_exec alacritty \
        --title "agent-qemu-workmux-tmux: $(basename "$top")" \
        --working-directory "$top" \
        -e agent-qemu-workmux-tmux
    '';
  };
in
{
  options.myconfig.ai.workmux.sandbox = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Provide `agent-qemu-workmux-tmux` (in-terminal) and
        `agent-qemu-alacritty-workmux-tmux` (Alacritty popup): run the whole
        workmux/tmux session — main repo, worktrees, agents — inside a single
        microvm.nix VM (its own kernel, ephemeral root, unprivileged `agent`
        user). This is the microVM counterpart of `myconfig.ai.workmux.jail`
        (`agent-bubblewrap-alacritty-workmux-tmux` / `agent-bubblewrap-workmux-tmux`): `agent-qemu-workmux-tmux`
        is the in-terminal entry point (like `agent-bubblewrap-workmux-tmux`) and
        `agent-qemu-alacritty-workmux-tmux` opens it in a dedicated Alacritty
        window (like `agent-bubblewrap-alacritty-workmux-tmux`). Off by default; requires KVM
        (`/dev/kvm`) on the host. See ./sandbox.nix.
      '';
    };
  };

  config = lib.mkIf (wmCfg.enable && cfg.enable) {
    home-manager.sharedModules = [
      {
        home.packages = [
          agent-qemu-workmux-tmux
          agent-qemu-alacritty-workmux-tmux
        ];
      }
    ];
  };
}
