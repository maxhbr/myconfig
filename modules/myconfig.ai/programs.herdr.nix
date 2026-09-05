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
  ...
}:
let
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
      # parent workspace").
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
  herdrConfig = ''
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
          agent-qemu-herdr
        ];
        xdg.configFile."herdr/config.toml".text = herdrConfig;
      }
    ];
  };
}
