# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-aoe-entry` — the INTERACTIVE payload of a
# `multiplexer = "aoe"` mysbx sandbox (../docs/design/config.md D17,
# ../docs/design/cli.md D11).
#
# Agent of Empires (`aoe`) is a tmux-based terminal session manager for
# AI coding agents; the host installation lives in
# ../../programs/programs.agent-of-empires/. Because it drives tmux, the
# `TMUX_TMPDIR` the argv builder exports is what keeps its server on
# the private, in-sandbox socket (D16/D17) — the same mechanism as for
# plain tmux and workmux, and the reason `aoe` needed no per-tool
# isolation work of its own.
#
# `aoe` creates its own state lazily on first run, under the sandbox
# `HOME` (`/mysbx-home`, a tmpfs — D14): per sandbox and per run, unless
# a repository declares a `state-dirs` entry for it (D15).
#
# Worktree placement follows the workmux convention
# (`<repo>__worktrees/<branch>`): `[worktree] path_template` is set to
# an ABSOLUTE path computed from the repo root at start time, written
# into a marked block of `aoe`'s global `config.toml`; a user-owned
# `[worktree]` table (persisted via `state-dirs`) is left alone.
{
  lib,
  writeShellApplication,
  coreutils,
  gnused,
  gnugrep,
  bashInteractive,
  # The interactive payload shell of the sandbox (../default.nix
  # `shell.path`, bd myconfig-l44): the HOST's shell, so a pane
  # lands in the same shell as the host and the payload. `null`
  # keeps this entry's own bash.
  payloadShell ? null,
  tmux,
  # The agent-of-empires package that runs inside the sandbox
  # (`myconfig.ai.dev.mysbx.aoe.package`, normally
  # `myconfig.ai.dev.agent-of-empires.package`).
  aoe,
}:
let
  muxLib = import ./mux-entry-lib.nix { inherit lib; };
in
writeShellApplication {
  name = "mysbx-aoe-entry";
  runtimeInputs = [
    aoe
    tmux
    coreutils
    gnused
    gnugrep
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-aoe-entry"}
    ${muxLib.pinShell (if payloadShell != null then payloadShell else lib.getExe bashInteractive)}

    ${muxLib.worktreesSibling "mysbx-aoe-entry"}

    config_base="''${XDG_CONFIG_HOME:-}"
    case $config_base in
        /*) ;;
        *) config_base="$HOME/.config" ;;
    esac
    aoe_config="$config_base/agent-of-empires/config.toml"
    begin_marker="# BEGIN mysbx-aoe-entry"
    end_marker="# END mysbx-aoe-entry"
    mkdir -p "$(dirname "$aoe_config")"
    touch "$aoe_config"
    stripped="$(sed "/^$begin_marker\$/,/^$end_marker\$/d" "$aoe_config")"
    case $worktrees in
        *[[:cntrl:]]*)
            echo "mysbx-aoe-entry: repository path contains control characters:" >&2
            echo "mysbx-aoe-entry: $worktrees — aoe keeps its worktree default" >&2
            printf '%s\n' "$stripped" >"$aoe_config"
            ;;
        *)
            if printf '%s\n' "$stripped" | grep -q '^[[:space:]]*\[worktree\]'; then
                echo "mysbx-aoe-entry: $aoe_config has its own [worktree] table; not overriding it" >&2
                printf '%s\n' "$stripped" >"$aoe_config"
            else
                template="$worktrees/{branch}"
                template="''${template//\\/\\\\}"
                template="''${template//\"/\\\"}"
                {
                    [ -n "$stripped" ] && printf '%s\n' "$stripped"
                    printf '%s\n' "$begin_marker"
                    printf '[worktree]\npath_template = "%s"\n' "$template"
                    printf '%s\n' "$end_marker"
                } >"$aoe_config"
            fi
            ;;
    esac

    # In a foreground run the `exec` form applies: `aoe` owns the
    # terminal and its exit code propagates unchanged
    # (../docs/design/cli.md D8). In a detached run
    # (`MYSBX_DETACHABLE=1`, detach.md B7) the gate loops instead, so
    # quitting the aoe TUI does not end the sandbox; aoe is re-run to
    # reattach. Liveness is `tmux ls` on plain tmux — aoe's server
    # lives on the `TMUX_TMPDIR` default socket. That a re-run of aoe
    # reattaches to its server is the bead's manual acceptance
    # (bd myconfig-7ioq.6).
    ${muxLib.detachableLoop {
      attach = ''aoe "$@"'';
      alive = "tmux ls >/dev/null 2>&1";
      execLine = ''exec aoe "$@"'';
    }}
  '';

  meta = {
    description = "Interactive payload of an agent-of-empires mysbx sandbox";
    mainProgram = "mysbx-aoe-entry";
    platforms = lib.platforms.linux;
  };
}
