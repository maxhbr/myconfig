# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-tmux-entry` — the INTERACTIVE payload of a
# `multiplexer = "tmux"` mysbx sandbox (../docs/design/config.md D17,
# ../docs/design/cli.md D11).
#
# The plain-tmux sibling of ./workmux-entry.nix: same session, same
# private socket, same pinned pane shell — without the workmux sidebar
# and dashboard bootstrap. It is what a repository selects when it
# wants panes and windows but not the agent-worktree workflow.
#
# mysbx execs this script instead of the payload shell (it is pinned
# into the wrapper as `MYSBX_MUX_ENTRY_TMUX`, see ./mysbx.nix).
# Everything it needs is in its own closure — it does not rely on the
# sandbox `PATH` (`MYSBX_TOOLS_PATH`), which a host may narrow.
{
  lib,
  writeShellApplication,
  coreutils,
  bashInteractive,
  tmux,
}:
let
  muxLib = import ./mux-entry-lib.nix { inherit lib; };
in
writeShellApplication {
  name = "mysbx-tmux-entry";
  runtimeInputs = [
    tmux
    coreutils
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-tmux-entry"}
    ${muxLib.sessionName "mysbx"}
    ${muxLib.pinShell (lib.getExe bashInteractive)}

    socket="$socket_dir/socket"

    # Pin the private socket for every tmux call in this script.
    tmux() { command tmux -S "$socket" "$@"; }

    # Create the session detached if it does not exist yet. This MUST be
    # a single tmux invocation: tmux defaults to `exit-empty on`, so a
    # server started with no sessions exits again immediately, and a
    # `set-option` in a second process would fail with "no server
    # running" — leaving the default-shell unpinned and every pane
    # dead. Same reasoning (and the same fix) as ./workmux-entry.nix.
    if ! tmux has-session -t "=$session" 2>/dev/null; then
      tmux \
        set-option -g default-shell "$shell" \; \
        set-option -g default-command "$shell" \; \
        new-session -d -s "$session"
    fi

    # NOTE: `exec` bypasses shell functions, so the `tmux()` wrapper does
    # not apply here and `-S "$socket"` must be repeated — otherwise this
    # would attach to tmux's DEFAULT socket. A fresh sandbox is never
    # already inside tmux, so this always attaches, never switches.
    exec tmux -S "$socket" attach-session -t "=$session"
  '';

  meta = {
    description = "Interactive payload of a tmux-multiplexer mysbx sandbox";
    mainProgram = "mysbx-tmux-entry";
    platforms = lib.platforms.linux;
  };
}
