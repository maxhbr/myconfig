# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-workmux-entry` — the INTERACTIVE payload of a
# `multiplexer = "workmux"` mysbx sandbox (../docs/design/config.md D16,
# generalized by D17, ../docs/design/cli.md D11).
#
# mysbx execs this script instead of the payload shell (it is pinned into
# the wrapper as `MYSBX_MUX_ENTRY_WORKMUX`, see ./mysbx.nix). It runs
# INSIDE the sandbox and boots the workmux tmux session there:
#
#   * on the socket `$TMUX_TMPDIR/socket`, where `TMUX_TMPDIR` is set by
#     the argv builder to `/mysbx-home/.mysbx-tmux` — a path in the
#     sandbox home tmpfs, so the tmux server is reachable from this one
#     sandbox and from nowhere else (D16). The script never picks the
#     path itself: an unset `TMUX_TMPDIR` is a hard error, because
#     tmux's own default (`/tmp/tmux-<uid>`) would silently be a
#     different, run-local-but-unguarded location.
#   * with a real interactive bash pinned as the pane shell: the sandbox
#     has no `/etc/passwd`, so tmux would fall back to a login shell it
#     cannot resolve and every pane would die immediately ("no server
#     running"). Same pin, and the same one-invocation bootstrap, as
#     ../../myconfig.ai.workmux/jail.nix.
#   * with a session name derived from the repo, so the mysbx sandboxes
#     of two repositories never look like one session in a shared
#     configuration. (Their sockets are already disjoint; the name keeps
#     `tmux ls` readable.)
#
# Everything this script needs is in its own closure — it does not rely
# on the sandbox `PATH` (`MYSBX_TOOLS_PATH`), which a host may narrow.
{
  lib,
  writeShellApplication,
  coreutils,
  bashInteractive,
  tmux,
  # The workmux package that runs inside the sandbox
  # (`myconfig.ai.mysbx.workmux.package`, normally
  # `myconfig.ai.workmux.package`).
  workmux,
}:
let
  # The socket validation, the session name and the pane-shell pin are
  # the same for every multiplexer entry (./mux-entry-lib.nix); only
  # the bootstrap below is workmux's own.
  muxLib = import ./mux-entry-lib.nix { inherit lib; };
in
writeShellApplication {
  name = "mysbx-workmux-entry";
  runtimeInputs = [
    workmux
    tmux
    coreutils
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-workmux-entry"}
    ${muxLib.sessionName "workmux"}
    ${muxLib.pinShell (lib.getExe bashInteractive)}

    socket="$socket_dir/socket"

    # Pin the private socket for every tmux call in this script.
    tmux() { command tmux -S "$socket" "$@"; }

    # Create the session detached if it does not exist yet. This MUST be
    # a single tmux invocation: tmux defaults to `exit-empty on`, so a
    # server started with no sessions exits again immediately, and a
    # `set-option` in a second process would fail with "no server
    # running" — leaving the default-shell unpinned and every pane
    # dead. Same reasoning (and the same fix) as
    # ../../myconfig.ai.workmux/jail.nix.
    if ! tmux has-session -t "=$session" 2>/dev/null; then
      tmux \
        set-option -g default-shell "$shell" \; \
        set-option -g default-command "$shell" \; \
        new-session -d -s "$session"
    fi

    # Bootstrap the sidebar + dashboard exactly once, tracked in a
    # session option so a half-created session still gets its dashboard
    # on the next attach. The trailing colon in `=$session:` forces
    # session resolution for send-keys and targets its active pane.
    if [ "$(tmux show-options -t "=$session:" -qv @workmux_bootstrapped)" != 1 ]; then
      tmux set-option -t "=$session:" @workmux_bootstrapped 1
      tmux send-keys -t "=$session:" 'workmux sidebar --session; workmux dashboard' Enter
    fi

    # NOTE: `exec` bypasses shell functions, so the `tmux()` wrapper does
    # not apply here and `-S "$socket"` must be repeated — otherwise this
    # would attach to tmux's DEFAULT socket. A fresh sandbox is never
    # already inside tmux, so this always attaches, never switches.
    exec tmux -S "$socket" attach-session -t "=$session"
  '';

  meta = {
    description = "Interactive payload of a workmux-enabled mysbx sandbox";
    mainProgram = "mysbx-workmux-entry";
    platforms = lib.platforms.linux;
  };
}
