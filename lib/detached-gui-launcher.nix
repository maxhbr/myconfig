# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shared shell prelude for wrapper scripts that spawn a *GUI* terminal
# (Alacritty) from an interactive CLI.
#
# Problem: such a wrapper `exec`s Alacritty, so it stays attached to the
# calling shell — the terminal is blocked until the GUI window is closed, and
# closing the shell kills the window. Users had to type `... &disown` manually.
#
# Solution: replace the final `exec <gui-command>` with
# `gui_launcher_exec <gui-command>`, which re-launches the command via
# `setsid --fork` in its own session, with stdin from /dev/null and
# stdout/stderr appended to a log file, and returns immediately.
#
# Behaviour / escape hatches (documented in
# modules/myconfig.ai/myconfig.ai.workmux/README.md and
# modules/myconfig.ai/docs/README.md):
#   * detach only when stdout is a TTY (i.e. called from an interactive shell);
#     non-interactive callers (scripts, .desktop files, pipes) keep the old
#     foreground/`exec` semantics
#   * `--foreground` as the first argument forces the foreground path
#   * `MYCONFIG_GUI_LAUNCHER_FOREGROUND=1` forces the foreground path
#
# Everything the wrapper does *before* `gui_launcher_exec` (validation, error
# messages, non-zero exits) keeps running in the foreground, so errors stay
# visible and the exit status is meaningful.
{
  lib,
  pkgs,
}:

{
  # `name` is only used for the log file name and user-facing messages.
  name,
}:

let
  setsid = lib.getExe' pkgs.util-linux "setsid";
  mkdir = lib.getExe' pkgs.coreutils "mkdir";
in
''
  # --- detached GUI launcher (lib/detached-gui-launcher.nix) --------
  gui_launcher_foreground=0
  if [ "''${MYCONFIG_GUI_LAUNCHER_FOREGROUND:-0}" = "1" ]; then
      gui_launcher_foreground=1
  fi
  if [ "''${1:-}" = "--foreground" ]; then
      gui_launcher_foreground=1
      shift
  fi
  # Without a TTY on stdout there is no interactive shell to detach from;
  # keep the plain foreground behaviour for scripts and desktop launchers.
  if [ ! -t 1 ]; then
      gui_launcher_foreground=1
  fi

  # Start "$@" detached from the calling shell, unless a foreground run was
  # requested. Never returns in the foreground case (it `exec`s).
  gui_launcher_exec() {
      if [ "$gui_launcher_foreground" = "1" ]; then
          exec "$@"
      fi

      local gui_launcher_log_dir gui_launcher_log
      gui_launcher_log_dir="''${XDG_STATE_HOME:-$HOME/.local/state}/myconfig-gui-launchers"
      if ! ${mkdir} -p "$gui_launcher_log_dir" 2>/dev/null; then
          gui_launcher_log_dir="''${TMPDIR:-/tmp}"
      fi
      gui_launcher_log="$gui_launcher_log_dir/${name}.log"

      ${setsid} --fork "$@" </dev/null >>"$gui_launcher_log" 2>&1
      echo "${name}: started detached (log: $gui_launcher_log)" >&2
  }
  # -------------------------------------------------------------------------
''
