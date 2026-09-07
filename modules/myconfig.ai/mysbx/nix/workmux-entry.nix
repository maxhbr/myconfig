# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-workmux-entry` — the INTERACTIVE payload of a `workmux = true`
# mysbx sandbox (../docs/design/config.md D16, ../docs/design/cli.md D11).
#
# mysbx execs this script instead of the payload shell (it is pinned into
# the wrapper as `MYSBX_WORKMUX_ENTRY`, see ./mysbx.nix). It runs INSIDE
# the sandbox and boots the workmux tmux session there:
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
writeShellApplication {
  name = "mysbx-workmux-entry";
  runtimeInputs = [
    workmux
    tmux
    coreutils
    bashInteractive
  ];
  text = ''
    # The socket directory is mysbx infrastructure, not a choice of this
    # script (../docs/design/config.md D16): it is set by the argv
    # builder, after [env], so no configuration layer can repoint it.
    if [ -z "''${TMUX_TMPDIR:-}" ]; then
      echo "mysbx-workmux-entry: TMUX_TMPDIR is not set." >&2
      echo "mysbx-workmux-entry: this script is the payload of a mysbx sandbox" >&2
      echo "mysbx-workmux-entry: with \`workmux = true\`; run \`mysbx\` instead." >&2
      exit 1
    fi
    socket_dir="$TMUX_TMPDIR"
    socket="$socket_dir/socket"
    mkdir -p "$socket_dir"
    chmod 0700 "$socket_dir"

    # The session name: the repo basename plus a short hash of its path,
    # so two checkouts with the same basename stay distinguishable. mysbx
    # `--chdir`s into the repo root, so `$PWD` IS the repo — no `git
    # rev-parse` needed (and none possible in a sandbox whose git
    # metadata a layer did not approve).
    repo_root="$PWD"
    path_hash="$(printf %s "$repo_root" | sha256sum | cut -c1-4)"
    session="workmux-$(basename "$repo_root")-$path_hash"

    # tmux resolves the pane shell from /etc/passwd, which the mysbx base
    # does not bind at all — pin a real interactive bash both via SHELL
    # (which `default-shell` falls back to) and via the tmux options.
    shell=${lib.escapeShellArg (lib.getExe bashInteractive)}
    export SHELL="$shell"

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
