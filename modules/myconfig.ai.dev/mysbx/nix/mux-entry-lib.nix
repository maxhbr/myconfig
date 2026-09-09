# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shell fragments shared by the multiplexer entry scripts of
# ../docs/design/config.md D17 (`./tmux-entry.nix`,
# `./workmux-entry.nix`, `./herdr-entry.nix`, `./aoe-entry.nix`).
#
# Each of those scripts is the INTERACTIVE payload of a mysbx sandbox
# with `multiplexer = "<its name>"`: it runs inside the sandbox and
# starts its multiplexer there. The two things all of them have to get
# right are the same, so they live here once:
#
#   * `requireSocketDir` — the private socket directory is mysbx
#     infrastructure, not a choice of the script: the argv builder
#     exports it as `TMUX_TMPDIR` after `[env]`
#     (`bwrap.rs::MUX_SOCKET_DIR`), so no configuration layer can
#     repoint it. An unset value is a hard error rather than a
#     fallback, because tmux's own default (`/tmp/tmux-<uid>`) would be
#     a different, unguarded location — the very thing D16/D17 keep the
#     socket away from.
#   * `sessionName` — a session name derived from the repo, so the
#     sandboxes of two repositories never look like one session in a
#     shared configuration. (Their sockets are already disjoint; the
#     name keeps `tmux ls` readable.)
{ lib }:
{
  # Validates `TMUX_TMPDIR` and creates `$socket_dir`. The socket FILE
  # inside it is named by the tmux-based entries themselves
  # (`$socket_dir/socket`) — herdr keeps its own socket under the
  # tmpfs `HOME` instead, so this fragment stops at the directory.
  # `self` is the script's own name, for the diagnostics.
  requireSocketDir = self: ''
    # The socket directory is mysbx infrastructure, not a choice of this
    # script (../docs/design/config.md D17): it is set by the argv
    # builder, after [env], so no configuration layer can repoint it.
    if [ -z "''${TMUX_TMPDIR:-}" ]; then
      echo "${self}: TMUX_TMPDIR is not set." >&2
      echo "${self}: this script is the payload of a mysbx sandbox" >&2
      echo "${self}: with \`multiplexer = \"…\"\`; run \`mysbx\` instead." >&2
      exit 1
    fi
    socket_dir="$TMUX_TMPDIR"
    mkdir -p "$socket_dir"
    chmod 0700 "$socket_dir"
  '';

  # Sets `repo_root` and `session` ("<prefix>-<repo>-<hash>"). mysbx
  # `--chdir`s into the repo root, so `$PWD` IS the repo — no `git
  # rev-parse` needed (and none possible in a sandbox whose git
  # metadata a layer did not approve). The short hash keeps two
  # checkouts with the same basename distinguishable.
  sessionName = prefix: ''
    repo_root="$PWD"
    path_hash="$(printf %s "$repo_root" | sha256sum | cut -c1-4)"
    session="${prefix}-$(basename "$repo_root")-$path_hash"
  '';

  # tmux resolves the pane shell from /etc/passwd, which the mysbx base
  # does not bind at all — every tmux-based multiplexer therefore needs
  # a real interactive shell pinned via `SHELL` (which tmux's
  # `default-shell` falls back to). Without it every pane dies
  # immediately with "no server running". Same pin as
  # ../../myconfig.ai.workmux/jail.nix.
  pinShell = shellExe: ''
    shell=${lib.escapeShellArg shellExe}
    export SHELL="$shell"
  '';
}
