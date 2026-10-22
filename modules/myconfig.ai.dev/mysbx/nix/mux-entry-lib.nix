# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Shell fragments shared by the multiplexer entry scripts of
# ../docs/design/config.md D17 (`./tmux-entry.nix`,
# `./workmux-entry.nix`, `./herdr-entry.nix`, `./aoe-entry.nix`).
#
# Each of those scripts is the INTERACTIVE payload of a mysbx sandbox
# with `multiplexer = "<its name>"`: it runs inside the sandbox and
# starts its multiplexer there. What all of them have to get right
# lives here once:
#
#   * `requireSocketDir` — the private socket directory is mysbx
#     infrastructure, not a choice of the script: the argv builder
#     exports it as `TMUX_TMPDIR` after `[env]`
#     (`bwrap.rs::MUX_SOCKET_DIR`), so no configuration layer can
#     repoint it. An unset value is a hard error rather than a
#     fallback, because tmux's own default (`/tmp/tmux-<uid>`) would be
#     a different, unguarded location — the very thing D16/D17 keep the
#     socket away from.
#   * `sessionName` — a session name derived from the repo and
#     optionally the session name (when `--session NAME` is used), so
#     the sandboxes of two repositories never look like one session in
#     a shared configuration. Window mode uses
#     `<prefix>-<repo-basename>`; session mode uses
#     `<prefix>-<repo-basename>-<session-name>`. (Their sockets are
#     already disjoint; the name keeps `tmux ls` readable.)
#   * `detachableLoop` — the reattach loop a detached run needs so a
#     multiplexer's own detach does not end the sandbox
#     (../docs/design/detach.md B7).
{ lib }:
{
  # Validates `TMUX_TMPDIR` and creates `$socket_dir`. The socket FILE
  # inside it is named by the tmux-based entries themselves
  # (`$socket_dir/socket`) — herdr keeps its own socket under the
  # tmpfs `HOME` instead, and orca keeps its state under
  # `$HOME/.config/{orca,Orca}` (see ./orca-entry.nix), so this
  # fragment stops at the directory.
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

  # Sets `repo_root` and `session`. The naming scheme depends on
  # whether a session name is provided:
  #   * window mode (no session): `${prefix}-<repo-basename>`
  #   * session mode: `${prefix}-<repo-basename>-<session-name>`
  # mysbx `--chdir`s into the repo root, so `$PWD` IS the repo — no
  # `git rev-parse` needed (and none possible in a sandbox whose git
  # metadata a layer did not approve). The session name is passed via
  # `MYSBX_SESSION_NAME` when `--session NAME` is used.
  sessionName = prefix: ''
    repo_root="$PWD"
    repo_basename="$(basename "$repo_root")"
    if [ -n "''${MYSBX_SESSION_NAME:-}" ]; then
      # session mode: `mysbx-<repo>-<session-name>`
      session="${prefix}-''$repo_basename-''${MYSBX_SESSION_NAME}"
    else
      # window mode: `mysbx-<repo>`
      session="${prefix}-''$repo_basename"
    fi
  '';

  # Sets `repo_root` and `worktrees` — the workmux-convention
  # `<repo>__worktrees` sibling — and warns when the sibling is absent.
  # The argv builder binds the sibling rw at its real host path when it
  # exists (`repo.rs::Repo::worktrees`); an interactive run creates it
  # before the argv is built (../docs/design/config.md D13), and a clone
  # run creates the session's OWN directory and binds it at this same
  # path (workspace.md D3). So `-d` is the bind test, and reaching this
  # warning means the run did not create it — a `run -- CMD`, a
  # `--dry-run` (which never reaches the payload), or a backend whose
  # sibling is not bound.
  # `$PWD` is the repo root: mysbx `--chdir`s there.
  worktreesSibling = self: ''
    repo_root="$PWD"
    worktrees="$(dirname "$repo_root")/$(basename "$repo_root")__worktrees"
    if [ ! -d "$worktrees" ]; then
        echo "${self}: no $worktrees — this run did not create it (mysbx config.md D13);" >&2
        echo "${self}: worktrees cannot land there until the sibling exists on the host" >&2
    fi
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

  # The reattach loop of a detached run (../docs/design/detach.md
  # B7): with `MYSBX_DETACHABLE=1` the entry loops instead of
  # exec'ing its client, so the multiplexer's own detach does not end
  # the sandbox. Without the variable the entry behaves exactly as
  # before — the `else` branch carries the entry's original exec line
  # verbatim (../docs/design/cli.md D8).
  #
  # Decisions:
  #   * `attach` runs in the foreground WITHOUT exec — the loop must
  #     regain control when the client detaches — and its nonzero exit
  #     ends the run with that code (cli.md D8 propagation).
  #   * `alive` is the liveness probe: probe false (the server has no
  #     session left) exits 0 — the run is over, not failed.
  #   * The prompt goes to stderr, the entries' diagnostic channel;
  #     dtach forwards it. `^\` is dtach's client-side key, so the
  #     prompt is informational only — no trap.
  #   * The wait POLLS: `read -t 1` times out after 1s, and each
  #     timeout re-runs `alive` (no `sleep`, no `seq` — `read -t` is
  #     the timeout primitive), so a last-pane exit while the prompt
  #     is shown ends the run within ~1s instead of wedging the
  #     registry until someone types (B7 liveness).
  #   * EOF on `read` (the dtach master died) exits 0, like `q`. The
  #     code is captured with `||` — under `if ! read …; then rc=$?`
  #     the `$?` would be the NEGATION's status (always 0) — and a
  #     timed-out `read` (>128) is told apart from EOF (1) by it.
  #   * Any non-`q` answer reattaches.
  #   * No SIGWINCH trap: a trapped WINCH interrupts `read` and would
  #     conflate with EOF (ending the sandbox on mysbx attach).
  detachableLoop =
    {
      attach,
      alive,
      execLine,
    }:
    ''
      if [ "''${MYSBX_DETACHABLE:-}" = "1" ]; then
          while :; do
              ${attach}
              if ! ${alive}; then
                  exit 0
              fi
              printf '%s\n' 'session still running — Enter: reattach, q: end the sandbox, ^\: detach mysbx' >&2
              while :; do
                  rc=0
                  read -r -t 1 mux_reply || rc=$?
                  if [ "$rc" -gt 128 ]; then
                      if ! ${alive}; then
                          exit 0
                      fi
                      continue
                  fi
                  if [ "$rc" -ne 0 ]; then
                      exit 0
                  fi
                  break
              done
              if [ "$mux_reply" = "q" ]; then
                  exit 0
              fi
          done
      else
          ${execLine}
      fi
    '';
}
