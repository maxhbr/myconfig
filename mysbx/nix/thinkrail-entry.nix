# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-thinkrail-entry` — the INTERACTIVE payload of a
# `multiplexer = "thinkrail"` mysbx sandbox (../docs/design/config.md D17,
# ../docs/design/cli.md D11).
#
# ThinkRail (https://github.com/JetBrains/thinkrail, packaged in
# ../../modules/myconfig.ai.dev/programs/programs.thinkrail.nix) is, like
# orca, not a terminal multiplexer: it is an agentic IDE around the pi
# coding agent whose interactive surface is a browser UI. But unlike the
# orca entry (./orca-entry.nix) nothing here is a desktop app at all:
# the `thinkrail` CLI boots the engine host — an HTTP+WS server —
# in-process and serves the UI from it. The interactive payload of the
# sandbox is therefore the engine host itself, and the operator reaches
# it with the URL it prints on readiness (`thinkrail →
# http://127.0.0.1:<port>`). The launcher requires a foreground run
# on bubblewrap with a shared network namespace (config.md D17).
#
# The display decision: there is none. No Electron, no Xvfb, no
# `$DISPLAY` — the engine host is a plain Bun HTTP server, and the
# documented remote form `--no-open` (upstream README: "opens the app in
# a browser"; `--no-open` is the headless/remote variant) skips the
# `xdg-open` call, which would be a no-op inside the sandbox anyway
# (mysbx attaches no host display and no browser exists in the payload).
#
# Set THINKRAIL_PORT in the layer's [env] to request a port. ThinkRail
# can fall back to a free port, so use the URL it prints. The CLI's
# explicit --host overrides THINKRAIL_HOST (apps/cli/src/args.ts):
# the IDE has no application authentication and must bind to loopback.
#
# Analytics: the programs module exports `THINKRAIL_NO_ANALYTICS` via its
# config.env when enabled, which applies to every mysbx sandbox of that
# host — including this one. This entry does not force it, so a repo
# that wants analytics can opt back in from `[env]`.
#
# The project: mysbx `--chdir`s into the repo root before the entry
# runs, the repo is mounted rw, and `$PWD` IS the repo — passed as the
# documented `[project-dir]` positional so the IDE opens this project
# on launch instead of landing on its picker.
#
# State: ThinkRail keeps app state under `$HOME/.thinkrail` and rotating
# logs under `$HOME/.thinkrail/logs`. Inside the sandbox `$HOME` is the
# tmpfs `/mysbx-home` (config.md D14), so both are per-run and
# per-sandbox by construction — the same isolation herdr's
# `~/.config/herdr` state gets — unless a repository persists them with
# a `state-dirs` entry (config.md D15), e.g.
# `state-dirs = [".thinkrail"]`.
{
  lib,
  writeShellApplication,
  coreutils,
  bashInteractive,
  # The interactive payload shell of the sandbox (../default.nix
  # `shell.path`, bd myconfig-l44): the HOST's shell, so a terminal
  # pane inside the IDE lands in the same shell as the host and the
  # payload. `null` keeps this entry's own bash.
  payloadShell ? null,
  # The thinkrail CLI that runs inside the sandbox
  # (`myconfig.ai.dev.mysbx.thinkrail.package`, normally
  # `pkgs.thinkrail` of the flake overlay / programs.thinkrail.nix).
  thinkrail,
}:
let
  muxLib = import ./mux-entry-lib.nix { inherit lib; };
in
writeShellApplication {
  name = "mysbx-thinkrail-entry";
  runtimeInputs = [
    coreutils
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-thinkrail-entry"}
    ${muxLib.pinShell (if payloadShell != null then payloadShell else lib.getExe bashInteractive)}

    # `exec`: the engine host owns the foreground, its startup mark
    # (`thinkrail → http://…`) goes to the sandbox's stdout, and its
    # exit code propagates unchanged (../docs/design/cli.md D8).
    # `--no-open`: the browser opens on the operator's machine, not in
    # the sandbox (see the header).
    exec ${lib.getExe thinkrail} --no-open --host 127.0.0.1 "$PWD"
  '';

  meta = {
    description = "Interactive payload of a thinkrail-multiplexer mysbx sandbox (the ThinkRail engine host, headless)";
    mainProgram = "mysbx-thinkrail-entry";
    platforms = lib.platforms.linux;
  };
}
