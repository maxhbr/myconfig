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
{
  lib,
  writeShellApplication,
  coreutils,
  bashInteractive,
  tmux,
  # The agent-of-empires package that runs inside the sandbox
  # (`myconfig.ai.mysbx.aoe.package`, normally
  # `myconfig.ai.agent-of-empires.package`).
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
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-aoe-entry"}
    ${muxLib.pinShell (lib.getExe bashInteractive)}

    # mysbx `--chdir`s into the repo root, so `aoe` manages the
    # repository it was started for without being told which one.
    # `exec`: `aoe` owns the terminal and its exit code propagates
    # unchanged (../docs/design/cli.md D8).
    exec aoe "$@"
  '';

  meta = {
    description = "Interactive payload of an agent-of-empires mysbx sandbox";
    mainProgram = "mysbx-aoe-entry";
    platforms = lib.platforms.linux;
  };
}
