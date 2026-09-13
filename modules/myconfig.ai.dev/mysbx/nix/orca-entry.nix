# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# `mysbx-orca-entry` — the INTERACTIVE payload of a
# `multiplexer = "orca"` mysbx sandbox (../docs/design/config.md D17,
# ../docs/design/cli.md D11).
#
# Orca (https://onorca.dev, packaged in ../../services.orca.nix) is the
# odd one out among the multiplexers: it is not a terminal multiplexer
# at all but an Electron desktop app / agent orchestrator. What runs as
# the interactive payload of a sandbox is therefore NOT the Orca desktop
# window — there is no way to attach a GUI surface to a mysbx sandbox,
# and the Electron window has no headless entry point. It is the Orca
# RUNTIME SERVER (`orca serve`), the form the upstream project itself
# documents for a headless Linux host
# (stablyai/orca `docs/reference/headless-linux-server.md`): the
# operator drives the session from the Orca desktop or mobile client,
# which pairs with the sandboxed runtime over the WebSocket endpoint
# `serve` prints on readiness.
#
# The display decision (documented, per bd myconfig-1os): `serve`
# needs *a* display for Electron's startup, and Orca auto-starts its
# own Xvfb on `:99` when `$DISPLAY` is unset — inside the sandbox that
# is the only sane choice: mysbx deliberately forwards nothing display
# related (`FORWARDED_ENV_VARS` is terminal/locale/credentials only),
# so `$DISPLAY` is never set and the AppImage's bundled auto-start
# always applies. No Xvfb is started by this script, none is plumbed
# from the host.
#
# The network decision: the pairing endpoint must be reachable from the
# client the operator pairs with, so this payload needs the shared
# network (config.md D5/D9 — `network` defaults to `true`; a
# `network = false` layer makes the server unreachable from outside the
# sandbox, which is a refused-for-practical-purposes session, not a
# sandboxed escape). Orca may bind a fallback port when 6768 is taken,
# so the port is NOT pinned here — the ready block on stdout is the
# source of truth for what was bound.
#
# State: Orca keeps its profile under `$HOME/.config/{orca,Orca}`
# (upstream headless guide, "Upgrade" — "state lives in the service
# user's home, not next to the binary"). Inside the sandbox `$HOME` is
# the tmpfs `/mysbx-home` (config.md D14), so the profile is per-run
# and per-sandbox by construction — the same isolation herdr's
# `~/.config/herdr` state gets — unless a repository persists it with a
# `state-dirs` entry (config.md D15), e.g.
# `state-dirs = [".config/orca", ".config/Orca"]`.
{
  lib,
  writeShellApplication,
  coreutils,
  bashInteractive,
  # `appimage-run` extracts the AppImage's squashfs payload (no FUSE
  # device, no /dev/fuse needed inside the sandbox) and provides the
  # FHS runtime the AppImage expects — the same launch path the
  # ../../services.orca.nix launcher and `orca-serve` systemd service
  # use, so the sandbox runs the identical bits.
  appimage-run,
  # `xvfb` is on $PATH for Orca's own `:99` auto-start; it is NOT
  # started here (the AppImage owns that lifecycle, like it does for
  # the host-side service).
  xvfb,
  # The Orca AppImage that runs inside the sandbox
  # (`myconfig.ai.dev.mysbx.orca.package`, normally
  # `myconfig.ai.orca.package` of ../../services.orca.nix).
  orca,
}:
let
  muxLib = import ./mux-entry-lib.nix { inherit lib; };
in
writeShellApplication {
  name = "mysbx-orca-entry";
  runtimeInputs = [
    appimage-run
    xvfb
    coreutils
    bashInteractive
  ];
  text = ''
    ${muxLib.requireSocketDir "mysbx-orca-entry"}
    ${muxLib.pinShell (lib.getExe bashInteractive)}

    # Software rendering, like the host-side `orca-serve` unit of
    # ../../services.orca.nix: a sandbox has no GPU guarantees, and
    # LIBGL_ALWAYS_SOFTWARE=1 keeps Electron off the host's DRI
    # devices (which are not mounted anyway).
    export LIBGL_ALWAYS_SOFTWARE=1

    # mysbx `--chdir`s into the repo root, so the sandboxed runtime
    # works on the repository it was started for; Orca picks the
    # workspace up from $PWD like every other entry's tool does.
    # `exec`: the runtime owns the foreground, its ready block and
    # pairing URL go to the sandbox's stdout, and its exit code
    # propagates unchanged (../docs/design/cli.md D8).
    exec appimage-run ${orca} serve "$@"
  '';

  meta = {
    description = "Interactive payload of an orca-multiplexer mysbx sandbox (the Orca runtime server, headless)";
    mainProgram = "mysbx-orca-entry";
    platforms = lib.platforms.linux;
  };
}
