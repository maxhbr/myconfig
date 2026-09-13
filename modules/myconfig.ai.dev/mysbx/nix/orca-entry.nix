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
# window (`orca open`) but the Orca RUNTIME SERVER (`orca serve`) —
# both forms start headless fine (see below), the difference is who can
# reach the interactive surface: a window rendered inside a mysbx
# sandbox is invisible and undrivable, because mysbx attaches no host
# display (nothing display related is forwarded, no X/Wayland socket
# is bound) and its `gui` form runs a terminal OUTSIDE the sandbox.
# The `serve` interactive surface — the WebSocket pairing endpoint it
# prints on readiness — is the one thing reachable from outside, over
# the shared network namespace: the operator pairs with the sandboxed
# runtime from their Orca desktop/mobile client (`orca environment add
# --pairing-code`, or `ORCA_PAIRING_CODE` on the client side). This is
# the headless-server form the upstream project documents for a Linux
# server (stablyai/orca `docs/reference/headless-linux-server.md`).
#
# The display decision (documented, per bd myconfig-1os): Electron
# still wants *a* display, and Orca auto-starts its own Xvfb on `:99`
# when `$DISPLAY` is unset — inside the sandbox `$DISPLAY` is never
# set (mysbx forwards nothing display related), so the bundled
# auto-start always applies. No Xvfb is started by this script, none is
# plumbed from the host. Expect the startup dbus noise
# (`Failed to connect to the bus: ... /run/dbus/system_bus_socket`) on
# stderr: there is no system bus inside the sandbox, and orca continues
# without it.
#
# The network decision: the pairing endpoint must be reachable from
# the client the operator pairs with, so this payload needs the shared
# network (config.md D5/D9 — `network` defaults to `true`; a
# `network = false` layer makes the server unreachable from outside the
# sandbox, which is a refused-for-practical-purposes session, not a
# sandboxed escape). The endpoint knobs mirror the host-side
# `orca-serve` unit of ../../services.orca.nix (`--port`,
# `--pairing-address`) and are config-layer surface, not host env:
# mysbx forwards only its fixed allowlist, so set them in a layer's
# `[env]` table:
#
#   ORCA_PORT            pin the port (unset: orca binds 6768 or a
#                        fallback, and the ready block on stdout is
#                        the source of truth). Pin it when the HOST
#                        itself runs `orca-serve` — the sandbox shares
#                        the host loopback, so two unpinned runtimes
#                        would race for 6768.
#   ORCA_PAIRING_ADDRESS the address orca advertises in the pairing
#                        code (unset: loopback — right for a client
#                        on the same host). A remote client needs the
#                        LAN/Tailscale name here, exactly like
#                        `myconfig.ai.orca.pairingAddress` host-side.
#
# State: Orca keeps its profile under `$HOME/.config/{orca,Orca}` and
# the appimage-run extraction cache under `$HOME/.cache/appimage-run`
# (upstream headless guide: "state lives in the service user's home,
# not next to the binary"). Inside the sandbox `$HOME` is the tmpfs
# `/mysbx-home` (config.md D14), so both are per-run and per-sandbox by
# construction — the same isolation herdr's `~/.config/herdr` state
# gets — and every run pays the AppImage re-extraction, unless a
# repository persists them with `state-dirs` entries (config.md D15),
# e.g. `state-dirs = [".config/orca", ".config/Orca",
# ".cache/appimage-run"]`.
#
# The workspace: mysbx `--chdir`s into the repo root before the entry
# runs, and the repo is mounted rw, so the paired client CAN register
# it (`orca repo add --path .`). Whether `serve` itself reads `$PWD`
# is not pinned by its `--help`; workspaces are driven through the
# paired client, and the undocumented `--project-root` flag is
# deliberately not passed rather than guessed at.
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

    # The endpoint knobs of the header comment, as argv for `serve`.
    # Both optional: unset means orca's own defaults (6768 or
    # fallback, loopback advertised) — see the header for when to pin.
    serve_args=()
    if [ -n "''${ORCA_PORT:-}" ]; then
      serve_args+=(--port "$ORCA_PORT")
    fi
    if [ -n "''${ORCA_PAIRING_ADDRESS:-}" ]; then
      serve_args+=(--pairing-address "$ORCA_PAIRING_ADDRESS")
    fi

    # `exec`: the runtime owns the foreground, its ready block and
    # pairing URL go to the sandbox's stdout, and its exit code
    # propagates unchanged (../docs/design/cli.md D8).
    exec appimage-run ${orca} serve "''${serve_args[@]}" "$@"
  '';

  meta = {
    description = "Interactive payload of an orca-multiplexer mysbx sandbox (the Orca runtime server, headless)";
    mainProgram = "mysbx-orca-entry";
    platforms = lib.platforms.linux;
  };
}
