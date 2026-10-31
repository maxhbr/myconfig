# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  lib,
  stdenv,
  beads,
  python3,
  writeShellApplication,
}:

let
  app = stdenv.mkDerivation {
    pname = "beads-viewer-app";
    version = "0.1.0";

    src = ./.;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/beads-viewer
      cp index.html dagre-0.8.5.js dagre-0.8.5.LICENSE server.py $out/lib/beads-viewer/
      runHook postInstall
    '';

    meta = {
      description = "Static assets of the beads-viewer web app";
      platforms = lib.platforms.unix;
    };
  };
in
writeShellApplication {
  name = "beads-viewer";
  runtimeInputs = [
    beads
    python3
  ];
  runtimeEnv = {
    BEADS_VIEWER_APP = "${app}/lib/beads-viewer";
  };
  text = ''
    exec python "$BEADS_VIEWER_APP/server.py" "$@"
  '';

  meta = {
    description = "Single-page web viewer rendering bd (beads) issues as a dependency graph";
    mainProgram = "beads-viewer";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
