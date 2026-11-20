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
    pname = "beads-server-app";
    version = "0.1.0";

    src = ./.;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/share/beads-server
      cp server.py bdcache.py $out/share/beads-server/
      cp -r static $out/share/beads-server/static
      runHook postInstall
    '';

    meta = {
      description = "Assets of the beads-server web app";
      platforms = lib.platforms.unix;
    };
  };
in
writeShellApplication {
  name = "beads-server";
  runtimeInputs = [
    beads
    python3
  ];
  runtimeEnv = {
    BEADS_SERVER_APP = "${app}/share/beads-server";
    BD_DISABLE_METRICS = "1";
  };
  text = ''
    exec python3 -u "$BEADS_SERVER_APP/server.py" "$@"
  '';

  meta = {
    description = "Live kanban board over the bd (beads) issue database";
    mainProgram = "beads-server";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
