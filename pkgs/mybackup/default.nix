# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "myborgbackup-${version}";

  src = ./.;

  patchPhase = ''
    sed -i -e 's%borgCmd=.*%borgCmd=${pkgs.borgbackup}/bin/borg%g' myborgbackup.sh
    sed -i -e 's%cryptsetupCmd=.*cryptsetupCmd=${pkgs.cryptsetup}/bin/cryptsetup%g' backupLuksHeader.sh
  '';

  buildPhase = "";

  installPhase = ''
    bin=$out/bin
    mkdir -p $bin

    cp *.sh $bin
    chmod +x "$bin/*.sh"
  '';
}
