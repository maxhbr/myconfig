# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "tern-wrapper-${version}";

  src = ./.;

  buildPhase = "";

  installPhase = ''
    set -e
    bin=$out/bin
    mkdir -p $bin

    for sh in *.sh; do
      chmod +x $sh
      cp $sh $bin
    done
  '';
}
