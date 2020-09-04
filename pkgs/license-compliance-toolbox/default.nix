# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "license-compliance-toolbox-${version}";

  src = ./.;

  patchPhase = ''
    sed -i -e 's%docker %${pkgs.docker}/bin/docker %g' *.sh
    sed -i -e 's%git %${pkgs.git}/bin/git %g' *.sh
  '';

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
