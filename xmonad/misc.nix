# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

let
  xmobarrc = ./xmobarrc;
in stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-xmonad-misc-${version}";

  src = ./bin;

  buildPhase = "";

  installPhase = ''
    share=$out/share
    bin=$out/bin
    mkdir -p $share $bin

    cp * $bin

    cp ${xmobrrc} $share
    sed -i -e 's%/home/mhuber/.xmonad/bin%$bin%g' $share/xmobarrc
  '';
}
