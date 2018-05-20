# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

let
  xmobarrc = ./xmobarrc;
  xmobarrcMin = ./xmobarrc.minimal;
  share = ./share;
in stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-xmonad-misc-${version}";

  src = ./bin;

  patchPhase = ''
    sed -i -e 's%xmessage%${pkgs.xorg.xmessage}/bin/xmessage%g' battery-monitor.sh
    sed -i -e 's%acpi %${pkgs.acpi}/bin/acpi %g' battery-monitor.sh
  '';

  buildPhase = "";

  installPhase = ''
    share=$out/share
    bin=$out/bin
    mkdir -p $share $bin

    cp * $bin

    cp ${xmobarrc} $share/xmobarrc
    cp ${xmobarrcMin} $share/xmobarrc.minimal
    sed -i -e 's%/home/mhuber/.xmonad/bin%'"$bin"'%g' $share/xmobarrc
    cp ${share}/* $share
  '';
}
