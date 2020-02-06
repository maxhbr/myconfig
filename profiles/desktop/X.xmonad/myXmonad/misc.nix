# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-xmonad-misc-${version}";

  src = ./.;

  patchPhase = ''
    sed -i -e 's%xmessage%${pkgs.xorg.xmessage}/bin/xmessage%g' bin/battery-monitor.sh
    sed -i -e 's%acpi %${pkgs.acpi}/bin/acpi %g' bin/battery-monitor.sh
  '';

  buildPhase = "";

  installPhase = ''
    share=$out/share
    bin=$out/bin
    mkdir -p $share $bin

    cp bin/* $bin
    cp share/* $share
  '';
}
