# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs ? import <nixpkgs> { },
  stdenv ? pkgs.stdenv,
}:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-xmonad-misc-${version}";

  src = ./.;

  patchPhase = ''
    sed -i -e 's%xmessage%${pkgs.xorg.xmessage}/bin/xmessage%g' battery-monitor.sh
    sed -i -e 's%acpi %${pkgs.acpi}/bin/acpi %g' battery-monitor.sh
  '';

  buildPhase = "";

  installPhase = ''
    bin=$out/bin
    mkdir -p $bin

    cp battery-monitor.sh $bin
    chmod +x "$bin/battery-monitor.sh"
  '';
}
