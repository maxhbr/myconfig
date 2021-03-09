# Copyright 2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "mute_telco-${version}";

  src = ./.;

  patchPhase = ''
    sed -i -e 's%pacmd%${pkgs.pulseaudio}/bin/pacmd%g' mute_telco.sh
    sed -i -e 's%blink1-tool%${pkgs.blink1-tool}/bin/blink1-tool%g' mute_telco.sh
  '';

  installPhase = ''
    bin=$out/bin
    mkdir -p $bin
    cp mute_telco.sh $bin/mute_telco
  '';
}
