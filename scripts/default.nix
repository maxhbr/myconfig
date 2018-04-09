# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-scripts-${version}";

  src = ./.;

  installPhase = ''
    bin="$out/bin"
    mkdir -p $bin
    cp -r * $bin
  '';

  meta = with stdenv.lib; {
    description = "My scripts related to myconfig";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
