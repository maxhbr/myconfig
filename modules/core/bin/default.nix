# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-scripts-${version}";

  src = ./.;

  buildPhase = ''
    # replace nix-shell headers
    find . -iname '*.sh' \
        -exec sed -i "1s%.*%#!/usr/bin/env bash%" {} \; \
        -exec sed -i -e '2d' {} \;

    sed -i -e '/borgCmd=/ s%=.*%="${pkgs.borgbackup}/bin/borg";%' myborgbackup.sh
  '';
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
