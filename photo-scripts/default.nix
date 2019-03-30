# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-photo-scripts-${version}";

  src = ./.;

  buildPhase = ''
    # replace nix-shell headers
    find . -iname '*.sh' \
        -exec sed -i "1s%.*%#!/usr/bin/env bash%" {} \; \
        -exec sed -i -e '2d' {} \;

    # hardlink binaries
    sed -i -e 's%align_image_stack%${pkgs.hugin}/bin/align_image_stack%g' alignImages.sh
    sed -i -e 's%enfuse%${pkgs.enblend-enfuse}/bin/enfuse%g' focusStackImages.sh
    sed -i -e 's%convert%${pkgs.imagemagick}/bin/convert%g' cropImages.sh
    sed -i -e 's%convert%${pkgs.imagemagick}/bin/convert%g' scale-images-for-web.sh
  '';

  installPhase = ''
    bin="$out/bin"
    mkdir -p $bin
    cp -r *.sh $bin
  '';

  meta = with stdenv.lib; {
    description = "My scripts for automating management and editing of photos";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
