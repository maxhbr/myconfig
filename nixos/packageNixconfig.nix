# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation {
  version = "0.1";
  name = "maxhbr-nixos-config-src";

  src = builtins.filterSource
    (path: type: baseNameOf path != "result" && baseNameOf path != "packageNixconfig.nix")
    ./.;

  # buildPhase = "";
  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';

  meta = with stdenv.lib; {
    description = "maxhbr's nixos configuration sources";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
