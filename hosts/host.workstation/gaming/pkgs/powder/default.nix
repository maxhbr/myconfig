# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# build with:
# nix-build \
#     --keep-failed \
#     -E 'with import <nixpkgs> {}; callPackage ./default.nix {}'

{
  stdenv,
  fetchurl,
  unzip,
  pkgs,
}:
# based on: https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=powder
stdenv.mkDerivation {
  name = "powder118";

  src = fetchurl {
    url = "http://zincland.com/powder/release/powder118_src.tar.gz";
    sha256 = "1mzk338sz7g77k09pw6cb47lg60si76x2mrm83r1zpf7f8wjz0af";
  };

  buildInputs = [
    pkgs.SDL
    pkgs.gcc6
  ];

  buildPhase = ''
    patchShebangs .
    ./buildall.sh --use-home-dir
  '';

  patches = [
    ./powder-117-gcc6.patch # see: https://gitweb.gentoo.org/repo/gentoo.git/commit/?id=0b360332de4c8f4ab442180cb9312262733b4cad
  ];

  installPhase = ''
    install -Dm755 powder "$out/bin/powder"
  '';

  meta = with pkgs.lib; {
    homepage = "http://zincland.com/powder";
    description = "A graphical roguelike, originally designed for the Game Boy Advance";
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = [ "mail@maximilian-huber.de" ];
  };
}
