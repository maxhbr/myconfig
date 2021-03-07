# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> { } }:

let stdenv = pkgs.stdenv;

in stdenv.mkDerivation rec {
  name = "find-cursor-${version}";
  version = "2020-05-28";
  src = pkgs.fetchFromGitHub {
    owner = "Carpetsmoker";
    repo = "find-cursor";
    rev = "9ed9ec581ca152d33cbb85b0929ef879e690568b";
    sha256 = "1ybwbamxl3mysack02bjh13qrcq57wnsjxwindgp6c724cbfpw3a";
  };

  nativeBuildInputs = [ pkgs.makeWrapper ]
    ++ (with pkgs.xorg; [ libX11 libXext libXdamage libXrender ]);

  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp find-cursor $out/bin/find-cursor
    chmod +x $out/bin/find-cursor
  '';

  meta = {
    homepage = "https://github.com/Carpetsmoker/find-cursor";
    description = "Simple XLib program to highlight the cursor position.";
    license = pkgs.lib.licenses.mit;
    maintainers = with pkgs.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
