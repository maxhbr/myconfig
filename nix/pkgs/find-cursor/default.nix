{ pkgs ? import <nixpkgs> {} }:

let
  stdenv = pkgs.stdenv;
in

stdenv.mkDerivation rec {
  name = "find-cursor-${version}";
  version = "0.0.0";
  src = pkgs.fetchFromGitHub {
    owner  = "Carpetsmoker";
    repo   = "find-cursor";
    rev    = "24962c42000e08b052e7423a526c684f837fd42c";
    sha256 = "1sw33758wyzn607smkx8rd5dm16ffy20w736agrdxdiigach5wb5";
  };

  nativeBuildInputs = [ pkgs.makeWrapper ] ++ (with pkgs.xorg; [ libX11 libXext libXdamage libXrender ]);

  buildPhase = ''
    make all
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp find-cursor $out/bin/find-cursor
    chmod +x $out/bin/find-cursor
  '';

  meta = {
    homepage = https://github.com/Carpetsmoker/find-cursor;
    description = "Simple XLib program to highlight the cursor position.";
    license = stdenv.lib.licenses.mit;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
