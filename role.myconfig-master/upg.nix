# from: https://github.com/NixOS/nixpkgs/pull/34752
{ stdenv, fetchurl, unzip, makeWrapper }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "upg-${version}";

  phases = [ "installPhase" ];

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    makeWrapper /home/mhuber/myconfig/rebuild.sh $out/bin/upg
    makeWrapper /home/mhuber/myconfig/rebuild.sh $out/bin/upg-fast --add-flags --fast
    makeWrapper /home/mhuber/myconfig/rebuild.sh $out/bin/upg-dry --add-flags --dry-run
  '';
}
