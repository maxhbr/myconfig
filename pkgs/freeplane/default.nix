# from: https://github.com/NixOS/nixpkgs/pull/34752
{ stdenv, fetchurl, unzip, makeWrapper }:

stdenv.mkDerivation rec {
  version = "1.8.6";
  name = "freeplane-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/freeplane/files/freeplane%20stable/freeplane_bin-${version}.zip/download";
    sha512 = "185d69rhyw4i6k2chidsqgbif9qikk39li4fvw68aad3fl60p0blr48l5hgrkcfp39nkxkp188g76srnqi19kgrj1had1g57ad16jcl";
  };

  nativeBuildInputs = [
    unzip makeWrapper
  ];

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
    mkdir -p $out/{bin,share}
    cp -r freeplane-${version}/* $out/share
    makeWrapper $out/share/freeplane.sh $out/bin/freeplane
  '';

  meta = with stdenv.lib; {
    description = "Mind-mapping software";
    homepage = https://www.freeplane.org/wiki/index.php/Home;
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
  };
}
