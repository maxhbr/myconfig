# from: https://github.com/NixOS/nixpkgs/pull/34752
{ stdenv, fetchurl, unzip, makeWrapper }:

stdenv.mkDerivation rec {
  version = "1.8.2";
  name = "freeplane-${version}";

  src = fetchurl {
    url = "https://sourceforge.net/projects/freeplane/files/freeplane%20stable/freeplane_bin-1.8.2.zip/download";
    sha512 = "1a1a24fdffc932e2bc4af93ea0cbef1e9fcb9c7a5e92bac490c6ac4ed5485ae92d353aa41d259e40df0b256c9958c3978e967eea6f8991ae621deb44bc6c22b6";
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
