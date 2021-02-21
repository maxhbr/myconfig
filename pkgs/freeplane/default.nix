{ stdenv, lib, fetchurl, unzip, makeWrapper, jre }:

stdenv.mkDerivation rec {
  version = "1.8.11";
  pname = "freeplane";

  src = fetchurl {
    url =
      "https://sourceforge.net/projects/freeplane/files/freeplane%20stable/freeplane_bin-${version}.zip/download";
    sha1 = "1038fc4e3b5fa86643e6d49d225fd811936c95fa";
  };

  nativeBuildInputs = [ unzip makeWrapper ];

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/{bin,share}
    cp -r freeplane-${version}/* $out/share
    makeWrapper $out/share/freeplane.sh $out/bin/freeplane \
      --set JAVA_HOME "${jre}/lib/openjdk"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Mind-mapping software";
    homepage = "https://www.freeplane.org/wiki/index.php/Home";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ maxhbr ];
    platforms = platforms.linux;
  };
}
