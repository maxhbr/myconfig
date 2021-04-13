{ stdenv, lib, rpmextract, requireFile, libudev }:

with lib;

stdenv.mkDerivation rec {
  name = "jlink-${version}";
  version = "700";

  src =
    if stdenv.system == "x86_64-linux" then
      requireFile {
        name = "JLink_Linux_V${version}_x86_64.rpm";
        url = "https://www.segger.com/downloads/jlink/JLink_Linux_V${version}_x86_64.rpm";
        sha256 = "06g1x25afhpczaz3xlzyrfa00456mfjn9f0n8nf166faczwkcaj9";
      }
    else
      abort "${name} requires x86_64 Linux";

  buildInputs = [ rpmextract ];
  phases = [ "unpackPhase" "installPhase" "fixupPhase" "distPhase" ];

  RPATH="${stdenv.cc.cc.lib}/lib:${lib.getLib libudev}/lib";
  unpackPhase = "rpmextract $src";
  installPhase = readFile ./install.sh;

  meta = {
    description = "SEGGER J-Links are the most widely used line of debug probes available today";
    homepage = https://www.segger.com/downloads/jlink;
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = with maintainers; [ pjones ];
  };
}
