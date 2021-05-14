{ stdenv, lib, rpmextract, requireFile, libudev, lxqt, xorg, fontconfig.lib }:

with lib;

stdenv.mkDerivation rec {
  name = "jlink-${version}";
  version = "721a";

  src =
    if stdenv.system == "x86_64-linux" then
      requireFile {
        name = "JLink_Linux_V${version}_x86_64.rpm";
        url = "https://www.segger.com/downloads/jlink/JLink_Linux_V${version}_x86_64.rpm";
        sha256 = "63328d0aa9fddb05c04238206d11d80b5918552aa240c6d1d887cf77c1566185";
      }
    else
      abort "${name} requires x86_64 Linux";

  nativeBuildInputs = [rpmextract];
  buildInputs = [
                  # stdenv.cc.cc.lib
                  libudev
                  lxqt.libqtxdg
                  xorg.libXrender
                  fontconfig.lib
                ];
  phases = [ "unpackPhase" "installPhase" "fixupPhase" "distPhase" ];

  RPATH=  lib.makeLibraryPath buildInputs;
    # "${stdenv.cc.cc.lib}/lib:${lib.getLib libudev}/lib:${lib.getLib lxqt.libqtxdg}/lib";
  unpackPhase = "rpmextract $src";
  installPhase = readFile ./install.sh;

  meta = {
    description = "SEGGER J-Links are the most widely used line of debug probes available today";
    homepage = https://www.segger.com/downloads/jlink;
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = with maintainers; [ pjones maxhbr ];
  };
}
