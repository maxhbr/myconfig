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
        sha256 = "badc316b8dbbd1800e0d7a0b7fce4d369ffa352ff35e148675af439531991761";
      }
    # else if stdenv.system == "i686-linux" then
    #   requireFile {
    #     name = "JLink_Linux_V${version}_i386.rpm";
    #     url = "https://www.segger.com/downloads/jlink/JLink_Linux_V${version}_i386.rpm";
    #     sha256 = "c8e169163980165eecbeac90787d649ea3ea036140e9c6ac613bc339c30445df";
    #   }
    else
      abort "${name} requires i686-linux or x86_64 Linux";

  buildInputs = [ rpmextract ];
  phases = [ "unpackPhase" "installPhase" "fixupPhase" "distPhase" ];

  RPATH="${stdenv.cc.cc.lib}/lib:${libudev.lib}/lib";
  unpackPhase = "rpmextract $src";
  installPhase = readFile ./install.sh;

  meta = {
    description = "SEGGER J-Links are the most widely used line of debug probes available today";
    longDescription = "TODO:";
    homepage = https://www.segger.com/downloads/jlink;
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = with maintainers; [ pjones ];
  };
}
