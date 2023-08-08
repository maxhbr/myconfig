# https://www.nordicsemi.com/-/media/Software-and-other-downloads/Desktop-software/nRF-command-line-tools/sw/Versions-10-x-x/10-12-2/nRF-Command-Line-Tools_10_12_2_Linux-amd64.zip
{ stdenv, lib, fetchzip, autoPatchelfHook, makeWrapper, makeDesktopItem
, copyDesktopItems, segger-jlink
# TODO: cleanup unnecessary deps:
, fontconfig, freetype, libusb, libICE, libSM, udev, libX11, libXext, libXcursor
, libXfixes, libXrender, libXrandr }:

let

  pname = "nRF-command-line-tools";
  version = "10-12-2";
  version_us = "10_12_2";
  url =
    "https://www.nordicsemi.com/-/media/Software-and-other-downloads/Desktop-software/${pname}/sw/Versions-10-x-x/${version}/${pname}_${version_us}_Linux-amd64.zip";

in stdenv.mkDerivation rec {
  inherit pname version;

  src = fetchzip {
    inherit url;
    sha256 = "1cChtf0GKK0Rrav2IZ+xYT8gPnAr0UqAe0zoobQUzG0=";
  };

  dontConfigure = true;
  dontBuild = true;
  dontStrip = true;
  preferLocalBuild = true;

  nativeBuildInputs = [ autoPatchelfHook makeWrapper ];

  buildInputs = [
    segger-jlink
    udev
    stdenv.cc.cc.lib
    fontconfig
    freetype
    libICE
    libSM
    libX11
    libXext
    libXcursor
    libXfixes
    libXrender
    libXrandr
    libusb
  ];

  runtimeDependencies = [ udev ];

  installPhase = ''
    runHook preInstall
    tar -xf nRF-Command-Line-Tools_10_12_2_Linux-amd64.tar.gz
    tar -xf ./nRF-Command-Line-Tools_10_12_2.tar
    mkdir -p $out/{bin,lib}
    cp -r nrfjprog $out

    makeWrapper $out/nrfjprog/nrfjprog $out/bin/nrfjprog \
      --prefix PATH : "${lib.makeBinPath buildInputs}" \
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}"

    ln -s $out/nrfjprog/*.so $out/lib

    runHook postInstall
  '';

  # preFixup = ''
  #   patchelf --add-needed libudev.so.1 $out/JLink/libjlinkarm.so
  # '';

  meta = with lib; {
    homepage =
      "https://www.nordicsemi.com/Software-and-tools/Development-Tools/nRF-Command-Line-Tools/Download#infotabs";
    description = "nRF-command-line-tools";
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = with maintainers; [ maxhbr ];
  };
}
