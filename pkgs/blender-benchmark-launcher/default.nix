{ stdenv
, fetchurl
, autoPatchelfHook
, blender
, makeWrapper
, libGL
, libX11
}:

stdenv.mkDerivation rec {
  name = "blender-benchmark-launcher-${version}";
  version = "2.0.5";

  src = fetchurl {
    url = "https://opendata.blender.org/cdn/BlenderBenchmark2.0/launcher/benchmark-launcher-2.0.5-linux.tar.gz";
    sha256 = "e19e3bb0400351da9635872a38c2573a59d06ba22c21a939e05d47cbbcf46069";
  };

  nativeBuildInputs = [
    autoPatchelfHook makeWrapper
  ];

  buildInputs = [
    stdenv.cc.cc.lib
    blender
    libGL
    libX11
  ];

  unpackPhase = ''
    mkdir -p  benchmark-launcher
    tar -xf $src -C benchmark-launcher
  '';

  installPhase = ''
    mkdir -p $out/share
    cp -r benchmark-launcher/* $out/share
    makeWrapper $out/share/benchmark-launcher $out/bin/benchmark-launcher
  '';

  meta = with stdenv.lib; {
    homggepage = https://opendata.blender.org/;
    description = "Blender Benchmark Launcher CLI";
    platforms = platforms.linux;
    maintainers = with maintainers; [];
  };
}
