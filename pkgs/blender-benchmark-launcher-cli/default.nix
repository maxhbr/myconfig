{ stdenv
, fetchurl
, autoPatchelfHook
, blender
, makeWrapper
}:

stdenv.mkDerivation rec {
  name = "blender-benchmark-launcher-cli-${version}";
  version = "2.0.5";

  src = fetchurl {
    url = "https://opendata.blender.org/cdn/BlenderBenchmark2.0/launcher/benchmark-launcher-cli-2.0.5-linux.tar.gz";
    sha256 = "16bd3d04826f0aaf9069967b4d0a733eb4e728abb9ef8ab1a472461eed95d7d0";
  };

  nativeBuildInputs = [
    autoPatchelfHook makeWrapper
  ];

  buildInputs = [
    stdenv.cc.cc.lib
    blender
  ];

  unpackPhase = ''
    mkdir -p  benchmark-launcher-cli
    tar -xf $src -C benchmark-launcher-cli
  '';

  installPhase = ''
    mkdir -p $out/share
    cp -r benchmark-launcher-cli/* $out/share
    makeWrapper $out/share/benchmark-launcher-cli $out/bin/benchmark-launcher-cli
  '';

  meta = with stdenv.lib; {
    homggepage = https://opendata.blender.org/;
    description = "Blender Benchmark Launcher CLI";
    platforms = platforms.linux;
    maintainers = with maintainers; [];
  };
}
