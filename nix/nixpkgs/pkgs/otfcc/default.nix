{ stdenv, fetchurl, unzip, premake5 }:

stdenv.mkDerivation rec {
  name = "otfcc-${version}";
  version = "0.6.3";

  src = fetchurl {
    url = "https://github.com/caryll/otfcc/archive/v${version}.tar.gz";
    sha256 = "0zf0037k5njppyl1dj11ivj7i435p15kj9jcv2w9cdk5yj0i45jq";
  };

  buildInputs = [ unzip premake5 ];

  buildPhase = ''
    premake5 gmake
    (cd build/gmake && make config=release_x64)
  '';

  installPhase = ''
    install -Dm755 bin/Release-x64/otfccbuild $out/bin/otfccbuild
    install -Dm755 bin/Release-x64/otfccdump  $out/bin/otfccdump
  '';

  meta = with stdenv.lib; {
    homepage = "https://github.com/caryll/otfcc";
    description = "A library and utility used for parsing and writing OpenType font files";
    license = licenses.asl20;
    platforms = platforms.linux;
    maintainers = [ maintainers.fmthoma ];
  };
}
