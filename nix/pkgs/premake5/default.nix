{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation {
  name = "premake-5.0.0-alpha11";

  src = fetchurl {
    url = "https://github.com/premake/premake-core/releases/download/v5.0.0-alpha11/premake-5.0.0-alpha11-src.zip";
    sha256 = "1mbcxbbfg6yp0nbvjbmx6vihxnygdwzvscj4chjbd1ac4af737wg";
  };

  buildInputs = [ unzip ];

  buildPhase = ''
    (cd build/gmake.unix/ && make config=release)
  '';

  installPhase = ''
    install -Dm755 bin/release/premake5 $out/bin/premake5
  '';

  meta = with stdenv.lib; {
    homepage = http://industriousone.com/premake;
    description = "A simple build configuration and project generation tool using lua";
    license = licenses.bsd3;
    platforms = platforms.linux;
    maintainers = [ maintainers.fmthoma ];
  };
}
