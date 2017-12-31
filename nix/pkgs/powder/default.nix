{ stdenv, fetchurl, unzip, pkgs }:
# based on: https://aur.archlinux.org/cgit/aur.git/tree/PKGBUILD?h=powder
stdenv.mkDerivation {
  name = "powder117";

  src = fetchurl {
    url = "http://zincland.com/powder/release/powder117_src.tar.gz";
    sha256 = "070346921ee83bc40943b1e1cb576ab3222cecc319fe10f5c138a4deec85e861";
  };

  buildInputs = [ pkgs.SDL pkgs.gcc5 ];

  buildPhase = ''
    patchShebangs .
    ./buildall.sh --use-home-dir
  '';

  installPhase = ''
    install -Dm755 powder "$out/bin/powder"
  '';

  meta = with stdenv.lib; {
    homepage = http://zincland.com/powder;
    description = "A graphical roguelike, originally designed for the Game Boy Advanc";
    license = licenses.unfree;
    platforms = platforms.linux;
    maintainers = [ "mail@maximilian-huber.de" ];
  };
}
