{ pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  background ? (pkgs.callPackage ./.. { inherit pkgs stdenv; })}:

stdenv.mkDerivation rec {
  version = "0.1";
  name = "my-slim-theme-${version}";

  src = ./.;


  buildInputs = with pkgs; [ imagemagick coreutils ];

  buildPhase = ''
    ln -s ${background}/share/romben3.png background.png
  '';
  installPhase = ''
    share="$out/share/slim-theme"
    mkdir -p $share

    cp -r * $share
  '';

  meta = with stdenv.lib; {
    description = "My slim theme";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
