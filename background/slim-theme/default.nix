{ pkgs ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  background ? (pkgs.callPackage ./.. { inherit pkgs stdenv; })}:

stdenv.mkDerivation rec {
  version = "0.1";
  name = "my-slim-theme-${version}";

  src = ./.;


  buildInputs = with pkgs; [ imagemagick coreutils ];

  buildPhase = ''
    mkdir -p my-slim-theme
    cp ${background}/share/romben3.png my-slim-theme/background.png
    mv panel.png slim.theme my-slim-theme
    tar -czf my-slim-theme.tar.gz my-slim-theme
    # ${pkgs.zip}/bin/zip -r my-slim-theme.zip my-slim-theme
  '';
  installPhase = ''
    share="$out/share"
    mkdir -p $share
    cp -r my-slim-theme* $share
  '';

  meta = with stdenv.lib; {
    description = "My slim theme";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
