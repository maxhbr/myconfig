{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "0.1";
  name = "my-backgrounds-${version}";

  src = builtins.filterSource
    (path: type: baseNameOf path != "slim-theme")
    ./.;

  buildInputs = with pkgs; [ imagemagick coreutils ];

  buildPhase = ''
    mkdir -p 1080 1440 2160
    for img in *.png; do
      echo "scale: $img..."
      convert "$img" -resize 1920x1080\> "1080/$img"
      convert "$img" -resize 2560x1440\> "1440/$img"
      convert "$img" -resize 3840x2160\> "2160/$img"
    done

    sed -i 's%feh%${pkgs.feh}/bin/feh%' scripts/myRandomBackground.sh
    sed -i 's%i3lock%${pkgs.i3lock}/bin/i3lock%' scripts/myScreenLock.sh
    sed -i 's%xrandr%${pkgs.xorg.xrandr}/bin/xrandr%' scripts/myScreenLock.sh
  '';
  installPhase = ''
    share="$out/share"
    mkdir -p $share
    cp -r *.png 1080 1440 2160 myBgs.sh $share/

    bin="$out/bin"
    mkdir -p $bin
    cp scripts/myRandomBackground.sh $bin/myRandomBackground
    cp scripts/myScreenLock.sh $bin/myScreenLock
    for exe in $bin/*; do
      chmod +x "$exe"
      sed -i 's%^DIR.*%DIR="'"$out"'/share"%' "$exe"
    done
  '';

  meta = with stdenv.lib; {
    description = "My background images";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
