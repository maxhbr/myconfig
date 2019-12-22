# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs ? import <nixpkgs> {}, stdenv ? pkgs.stdenv }:

stdenv.mkDerivation rec {
  version = "1.0";
  name = "my-backgrounds-${version}";

  src = ./.;

  buildInputs = with pkgs; [ imagemagick coreutils ];

  buildPhase = ''
    mkdir -p 1080 1440 2160
    for img in *.png; do
      echo "scale: $img..."
      convert "$img" -resize 1920x1080\> "1080/$img"
      convert "$img" -resize 2560x1440\> "1440/$img"
      convert "$img" -resize 3840x2160\> "2160/$img"
    done

    # link default background
    ln -s romben3.png background.png

    sed -i 's%feh%${pkgs.feh}/bin/feh%' bg.sh
    sed -i 's%i3lock%${pkgs.i3lock}/bin/i3lock%' bg.sh
    sed -i 's%xrandr%${pkgs.xorg.xrandr}/bin/xrandr%' bg.sh
  '';

  installPhase = ''
    share=$out/share
    mkdir -p $share
    cp -r *.png 1080 1440 2160 $share

    sed -i 's%^DIR.*%DIR="'"$share"'"%' bg.sh

    bin="$out/bin"
    mkdir -p $bin
    cp bg.sh "$bin/.bg.sh"

    printf "#!${pkgs.bash}/bin/bash\n${pkgs.bash}/bin/bash $bin/.bg.sh --set" > $bin/myRandomBackground
    printf "#!${pkgs.bash}/bin/bash\n${pkgs.bash}/bin/bash $bin/.bg.sh --lock" > $bin/myScreenLock
    chmod +x $bin/*
    chmod -x $bin/.bg.sh
  '';

  meta = with stdenv.lib; {
    description = "My background images";
    homepage = https://github.com/maxhbr/myconfig;
    license = licenses.mit;
    platforms = platforms.unix;
    maintainers = [ ];
  };
}
