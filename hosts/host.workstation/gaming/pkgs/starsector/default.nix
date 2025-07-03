# from: https://discourse.nixos.org/t/game-got-starsector-to-run-on-nixos/5419
{
  stdenv,
  lib,
  unzip,
  requireFile,
  makeWrapper,
  xorg,
  openjdk8,
  alsaLib,
  at-spi2-atk,
  cairo,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk2-x11,
  libav,
  libX11,
  libXcursor,
  libXext,
  libxml2,
  libXrandr,
  libxslt,
  libXtst,
  libXxf86vm,
  openal,
  pango,
}:

stdenv.mkDerivation rec {
  pname = "starsector";
  version = "0.9.1a-RC8";

  src = ./. + "/starsector_linux-${version}.zip";
  # requireFile {
  #   name = "starsector_linux-${version}.zip";
  #   sha256 = "0aaaa4b58a3e773429217e244b154ba3a997a2b52a1d06f81ed523e82ef40271";
  #   url = "https://s3.amazonaws.com/fractalsoftworks/starsector/starsector_linux-0.9.1a-RC8.zip";
  # };

  nativeBuildInputs = [
    unzip
    makeWrapper
  ];
  buildInputs = with xorg; [
    alsaLib
    at-spi2-atk
    cairo
    fontconfig
    freetype
    gdk-pixbuf
    glib.out
    gtk2-x11
    libav
    libX11
    libXcursor
    libXext
    libxml2
    libXrandr
    libxslt
    libXtst
    libXxf86vm
    openal
    openjdk8.out
    pango
    stdenv.cc.cc
  ];

  doBuild = false;

  unpackPhase = ''
    unzip $src
  '';

  # need to cd into $out in order for classpath to pick up correct jar files
  installPhase = ''
    mkdir -p $out/bin
    cp -r ./starsector/* $out
    rm -r $out/jre_linux # remove jre7

    wrapProgram $out/starsector.sh \
      --prefix PATH : ${openjdk8}/bin \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath buildInputs} \
      --set XDG_DATA_HOME "/home/mhuber/.local/share" \
      --run "mkdir -p \$XDG_DATA_HOME/starsector; cd $out"
    ln -s $out/starsector.sh $out/bin/starsector
  '';

  # it tries to run everything with relative paths, which makes it CWD dependent
  # also point mod, screenshot, and save directory to $XDG_DATA_HOME
  prePatch = ''
    substituteInPlace starsector/starsector.sh \
      --replace "./jre_linux/bin/java" "${openjdk8}/bin/java" \
      --replace "./native/linux" "$out/native/linux" \
      --replace "./" "\$XDG_DATA_HOME/starsector/"

    substituteInPlace starsector/data/config/settings.json \
      --replace "allowAnyJavaVersion\":false" "allowAnyJavaVersion\":true"
  '';

  meta = with lib; {
    homggepage = "https://fractalsoftworks.com/";
    description = "Starsector (formerly “Starfarer”) is an in-development open-world single-player space-combat, roleplaying, exploration, and economic game.";
    platforms = platforms.linux;
    maintainers = with maintainers; [ ];
  };
}
