{
  lib,
  stdenv,
  unzip,
  requireFile,
  autoPatchelfHook,
  gdk-pixbuf,
  gtk2-x11,
  steam-run,
}:

stdenv.mkDerivation rec {
  name = "ktane-${version}";
  version = "1.8.3";

  src = ./Keep_Talking_and_Nobody_Explodes_1.8.3_-_StandaloneLinuxUniversal_Default.zip;

  nativeBuildInputs = [
    unzip
    autoPatchelfHook
  ];

  buildInputs = [
    gtk2-x11
    gdk-pixbuf
    stdenv.cc.cc.lib
    steam-run
  ];

  unpackPhase = ''
    unzip $src
  '';

  installPhase = ''
    cd "Keep Talking and Nobody Explodes"
    mkdir -p $out/share
    cp -r ktane_Data ktane.x86_64 $out/share
    chmod +x $out/share/ktane.x86_64
    mkdir -p $out/bin
    cat <<EOF >$out/bin/ktane
    #!/bin/sh
    steam-run $out/share/ktane.x86_64
    EOF
    chmod +x $out/bin/ktane
  '';

  meta = with lib; {
    homggepage = "http://www.bombmanual.com/";
    description = "Keep Talking and Nobody Explodes";
    platforms = platforms.linux;
    maintainers = with maintainers; [ ];
  };
}
