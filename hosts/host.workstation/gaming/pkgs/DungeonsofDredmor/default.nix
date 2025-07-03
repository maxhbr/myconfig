{
  stdenv,
  unzip,
  requireFile,
  autoPatchelfHook,
  gdk-pixbuf,
  gtk2-x11,
  steam-run,
}:

stdenv.mkDerivation rec {
  name = "DungeonsofDredmor-${version}";
  version = "1389995827";

  src = ./DungeonsofDredmor_Complete_linux_1389995827.tar.gz;

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
    mkdir -p dod
    tar -xf $src -C dod
  '';

  installPhase = ''
    mkdir -p $out/share
    ls
    cp -r dod/* $out/share
    mkdir -p $out/bin
    cat <<EOF >$out/bin/dod
    #!/bin/sh
    steam-run $out/share/Dredmor
    EOF
    chmod +x $out/bin/dod
  '';

  meta = with pkgs.lib; {
    homggepage = "http://www.bombmanual.com/";
    description = "Keep Talking and Nobody Explodes";
    platforms = platforms.linux;
    maintainers = with maintainers; [ ];
  };
}
