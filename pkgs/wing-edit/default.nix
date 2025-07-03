{
  stdenv,
  requireFile,
  pkgs,
  autoPatchelfHook,
  alsa-lib,
  freetype,
  curl,
  glibc,
}:
let
  version = "3.2.1";
  # Run
  # $ nix-prefetch-url --type sha256 file:///<path/to/Wing-Edit_LINUX_${version}.tar.gz>
  src = requireFile {
    name = "Wing-Edit_LINUX_${version}.tar.gz";
    url = "https://www.behringer.com/downloads.html";
    sha256 = "0w6xixppldxhh2nmqzjqr01xjg4dbpkank2kzkcckbr6gb6dvfb6";
  };
in
stdenv.mkDerivation {
  pname = "wing-edit";
  inherit version src;
  buildInputs = [
    glibc
    alsa-lib
    freetype
    curl
    stdenv.cc.cc.lib
  ];
  nativeBuildInputs = [ autoPatchelfHook ];

  sourceRoot = ".";
  installPhase = ''
    runHook preInstall
    install -Dm755 WING-Edit "$out/bin/WING-Edit"
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    homepage = "https://www.behringer.com/";
    description = "WING-Edit";
    # license = licenses.tbd;
    platforms = platforms.linux;
    maintainers = [ ];
  };
}

# { lib, stdenv, requireFile, pkgs, makeWrapper, alsa-lib, freetype, curl, glibc, zlib
# }:
# let
#   version = "3.2.1";
#   # Run
#   # $ nix-prefetch-url --type sha256 file:///<path/to/Wing-Edit_LINUX_${version}.tar.gz>
#   src = requireFile {
#     name = "Wing-Edit_LINUX_${version}.tar.gz";
#     url = "https://www.behringer.com/downloads.html";
#     sha256 = "0w6xixppldxhh2nmqzjqr01xjg4dbpkank2kzkcckbr6gb6dvfb6";
#   };
#   loader  = "${glibc.out}/lib/ld-linux-x86-64.so.2";
#   libPath = lib.makeLibraryPath [ glibc alsa-lib freetype curl zlib stdenv.cc.cc.lib ];
# in stdenv.mkDerivation {
#   pname = "wing-edit";
#   inherit version src;
#   buildInputs = [ glibc alsa-lib freetype curl stdenv.cc.cc.lib ];
#   # nativeBuildInputs = [ autoPatchelfHook ];
#   nativeBuildInputs = [ makeWrapper ];

#   sourceRoot = ".";
#   installPhase = ''
#     # runHook preInstall
#     # install -Dm755 WING-Edit "$out/bin/WING-Edit"
#     # runHook postInstall
#     runHook preInstall
#     mkdir -p $out/libexec
#     install -Dm755 WING-Edit $out/libexec/WING-Edit

#     # create a tiny launcher in $out/bin
#     makeWrapper ${loader} $out/bin/WING-Edit \
#       --add-flags "$out/libexec/WING-Edit" \
#       --prefix LD_LIBRARY_PATH : "${libPath}"
#     runHook postInstall
#   '';

#   meta = with pkgs.lib; {
#     homepage = "https://www.behringer.com/";
#     description = "Offline editor / remote control for the Behringer WING console";
#     license     = licenses.unfree;
#     platforms = platforms.linux;
#     maintainers = [ ];
#   };
# }
