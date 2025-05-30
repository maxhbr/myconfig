{ stdenv, requireFile, pkgs, autoPatchelfHook, alsa-lib, freetype, curl, glibc
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
in stdenv.mkDerivation {
  pname = "wing-edit";
  inherit version src;
  buildInputs = [ glibc alsa-lib freetype curl stdenv.cc.cc.lib ];
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
