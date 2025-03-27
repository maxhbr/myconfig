{ stdenv, requireFile, pkgs, autoPatchelfHook, alsa-lib, freetype, curl, glibc
}:
let
  version = "3.1";
  #  Run `nix store add-file <path/to/Wing-Edit_LINUX_${version}.tar.gz>`, get the output path. Use that output in
  # `nix hash path <output path of nix store add-file>`
  # Paste that into the hash line below.
  src = requireFile {
    name = "Wing-Edit_LINUX_${version}.tar.gz";
    url = "https://www.behringer.com/downloads.html";
    sha256 = "1dyvm7k16j564vjgcfrygxy7qs485s11i1ljkgn792lylazwl4fy";
    # hash = "sha256-tP8vOduKwCfNkXoiaDH0arVywDc96cED/iFhPh06hos=";
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
