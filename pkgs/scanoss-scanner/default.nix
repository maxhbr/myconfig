{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv,
  fetchurl,
  autoPatchelfHook,
  dpkg,
  makeWrapper,
  glibc,
  gcc-unwrapped,
  openssl
}:
stdenv.mkDerivation rec {
  version = "1.01";
  name = "scanoss-scanner-${version}";

  system = "x86_64-linux";
  src = fetchurl {
    url = "https://github.com/scanoss/scanner.c/raw/master/scanoss-scanner-${version}_amd64.deb";
    sha256 = "149lxj3rgh9bp6m7av73f6i9qgwfi6ja5h45069cdzsp2q3wqd79";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    dpkg
    makeWrapper
  ];

  buildInputs = [
    glibc
    gcc-unwrapped
    openssl
  ];

  unpackPhase = "true";

  installPhase = ''
    dpkg -x $src $out
    mkdir -p "$out/bin"
    mv "$out/usr/bin/scanner" "$out/bin/.scanner"
    rm -r "$out/usr"
    makeWrapper "$out/bin/.scanner" "$out/bin/scanoss-scanner"
  '';

  meta = with stdenv.lib; {
    description = "This is a simple implementation of a console file scanner using the SCANOSS OSSKB, which allows you to perform identification of Open Source components, files or even snippets in your own code.";
    homepage = https://github.com/scanoss/scanner.c;
    license = licenses.gpl2;
    maintainers = with stdenv.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
