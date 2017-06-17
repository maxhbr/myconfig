{stdenv, pkgs, fetchurl}:
pkgs.modemmanager.overrideDerivation (super: rec {
  name = "ModemManager-${version}";
  version = "1.6.6";
  src = fetchurl {
    url = "http://www.freedesktop.org/software/ModemManager/${name}.tar.xz";
    sha256 = "0hwalp5ixg4b1qdj3ni61nkvjlxclb7zw7j548pwpmqdkq6sll5h";
  };
})
