{ stdenv, lib, fetchurl, makeWrapper, buildFHSUserEnv }:

let
  fhsEnv = buildFHSUserEnv {
    name = "esp32-toolchain-env";
    targetPkgs = pkgs: with pkgs; [ zlib ];
    runScript = "";
  };
in

stdenv.mkDerivation rec {
  name = "esp32-toolchain";
  version = "2020r3";

  src = fetchurl {
    url = "https://dl.espressif.com/dl/xtensa-esp32-elf-gcc8_4_0-esp-2020r3-linux-amd64.tar.gz";
    sha256 = "Z0CAoS+cXr5aOlzlHG3q7/5t+wbWQWIz34byW1dOknk=";
  };

  buildInputs = [ makeWrapper ];

  phases = [ "unpackPhase" "installPhase" ];

  installPhase = ''
    cp -r . $out
    for FILE in $(ls $out/bin); do
      FILE_PATH="$out/bin/$FILE"
      if [[ -x $FILE_PATH ]]; then
        mv $FILE_PATH $FILE_PATH-unwrapped
        makeWrapper ${fhsEnv}/bin/esp32-toolchain-env $FILE_PATH --add-flags "$FILE_PATH-unwrapped"
      fi
    done
  '';

  meta = with lib; {
    description = "ESP32 toolchain";
    homepage = https://docs.espressif.com/projects/esp-idf/en/stable/get-started/linux-setup.html;
    license = licenses.gpl3;
  };
}
