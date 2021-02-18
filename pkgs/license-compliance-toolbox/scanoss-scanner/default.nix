{ pkgs ? import <nixpkgs> { }, stdenv ? pkgs.stdenv, fetchurl, autoPatchelfHook
, dpkg, makeWrapper, glibc, gcc-unwrapped, openssl, jq, ncurses }:
let
  scanoss-dir = pkgs.writeShellScriptBin "scanoss-dir" ''
    set -euo pipefail

    run() {
        local file="$1"

        for counter in {1..9}; do
            if [[ $counter -eq 1 ]]; then
                (>&2 echo "[$counter] $file")
            else
                (>&2 echo "$(${ncurses}/bin/tput bold)$(${ncurses}/bin/tput setaf 1)[$counter] $file$(${ncurses}/bin/tput sgr0)")
                sleep $(( $counter / 3 ))
            fi
            local result=$(scanoss-scanner "$file" | sed '/^[[:digit:]]*[[:space:]]*$/d' | awk 1 RS='\r\n' ORS=)

            if ${jq}/bin/jq -e . >/dev/null 2>&1 <<<"$result"; then
                echo "$result"
                return
            fi
        done
        (>&2 echo "... failed")
        cat <<EOF
    {
      "$file": []
    }
    EOF
    }

    main() {
        local workdir="$(readlink -f "$1")"
        if [[ ! -d "$workdir" ]]; then
            echo "the folder workdir=$workdir does not exist"
            exit 1
        fi

        (cd "$workdir";
         find . -type f \
             -not -empty \
             -not -path '*/\.git/*' \
             -not -path '*/\.svn/*' |
             while read file; do
                 run "$file"
             done  |
             tee "''${workdir}_sca.raw.json" |
             ${jq}/bin/jq -n '[inputs] | add' > "''${workdir}_sca.json"
          rm "''${workdir}_sca.raw.json"
        )
    }

    main "$@"
    times
      '';
in stdenv.mkDerivation rec {
  version = "1.01";
  name = "scanoss-scanner-${version}";

  system = "x86_64-linux";
  src = fetchurl {
    url =
      "https://github.com/scanoss/scanner.c/raw/master/scanoss-scanner-${version}_amd64.deb";
    sha256 = "149lxj3rgh9bp6m7av73f6i9qgwfi6ja5h45069cdzsp2q3wqd79";
  };

  nativeBuildInputs = [ autoPatchelfHook dpkg makeWrapper ];

  buildInputs = [ glibc gcc-unwrapped openssl ];

  unpackPhase = "true";

  installPhase = ''
    dpkg -x $src $out
    mkdir -p "$out/bin"
    mv "$out/usr/bin/scanner" "$out/bin/.scanner"
    rm -r "$out/usr"
    makeWrapper "$out/bin/.scanner" "$out/bin/scanoss-scanner"
    cp "${scanoss-dir}/bin/scanoss-dir" "$out/bin/"
    sed -i -e 's%scanoss-scanner%'"$out"'/bin/scanoss-scanner%g' "$out/bin/scanoss-dir"
  '';

  meta = with pkgs.lib; {
    description =
      "This is a simple implementation of a console file scanner using the SCANOSS OSSKB, which allows you to perform identification of Open Source components, files or even snippets in your own code.";
    homepage = "https://github.com/scanoss/scanner.c";
    license = licenses.gpl2;
    maintainers = with pkgs.lib.maintainers; [ ];
    platforms = [ "x86_64-linux" ];
  };
}
