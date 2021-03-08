{
  description = "license-compliance-toolbox flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    ort = {
      url = "github:oss-review-toolkit/ort";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {

      packages.x86_64-linux = {
        ort = pkgs.callPackage ./oss-review-toolkit-ort { };
        scancode = pkgs.callPackage ./nexB-scancode-toolkit { };
        tern = pkgs.callPackage ./tern-tools-tern { };
        scanoss = pkgs.callPackage ./scanoss-scanner { };
        license-compliance-toolbox = pkgs.buildEnv {
          name = "license-compliance-toolbox";
          paths = with self.packages.x86_64-linux; [
            ort
            scancode
            tern
            scanoss
            (pkgs.writeScriptBin "fossology.sh"
              (builtins.readFile ./fossology.sh))
            (pkgs.writeScriptBin "dependencytrac.sh"
              (builtins.readFile ./dependencytrac.sh))

            (pkgs.writeShellScriptBin "tdd.sh" ''
            getOutFolder() {
                local input="$1"
                local workdir="$(readlink -f "$input")"
                local out="''${workdir%_tdd}_tdd"
                mkdir -p "$out"
                echo "$out"
            }

            getSourceDir() {
                local input="$1"
                local out="$(getOutFolder "$input")"
                local source="$out/$(basename "$input")"
                if [[ -d "$input" ]]; then
                    cp -r "$input" "$source"
                    (>&2 ${scancode}/bin/scancode.sh -ex "$source")
                fi
                echo "$source"
            }

            main() {
                local input="$1"
                local out="$(getOutFolder "$input")"
                local sourceDir="$(getSourceDir "$input")"

                ${scancode}/bin//scancode.sh "$sourceDir" || true
                ${ort}/bin/ort.sh all "$sourceDir" || true
            }

            main "$@"
          '')
          ];
        };
      };

      defaultPackage.x86_64-linux =
        self.packages.x86_64-linux.license-compliance-toolbox;
    };
}
