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
      nixosModule = {
        config.environment.systemPackages =
          [ self.defaultPackage.x86_64-linux ];
      };

      packages.x86_64-linux = {
        ort = #pkgs.callPackage ./oss-review-toolkit-ort { };
            pkgs.writeScriptBin "ort.sh"
              (builtins.readFile ./oss-review-toolkit-ort/ort.sh);

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

              collectMetadataForFiles() (
                  local input="$1"; shift
                  local output="$1/cmff"; shift
                  if [[ ! -d "$input" ]]; then
                      return
                  fi
                  if [[ -d "$output" ]]; then
                      return
                  fi

                  cd "$input"

                  find . -type f -print0 |
                      while IFS= read -r -d "" file; do
                          fileDir="$output/$file"
                          (set +e
                           mkdir -p "$fileDir"
                           md5sum "$file" > "$fileDir/md5sum"
                           sha1sum "$file" > "$fileDir/sha1sum"
                           sha256sum "$file" > "$fileDir/sha256sum"
                           sha512sum "$file" > "$fileDir/sha512sum"
                           file "$file" > "$fileDir/file"
                           wc "$file" > "$fileDir/wc"
                           du -h "$file" > "$fileDir/du"
                           ${pkgs.exiftool}/bin/exiftool "$file" > "$fileDir/exiftool"
                          )
                      done
                  find . -type d -print0 |
                      while IFS= read -r -d "" dir; do
                          dirDir="$output/$dir"
                          (set +e
                           mkdir -p "$fileDir"
                           ls -alF $dir > "$dirDir/ls"
                           # TODO: cloc every dir?
                          )
                      done
              )

              main() {
                  local input="$1"
                  local out="$(getOutFolder "$input")"
                  local sourceDir="$(getSourceDir "$input")"

                  {
                      ${pkgs.cloc}/bin/cloc "$input" > "$out/original.cloc"
                      ${pkgs.cloc}/bin/cloc "$sourceDir" > "$out/extracted.cloc"
                      collectMetadataForFiles "$sourceDir" "$out"
                      ${scancode}/bin/scancode.sh "$sourceDir" || true
                      ${ort}/bin/ort.sh all "$sourceDir" || true
                      if [[ -f "''${sourceDir}_ort/analyzer-result.yml" ]]; then
                          ${ort}/bin/ort.sh list-packages "''${sourceDir}_ort/analyzer-result.yml" > "''${sourceDir}_ort/packages" || true
                      fi
                      ${tern}/bin/tern-docker.sh "$sourceDir" || true
                  } | tee -a "$out/log"
              }

              for dir in "$@"; do
                  main "$dir"
              done
            '')
          ];
        };
      };

      defaultPackage.x86_64-linux =
        self.packages.x86_64-linux.license-compliance-toolbox;
    };
}
