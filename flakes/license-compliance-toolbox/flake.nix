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
                           echo "$file" >> "$output/_files"
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

              findDefinitionFiles() (
                  local input="$1"; shift
                  local output="$1/definitionFiles"; shift
                  mkdir -p "$output"
                  if [[ -d "$output" ]]; then
                      return
                  fi

                  set -o noglob

                  find . -iname "go.mod" -exec bash -c "echo '{}' >> '$output/GoMod'" \;
                  find . -iname "bower.json" -exec bash -c "echo '{}' >> '$output/Bower'" \;
                  find . -iname "Cargo.toml" -exec bash -c "echo '{}' >> '$output/Cargo'" \;
                  find . -iname "Gemfile" -exec bash -c "echo '{}' >> '$output/Bundler'" \;
                  find . -iname "Podfile" -exec bash -c "echo '{}' >> '$output/CocoaPods'" \;
                  find . -iname "Cartfile.resolved" -exec bash -c "echo '{}' >> '$output/Carthage'" \;
                  find . \( -iname "build.gradle" -o -iname "build.gradle.kts" -o -iname "settings.gradle" -o -iname "settings.gradle.kts" \) -exec bash -c "echo '{}' >> '$output/Gradle'" \;
                  find . -iname "pom.xml" -exec bash -c "echo '{}' >> '$output/Maven'" \;
                  find . -iname "package.json" -exec bash -c "echo '{}' >> '$output/Npm'" \;
                  find . \( -iname "conanfile.txt" -o -iname "conanfile.py" \) -exec bash -c "echo '{}' >> '$output/Conan'" \;
                  find . \( -iname "*requirements*.txt" -o -iname "setup.py" \) -exec bash -c "echo '{}' >> '$output/Pip'" \;
                  find . \( -iname "build.sbt" -o -iname "build.scala" \) -exec bash -c "echo '{}' >> '$output/Sbt'" \;
                  find . -iname "pubspec.yaml" -exec bash -c "echo '{}' >> '$output/Pub'" \;
                  find . -iname "packages.config" -exec bash -c "echo '{}' >> '$output/NuGet'" \;
                  find . -iname "Gopkg.toml" -exec bash -c "echo '{}' >> '$output/GoDep'" \;
                  find . \( -iname "*.spdx" -o -iname "*.spdx.rdf" -o -iname "*.spdx.yml" -o -iname "*.spdx.yaml" -o -iname "*.spdx.json" \) -exec bash -c "echo '{}' >> '$output/SpdxDocumentFile'" \;
                  find . -iname "stack.yaml" -exec bash -c "echo '{}' >> '$output/Stack'" \;
                  find . -iname "Pipfile.lock" -exec bash -c "echo '{}' >> '$output/Pipenv'" \;
                  find . -iname "composer.json" -exec bash -c "echo '{}' >> '$output/Composer'" \;
                  find . \( -iname "*.csproj" -o -iname "*.fsproj" -o -iname "*.vcxproj" \) -exec bash -c "echo '{}' >> '$output/DotNet'" \;
                  find . -iname "package.json" -exec bash -c "echo '{}' >> '$output/Yarn'" \;
              )

              main() {
                  local input="$1"
                  local out="$(getOutFolder "$input")"
                  local sourceDir="$(getSourceDir "$input")"

                  {
                      findDefinitionFiles "$sourceDir" "$out"
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
