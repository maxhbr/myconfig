{ stdenv, fetchgit, jdk11, gradleGen, pkgconfig, perl, yarn, nodejs-12_x }:

# based on https://github.com/abbradar/nixpkgs/blob/784cf70d384adaf1fea8028d1fd98d0d6ee5cfad/pkgs/development/compilers/openjdk/openjfx/11.nix

let
  gradle_ = (gradleGen.override {
    java = jdk11;
  }).gradle_6_8;

  makePackage = args: stdenv.mkDerivation ({
    version = "master_76986516f8d72ff5aa343cd8eaf565c3b97531b4";

    src = fetchgit {
      url = "https://github.com/oss-review-toolkit/ort";
      rev = "76986516f8d72ff5aa343cd8eaf565c3b97531b4";
      sha256 = "17f4g6xw5ipj36p9pnyk9mksiq12aqqrb2k6vzi33cahwyjrmr4i";
      leaveDotGit = true;
      fetchSubmodules = true;
      deepClone = true;
    };

    buildInputs = [ yarn nodejs-12_x ];
    nativeBuildInputs = [ gradle_ perl pkgconfig yarn nodejs-12_x ];

    dontUseCmakeConfigure = true;

    buildPhase = ''
      runHook preBuild

      export JAVA_HOME="${jdk11}/lib/openjdk"
      export HOME=$(mktemp -d)
      export XDG_CONFIG_HOME=$HOME
      export GRADLE_USER_HOME=$(mktemp -d)
      mkdir -p "''${GRADLE_USER_HOME}/nodejs"
      ln -s "${nodejs-12_x}" "''${GRADLE_USER_HOME}/nodejs/node-v12.16.1-linux-x64"

      gradle --no-daemon $gradleFlags installDist

      runHook postBuild
    '';
  } // args);

  # Fake build to pre-download deps into fixed-output derivation.
  # We run nearly full build because I see no other way to download everything that's needed.
  # Anyone who knows a better way?
  deps = makePackage {
    pname = "oss-review-toolkit-deps";

    # perl code mavenizes pathes (com.squareup.okio/okio/1.13.0/a9283170b7305c8d92d25aff02a6ab7e45d06cbe/okio-1.13.0.jar -> com/squareup/okio/okio/1.13.0/okio-1.13.0.jar)
    installPhase = ''
      echo "#!/bin/sh" > $out/installScript.sh
      echo "set +e" >> $out/installScript.sh
      find $GRADLE_USER_HOME -type f -regex '.*\.\(jar\|pom\)' \
        | perl -pe 's#(.*/([^/]+)/([^/]+)/([^/]+)/[0-9a-f]{30,40}/([^/\s]+))$# ($x = $2) =~ tr|\.|/|; "install -Dm444 $1 \$out/$x/$3/$4/$5" #e' | tee -a $out/installScript.sh
      chmod +x $out/installScript.sh
      $out/installScript.sh || true
      cp -r $GRADLE_USER_HOME $out/GRADLE_USER_HOME
      rm -rf $out/tmp
    '';


    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash =
      # Downloaded AWT jars differ by platform.
      if stdenv.system == "x86_64-linux" then "165djgva1lkd2fgl4y82kl4hplz1k5dz4il92b5dbb8wipqdwdzq"
      else if stdenv.system == "i686-linux" then throw "Unsupported platform"
      else throw "Unsupported platform";
  };


in makePackage {
  pname = "oss-review-toolkit";

  preBuild = ''
    substituteInPlace build.gradle.kts \
      --replace 'mavenCentral()' 'mavenLocal(); maven { url uri("${deps}") }'
  '';

  installPhase = ''
    cp -r ./cli/build/install/ort/* $out
  '';

  stripDebugList = [ "." ];

  postFixup = ''
    # Remove references to bootstrap.
    find "$out" -name \*.so | while read lib; do
      new_refs="$(patchelf --print-rpath "$lib" | sed -E 's,:?${jdk11}[^:]*,,')"
      patchelf --set-rpath "$new_refs" "$lib"
    done
    # Test to make sure that we don't depend on the bootstrap
    if grep -q -r '${jdk11}' "$out"; then
      echo "Extraneous references to ${jdk11} detected" >&2
      exit 1
    fi
  '';

  passthru.deps = deps;

  meta = with stdenv.lib; {
    homepage = https://github.com/oss-review-toolkit/ort;
    license = "Apache-2.0";
    description = "The OSS Review Toolkit (ORT) aims to assist with the tasks that commonly need to be performed in the context of license compliance checks, especially for (but not limited to) Free and Open Source Software dependencies.";
    maintainers = with maintainers; [ ];
    # platforms = ;
  };
}
