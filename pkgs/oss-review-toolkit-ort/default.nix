{
  lib,
  stdenv,
  fetchgit,
  jdk11,
  gradleGen,
  nodejs-12_x,
  makeWrapper,
  # runtime requirements for ort
  git,
  mercurial,
  cvs,
  licensee,
  ruby,
  python3,
  python3Packages,
}:

let
  rev = "fa95a4eb8ec1d21c59ad4144575b0f219ad192be";
  srcFixedOutputSha256 = "0pmICW0OeFzmQaSKruWJJF3XSLUibTWjhpXsKkNKD+E=";
  installFixedOutputSha256 = "1wgzwvghh6nws7nvfivpqp6j9nfr0sig996690wmsiw1b0zvvpny";

  gradle_ = (gradleGen.override { java = jdk11; }).gradle_6_8;

  version = "master_${rev}";

  install = stdenv.mkDerivation {
    pname = "oss-review-toolkit-install";
    inherit version;

    src = fetchgit {
      url = "https://github.com/oss-review-toolkit/ort";
      inherit rev;
      sha256 = srcFixedOutputSha256;
      leaveDotGit = true;
      fetchSubmodules = true;
      deepClone = true;
    };

    nativeBuildInputs = [ gradle_ ];

    dontUseCmakeConfigure = true;

    buildPhase = ''
      runHook preBuild

      export GRADLE_USER_HOME=$(mktemp -d)
      export XDG_CONFIG_HOME=$GRADLE_USER_HOME/.config
      mkdir -p "''${GRADLE_USER_HOME}/nodejs"
      ln -s "${nodejs-12_x}" "''${GRADLE_USER_HOME}/nodejs/node-v12.16.1-linux-x64"

      gradle --no-daemon $gradleFlags installDist

      runHook postBuild
    '';

    installPhase = ''
      cp -r ./cli/build/install/* $out
    '';

    outputHashAlgo = "sha256";
    outputHashMode = "recursive";
    outputHash =
      if stdenv.system == "x86_64-linux" then installFixedOutputSha256 else throw "Unsupported platform";
  };

in
stdenv.mkDerivation {
  pname = "oss-review-toolkit";
  inherit version;

  src = install;

  buildInputs = [ makeWrapper ];

  buildPhase = ''
    cp ${./ort.sh} ./bin/ort.sh
    sed -i -e 's%=ort%='"$out/bin/ort"'%' ./bin/ort.sh
    rm ./bin/ort.bat
  '';
  installPhase = ''
    mkdir -p $out
    cp -r ./* $out
    wrapProgram "$out/bin/ort" \
      --set LANG en_US.UTF-8 \
      --prefix PATH ":" "${git}/bin" \
      --prefix PATH ":" "${mercurial}/bin" \
      --prefix PATH ":" "${cvs}/bin" \
      --prefix PATH ":" "${licensee}/bin" \
      --prefix PATH ":" "${python3}/bin" \
      --prefix PATH ":" "${python3Packages.virtualenv}/bin"
      # --prefix PATH : "''${lib.makeBinPath [ git mercurial cvs licensee ruby python3 python3Packages ]}"
  '';

  stripDebugList = [ "." ];

  passthru.deps = install;

  meta = with lib; {
    homepage = "https://github.com/oss-review-toolkit/ort";
    license = "Apache-2.0";
    description = "The OSS Review Toolkit (ORT) aims to assist with the tasks that commonly need to be performed in the context of license compliance checks, especially for (but not limited to) Free and Open Source Software dependencies.";
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
  };
}
