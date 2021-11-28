# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  gmvn = with pkgs;
    writeScriptBin "gmvn" ''
      #!${stdenv.shell}

      # is a substitute for `mvn`, but creates a local repository for each git project
      # if there is no git repo, it falls back to repo in current folder

      repo="$(${pkgs.git}/bin/git rev-parse --show-toplevel)/.m2/repository"
      RESULT=$?
      set -e
      if [[ $RESULT -eq 0 ]]; then
        repo="$(pwd)/.m2/repository"
      fi

      echo "repo is: $repo"
      exec ${pkgs.maven}/bin/mvn \
        -Dmaven.repo.local="$repo" \
        "$@"
        '';
in {
  config = {
    nixpkgs.overlays = [
      (self: super:
        let defaultJdk = self.openjdk11;
        in {
          maven = super.maven.override { jdk = defaultJdk; };
          # gradle = (pkgs.gradleGen.override { java = defaultJdk; }).gradle_latest;
          jdk = defaultJdk;
        })
    ];

    # generates:
    # - /run/booted-system/pkgs/openjdk8
    # - /run/current-system/pkgs/openjdk8
    # see: https://discourse.nixos.org/t/always-symlinking-the-latest-jdk-to-a-certain-path/3099/6
    system.extraSystemBuilderCmds = ''
      mkdir -p $out/pkgs/

      ln -s ${pkgs.maven} $out/pkgs/maven

      ln -s ${pkgs.openjdk8_headless} $out/pkgs/openjdk8
      ln -s ${pkgs.openjdk11} $out/pkgs/openjdk11
    '';

    environment = {
      systemPackages = with pkgs; [ jdk maven gradle gmvn ];
      shellAliases = { mvnDebug = "${pkgs.maven}/maven/bin/mvnDebug"; };
      variables = {
        JAVA_8_HOME = "/run/current-system/pkgs/openjdk8/lib/openjdk";
        JAVA_11_HOME = "/run/current-system/pkgs/openjdk11/lib/openjdk";
        JAVA_HOME = "/run/current-system/pkgs/openjdk11/lib/openjdk";
      };
    };
  };
}
