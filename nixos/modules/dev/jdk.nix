# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  gmvn = with pkgs; writeScriptBin "gmvn" ''
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
exec ${pkgs.mvn}/bin/mvn \
  -Dmaven.repo.local="$repo" \
  "$@"
  '';
in
{
  imports = [
    ./common.nix
  ];
  config = {
    # generates:
    # - /run/booted-system/pkgs/openjdk8
    # - /run/current-system/pkgs/openjdk8
    # - /nix/var/nix/profiles/system/pkgs/openjdk8
    # - ...
    # see: https://discourse.nixos.org/t/always-symlinking-the-latest-jdk-to-a-certain-path/3099/6
    system.extraSystemBuilderCmds = ''
      mkdir -p $out/pkgs/
      ln -s ${pkgs.openjdk8} $out/pkgs/openjdk8
      ln -s ${pkgs.openjdk11} $out/pkgs/openjdk11
      ln -s ${pkgs.openjdk12} $out/pkgs/openjdk12
    '';

    nixpkgs.overlays = [(self: super: {
      maven = super.maven.override {
        jdk = self.openjdk11;
      };
    })];

    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        openjdk openjdk8 openjdk11 openjdk12
        maven gradle
        gmvn
      ];
    };
    environment = {
      variables = {
        JAVA_8_HOME = "/run/current-system/pkgs/openjdk8/lib/openjdk";
        JAVA_11_HOME = "/run/current-system/pkgs/openjdk11/lib/openjdk";
        JAVA_HOME = "/run/current-system/pkgs/openjdk11/lib/openjdk";
        JAVA_12_HOME = "/run/current-system/pkgs/openjdk12/lib/openjdk";
      };
    };
  };
}
