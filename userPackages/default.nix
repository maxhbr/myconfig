# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on: https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb

{ pkgs ? import <nixpkgs> {}
, name ? "user-env"
}: with pkgs;
buildEnv {
  inherit name;
  extraOutputsToInstall = ["out" "bin" "lib"];
  paths = (
    let
      path = ./roles;
      content = builtins.readDir path;
    in
      # todo: add filter depending on the roles
      lib.foldr (m1: m2: m1 ++ m2)
                []
                (map (n: import (path + ("/" + n)) pkgs)
                     (builtins.filter (n: builtins.match ".*\\.nix" n != null)
                                      (builtins.attrNames content)))
  ) ++ [
    myconfig.scripts
    myconfig.background

    # To allow easily seeing which nixpkgs version the profile was built from, place the version string in ~/.nix-profile/nixpkgs-version
    (writeTextFile {name = "nixpkgs-version"; destination = "/nixpkgs-version"; text = lib.version;})
    (writeTextFile {name = "nixpkgs-unstable-version"; destination = "/nixpkgs-unstable-version"; text = unstable.lib.version;})
    # Manifest to make sure imperative nix-env doesn't work (otherwise it will overwrite the profile, removing all packages other than the newly-installed one).
    (writeTextFile {
      name = "break-nix-env-manifest";
      destination = "/manifest.nix";
      text = ''
        throw ''\''
          Your user environment is a buildEnv which is incompatible with
          nix-env's built-in env builder. Edit your home expression and run
          update-profile instead!
        ''\''
      '';
    })
  ];
}
