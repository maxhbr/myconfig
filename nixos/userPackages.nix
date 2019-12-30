# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on: https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb
{ config, pkgs, lib, ... }:

{
  options = {
    userPackages = with lib; mkOption {
      description = "The packages to be installed in user env";
      type = types.listOf types.package;
      default = [];
      example = [];
    };
  };
  config = {
    nixpkgs.overlays = [
      (self: super: {
        userPackages = super.buildEnv {
          name = "userPackages";
          extraOutputsToInstall = ["out" "bin" "lib"];
          paths = config.userPackages ++ [
            self.myconfig.scripts

            # To allow easily seeing which nixpkgs version the profile was built from, place the version string in ~/.nix-profile/nixpkgs-version
            (super.writeTextFile {name = "nixpkgs-version"; destination = "/nixpkgs-version"; text = self.lib.version;})
            (super.writeTextFile {name = "nixpkgs-unstable-version"; destination = "/nixpkgs-unstable-version"; text = self.unstable.lib.version;})

            # Manifest to make sure imperative nix-env doesn't work (otherwise it will overwrite the profile, removing all packages other than the newly-installed one).
            (super.writeTextFile {
              name = "break-nix-env-manifest";
              destination = "/manifest.nix";
              text = ''
                throw \'\'
                  Your user environment is a buildEnv which is incompatible with
                  nix-env's built-in env builder. Edit your home expression and run
                  update-profile instead!
                \'\'
              '';
            })
          ];
        };
      })
    ];
    environment.systemPackages = with pkgs; [ userPackages ];
  };
}

