# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
funs: pkgs: let
  callPackage = funs.lib.callPackageWith pkgs;

  background = callPackage ./background {
    inherit pkgs;
  };
  slim-theme = callPackage ./background/slim-theme {
    inherit background pkgs;
  };
  scripts = callPackage ./scripts {
    inherit background pkgs;
  };
  my-xmonad = funs.haskellPackages.callPackage ./xmonad {
    inherit scripts pkgs;
  };

  photo-scripts = callPackage ./photo-scripts {
    inherit pkgs;
  };
in {
  myconfig = {
    nixos-config = import ./nixos;
    inherit
      scripts
      my-xmonad
      background
      slim-theme;
    myconfig-all = funs.buildEnv {
      name = "myconfig-all";
      paths = [scripts my-xmonad background slim-theme];
      pathsToLink = [ "/share" "/bin" ];
    };
  };
  inherit photo-scripts;
}
