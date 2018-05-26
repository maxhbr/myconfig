# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
funs: pkgs: let
  callPackage = funs.lib.callPackageWith pkgs;

  scripts = callPackage ./scripts {
    inherit background pkgs;
  };
  my-xmonad = funs.haskellPackages.callPackage ./xmonad {
    inherit pkgs scripts;
    my-xmonad-misc = callPackage ./xmonad/misc.nix { inherit pkgs; };
    find-cursor = callPackage ./xmonad/find-cursor.nix { inherit pkgs; };
  };
  background = callPackage ./background { inherit pkgs; };
  slim-theme = callPackage ./background/slim-theme {
    inherit background pkgs;
  };
in {
  myconfig = {
    inherit scripts my-xmonad background slim-theme;
    all = funs.buildEnv {
      name = "myconfig-all";
      paths = [scripts my-xmonad background slim-theme];
      pathsToLink = [ "/share" "/bin" ];
    };
    nixos-config = import ./nixos;
  };
}
