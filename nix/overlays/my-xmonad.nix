# Copyright 2017-2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
funs: pkgs: {
  my-xmonad = funs.haskellPackages.callPackage ../../xmonad {
    inherit pkgs;
  };
}
