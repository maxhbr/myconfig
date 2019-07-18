# Copyright 2018 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
self: super: let
  pass = super.pass.overrideDerivation ( drv: {
    # should work for 1.7.3
    patches = drv.patches ++ [ ./patches/pass_-_copy_by_default.diff ];
    doInstallCheck = false;
  });
in {
  inherit pass;
  pass-git-helper = super.python3Packages.callPackage ./pkgs/pass-git-helper.nix {
    inherit (super.python3Packages) buildPythonApplication;
    inherit (self.python3Packages) pyxdg;
  };
}
