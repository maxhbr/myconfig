# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on:
#   https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#   https://discourse.nixos.org/t/declarative-package-management-for-normal-users/1823
#
self: super:
{
  userPackages = super.userPackages or {} // {
    ############################################################################
    # Default packages:
    cacert = self.cacert;
    nix = self.nix; # don't enable this on multi-user.
    myconfig = self.myconfig.all;

    ############################################################################
    # script to update
    nix-rebuild = super.writeScriptBin "nix-rebuild" ''
      #!${super.stdenv.shell}
      set -e
      if ! command -v nix-env &>/dev/null; then
        echo "warning: nix-env was not found in PATH, add nix to userPackages" >&2
        PATH=${self.nix}/bin:$PATH
      fi

      nix-env -f '<nixpkgs>' -r -iA userPackages "$@"

      echo "installed user packages:"
      nix-env --query | cat
    '';

    ############################################################################
    # Custom packages:
  };
}
