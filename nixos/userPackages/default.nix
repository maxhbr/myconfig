# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT

# based on:
#   https://gist.github.com/LnL7/570349866bb69467d0caf5cb175faa74
#   https://discourse.nixos.org/t/declarative-package-management-for-normal-users/1823
#

self: super:

let
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  rolesFile = ../machines/. + ("/" + hostName + ".roles.nix");
  roles = if builtins.pathExists rolesFile
          then import rolesFile
          else [];

in

{
  userPackages = super.userPackages or {} // {
    ############################################################################
    # Default packages:
    cacert = self.cacert;
    nix = self.nix; # don't enable this on multi-user.

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
    myconfig = self.myconfig.all;

    inherit (self)
      # cli:
      ranger
      emacs vim
      elinks w3m
      tmux
      manpages
      taskwarrior
      pass
      ag
      file
      rlwrap

      # admin:
      htop iftop iptraf-ng iotop bmon s-tui
      mtr bind bridge-utils
      pwgen # unstable.mkpasswd
      usbutils
      sysstat
      tcpdump
      cryptsetup
      lsof
      psmisc # contains: killall, pstree

      #others:
      pmount fuse
      rsnapshot

      # my backup tool
      borgbackup;
  } // super.lib.foldr (m1: m2: m1 // m2)
                       {}
                       (map (role: import (./. + ("/" + role + ".nix")) { pkgs = self; })
                            (builtins.filter (role: builtins.pathExists (./. + ("/" + role + ".nix")))
                                             roles));
}
