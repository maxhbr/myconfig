# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ hostName, hostId, otherImports ? [], ... }:
{
  imports = otherImports ++ [
    ./base.nix
    ../roles
  ] ++ (if builtins.pathExists (../machines + "/${hostName}.nix")
        then [(../machines + "/${hostName}.nix")]
        else []);

  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";

  # nix.nixPath = [
  #   # "nixpkgs=channel:nixos-18.03"
  #   # "unstable=channel:nixos-unstable"
  #   ("nixpkgs=" + <nixpkgs>)
  #   # "nixpkgs-overlays=/etc/nix/overlays"
  #   # "nixos-config=/etc/nixos/configuration.nix"
  # ];

  # nixpkgs.overlays = nixpkgs.config.overlays;
  boot.kernel.sysctl = {
    # "fs.inotify.max_user_watches" = 524288;
    "vm.swappiness" = 1;
  };
}
