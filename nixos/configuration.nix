{ config, pkgs, ... }:

let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile ./hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
  hostId = "${builtins.readFile ./hostid}";
###############################################################################

  pkgsConfig = import ../nix/nixpkgs-config.nix {
    inherit pkgs;
  };

in {
  imports = [
    ./hardware-configuration.nix
    ./configuration-common.nix
    (./machines + "/${hostName}.nix")
  ];

  nixpkgs.config = pkgsConfig;

  networking = {
    hostId = "${hostId}";
    hostName = "${hostName}";
  };
}

# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
