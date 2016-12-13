{ config, pkgs, ... }:

let
  hostName = "${builtins.readFile ./hostname}";
###############################################################################
in {
  imports =
    [
      ./hardware-configuration.nix
      ./configuration-common.nix
      # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
      (./machines + "/${hostName}.nix")
    ];

  networking = {
    # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
    # head -c4 /dev/urandom | od -A none -t x4
    hostId = "54510fe1";
    hostName = "${hostName}";
  };
}

# vim:set ts=2 sw=2 sts=2 et foldmethod=marker foldlevel=0 foldmarker={{{,}}}:
