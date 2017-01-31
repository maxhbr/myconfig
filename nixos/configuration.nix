{ config, pkgs, lib, ... }:
let

  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
  hostId = "${builtins.readFile /etc/nixos/hostid}";

  hardwareConfig = import /etc/nixos/hardware-configuration.nix {
    inherit config lib pkgs;
  };

  baseConfig = hardwareConfig // {
    networking = {
      hostId = "${hostId}";
      hostName = "${hostName}";
    };

    nixpkgs.config = import ../nix/nixpkgs-config.nix;
  };

in import ./machines { config = baseConfig; }
