{ config, pkgs, lib, ... }:

let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
  hostId = "${builtins.readFile /etc/nixos/hostid}";

in import ./core {
  system.copySystemConfiguration = true;
  inherit config hostName hostId;
  otherImports = [ ./hardware-configuration.nix ];
} // {  environment.etc = {
    nixos-orig.source = ./.;
  };
}
