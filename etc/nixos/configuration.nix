{ config, pkgs, ... }:

let hostName = "${builtins.readFile /etc/nix-hostname}";
in rec {
  imports = [
    # Include the specifics of the machine
    "/etc/nixos/${hostName}/configuration.nix"
    # Include the common parts
    ./common.nix
    ./packages.nix
  ];
  networking.hostName = "${hostName}";
}
