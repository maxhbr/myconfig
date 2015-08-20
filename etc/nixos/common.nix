{ config, pkgs, ... }:

let hostName = "${builtins.readFile /etc/nix-hostname}";
in rec {
  imports = [
    # "/etc/nixos/${hostName}/configuration.nix"
    ./common.nix
    ./packages.nix
  ];
  networking.hostName = "${hostName}";
}
