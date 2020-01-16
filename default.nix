{ pkgs, ... }:
let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
  hostId = "${builtins.readFile /etc/nixos/hostid}";
  # for bootstrapping
  importall = import ./lib/helper/importall.nix;

in {
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./lib
    ./modules/core
  ]
    # the machine specific configuration is placed at ./hosts/<hostName>.nix
    ++ (let
          path = /etc/nixos/hardware-configuration.nix;
        in if builtins.pathExists path
             then [path]
             else [])
    # all files in /etc/nixos/imports are sourced
    ++ (importall /etc/nixos/imports)
    # all files in ./imports are sourced
    ++ (importall ./imports)
    # the machine specific configuration is placed at ./hosts/<hostName>.nix
    ++ (let
          path = (./hosts + "/${hostName}.nix");
        in if builtins.pathExists path
             then [path]
             else []);

  config = {
    networking.hostId = "${hostId}";
    networking.hostName = "${hostName}";
    system.copySystemConfiguration = true;

    nixpkgs = {
      config = pkgs: {
        allowUnfree = true;
      };
      overlays = let
          path = ./overlays;
          content = builtins.readDir path;
        in if builtins.pathExists path
          then map (n: import (path + ("/" + n)))
                (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
                  (builtins.attrNames content))
          else [];
    };
    home-manager.users.mhuber = {
      nixpkgs.config.allowUnfree = true;
    };
  };
}
