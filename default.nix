{ pkgs, ... }:
let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee /etc/nixos/hostid
  hostId = "${builtins.readFile /etc/nixos/hostid}";

  importall = path:
    if builtins.pathExists path
      then let
          content = builtins.readDir path;
        in map (n: import (path + ("/" + n)))
          (builtins.filter (n: builtins.match ".*\\.nix" n != null || builtins.pathExists (path + ("/" + n + "/default.nix")))
              (builtins.attrNames content))
      else [];
in {
  imports = [/etc/nixos/hardware-configuration.nix]
  # all files in ./imports are sourced
    ++ (importall ./imports)
  # the machine specific configuration is placed at ./hosts/<hostName>.nix
    ++ (let
          path = (./hosts + "/${hostName}.nix");
        in if builtins.pathExists path
             then [path]
             else [])
  # all files in ./default.nix.d are sourced
    ++ (importall ./default.nix.d);

  config = {
    networking.hostId = "${hostId}";
    networking.hostName = "${hostName}";
    system.copySystemConfiguration = true;

    nixpkgs.config = pkgs: {
      allowUnfree = true;
    };
    home-manager.users.mhuber = {
      nixpkgs.config.allowUnfree = true;
    };
  };
}
