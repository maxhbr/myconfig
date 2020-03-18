{ config, pkgs, lib, ... }: let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  hostId = if builtins.pathExists /etc/nixos/hostid
           then builtins.readFile /etc/nixos/hostid
           else "12345678";

  importall = import ./core/lib/helper/importall.nix;
in {
  imports = (let
      path = /etc/nixos/hardware-configuration.nix;
    in if builtins.pathExists path
       then [path]
       else [])
    # all files in /etc/nixos/imports are sourced
    ++ (importall /etc/nixos/imports)
    # all files in ./imports are sourced
    ++ (importall ../imports);
  config = {
    networking.hostId = hostId;
  };
}
