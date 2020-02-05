{ pkgs, ... }:
let
  # echo -n "HOSTNAME" | sudo tee ./hostname
  hostName = if builtins.pathExists ./hostname
             then builtins.readFile ./hostname
             else "minimal";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  hostId = if builtins.pathExists ./hostid
           then builtins.readFile ./hostid
           else "12345678";
  # for bootstrapping
  importall = import ./lib/helper/importall.nix;
in {
  imports = (let
      path = /etc/nixos/hardware-configuration.nix;
    in if builtins.pathExists path
       then [path]
       else [])
    ++ [ ./lib ./profiles/core ]
    # all files in /etc/nixos/imports are sourced
    ++ (importall /etc/nixos/imports)
    # all files in ./imports are sourced
    ++ (importall ./imports)
    # the machine specific configuration is placed at ./hosts/<hostName>.nix
    ++ (let
          path = (./profiles/machines + "/${hostName}");
        in if builtins.pathExists path
           then [path]
           else []);

  config = {
    networking.hostId = hostId;
    networking.hostName = hostName;
  };
}
