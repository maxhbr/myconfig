{ hostName }:
let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  hostId = if builtins.pathExists /etc/nixos/hostid
           then builtins.readFile /etc/nixos/hostid
           else "12345678";
  # for bootstrapping
  importall = import ./lib/helper/importall.nix;
in
{ pkgs, ... }: {
  imports = (let
      path = /etc/nixos/hardware-configuration.nix;
    in if builtins.pathExists path
       then [path]
       else [])
    ++ [ ./lib ]
    # all files in /etc/nixos/imports are sourced
    ++ (importall /etc/nixos/imports)
    # all files in ./imports are sourced
    ++ (importall ../imports)
    # the machine specific configuration is placed at ./hosts/<hostName>.nix
    ++ (let
          path = (./. + "/${hostName}");
        in if builtins.pathExists path
           then [path]
           else []);

  config = {
    networking.hostId = hostId;
    networking.hostName = hostName;
  };
}
