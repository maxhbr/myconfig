{ config, ... }:
let
  # echo -n "HOSTNAME" | sudo tee /etc/nixos/hostname
  hostName = "${builtins.readFile /etc/nixos/hostname}";
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done
  hostId = "${builtins.readFile /etc/nixos/hostid}";

in {
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";

  imports = [
    ../profiles/core
    (../machines + "/${hostName}.nix")
  ];
}
