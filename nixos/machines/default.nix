{ config, hostId, hostName, ... }:

{
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  nixpkgs.config = import ../../nix/nixpkgs-config.nix;

  imports = [
    ../profiles/core
    (../machines + "/${hostName}.nix")
  ];
}
