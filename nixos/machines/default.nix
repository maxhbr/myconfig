{ config, hostName, hostId,
  otherImports ? [],
  ... }:

{
  networking.hostId = "${hostId}";
  networking.hostName = "${hostName}";
  nixpkgs.config = import ../../nix/nixpkgs-config.nix;

  imports = otherImports ++ [
    ../profiles/core
    (../machines + "/${hostName}.nix")
  ];
}
