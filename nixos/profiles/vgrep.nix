{ config, pkgs, ... }:

let
  vgrep = pkgs.haskellPackages.callPackage ../pkgs/vgrep.nix {};
in {
  environment.systemPackages = with pkgs; [
    vgrep
  ];
}
