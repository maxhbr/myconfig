{ config, pkgs, ... }:

let
  vgrep = pkgs.haskellPackages.callPackage ../pkgs/tools/vgrep.nix {};
in {
  environment.systemPackages = with pkgs; [
    vgrep
  ];
}
