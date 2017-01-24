{ config, pkgs, unstable, ... }:

{
  environment.systemPackages = with pkgs; [
    haskellPackages.vgrep
  ];
}
