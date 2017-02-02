{ config, pkgs, ... }:

{
  imports = [
    ../profiles/dev.nix
    ../profiles/vgrep.nix
  ];
}
