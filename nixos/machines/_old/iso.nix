{ config, pkgs, ... }:

{
  imports = [
    ../profiles/openssh.nix
    ../profiles/dev.nix
  ];
}
