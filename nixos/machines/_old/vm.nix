{ config, pkgs, ... }:

{
  imports = [
    ../profiles/openssh.nix
  ];
}
