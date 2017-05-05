{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/services/x11/terminal-server.nix>
    ./openssh.nix
  ];
}
