{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
    <nixpkgs/nixos/modules/services/x11/terminal-server.nix>
  ];
}
