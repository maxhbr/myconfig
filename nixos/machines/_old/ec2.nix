{ config, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-config.nix>
    ../profiles/terminal.nix
  ];
}
