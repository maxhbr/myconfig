{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./imports/guest.nix
    ./imports/users.nix
    ./mhuber.nix
  ];
  environment.systemPackages = with pkgs; [
    git
    git-lfs
    vim
  ];
}
