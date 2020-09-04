# Just a bare configuration.nix for use with LXD
# https://www.srid.ca/2012301.html#running-nixos-in-lxd
{ config, pkgs, ... }:

{
  imports = [ ../../nixos/core/default.nix ];

  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  environment.systemPackages = [ pkgs.tmux ];
}
