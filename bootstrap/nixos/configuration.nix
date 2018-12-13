{ config, pkgs, ... }:

{
  imports = [ 
    ./hardware-configuration.nix 
    ./guest.nix
    ./users.nix
    ./vagrant.nix
  ];

  # we always want git and vim
  environment.systemPackages = with pkgs; [ 
    git git-lfs
    vim
  ];


}
