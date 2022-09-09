{ config, lib, pkgs, ... }:
{
  home-manager.sharedModules = [{
    home.packages = with pkgs;
    [ khal vdirsyncer
    ];
  }];
}
