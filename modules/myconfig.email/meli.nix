{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig;
in
{
  config = lib.mkIf cfg.email.enable {
    home-manager.sharedModules = [(
      { config, lib, pkgs, ... }:
      {
        config = lib.mkIf config.programs.meli.enable {
          programs.meli = {
          };
        };
      }
    )];
  };
}
