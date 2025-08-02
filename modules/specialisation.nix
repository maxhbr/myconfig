{
  config,
  pkgs,
  lib,
  myconfig,
  inputs,
  ...
}:
{
  imports = [
    ({ lib, config, pkgs, ... }: {
      config = lib.mkIf (config.specialisation != {}) {
        # Config that should only apply to the default system, not the specialised ones
        system.nixos.tags = [ "default" ];
      };
    })
  ];
  specialisation = {
    experimental = {
      inheritParentConfig = true;
      configuration = {
      };
    };
  };
}