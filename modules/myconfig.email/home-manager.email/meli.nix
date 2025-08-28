{ config, lib, pkgs, ... }:
{
  config = lib.mkIf config.programs.meli.enable {
    programs.meli = {
    };
  };
}
