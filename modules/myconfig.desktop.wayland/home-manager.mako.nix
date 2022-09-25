{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.programs.mako.enable {
    programs.mako = {
      backgroundColor = "#285577BB";
      defaultTimeout = 5000;
    };
    services.dunst.enable = lib.mkForce false; # is that causing slack freeze
  };
}
