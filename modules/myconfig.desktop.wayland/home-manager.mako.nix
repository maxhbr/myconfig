{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.mako.enable {
    services.mako = {
      backgroundColor = "#285577BB";
      settings = {
        default-timeout = 5000;
      };
    };
    services.dunst.enable = lib.mkForce false;
  };
}
