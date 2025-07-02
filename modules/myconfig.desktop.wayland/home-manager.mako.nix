{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.mako.enable {
    services.mako = {
      settings = {
        background-color = "#285577BB";
        default-timeout = 20000;
      };
    };
  };
}
