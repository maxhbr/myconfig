{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.services.mako.enable {
    services.mako = {
      backgroundColor = "#285577BB";
      settings = { 
        default-timeout = 20000; 
      };
    };
  };
}
