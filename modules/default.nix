{ config, lib, pkgs, ... }:

let
  user = config.myconfig.user;
in {
  imports = [
    ./lib
    ./make-linux-fast-again.nix
    ./myconfig.nix
    ./service.postgresql.nix
  ];
  config = {
    home-manager.users."${user}" = {
      imports = [

      ];
    };
  };
}
