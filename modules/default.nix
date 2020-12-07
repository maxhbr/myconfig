{ config, lib, pkgs, ... }:

let
  user = config.myconfig.user;
in {
  imports = [
    # lib
    ./lib
    # modules
    ./nixos.user.nix
    ./nixos.networking
    ./nixos.gc.nix
    ./nixos.nix.nix
    ./make-linux-fast-again.nix
    ./myconfig.nix
    ./service.postgresql.nix
  ];
  config = {
    home-manager.users."${user}" = {
      imports = [

      ];
    };
    assertions = [
      {
        assertion = config.networking.hostId != null;
        message = "config.networking.hostId should be set!";
      }
      {
        assertion = config.networking.hostName != "nixos";
        message = "config.networking.hostName should be set!";
      }
    ];
  };
}
