{ config, pkgs, lib, ... }:
let mybackup = pkgs.callPackage ../pkgs/mybackup { inherit pkgs; };
in {
  imports = [
    ./lib
    ./core.nix
    ./gnupg.nix
    ./vim
    ./zsh
    ./tmux
    ./git
    ./pass
    ./nixos.networking
    ./nixos.nix.nix
    ./user.mhuber.nix
    ./dic.nix
    ./service.openssh.nix
    ./service.syncthing.nix
    ./service.stubby.nix
  ];

  config = {
    environment.systemPackages = [ mybackup ];
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
