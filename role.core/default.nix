{ config, pkgs, lib, ... }:
let mybackup = pkgs.callPackage ../pkgs/mybackup { inherit pkgs; };
in {
  imports = [
    ../modules
    ./core.nix
    ./gnupg.nix
    ./vim
    ./zsh
    ./fish
    ./tmux
    ./git
    ./pass
    ./dic.nix
    ./service.openssh.nix
    ./service.syncthing.nix
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
