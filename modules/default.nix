{ config, lib, pkgs, ... }:
let
  user = config.myconfig.user;
  modules = [ # modules
    ./core.nix
    ./make-linux-fast-again.nix
    ./myconfig.nix
    ./myconfig.service.deconz.nix
    ./mybackup.nix
    ./nixos.gc.nix
    ./nixos.networking
    ./nixos.nix.nix
    ./nixos.user.nix
    ./gnupg.nix
    ./pass
    ./service.openssh.nix
    ./service.postgresql.nix
    ./service.syncthing.nix
    ./shell.common
    ./shell.dic.nix
    ./shell.fish
    ./shell.git
    ./shell.tmux
    ./shell.vim
    ./shell.zsh

  ];
  hm-modules = [# home-manager modules

  ];
in {
  imports = [./lib] ++ modules;
  config = {
    home-manager.users."${user}" = {
      imports = hm-modules;
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
