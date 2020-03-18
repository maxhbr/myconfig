{ config, pkgs, lib, ... }: {
  imports = [
    ./lib
    ./modules/core.nix
    ./modules/vim
    ./modules/zsh
    ./modules/tmux
    ./modules/pass
    ./modules/git
    ./modules/myborgbackup
    ./modules/nixos.networking
    ./modules/nixos.nix.nix
    ./modules/user.mhuber.nix
    ./modules/dic.nix
  ];

  config = {
    environment = {
      shellAliases = {
        upg = "~/myconfig/rebuild.sh";
        upg-fast = "~/myconfig/rebuild.sh --fast";
        upg-fast-no-tmux = "~/myconfig/rebuild.sh --no-tmux --fast";
        upg-dry = "~/myconfig/rebuild.sh --dry-run";
      };
    };
  };
}
