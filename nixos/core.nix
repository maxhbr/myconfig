{ config, pkgs, lib, ... }: let
  importall = import ./lib/helper/importall.nix;
  myconfigImports = importall ../imports;

  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  hostId = if builtins.pathExists /etc/nixos/hostid
           then builtins.readFile /etc/nixos/hostid
           else "12345678";
in {
  imports = [
    ./lib
    ./modules/core.nix
    ./modules/gnupg.nix
    ./modules/vim
    ./modules/zsh
    ./modules/tmux
    ./modules/git
    ./modules/pass
    ./modules/myborgbackup
    ./modules/nixos.networking
    ./modules/nixos.nix.nix
    ./modules/user.mhuber.nix
    ./modules/dic.nix
  ] ++ myconfigImports;

  config = {
    environment = {
      shellAliases = {
        upg = "~/myconfig/rebuild.sh";
        upg-fast = "~/myconfig/rebuild.sh --fast";
        upg-fast-no-tmux = "~/myconfig/rebuild.sh --no-tmux --no-git --fast";
        upg-dry = "~/myconfig/rebuild.sh --dry-run";
      };
    };
    networking.hostId = hostId;
  };
}
