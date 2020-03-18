{ config, pkgs, lib, ... }: let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
  hostId = if builtins.pathExists /etc/nixos/hostid
           then builtins.readFile /etc/nixos/hostid
           else "12345678";
  # for bootstrapping
  importall = import ./lib/helper/importall.nix;
in {
  imports = (let
      path = /etc/nixos/hardware-configuration.nix;
    in if builtins.pathExists path
       then [path]
       else [])
    ++ [ ./lib ]
    # all files in /etc/nixos/imports are sourced
    ++ (importall /etc/nixos/imports)
    # all files in ./imports are sourced
    ++ (importall ../../imports)
    ++ [
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
    networking.hostId = hostId;
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
