{ pkgs, ... }:
let
  # cksum /etc/machine-id | while read c rest; do printf "%x" $c; done | sudo tee ./hostid
in
{
  imports = [ ./user.myconfig.nix ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [ nixos-unstable.nixfmt ];
      programs.zsh.shellAliases = {
        upg-get-hostId = ''cksum /etc/machine-id | while read c rest; do printf "%x" $c; done'';
      };
    };
    boot.binfmt.emulatedSystems = [
      "aarch64-linux"
      "armv6l-linux"
    ];
  };
}
