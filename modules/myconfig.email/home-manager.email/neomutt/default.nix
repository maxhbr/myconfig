{ config, lib, pkgs, ... }:

# see:
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt.nix
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt-accounts.nix

{
  programs.neomutt = lib.mkIf config.programs.neomutt.enable {
    sidebar = {
      enable = true;
    };
    sort = "threads";
    vimKeys = true;
    settings = {
      sort_aux = "last-date-received";
    };
    # editor = "emacs";
  };
}
