{ config, lib, pkgs, ... }:

{
  imports = [./neomutt ./astroid.nix ./mu.nix];
  config = {
    home.packages = with pkgs; [
      # offlineimap
      # isync
      abook
      urlview
      # # notmuch
      # sxiv
    ];
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.notmuch = {
      enable = true;
      hooks = {
        preNew = "mbsync --all";
      };
    };
    programs.astroid.enable = true;
    programs.neomutt.enable = true;
    programs.mu.enable = true;
  };
}
