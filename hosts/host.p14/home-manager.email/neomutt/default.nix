{ config, lib, pkgs, ... }:

# see:
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt.nix
# - https://github.com/nix-community/home-manager/blob/master/modules/programs/neomutt-accounts.nix

{
  home.packages = with pkgs; [
    # offlineimap
    # isync
    # abook
    # urlview
    # # notmuch
    # sxiv
    # # nixos-unstable.astroid
    # (writeShellScriptBin "runMbsync" ''
    #   MAILDIR="$HOME/Maildir"
    #   if [[ -f "$MAILDIR/config/mbsyncrc" ]]; then
    #     mkdir -p "$MAILDIR/mail" "$MAILDIR/tng"
    #     ${isync}/bin/mbsync -c "$MAILDIR/config/mbsyncrc" -a
    #   fi
    # '')
  ];
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
    };
  };
  programs.neomutt = {
    enable = true;
    sidebar = {
      enable = true;
    };
  };
}
