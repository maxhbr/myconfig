{ pkgs, config, ... }: {
  imports = [ ./mail.mu4e ./mail.mutt ];
  config = {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        offlineimap
        isync
        abook
        urlview
        # notmuch
        sxiv
        # nixos-unstable.astroid
        (writeShellScriptBin "runMbsync" ''
          MAILDIR="$HOME/Maildir"
          if [[ -f "$MAILDIR/config/mbsyncrc" ]]; then
            mkdir -p "$MAILDIR/mail" "$MAILDIR/tng"
            ${isync}/bin/mbsync -c "$MAILDIR/config/mbsyncrc" -a
          fi
        '')
      ];
    }];
    environment = { systemPackages = with pkgs; [ gnupg msmtp procmail ]; };
    # services.offlineimap = {
    #   enable = false;
    #   path = with pkgs; [ notmuch ];
    # };
  };
}
