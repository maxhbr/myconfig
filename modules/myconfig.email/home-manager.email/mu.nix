{ config, lib, pkgs, ... }:

{
  config = lib.mkIf config.programs.mu.enable {
    home.packages = with pkgs; [
      (writeShellScriptBin "runMuIndex" ''
        MAILDIR="$HOME/Maildir"
        if [[ -d "$MAILDIR" ]]; then
          cd "$MAILDIR"
          pkill -2 -u $UID mu
          sleep 1
          ${mu}/bin/mu index
        fi
      '')
    ];
    home.file = {
      ".doom.d/imports/mu4e-base-config.el".source = ./mu4e-base-config.el;
    };
  };
}
