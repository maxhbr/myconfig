# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }: {
  config = {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        mu
        gnome3.gnome-keyring # necessary for mu4e?
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
    }];
    environment = {
      shellAliases = {
        mu4e = "${pkgs.emacs}/bin/emacs -name ScratchMu4e &disown";
      };
    };
  };
}
