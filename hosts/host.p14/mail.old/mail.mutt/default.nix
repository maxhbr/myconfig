# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  mailclient-sh =
    pkgs.writeScriptBin "mailclient.sh" (builtins.readFile ./bin/mailclient.sh);
  mailrun-sh =
    pkgs.writeScriptBin "mailrun.sh" (builtins.readFile ./bin/mailrun.sh);
  mutt-bgrun =
    pkgs.writeScriptBin "mutt_bgrun" (builtins.readFile ./bin/mutt_bgrun);
in {
  config = {
    home-manager.sharedModules = [{
      home.packages = with pkgs; [
        # neomutt
        urlview
        sxiv
        mutt-bgrun
        # mailclient-sh
        # mailrun-sh offlineimap
      ];
      home.file = {
        ".muttrc".source = ./muttrc;
        ".mutt" = {
          source = ./mutt;
          recursive = true;
        };
        ".gnupg/toMutt.pl".source = gnupg/toMutt.pl;
        ".mailcap".source = ./mailcap;
      };
    }];
    environment.shellAliases = { mutt = "neomutt"; };
  };
}
