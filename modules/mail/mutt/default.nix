# Copyright 2017 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
let
  mailclient-sh = pkgs.writeScriptBin "mailclient.sh" (builtins.readFile ./bin/mailclient.sh);
  mailrun-sh = pkgs.writeScriptBin "mailrun.sh" (builtins.readFile ./bin/mailrun.sh);
in
{
  imports = [ ../common.nix ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        neomutt
        urlview
        sxiv
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
    };
  };
}
