# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, ... }:
let
  user = config.myconfig.user;
  jsonFile = ./. + "/spacemacs.json";
  json = builtins.fromJSON (builtins.readFile jsonFile);
in {
  config = {
    home-manager.users."${user}" = {
      home.file = {
        ".spacemacs.d" = {
          source = builtins.fetchGit {
            url = "https://github.com/syl20bnr/spacemacs.git";
            inherit (json) rev ref;
          };
          recursive = true;
        };
        ".spacemacs".source = ./spacemacs;
        ".spacemacs.d/private" = {
          source = ./spacemacs.d/private;
          recursive = true;
        };
        ".spacemacs.d/private/mykeybindings/keybindings.el" = {
          source = ../doom.d/keybindings.el;
        };
      };
    };
  };
}
