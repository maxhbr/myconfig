# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, myconfig, ... }: {
  config = {
    home-manager.sharedModules = [
      ({ pkgs, lib, config, ... }: {
        config = lib.mkIf config.programs.ripgrep.enable (let
          theme = "Monokai Extended Light";
          aliases = {
            rgi = "rg --no-ignore-vcs";
            ag = "rg";
            grep = "rg";
          };
        in {
          programs.ripgrep = {
            arguments = [ "--max-columns-preview" "--colors=line:style:bold" ];
          };
          programs.fish.shellAbbrs = aliases;
          programs.bash.shellAliases = aliases;
          programs.zsh.shellAliases = aliases;
        });
      })
      { programs.ripgrep.enable = true; }
    ];
  };
}
