# Copyright 2017-2020 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }: {
  config = {
    home-manager.sharedModules = [
      (let
        ls = "${pkgs.eza}/bin/eza";
        aliases = {
          inherit ls;
          ll = "${ls} -l";
          la = "${ls} -a";
          lt = "${ls} --tree";
          lla = "${ls} -la";
        };
      in {
        home.packages = [ pkgs.eza ];
        programs.bash.shellAliases = aliases;
        programs.zsh.shellAliases = aliases;
        programs.fish.shellAliases = aliases;
      })
    ];
  };
}
