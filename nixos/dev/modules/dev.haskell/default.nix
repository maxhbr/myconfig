# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../dev.nix
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs.unstable; [
        stack cabal-install cabal2nix
      ] ++ (with pkgs.unstable.haskellPackages; [
        # cabal-install
        ghc hlint pandoc
        hdevtools
      ]);
      home.file = {
        ".ghci".source = ./ghci;
        ".stack/config.yaml".source = ./stack/config.yaml;
      };
    };
  };
}
