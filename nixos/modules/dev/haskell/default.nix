# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ../common.nix
  ];
  config = {
    home-manager.users.mhuber = {
      home.packages = with pkgs; [
        stack cabal-install cabal2nix
      ] ++ (with pkgs.haskellPackages; [
        # cabal-install
        ghc hlint pandoc
        hdevtools
      ]);
      home.file = {
      ".ghci".source = ./ghci;
      };
    };
  };
}
