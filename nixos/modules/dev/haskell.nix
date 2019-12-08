# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, ... }:
{
  imports = [
    ./common.nix
  ];
  config = {
    environment.systemPackages = with pkgs; [
      stack cabal-install cabal2nix
    ] ++ (with pkgs.haskellPackages; [
      # cabal-install
      ghc hlint pandoc
      hdevtools
    ]);
  };
}
