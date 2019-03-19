# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs }:
{
  inherit (pkgs)
    meld
    gnumake cmake automake
    cloc
    pass-git-helper

    python python3

    stack cabal-install cabal2nix;
  inherit (pkgs.gitAndTools)
    gitFull
    tig;
  inherit (pkgs.haskellPackages)
    # cabal-install
    ghc hlint pandoc
    pointfree pointful
    hdevtools;
}
