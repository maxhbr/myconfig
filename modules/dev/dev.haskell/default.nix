# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.haskell;
  ghc = pkgs.haskellPackages.ghcWithPackages (
    hpkgs: with hpkgs; [
      cabal-install
      hoogle
      hlint
      ghcid
    ]
  );
in
{
  config = lib.mkIf cfg.enable {
    # The ghc env already exposes hlint; a second one would collide in
    # the strict mysbx tool env.
    myconfig.ai.dev.sandboxTools.extraPackages = [ ghc ];
    home-manager.users.mhuber = {
      home.packages =
        with pkgs;
        [
          stack
          sourceHighlight
          haskell-language-server
        ]
        ++ (with haskellPackages; [
          ghc
          hlint
          pandoc
        ]);
      home.file = {
        ".ghci".source = ./ghci;
        ".stack/config.yaml".source = ./stack/config.yaml;
      };
    };
  };
}
