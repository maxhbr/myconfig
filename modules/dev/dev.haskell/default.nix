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
    # `ghc` is the ghcWithPackages env above — it already exposes
    # hlint's binary, so no standalone hlint here.
    myconfig.dev.envs.haskell.packages = with pkgs; [
      stack
      sourceHighlight
      haskell-language-server
      ghc
      pandoc
    ];
    home-manager.users.mhuber = {
      home.file = {
        ".ghci".source = ./ghci;
        ".stack/config.yaml".source = ./stack/config.yaml;
      };
    };
  };
}
