# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.haskell;
in {
  config = lib.mkIf cfg.enable {
    home-manager.users.mhuber = {
      home.packages = with pkgs.nixos-unstable;
        [ stack sourceHighlight haskell-language-server ]
        ++ (with haskellPackages; [
          (ghcWithPackages (hpkgs:
            with hpkgs; [
              cabal-install
              hoogle
              hlint
              hindent
              ghcid
              # packunused
              # brittany
              # ghc-mod
            ])) # ghc
          hlint
          pandoc
          # unused
          # hdevtools
        ]);
      home.file = {
        ".ghci".source = ./ghci;
        ".stack/config.yaml".source = ./stack/config.yaml;
      };
    };
  };
}
