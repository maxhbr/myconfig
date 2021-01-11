# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ pkgs, config, lib, ... }:
let cfg = config.myconfig.dev.haskell;
in {
  config = lib.mkIf cfg.enable {
    home-manager.users.mhuber = {
      home.packages = with pkgs.unstable;
        [ stack cabal-install ] ++ (with haskellPackages; [
          # cabal-install
          ghc
          hlint
          pandoc
          # hdevtools
        ]);
      home.file = {
        ".ghci".source = ./ghci;
        ".stack/config.yaml".source = ./stack/config.yaml;
      };
    };
  };
}
