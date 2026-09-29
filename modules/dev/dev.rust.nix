# Copyright 2019 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.myconfig.dev.rust;
in
{
  config = lib.mkIf cfg.enable {
    # rustup downloads non-Nix toolchains, which do not run in the
    # sandboxes; they get the nixpkgs toolchain instead.
    myconfig.ai.dev.sandboxTools.extraPackages = with pkgs; [
      cargo
      rustc
      rustfmt
      clippy
    ];
    home-manager.sharedModules = [
      {
        home.packages = with pkgs; [
          # rustc
          # cargo
          # cargo-generate
          rustup
          llvmPackages_latest.llvm
          llvmPackages_latest.bintools
          llvmPackages_latest.lld
        ];
      }
    ];
  };
}
