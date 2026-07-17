# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Pins llama.cpp to a specific upstream build tag for this host, scoped via a
# local nixpkgs overlay (does NOT touch the global nixpkgs flake input).
# The fetcher mirrors the upstream package.nix shape exactly (leaveDotGit +
# postFetch) so the COMMIT file is generated from git and the SRI hash is
# computable. The GPU-flag override call site in
# modules/myconfig.ai/myconfig.ai.llama-cpp/services.llama-cpp.nix is left
# untouched: it does `pkgs.llama-cpp.override { cudaSupport = ...; }` on top
# of this overlay, and overrideAttrs/override compose, so the pinned version
# is preserved.
{ ... }:
let
  version = "10056";
  hash = "sha256-1EU1JUHfsTqZYdk55eYY4FHkH7uhLeMCT5Hy5xq7GA0=";
  npmDepsHash = "sha256-6s9skw1wzEfm9QKktTqea3J+oudQAsS6O2VnZEMXAdw="; # unchanged from upstream b9925
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp = prev.llama-cpp.overrideAttrs (oldAttrs: {
        inherit version;
        src = prev.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          tag = "b${version}";
          inherit hash;
          leaveDotGit = true;
          postFetch = ''
            git -C "$out" rev-parse --short HEAD > $out/COMMIT
            find "$out" -name .git -print0 | xargs -0 rm -rf
          '';
        };
        inherit npmDepsHash;
      });
    })
  ];
}
