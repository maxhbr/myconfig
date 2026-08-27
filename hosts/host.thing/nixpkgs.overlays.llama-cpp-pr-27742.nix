# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Sibling attr of the pinned `llama-cpp` (nixpkgs.overlays.llama-cpp.nix)
# with ggml-org/llama.cpp PR #27742 applied:
#   https://github.com/ggml-org/llama.cpp/pull/27742
# Exposes `llama-cpp-pr-27742` on the host's pkgs (same pinned b10056
# source + committed COMMIT/npmDepsHash handling, no new fetch).
#
# NOTE: the pinned source parameters (version/hash/npmDepsHash) are
# duplicated from nixpkgs.overlays.llama-cpp.nix rather than depending on
# prev being the pinned build: the overlay application order for
# `nixpkgs.overlays` does not guarantee that an earlier-listed overlay's
# rewrite of `llama-cpp` is visible to this one, so prev-derived pins
# silently fall back to the unpinned nixpkgs source. Keep the constants
# below in sync with ./nixpkgs.overlays.llama-cpp.nix when bumping.
#
# GPU-flag overrides are applied by the consumer
# (hosts/host.thing/myconfig.ai.llama-cpp/default.nix:
# `patched-llama-cpp-pkg`) and stay out of this overlay.
{ ... }:
let
  # keep in sync with ./nixpkgs.overlays.llama-cpp.nix
  version = "10056";
  hash = "sha256-1EU1JUHfsTqZYdk55eYY4FHkH7uhLeMCT5Hy5xq7GA0=";
  npmDepsHash = "sha256-6s9skw1wzEfm9QKktTqea3J+oudQAsS6O2VnZEMXAdw=";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp-pr-27742 = prev.llama-cpp.overrideAttrs (oldAttrs: {
        inherit version npmDepsHash;
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
        patches = (oldAttrs.patches or [ ]) ++ [ ./llama-cpp-pr-27742.patch ];
      });
    })
  ];
}
