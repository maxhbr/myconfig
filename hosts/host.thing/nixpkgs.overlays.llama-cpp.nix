# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Pins llama.cpp to a specific upstream build tag for this host, scoped via a
# local nixpkgs overlay (does NOT touch the global nixpkgs flake input).
#
# The fetcher mirrors the upstream package.nix shape exactly (leaveDotGit +
# postFetch) so the COMMIT file is generated from git and the SRI hash is
# computable. The GPU-flag override call site in
# modules/myconfig.ai/myconfig.ai.llama-cpp/services.llama-cpp.nix is left
# untouched: it does `pkgs.llama-cpp.override { cudaSupport = ...; }` on top
# of this overlay, and overrideAttrs/override compose, so the pinned version
# is preserved.
#
# This was verified to reach every backend the host actually runs:
#   pkgs.llama-cpp          (base, no GPU backend)
#   pkgs.llama-cpp-vulkan   = llama-cpp.override { vulkanSupport = true; }
#   pkgs.llama-cpp-rocm     = llama-cpp.override { rocmSupport   = true; }
#   pkgs.llama-cpp.override { cudaSupport = true; }   (built ad-hoc in lib/devices.nix)
# all evaluate to the same `version` / `src.rev` because modern nixpkgs
# `overrideAttrs` composes with `override` (makeDerivationExtensible). See
# the version-drift assertion in
# modules/myconfig.ai/myconfig.ai.llama-cpp/version-check.nix.
#
# --- Version selection -----------------------------------------------------
# b10056 (2026-07-16) was the previous pin; it predates Qwen3.8-27B's release
# and the August 2026 Strix-Halo Vulkan improvements, and does NOT include
# PR #25494 (Vulkan: dequant q8_0 KV once in coopmat1, merged 2026-08-19).
#
# The replacement is b10549, the newest STABLE (non-prerelease) upstream tag
# (2026-08-21) that includes PR #25494 and the Qwen3.8 / MTP / ngram-mod /
# draft-dflash speculative functionality. It was chosen over the newer
# prereleases b10642/b10646 (2026-08-26/27) to minimise drift from the
# nixpkgs llama-cpp package.nix this overlay builds on top of (nixpkgs ships
# b10408; b10549 is +141 commits vs ~+590 for b10646), lowering the risk of
# an unbuildable combination that cannot be caught without the hardware.
#
# Recompute reproducibly with (see doc/qwen38-gfx1151/research-notes.md):
#   nix build --impure --no-link --expr '(import <prefetch-src.nix>)'   # -> got: <hash>
#   nix build --impure --no-link --expr '(import <prefetch-npm.nix>)'   # -> got: <npmDepsHash>
# The npmDepsHash is identical to nixpkgs's b10408 value because the
# tools/ui/package-lock.json is byte-identical between b10408, b10549 and
# b10646 (verified, then confirmed by running fetchNpmDeps on b10549).
#
# Runtime build/benchmark testing on the gfx1151 hardware is a required gate
# before promoting any candidate served by this build — see
# doc/qwen38-gfx1151/.
{ ... }:
let
  version = "10549";
  hash = "sha256-ULVNojWLWvNCCqggfrK5+hZqmscbOaqoTa7n5r/jDm8=";
  npmDepsHash = "sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k="; # == nixpkgs b10408 (lock unchanged)
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
