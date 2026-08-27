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

  # Nathanw1014 strix-halo-vulkan fork, pinned to the measured reference
  # commit. Rechecked before pinning: commit 0eb5280 is on the
  # `strix-halo-vulkan` branch (not a moving branch). The fork's
  # `tools/ui/package-lock.json` is byte-identical to b10549's (and
  # b10408's), so it reuses the same npmDepsHash. Built as a Vulkan
  # variant on top of the overlaid b10549 llama-cpp-vulkan so it shares
  # the same build infrastructure (cmake flags, deps) and only swaps
  # the source. Exposed as a SEPARATE derivation (llama-cpp-strix-halo)
  # so the upstream Vulkan/ROCm/CUDA backends stay on b10549.
  forkVersion = "0eb5280-strix-halo";
  forkRev = "0eb528051a56f34567312ce63ab4e14a3fc71d89";
  forkHash = "sha256-2PG8G3P4q+S4TUH4Te/tOStrHqrDycpxJZeiBc+89kI=";
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

      # Pinned Nathanw1014 strix-halo-vulkan fork (Vulkan only). Built on
      # top of the overlaid llama-cpp-vulkan (b10549 build infra +
      # vulkanSupport) with the fork source swapped in. Used ONLY by the
      # DFlash2 candidate via its per-model `serverPackage`; does NOT
      # replace the upstream llama-cpp-vulkan used by every other Vulkan
      # model.
      llama-cpp-strix-halo = final.llama-cpp-vulkan.overrideAttrs (oldAttrs: {
        version = forkVersion;
        src = prev.fetchFromGitHub {
          owner = "Nathanw1014";
          repo = "llama.cpp";
          rev = forkRev;
          hash = forkHash;
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
