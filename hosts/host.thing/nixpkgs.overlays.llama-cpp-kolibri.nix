# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# llama.cpp pinned to hirschsn/llama.cpp commit 5f4d222898ddb12cf581edf1b7e841d80e0b2e17
# = head of PR ggml-org/llama.cpp#32891 ("model : add Kolibri-1.1 support",
# branch sh/kolibri-1.1). This is NOT pushed into pkgs.llama-cpp: only the
# Kolibri-1.1 model entry (myconfig.ai.llmops.llama-cpp/Kolibri-1.1.nix)
# uses it, via `serverPackage` on the ROCm0 device, so every other model
# stays on the inkling-pinned mainline build.
#
# Why a fork pin at all: mainline (and the 690b87d inkling pin) implements
# the `kolibri1` arch for Kolibri-1 ONLY. 1.1 changed the layout (2-block
# dense-FFN prefix, attention indexer, inter-layer K/V sharing) and needs
# this PR's loader plus GGML_OP_ROUND_NVFP4.
#
# ROCm/HIP feasibility (gfx1151): the PR's round-nvfp4.cu guards
# GGML_USE_HIP only on the native cvt-instruction fast path; the shipped
# software-rounding fallback kernel is plain CUDA C that hipcc compiles,
# like every pre-Ada NVIDIA GPU. The lightning-indexer kernels are even
# AMD-aware (amd_wmma_available covers RDNA3.5/RDNA4). Vulkan has NO
# round-nvfp4 kernel — models on this pin must not be served on Vulkan
# devices.
#
# See doc/TODOs/drop-llama-cpp-kolibri-fork-pin.md for removal, once the
# PR (or a successor) lands in mainline AND the inkling pin moves past it.
{ ... }:
let
  rev = "5f4d222898ddb12cf581edf1b7e841d80e0b2e17";
  # `version` is only used for the package name and LLAMA_BUILD_NUMBER (a
  # cosmetic string in --version output); the source is pinned by `rev`.
  version = "5f4d222-kolibri";
  # Same tarball hash as the flake prefetch of the head commit; matches
  # the fetchFromGitHub layout (git tree without .git).
  hash = "sha256-uJwVmoXg3pTVQ5oxbqTKZPiU5HckIlNexBW38fQiMWQ=";
  # Upstream moved the web UI from tools/ui to ui/apps/chat after v1.0.0;
  # the fork is based past that move, and its lockfile matches the inkling
  # pin's (verified: same fetchNpmDeps hash for ui/apps/chat).
  npmRoot = "ui/apps/chat";
  npmDepsHash = "sha256-3kJJJGgPvtoRozjjvXJ97ljxzt0tshwi88UUd1D3VsI=";
in
{
  nixpkgs.overlays = [
    (
      final: prev:
      let
        # Build ONLY the ROCm flavor under this pin (the model entry that
        # uses it is ROCm0-only). Overriding prev.llama-cpp-rocm keeps the
        # closure at one extra build; base/vulkan/cuda stay on the inkling
        # pin — version-check.nix only guards the upstream attributes
        # against each other, and this is a separate fork derivation used
        # solely through `serverPackage`.
        kolibriFork =
          (prev.llama-cpp.override {
            rocmSupport = true;
            # The stock build compiles all 16 gfx archs of clr.gpuTargets
            # (see the inkling pin's rocm drv); the fork only ever runs on
            # this host's iGPU.
            rocmGpuTargets = [ "gfx1151" ];
          }).overrideAttrs
            (oldAttrs: {
              inherit
                version
                npmRoot
                npmDepsHash
                ;
              src = prev.fetchFromGitHub {
                owner = "hirschsn";
                repo = "llama.cpp";
                inherit rev hash;
              };
              pname = "llama-cpp-kolibri";
              # The PR does not update tests/test-llama-archs.cpp for the
              # new kolibri1 hparams its loader requires: the dummy-model
              # fixture (test-generate-models) omits
              # `kolibri1.attention.indexer.candidate_block_size`, so every
              # generated kolibri GGUF fails to load (verified with a
              # plain CPU build; the official GGUF carries the key — the
              # conversion script writes it). test-llama-context only
              # "Not Run"s because its fixture is the same target.
              # Upstream report pending; re-enable when the PR updates
              # the fixtures. See doc/TODOs/fix-llama-cpp-kolibri-test-fixtures.md.
              disabledTests = (oldAttrs.disabledTests or [ ]) ++ [
                "test-generate-models"
                "test-llama-context"
              ];
            });
      in
      {
        llama-cpp-kolibri = kolibriFork;
      }
    )
  ];
}
