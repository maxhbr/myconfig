# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# llama.cpp pinned to ggml-org mainline commit 690b87d88151dbd775bb6eff5581663688785928
# ("model : inkling2_loop share the loop slots after the scale pass", #32704),
# a few commits past the merge of #32694 ("Inkling 2 Mini"). The pinned nixpkgs
# llama-cpp 1.0.0 (v1.0.0, 2026-11-13) predates both, so this pin is what makes
# the Inkling-2-Mini GGUFs servable:
#   - `inkling2_loop` arch (ggml-org/Inkling-2-Mini-GGUF layout)
#   - `inkling2v` vision projector (mmproj)
#   - `draft-dspark` in --spec-type
#
# The pin overrides ALL upstream backends (llama-cpp, llama-cpp-vulkan,
# llama-cpp-rocm) the way version-check.nix requires: the `-vulkan`/`-rocm`
# attributes in nixpkgs are plain `.override { vulkanSupport/rocmSupport =
# true; }` of `llama-cpp`, so they compose with this overlay's overrideAttrs
# and land on the same revision automatically (the version-check assertion
# enforces it at eval time).
#
# NOTE: the unsloth/Inkling-2-Mini-GGUF root-layout files (`inkling2` arch,
# 24 physical layers + loop tensors) are NOT loadable by any build reproducible
# from a git tree — Unsloth's official b12588-mix-c0c3de8 prebuilts merge their
# PR pinset at CI build time, so no single source tree exists for them. Serve
# the ggml-org quants instead.
#
# See doc/TODOs/drop-llama-cpp-unsloth-inkling-fork-pin.md for removal.
{ ... }:
let
  rev = "690b87d88151dbd775bb6eff5581663688785928";
  # `version` is only used for the package name and LLAMA_BUILD_NUMBER (a
  # cosmetic string in --version output); the source is pinned by `rev`.
  version = "690b87d";
  hash = "sha256-5uAFmAICM75/TncEPw8NMjH9xguhzYYAVjGix7t8nBk=";
  # Upstream moved the web UI from tools/ui to ui/apps/chat after v1.0.0, so
  # the nixpkgs npmRoot (tools/ui) and npmDepsHash no longer apply.
  npmRoot = "ui/apps/chat";
  npmDepsHash = "sha256-3kJJJGgPvtoRozjjvXJ97ljxzt0tshwi88UUd1D3VsI=";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp = prev.llama-cpp.overrideAttrs (oldAttrs: {
        inherit
          version
          npmRoot
          npmDepsHash
          ;
        src = prev.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          inherit rev hash;
        };
        # ggml-cuda.cu includes <nvml.h> unconditionally after v1.0.0
        # (SKU-aware FP16/BF16 accumulation gating, #30672); the NVML
        # usage is dlopen'd at runtime, only the header is needed.
        # nixpkgs' cudaBuildInputs does not ship it. Added
        # unconditionally: `cudaSupport` is a callPackage argument and
        # invisible to overrideAttrs, the header-only `cuda_nvml_dev` is
        # closure-cheap, and this way the `.override { cudaSupport =
        # true; }` variant that lib/devices.nix builds inherits the fix.
        buildInputs = (oldAttrs.buildInputs or [ ]) ++ [ prev.cudaPackages.cuda_nvml_dev ];
      });
    })
  ];
}
