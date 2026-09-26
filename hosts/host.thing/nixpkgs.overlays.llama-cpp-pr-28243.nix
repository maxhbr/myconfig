# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Build of ggml-org/llama.cpp PR #28243 applied on top of stock
# nixpkgs `llama-cpp`:
#   https://github.com/ggml-org/llama.cpp/pull/28243
# ("models: Qwen3.8-Flash-Next MTP" — multi-token prediction for the
# `qwen4exp` architecture, plus cross-model tensor borrowing for the
# `shared-` draft heads).
#
# Instead of pinning to a release tag and applying a fragile patch that
# breaks every time the base source drifts, this overlay fetches the
# PR head commit directly.  The PR branch lives in the `unslothai` and
# `danielhanchen` forks:
#   https://github.com/unslothai/llama.cpp/tree/mtp/qwen4exp-nextn
#   https://github.com/danielhanchen/llama.cpp/tree/qwen4exp/mtp
# and is mirrored as `refs/pull/28243/head` on the upstream repo.
#
# Why a patched build is needed: the stock nixpkgs llama-cpp (0.5.0,
# which contains merged PR #27742) knows the `qwen4exp` trunk but does
# NOT load the NEXTN (MTP) tensors and has no MTP graph for the
# architecture — see the requirements section of
# https://huggingface.co/unsloth/Qwen3.8-Flash-Next-GGUF/blob/main/MTP/README.md:
# a stock build "has no MTP graph for the qwen4exp architecture, no
# cross-model tensor borrowing, and no --spec-type draft-mtp option".
# This branch adds exactly those (`src/models/qwen4exp.cpp`:
# `graph_mtp`, `mtp_flags = !ml.load_mtp ? TENSOR_SKIP : 0`, and
# `borrow_shared_tensor` in src/llama-model-loader.cpp).
#
# Exposes `llama-cpp-pr-28243` on the host's pkgs.  GPU-flag overrides
# are applied by the consumer
# (hosts/host.thing/myconfig.ai.llama-cpp/default.nix:
# `patched-llama-cpp-pr-28243-pkg`) and stay out of this overlay.
{ ... }:
let
  # PR #28243 head commit (unslothai:mtp/qwen4exp-nextn, merged b10786:
  # "n_ff_exp is per-layer now"). The unsloth MTP README requires the
  # prebuilt tag `b10715-mix-86bd2d3` or newer; this head is b10786+.
  rev = "a9e9c3c5fed8a0bb5cc617532d0d16b8f59c13e0";
  # `version` is only used for the package name and LLAMA_BUILD_NUMBER
  # (a cosmetic integer printed in --version output).  It does NOT need
  # to correspond to a release tag — the source is pinned by `rev`.
  # The PR head is based on upstream b10786.
  version = "10786";
  hash = "sha256-SU0HhkGWpX5GqNPDWDZCxbpKklKpJuu47UTnNWyN7ZQ=";
  # `tools/ui/package-lock.json` is byte-identical to b10408 and to
  # the v0.4.0 release (verified by fetching and
  # hashing all three), so the nixpkgs npmDepsHash still applies.
  npmDepsHash = "sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k=";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp-pr-28243 = prev.llama-cpp.overrideAttrs (oldAttrs: {
        inherit version npmDepsHash;
        src = prev.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          inherit rev hash;
          leaveDotGit = true;
          postFetch = ''
            git -C "$out" rev-parse --short HEAD > $out/COMMIT
            find "$out" -name .git -print0 | xargs -0 rm -rf
          '';
        };
        patches = [ ];
      });
    })
  ];
}
