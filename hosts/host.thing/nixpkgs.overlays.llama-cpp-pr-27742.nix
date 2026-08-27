# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Sibling attr of the pinned `llama-cpp` (nixpkgs.overlays.llama-cpp.nix)
# with ggml-org/llama.cpp PR #27742 applied:
#   https://github.com/ggml-org/llama.cpp/pull/27742
# ("qwen4exp" architecture — Qwen3.8-Flash-Next support).
#
# Instead of pinning to a release tag and applying a fragile patch that
# breaks every time the base source drifts, this overlay fetches the PR
# head commit directly.  The PR branch lives in the `unslothai` fork:
#   https://github.com/unslothai/llama.cpp/tree/qwen4exp/qwen3.8-flash-next
# and is mirrored as `refs/pull/27742/head` on the upstream repo.
#
# Exposes `llama-cpp-pr-27742` on the host's pkgs.  GPU-flag overrides
# are applied by the consumer
# (hosts/host.thing/myconfig.ai.llama-cpp/default.nix:
# `patched-llama-cpp-pkg`) and stay out of this overlay.
{ ... }:
let
  # PR #27742 head commit (unslothai:qwen4exp/qwen3.8-flash-next).
  rev = "ef6876693f058169161143dc8e301ac104b45373";
  # `version` is only used for the package name and LLAMA_BUILD_NUMBER
  # (a cosmetic integer printed in --version output).  It does NOT need
  # to correspond to a release tag — the source is pinned by `rev`.
  version = "10056";
  hash = "sha256-I/rakfFFIWK7Zeo8duZ9tYppPEpViVLWfT22MEnsPLM=";
  npmDepsHash = "sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k=";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp-pr-27742 = prev.llama-cpp.overrideAttrs (oldAttrs: {
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
