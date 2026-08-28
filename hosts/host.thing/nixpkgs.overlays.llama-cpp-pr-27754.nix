# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Sibling attr of the pinned `llama-cpp` (nixpkgs.overlays.llama-cpp.nix)
# with ggml-org/llama.cpp PR #27754 applied:
#   https://github.com/ggml-org/llama.cpp/pull/27754
# ("model: add GLM-5-Next (GLM-5.3-Flash)" — `glm5next` architecture).
#
# Instead of pinning to a release tag and applying a fragile patch that
# breaks every time the base source drifts, this overlay fetches the PR
# head commit directly.  The PR branch lives in the `unslothai` fork:
#   https://github.com/unslothai/llama.cpp/tree/glm5next/upstream
# and is mirrored as `refs/pull/27754/head` on the upstream repo.
#
# Exposes `llama-cpp-pr-27754` on the host's pkgs.  GPU-flag overrides
# are applied by the consumer
# (hosts/host.thing/myconfig.ai.llama-cpp/default.nix:
# `patched-llama-cpp-pr-27754-pkg`) and stay out of this overlay.
{ ... }:
let
  # PR #27754 head commit (unslothai:glm5next/upstream).
  rev = "f30bed88717059d8a4728864c88f8abad8d329a0";
  # `version` is only used for the package name and LLAMA_BUILD_NUMBER
  # (a cosmetic integer printed in --version output).  It does NOT need
  # to correspond to a release tag — the source is pinned by `rev`.
  # The PR head is 52 commits ahead of the b10646 prerelease tag, the
  # nearest upstream build number below it.
  version = "10646";
  hash = "sha256-XviRdFC/jYkxGNM5C3sSZHOEQpqUS1kemA5uFf+mzP8=";
  npmDepsHash = "sha256-2Q7XhaLAArmviOLdQsNbYTfdyDE5pW9lR26cRHEVl9k=";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp-pr-27754 = prev.llama-cpp.overrideAttrs (oldAttrs: {
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
