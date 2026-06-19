# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, ... }:
let
  owner = "ggml-org";
  repo = "llama.cpp";
  # PR #24423: diffusion-gemma support (not yet merged)
  ref = "refs/pull/24423/head";
in
{
  nixpkgs.overlays = [
    (final: prev: {
      diffusionllama-cpp = prev.llama-cpp.overrideAttrs (oldAttrs: {
        src = prev.fetchFromGitHub {
          inherit owner repo;
          rev = ref;
          # Hash will be resolved at build time; override if needed
          sha256 = lib.fakeHash;
        };
        npmDeps = prev.fetchNpmDeps {
          name = "llama-cpp-diffusion-npm-deps";
          src = prev.fetchFromGitHub {
            inherit owner repo;
            rev = ref;
            sha256 = lib.fakeHash;
          };
          inherit (oldAttrs) patches;
          preBuild = ''
            pushd ${oldAttrs.npmRoot}
          '';
          hash = lib.fakeHash;
        };
        # CUDA is required for the diffusion-gemma GPU offload
        cudaSupport = true;
      });
    })
  ];
}
