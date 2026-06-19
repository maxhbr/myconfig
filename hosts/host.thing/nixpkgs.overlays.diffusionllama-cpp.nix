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
          sha256 = "sha256-qCYFGhY162I93xWdD7Xx1tXhCPB48lxIWxWm6cikC90=";
        };
        npmDeps = prev.fetchNpmDeps {
          name = "llama-cpp-diffusion-npm-deps";
          src = prev.fetchFromGitHub {
            inherit owner repo;
            rev = ref;
            sha256 = "sha256-qCYFGhY162I93xWdD7Xx1tXhCPB48lxIWxWm6cikC90=";
          };
          inherit (oldAttrs) patches;
          preBuild = ''
            pushd ${oldAttrs.npmRoot}
          '';
          # Hash will be resolved on first build; replace with real value.
          hash = lib.fakeHash;
        };
        # CUDA is required for the diffusion-gemma GPU offload
        cudaSupport = true;
      });
    })
  ];
}
