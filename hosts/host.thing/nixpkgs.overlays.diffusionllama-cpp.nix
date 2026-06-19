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
          hash = "sha256-pjdbI6NcZRlJVd62xhgbLhWrwFYwgsIwjORqvo1+VD8=";
        };
        # CUDA is required for the diffusion-gemma GPU offload
        cudaSupport = true;
        # Build the diffusion-gemma CLI binary (PR #24423)
        postBuild = (oldAttrs.postBuild or "") + ''
          cmake --build build -j --config Release --target llama-diffusion-cli
        '';
      });
    })
  ];
}
