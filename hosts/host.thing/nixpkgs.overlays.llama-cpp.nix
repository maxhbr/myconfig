# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, ... }:
{
  nixpkgs.overlays = [
    (final: prev: {
      llama-cpp = prev.llama-cpp.overrideAttrs (oldAttrs: rec {
        version = "8642";
        src = prev.fetchFromGitHub {
          owner = "ggml-org";
          repo = "llama.cpp";
          tag = "b${version}";
          hash = "sha256-CGoqQd4jdcVlttg1fFkUNW4v3Rxwfkj+TVlcHD59RhI=";
        };
        postPatch = (oldAttrs.postPatch or "") + ''
          echo "7c7d6ce" > COMMIT
        '';
        npmDepsHash = lib.fakeHash;
        npmDeps = prev.fetchNpmDeps {
          name = "llama-cpp-${version}-npm-deps";
          inherit src;
          inherit (oldAttrs) patches;
          preBuild = ''
            pushd ${oldAttrs.npmRoot}
          '';
          hash = lib.fakeHash;
        };
      });
    })
  ];
}
