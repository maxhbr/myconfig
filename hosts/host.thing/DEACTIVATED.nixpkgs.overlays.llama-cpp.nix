# Copyright 2025 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
{ lib, ... }:
let
  version = "8642";
  hash = "sha256-CGoqQd4jdcVlttg1fFkUNW4v3Rxwfkj+TVlcHD59RhI=";
  npmDepsHash = "sha256-DxgUDVr+kwtW55C4b89Pl+j3u2ILmACcQOvOBjKWAKQ=";
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
        };
        postPatch =
          (builtins.replaceStrings [ "rm tools/server/public/index.html.gz" ] [ "" ] (
            oldAttrs.postPatch or ""
          ))
          + ''
            echo "7c7d6ce" > COMMIT
          '';
        inherit npmDepsHash;
        npmDeps = prev.fetchNpmDeps {
          name = "llama-cpp-${version}-npm-deps";
          src = prev.fetchFromGitHub {
            owner = "ggml-org";
            repo = "llama.cpp";
            tag = "b${version}";
            inherit hash;
          };
          inherit (oldAttrs) patches;
          preBuild = ''
            pushd ${oldAttrs.npmRoot}
          '';
          hash = npmDepsHash;
        };
      });
    })
  ];
}
