# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Generates /run/myconfig/docs/models.md from the declared llama-cpp
# model definitions. Only produces output when at least one model is
# configured.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.myconfig.ai.llama-cpp;
  llamaLib = import ./lib { inherit lib pkgs; };
  docsLib = import ./lib/docs.nix { inherit lib; };

  unpackedModels = llamaLib.variants.unpackModels (cfg.models ++ cfg.scriptOnlyModels);
in
{
  config = lib.mkIf (unpackedModels != [ ]) {
    systemd.tmpfiles.rules =
      let
        docsMd = pkgs.writeText "models.md" (docsLib.renderModels unpackedModels);
      in
      [
        "d /run/myconfig/docs 0755 root root - -"
        "f /run/myconfig/docs/models.md 0644 root root - ${docsMd}"
      ];
  };
}
