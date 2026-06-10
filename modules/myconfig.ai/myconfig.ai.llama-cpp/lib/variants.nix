# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Model "variant" expansion: a single declarative model entry with
# `variants = { foo = { ... }; }` is expanded into multiple flat model
# entries (`<name>` plus `<name>-<variantName>`).
{ lib }:
let
  # Each unpacked model entry carries a `_kind` tag that downstream
  # publishers (router.nix, llama-swap.nix) propagate into
  # `myconfig.ai.localModels` so consumers can distinguish the
  # original ("base") model from a generated ("variant") one. The
  # third localModels kind ("alias") is emitted later — at the
  # publisher level — because aliases never have their own llama-cpp
  # model entry, they are just additional names pointing at a
  # base/variant model.
  #
  # Write `params` (not `args`): every consumer (scripts.nix,
  # llama-swap.nix, router.nix) reads `model.params`. The earlier
  # `args = ...` was a typo that silently dropped per-variant `params`
  # and `mmproj` overrides while letting `ctxSize` / `aliases` through.
  applyVariant =
    variantName: variant: model:
    (builtins.removeAttrs model [ "variants" ])
    // {
      name = "${model.name}-${variantName}";
      _kind = "variant";
      inherit (variant) aliases;
      params =
        model.params
        ++ variant.params
        ++ (lib.optionals (variant.mmproj != null) [
          "--mmproj"
          variant.mmproj
        ]);
    }
    // lib.optionalAttrs (variant.ctxSize != null) { inherit (variant) ctxSize; };

  unpackContainedVariants =
    model:
    [ ((builtins.removeAttrs model [ "variants" ]) // { _kind = "base"; }) ]
    ++ map (
      variantName:
      let
        variant = lib.getAttr variantName model.variants;
      in
      applyVariant variantName variant model
    ) (builtins.attrNames model.variants);
in
{
  inherit applyVariant unpackContainedVariants;

  # Convenience: expand a list of model definitions into the flat list of
  # post-variant model entries used everywhere else in the module.
  unpackModels = models: lib.concatMap unpackContainedVariants models;
}
