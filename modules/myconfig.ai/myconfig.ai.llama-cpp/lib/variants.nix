# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Model "variant" expansion: a single declarative model entry with
# `variants = { foo = { ... }; }` is expanded into multiple flat model
# entries (`<name>` plus `<name>-<variantName>`).
#
# NOTE: `applyVariant` currently writes an `args` attribute on the
# resulting model, but every consumer reads `model.params`. As a result,
# per-variant `params` / `mmproj` overrides are silently dropped today;
# only `ctxSize` and `aliases` actually flow through. This refactor
# preserves the existing (buggy) behaviour byte-for-byte; fixing the
# attribute name is left as a follow-up.
{ lib }:
let
  applyVariant =
    variantName: variant: model:
    (builtins.removeAttrs model [ "variants" ])
    // {
      name = "${model.name}-${variantName}";
      inherit (variant) aliases;
      args =
        model.args
        ++ variant.args
        ++ (lib.optionals (variant.mmproj != null) [
          "--mmproj"
          variant.mmproj
        ]);
    }
    // lib.optionalAttrs (variant.ctxSize != null) { inherit (variant) ctxSize; };

  unpackContainedVariants =
    model:
    [ (builtins.removeAttrs model [ "variants" ]) ]
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
