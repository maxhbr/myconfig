# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Model "variant" expansion: a single declarative model entry with
# `variants = { foo = { ... }; }` is expanded into multiple flat model
# entries (`<name>` plus `<name>-<variantName>`).
{ lib }:
let
  # Each unpacked model entry carries three computed fields that
  # downstream publishers (router.nix, llama-swap.nix) propagate into
  # `myconfig.ai.localModels` so consumers can distinguish (and trace
  # the lineage of) the entries they see:
  #
  #   _kind     -> "base" / "variant" — classification of the unpacked
  #                entry. A third localModels kind ("alias") is
  #                emitted later by the publishers, because aliases
  #                never have their own llama-cpp model entry, they
  #                are just additional names attached to a parent
  #                base/variant model.
  #
  #   _baseName -> the *name of the base model this entry derives
  #                from*. For a "base" entry that's its own name; for
  #                a "variant" entry that's `model.name` (the
  #                pre-variant-suffix name). Publishers use this to
  #                build the lineage portion of the `tags` list:
  #                  - base    -> lineage = []
  #                  - variant -> lineage = [ _baseName ]
  #                  - alias of base    -> lineage = [ <base.name> ]
  #                  - alias of variant -> lineage = [ <variant.name>,
  #                                                    <base.name> ]
  #
  #   _userTags -> the user-provided `tags` from the model declaration
  #                (and, for a variant, with the variant's own `tags`
  #                appended). Publishers merge this list after the
  #                lineage tags and dedupe to form the final published
  #                `tags`. Aliases inherit the full `_userTags` of
  #                their parent model entry verbatim — so a tag set on
  #                a base flows into the base entry itself, all of its
  #                variants, and every alias attached to either.
  #
  # Write `params` (not `args`): every consumer (scripts.nix,
  # llama-swap.nix, router.nix) reads `model.params`. The earlier
  # `args = ...` was a typo that silently dropped per-variant `params`
  # and `mmproj` overrides while letting `ctxSize` / `aliases` through.
  # Variants can also override `cacheType` and `parallel`.
  applyVariant =
    variantName: variant: model:
    (builtins.removeAttrs model [ "variants" ])
    // {
      name = "${model.name}-${variantName}";
      _kind = "variant";
      _baseName = model.name;
      _userTags = (model.tags or [ ]) ++ (variant.tags or [ ]);
      inherit (variant) aliases;
      params =
        model.params
        ++ variant.params
        ++ (lib.optionals (variant.mmproj != null) [
          "--mmproj"
          variant.mmproj
        ]);
    }
    // lib.optionalAttrs (variant.ctxSize != null) { inherit (variant) ctxSize; }
    // lib.optionalAttrs (variant.cacheType != null) { inherit (variant) cacheType; }
    // lib.optionalAttrs (variant.parallel != null) { inherit (variant) parallel; };

  unpackContainedVariants =
    model:
    [
      (
        (builtins.removeAttrs model [ "variants" ])
        // {
          _kind = "base";
          _baseName = model.name;
          _userTags = model.tags or [ ];
        }
      )
    ]
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
