# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Markdown renderer for llama-cpp model definitions.
#
# Pure library: takes a list of unpacked model entries (after
# `variants.unpackModels`) and returns the markdown string for
# `/run/myconfig/docs/models.md`.
{ lib }:
let
  # Escape a string for safe use inside a code span.
  esc = s: lib.replaceStrings [ "`" ] [ "\\`" ] s;

  # Format a list of strings as a comma-separated inline code list.
  codeList =
    items: if items == [ ] then "_(none)_" else lib.concatMapStringsSep ", " (s: "`${esc s}`") items;
in
{
  # Generate the full models overview markdown from a list of unpacked
  # model entries. Each entry carries `_kind`, `_baseName`, and
  # `_userTags` from `lib/variants.nix`.
  renderModels =
    models:
    let
      baseModels = builtins.filter (m: m._kind == "base") models;
      variantModels = builtins.filter (m: m._kind == "variant") models;

      # Group variants by their parent base name.
      variantGroups = lib.listToAttrs (
        map (name: {
          inherit name;
          value = builtins.filter (v: v._baseName == name) variantModels;
        }) (lib.unique (map (m: m._baseName) variantModels))
      );

      # Render a model entry. `indent` is prepended to every line
      # (empty for top-level models, "    " for variants nested under
      # their parent).  Variant entries use `#####` headings instead of
      # `###`.
      renderModel =
        indent: m:
        let
          devices = m.devices ++ (m.unlistedDevices or [ ]);
          multiDevice = lib.any (d: lib.hasPrefix "," d) devices;

          # `###` for base models, `#####` for variants.
          heading = if indent == "" then "###" else "#####";

          # Pre-compute optional lines in plain strings to avoid backtick
          # parsing issues inside `''...''` interpolations.
          paramsLine =
            if m.params != [ ] then
              "${indent}  - **Params**: ${lib.concatStringsSep " " (map (p: "`${esc p}`") m.params)}"
            else
              "";
          tensorSplitLine =
            if m.tensorSplit != null then "${indent}  - **Tensor split**: `${esc m.tensorSplit}`" else "";
          cacheTypeLine =
            if m.cacheType != null then
              "${indent}  - **Cache type**: `${esc m.cacheType}`"
            else
              "${indent}  - **Cache type**: _model default_";
          ctxSizeLine =
            if m.ctxSize != null then
              "${indent}  - **Ctx size**: ${toString m.ctxSize}"
            else
              "${indent}  - **Ctx size**: _model default_";
        in
        ''
          ${indent}${heading} ${m.name}
          ${indent}- **Path**: `${esc m.path}`
          ${indent}- **Devices**: ${codeList devices}
          ${indent}- **TTL**: ${toString m.ttl}s
          ${cacheTypeLine}
          ${ctxSizeLine}
          ${indent}- **Parallel**: ${toString m.parallel}
          ${indent}- **Group**: `${esc m.group}`
          ${indent}- **Aliases**: ${codeList m.aliases}
          ${indent}- **Tags**: ${codeList m._userTags}
          ${paramsLine}
          ${tensorSplitLine}
          ${indent}---

        '';

      renderVariants =
        baseName:
        let
          variants = variantGroups.${baseName} or [ ];
        in
        if variants == [ ] then
          ""
        else
          let
            indent = "    ";
          in
          ''
            ${indent}#### Variants

            ${lib.concatMapStrings (v: renderModel indent v) variants}
          '';

      renderBase = m: ''
        ## ${m.name}
        ${renderModel "" m}
        ${renderVariants m.name}
      '';
    in
    ''
      # llama-cpp Models

      Generated from `myconfig.ai.llama-cpp.models`.

      ${lib.concatMapStrings renderBase baseModels}
    '';
}
