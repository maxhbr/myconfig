# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Bridge from `myconfig.ai.llama-cpp.models[].pull-models` (declared in
# ./options.nix) to `myconfig.ai.pull_models.models`.
#
# Each llama-cpp model may carry an optional
#
#   pull-models = {
#     target_directory = /home/mhuber/models;
#     hf_spec          = [ "org/repo" "org/repo/sidecar.gguf" ];
#   };
#
# When set, every element of `hf_spec` is appended here to
# `myconfig.ai.pull_models.models.<target_directory>`. Manually-declared
# specs in `pull_models.models` still merge alongside the auto-collected
# ones via standard NixOS module semantics.
#
# This module is part of the llama-cpp module set rather than the
# pull_models module so that the pull_models module stays unaware of
# llama-cpp internals.
{
  config,
  options,
  lib,
  ...
}:
{
  # Only push into `myconfig.ai.pull_models.models` when that option is
  # actually declared in this evaluation. The llama-cpp module set is
  # also imported on its own (e.g. inside the `containers.llama-cpp-*`
  # NixOS containers in hosts/host.thing), where the pull_models module
  # is not in scope; without this guard those evaluations would fail
  # with "option `myconfig.ai.pull_models' does not exist".
  config = lib.optionalAttrs (options ? myconfig.ai.pull_models) {
    myconfig.ai.pull_models.models =
      let
        llamaModels = config.myconfig.ai.llama-cpp.models or [ ];
        withPull = builtins.filter (m: (m.pull-models or null) != null) llamaModels;
      in
      lib.foldl' (
        acc: m:
        let
          dir = toString m.pull-models.target_directory;
          specs = m.pull-models.hf_spec;
          prev = acc.${dir} or [ ];
        in
        acc // { ${dir} = prev ++ specs; }
      ) { } withPull;
  };
}
