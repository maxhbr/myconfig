# Copyright 2026 Maximilian Huber <oss@maximilian-huber.de>
# SPDX-License-Identifier: MIT
#
# Per-device llama-server router backend for the llama-cpp module.
#
# When `myconfig.ai.llama-cpp.router.enable = true`, generates one
# `llama-server_<Device>` home-manager wrapper per device that has at
# least one model declared in `myconfig.ai.llama-cpp.models`. Each
# wrapper runs `llama-server --models-preset <generated.ini>
# --models-max <N>`; the INI contains one section per model for that
# device.
#
# Coexists with ./llama-swap.nix — both backends consume the same
# `models` option and can be enabled independently.
#
# This commit only scaffolds the option subtree. Implementation lands
# in a follow-up commit so the bisect is clean: with `router.enable =
# false` (the default) no code path here evaluates, and the resulting
# toplevel drvPath is identical to the pre-scaffold state.
{ ... }:
{
  config = { };
}
